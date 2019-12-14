{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}
{-# LANGUAGE TypeFamilies        #-}


module LCDDriver
  ( driveLCD
  )
where


import           Cir
import           Conduit
import           Data.ByteString     (ByteString)
import qualified Data.Conduit.List   as CL
import           Data.Foldable
import           Data.Time.Zones
import           Home.Sensor.AMQP
import           Home.Sensor.Types
import           Home.Sensor.Zone
import           Import
import           LCDClient
import           MVConduit
import           Network.HTTP.Client (defaultManagerSettings,
                                      managerResponseTimeout,
                                      responseTimeoutMicro)
import           Network.Wreq
import qualified RIO.Map             as Map
import qualified RIO.Text            as T
import           RIO.Time
import           Text.Printf
import           Types
import qualified UnliftIO.Concurrent as Conc


data DriverInput
  = Tick
  | ButtonPress Button
  | SensorUpdate (Map SensorID SensorDatum)
  deriving (Eq, Show)


data DriverScreen
  = DSTime
  | DSTemp
  | DSPressHum
  | DSGeiger
  deriving (Eq, Show, Enum)


data DriverState = DriverState
  { dsScreenLCD1 :: Cir DriverScreen
  , dsScreenLCD2 :: Cir DriverScreen
  , dsTimeZone   :: Cir (Text,TZ)
  , dsSensorData :: Map SensorID SensorDatum
  } deriving (Eq)

instance Show DriverState where
  show DriverState {..} =
    "DriverState{ dsScreenLCD1="
      <> (show . cirElem) dsScreenLCD1
      <> ", dsScreenLCD2="
      <> (show . cirElem) dsScreenLCD2
      <> ", dsTimeZone="
      <> (T.unpack . fst . cirElem) dsTimeZone
      <> ", dsSensorData="
      <> show dsSensorData
      <> "}"


data LCDText = LCDText
  { lcdtClear :: Bool
  , lcdtLine1 :: Text
  , lcdtLine2 :: Text
  } deriving (Eq, Show)


driveLCD :: HasLogFunc env => HasCliOptions env => RIO env ()
driveLCD = do
  Options {..}       <- view getCliOptions
  (cmdSrc, lcd1Sink) <- connectToLCD _optionsLcd1Host 23
  let tmrSrc = timerSource

  mvInput <- newEmptyMVar

  void $ Conc.forkFinally
    (runConduit $ cmdSrc .| CL.map ButtonPress .| mvSink mvInput)
    (logError . displayShow)
  void $ Conc.forkFinally
    (runConduit $ tmrSrc .| CL.map (const Tick) .| mvSink mvInput)
    (logError . displayShow)

  let amqpUrl =
        "amqp://"
          <> _optionsAmqpUser
          <> ":"
          <> _optionsAmqpPassword
          <> "@"
          <> _optionsAmqpHost
          <> ":5672/"

  void $ Conc.forkFinally
    (  runResourceT
    $  runConduit
    $  sourceSensorData amqpUrl
    .| sampleAndHoldSensors 300
    .| CL.map SensorUpdate
    .| mvSink mvInput
    ) (logError . displayShow)

  allTzs                               <- liftIO allTimeZones

  txtSrcDup                            <- mvDup 2
  (stateOut, lcd1TextSrc, lcd2TextSrc) <- case txtSrcDup of
    (o, [s1, s2]) -> pure (o, s1, s2)
    _             -> error "mvDup :: wrong results"

  let iDs =
        DriverState (cirNext allDriverScreens) allDriverScreens allTzs Map.empty

      !lcd2Url = "http://" <> _optionsLcd2Host <> "/"

      lcd2Sink = do
        logInfo "lcd2Sink awaiting input ..."
        mT <- await
        logInfo . display $ "lcd2 txt: " <> tshow mT
        forM_ mT $ \LCDText {..} -> do
          let opts =
                defaults
                  &  (if lcdtClear then param "clear" .~ ["true"] else id)
                  &  param "line1"
                  .~ [lcdtLine1]
                  &  param "line2"
                  .~ [lcdtLine2]
                  &  manager
                  .~ Left
                       (defaultManagerSettings
                         { managerResponseTimeout = responseTimeoutMicro 3000000
                         }
                       )
          postRes <- liftIO . tryAny $ postWith opts lcd2Url ("" :: ByteString)
          either (logError . displayShow) (const (pure ())) postRes

        lcd2Sink

  void $ Conc.forkFinally
    ( runConduit
    $ lcd1TextSrc
    .| renderConduit dsScreenLCD1
    .| dedupConduit
    .| CL.map renderLCD1
    .| lcd1Sink
    ) (logError . displayShow)

  void $ Conc.forkFinally
    ( void
    $ runConduit
    $ lcd2TextSrc
    .| renderConduit dsScreenLCD2
    .| dedupConduit
    .| lcd2Sink
    ) (logError . displayShow)

  -- feed 2 LCDs from the same stream of state changes
  runConduit $ mvSource mvInput .| updateStateConduit iDs .| stateOut


dedupConduit :: Eq i => Show i => HasLogFunc env => ConduitT i i (RIO env) ()
dedupConduit = ddc Nothing
 where
  ddc !moI = do
    mI <- await
    case mI of
      Nothing -> ddc moI
      Just i  -> if moI == mI
        then ddc mI
        else do
          logInfo . displayShow $ i
          yield i
          ddc mI


updateStateConduit
  :: DriverState -- ^ initial state
  -> ConduitT DriverInput DriverState (RIO env) ()
updateStateConduit !dS = do
  mI <- await
  case mI of
    Nothing -> updateStateConduit dS
    Just i  -> do
      let dS' = nextDriverState i $! dS
      yield $! dS'
      updateStateConduit $! dS'


renderConduit
  :: HasLogFunc env
  => (DriverState -> Cir DriverScreen) -- ^ which screen
  -> ConduitT DriverState LCDText (RIO env) ()
renderConduit screen = awaitForever $ \dS -> do
  logDebug . displayShow $ dS
  t <- lift $ renderState (screen dS) dS
  yield t


-- | Ticks once per minute, close to the start of the minute.
timerSource :: HasLogFunc env => ConduitT () () (RIO env) ()
timerSource = do
  ts <- getCurrentTime

  let dt     = utctDayTime ts
      sleepT = 61 - (ceiling dt `mod` 60)

  threadDelay (sleepT * 1000000)
  logInfo "Tick"
  yield ()
  timerSource


renderState :: Cir DriverScreen -> DriverState -> RIO env LCDText
renderState screen dS@DriverState {..} = case cirElem screen of
  DSTime     -> renderDsTime dS
  DSTemp     -> renderDsTemp dS
  DSPressHum -> renderDsPressHum dS
  DSGeiger   -> renderDsGeiger dS


renderDsGeiger :: DriverState -> RIO env LCDText
renderDsGeiger DriverState {..} = do
  let gcCnts = gcRate <$> zoneSensorData sdGeigerCount Basement dsSensorData
      gcCnt :: Double = maximum $ 0.0 : gcCnts
  return $! LCDText True (T.pack $ printf "%.1fcpm" (gcCnt * 60.0)) ""


renderDsPressHum :: DriverState -> RIO env LCDText
renderDsPressHum DriverState {..} = do
  let press = renderPressure $ zonePressure dsSensorData Basement
      zHum  = zoneHumidity dsSensorData
      hBas  = renderHumidity . zHum $ Basement
      hFst  = renderHumidity . zHum $ FirstFloor
      hSnd  = renderHumidity . zHum $ SecondFloor
  return $! LCDText True (press <> " " <> hBas) (hFst <> "  " <> hSnd)



renderDsTemp :: DriverState -> RIO env LCDText
renderDsTemp DriverState {..} = do
  let zTemp = zoneTemperature dsSensorData
      tOut  = renderTemperature . zTemp $ Outdoor
      tBas  = renderTemperature . zTemp $ Basement
      tFst  = renderTemperature . zTemp $ FirstFloor
      tSnd  = renderTemperature . zTemp $ SecondFloor
  return $! LCDText True (tOut <> " " <> tBas) (tFst <> " " <> tSnd)


renderTemperature :: Temperature -> Text
renderTemperature (Celsius t) = T.pack $! printf "%5.1fC" t


renderHumidity :: Humidity -> Text
renderHumidity (Humidity h) = T.pack $! printf "%4.1f%%" h


renderPressure :: Pressure -> Text
renderPressure (Pascal p) = T.pack $! printf "%6.0fPa" p


renderDsTime :: DriverState -> RIO env LCDText
renderDsTime DriverState {..} = do
  ts <- liftIO getCurrentTime
  let (tzName, tz) = cirElem dsTimeZone
      localTs      = utcToLocalTimeTZ tz ts
      tsStr1       = formatTime defaultTimeLocale "%F %H:%M" localTs
      tsStr2       = formatTime defaultTimeLocale "%a %b %d" localTs
  return $ LCDText True (T.pack tsStr1) (T.pack tsStr2 <> "   " <> tzName)


nextDriverState :: DriverInput -> DriverState -> DriverState
nextDriverState !dIn dS@DriverState {..} = case dIn of
  Tick                    -> dS
  SensorUpdate sensorData -> dS { dsSensorData = sensorData }
  ButtonPress  btn        -> case btn of
    BUp     -> dS { dsScreenLCD1 = cirPrev dsScreenLCD1 }
    BDown   -> dS { dsScreenLCD1 = cirNext dsScreenLCD1 }
    BLeft   -> dS { dsScreenLCD2 = cirPrev dsScreenLCD2 }
    BRight  -> dS { dsScreenLCD2 = cirNext dsScreenLCD2 }
    BSelect -> dS { dsTimeZone = cirNext dsTimeZone }


allDriverScreens :: Cir DriverScreen
allDriverScreens = cirFromList $ DSTime :| [DSTemp .. DSGeiger]


allTimeZones :: IO (Cir (Text, TZ))
allTimeZones = do
  ny <- loadSystemTZ "America/New_York"
  kv <- loadSystemTZ "Europe/Kiev"
  return $ cirFromList $ (" NY", ny) :| [(" KV", kv), ("UTC", utcTZ)]


zonePressure :: Map SensorID SensorDatum -> Zone -> Pressure
zonePressure s2d z = zoneSensorAvg sdPressure z s2d


zoneHumidity :: Map SensorID SensorDatum -> Zone -> Humidity
zoneHumidity s2d z = zoneSensorAvg sdHumidity z s2d


zoneTemperature :: Map SensorID SensorDatum -> Zone -> Temperature
zoneTemperature s2d z = zoneSensorAvg sdTemperature z s2d


zoneSensorAvg
  :: (Ord a, Fractional a)
  => (SensorDatum -> Maybe a)
  -> Zone
  -> Map SensorID SensorDatum
  -> a
zoneSensorAvg getSd z s2d = if cnt > 0 then suma / cnt else 0
 where
  as          = zoneSensorData getSd z s2d
  (suma, cnt) = foldl (\(suma', cnt') a -> (suma' + a, cnt' + 1)) (0, 0) as


zoneSensorData
  :: (SensorDatum -> Maybe a) -> Zone -> Map SensorID SensorDatum -> [a]
zoneSensorData getSd z s2d =
  catMaybes $! fmap getSd $! catMaybes $! fmap (`Map.lookup` s2d) zss
  where zss = zoneSensors z


-- | LCD1 interprets special chars in a string,
--   TCP interface.
renderLCD1 :: LCDText -> Text
renderLCD1 LCDText {..} =
  (if lcdtClear then "\0" else "") <> lcdtLine1 <> "\n" <> lcdtLine2
