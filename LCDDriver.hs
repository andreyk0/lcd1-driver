{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module LCDDriver (
  driveLCD
) where


import           App
import           Cir
import           Control.Concurrent.Lifted hiding (yield)
import           Control.Lens
import           Control.Monad (void)
import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.Logger
import           Data.ByteString (ByteString)
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Foldable
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.Zones
import           Home.Sensor.AMQP
import           Home.Sensor.Types
import           Home.Sensor.Zone
import           LCDClient
import           MVConduit
import           Network.HTTP.Client (defaultManagerSettings, managerResponseTimeout, responseTimeoutMicro)
import           Network.Wreq
import           Text.Printf


data DriverInput = Tick
                 | ButtonPress !Button
                 | SensorUpdate !(Map SensorID SensorDatum)
                 deriving (Eq, Show)


data DriverScreen = DSTime
                  | DSTemp
                  | DSPressHum
                  | DSGeiger
                  deriving (Eq, Show, Enum)


data DriverState =
  DriverState { dsScreenLCD1:: !(Cir DriverScreen)
              , dsScreenLCD2:: !(Cir DriverScreen)
              , dsTimeZone:: !(Cir (Text,TZ))
              , dsSensorData:: !(Map SensorID SensorDatum)
              } deriving (Eq)

instance Show DriverState where
  show DriverState{..} = "DriverState{ dsScreenLCD1=" <> (show . cirElem) dsScreenLCD1 <>
                         ", dsScreenLCD2=" <> (show . cirElem) dsScreenLCD2 <>
                         ", dsTimeZone=" <> (T.unpack . fst . cirElem) dsTimeZone <>
                         ", dsSensorData=" <> show dsSensorData <>
                         "}"



data LCDText =
  LCDText { lcdtClear:: !Bool
          , lcdtLine1:: !Text
          , lcdtLine2:: !Text
          } deriving (Eq, Show)



driveLCD:: App ()
driveLCD = do
  Args{..} <- getArgs
  (cmdSrc, lcd1Sink) <- connectToLCD argsLCD1Host 23
  let tmrSrc = timerSource

  mvInput <- newEmptyMVar

  _ <- fork $ cmdSrc =$= CL.map ButtonPress $$ mvSink mvInput
  _ <- fork $ tmrSrc =$= CL.map (const Tick) $$ mvSink mvInput

  let amqpUrl = "amqp://" <> argsAmqpUser <> ":" <> argsAmqpPassword <> "@" <> argsAmqpHost <> ":5672/"
  _ <- fork $ sourceSensorData amqpUrl =$= sampleAndHoldSensors 300 =$= CL.map SensorUpdate $$ mvSink mvInput

  allTzs <- liftBase $ allTimeZones

  (stateOut, lcd1TextSrc : lcd2TextSrc : []) <- mvDup 2

  let iDs = DriverState (cirNext allDriverScreens) allDriverScreens allTzs Map.empty

      !lcd2Url = "http://" <> argsLCD2Host <> "/"
      lcd2Sink = do $(logInfo) $ "lcd2Sink awaiting input ..."
                    mT <- await
                    $(logInfo) $ "lcd2 txt: " <> (T.pack . show) mT
                    forM_ mT $ \LCDText{..} -> do
                      let opts = defaults & ( if lcdtClear then (param "clear" .~ ["true"]) else id)
                                          & param "line1" .~ [ lcdtLine1 ]
                                          & param "line2" .~ [ lcdtLine2 ]
                                          & manager .~ Left (defaultManagerSettings { managerResponseTimeout = responseTimeoutMicro 3000000 } )
                      catch (void . liftBase $ postWith opts lcd2Url ("" :: ByteString))
                            (\ (e::SomeException) -> $(logErrorSH) e)
                    lcd2Sink

  _ <- fork $ lcd1TextSrc =$= renderConduit dsScreenLCD1 =$= dedupConduit =$= CL.map renderLCD1 $$ lcd1Sink

  _ <- fork $ lcd2TextSrc =$= renderConduit dsScreenLCD2 =$= dedupConduit $$ lcd2Sink

  -- feed 2 LCDs from the same stream of state changes
  mvSource mvInput =$= updateStateConduit iDs $$ stateOut


dedupConduit:: (MonadLogger m, MonadBase IO m, Eq i, Show i)
            => Conduit i m i
dedupConduit = ddc Nothing
  where ddc !moI = do
          mI <- await
          case mI
            of Nothing -> ddc moI
               Just i  -> if moI == mI
                          then ddc mI
                          else do $(logInfoSH) i
                                  yield i
                                  ddc mI


updateStateConduit:: (MonadBase IO m, MonadLogger m)
                  => DriverState -- ^ initial state
                  -> Conduit DriverInput m DriverState
updateStateConduit !dS = do
  mI <- await
  case mI
    of Nothing -> updateStateConduit dS
       Just i -> do let dS' = nextDriverState i dS
                    yield dS'
                    updateStateConduit dS'


renderConduit:: (MonadBase IO m, MonadLogger m)
             => (DriverState -> Cir DriverScreen) -- ^ which screen
             -> Conduit DriverState m LCDText
renderConduit screen =
  awaitForever $ \dS -> do
    $(logDebugSH) dS
    t <- renderState (screen dS) dS
    yield t


-- | Ticks once per minute, close to the start of the minute.
timerSource:: (MonadBase IO m, MonadLogger m)
           => Source m ()
timerSource = do
  ts <- liftBase $ getCurrentTime

  let dt = utctDayTime ts
      sleepT = 61 - ( (ceiling dt) `mod` 60 )

  threadDelay $ (sleepT * 1000000)

  $(logInfo) "Tick"

  yield ()

  timerSource


renderState:: MonadBase IO m
           => Cir DriverScreen
           -> DriverState
           -> m LCDText
renderState screen dS@DriverState{..} = do
  case cirElem screen
    of DSTime -> renderDsTime dS
       DSTemp -> renderDsTemp dS
       DSPressHum  -> renderDsPressHum dS
       DSGeiger  -> renderDsGeiger dS


renderDsGeiger:: MonadBase IO m
              => DriverState
              -> m LCDText
renderDsGeiger DriverState{..} = do
  let gcCnts = fmap (gcRate) $ zoneSensorData sdGeigerCount Basement dsSensorData
      gcCnt :: Double = maximum $ 0.0 : gcCnts
  return $ LCDText True (T.pack $ printf "%.1fcpm" (gcCnt * 60.0)) ""


renderDsPressHum:: MonadBase IO m
                => DriverState
                -> m LCDText
renderDsPressHum DriverState{..} = do
  let press = renderPressure $ zonePressure dsSensorData Basement
      zHum = zoneHumidity dsSensorData
      hBas = renderHumidity . zHum $ Basement
      hFst = renderHumidity . zHum $ FirstFloor
      hSnd = renderHumidity . zHum $ SecondFloor
  return $ LCDText True (press <> " " <> hBas) (hFst <> "  " <> hSnd)



renderDsTemp:: MonadBase IO m
            => DriverState
            -> m LCDText
renderDsTemp DriverState{..} = do
  let zTemp = zoneTemperature dsSensorData
      tOut = renderTemperature . zTemp $ Outdoor
      tBas = renderTemperature . zTemp $ Basement
      tFst = renderTemperature . zTemp $ FirstFloor
      tSnd = renderTemperature . zTemp $ SecondFloor
  return $ LCDText True (tOut <> " " <> tBas) (tFst <> " " <> tSnd)


renderTemperature:: Temperature
                 -> Text
renderTemperature (Celsius t) = T.pack $ printf "%5.1fC" t


renderHumidity:: Humidity
              -> Text
renderHumidity (Humidity h) = T.pack $ printf "%4.1f%%" h


renderPressure:: Pressure
              -> Text
renderPressure (Pascal p) = T.pack $ printf "%6.0fPa" p


renderDsTime:: MonadBase IO m
           => DriverState
           -> m LCDText
renderDsTime DriverState{..} = do
  ts <- liftBase $ getCurrentTime
  let (tzName, tz) = cirElem dsTimeZone
      localTs = utcToLocalTimeTZ tz ts
      tsStr1 = formatTime defaultTimeLocale "%F %H:%M" localTs
      tsStr2 = formatTime defaultTimeLocale "%a %b %d" localTs
  return $ LCDText True (T.pack tsStr1) (T.pack tsStr2 <> "   " <> tzName)


nextDriverState:: DriverInput
               -> DriverState
               -> DriverState
nextDriverState !dIn !dS@DriverState{..} =
  case dIn
    of Tick -> dS
       SensorUpdate sensorData -> dS { dsSensorData = sensorData }
       ButtonPress btn ->
         case btn
           of BUp     -> dS { dsScreenLCD1 = cirPrev dsScreenLCD1 }
              BDown   -> dS { dsScreenLCD1 = cirNext dsScreenLCD1 }
              BLeft   -> dS { dsScreenLCD2 = cirPrev dsScreenLCD2 }
              BRight  -> dS { dsScreenLCD2 = cirNext dsScreenLCD2 }
              BSelect -> dS { dsTimeZone   = cirNext dsTimeZone   }


allDriverScreens:: Cir DriverScreen
allDriverScreens = cirFromList $ enumFromTo DSTime DSGeiger


allTimeZones:: IO (Cir (Text,TZ))
allTimeZones = do
  ny <- loadSystemTZ "America/New_York"
  kv <- loadSystemTZ "Europe/Kiev"
  return $ cirFromList [ (" NY", ny), (" KV", kv), ("UTC", utcTZ) ]


zonePressure:: Map SensorID SensorDatum
               -> Zone
               -> Pressure
zonePressure s2d z = zoneSensorAvg sdPressure z s2d


zoneHumidity:: Map SensorID SensorDatum
               -> Zone
               -> Humidity
zoneHumidity s2d z = zoneSensorAvg sdHumidity z s2d


zoneTemperature:: Map SensorID SensorDatum
               -> Zone
               -> Temperature
zoneTemperature s2d z = zoneSensorAvg sdTemperature z s2d


zoneSensorAvg:: (Ord a, Fractional a)
             => (SensorDatum -> Maybe a)
             -> Zone
             -> Map SensorID SensorDatum
             -> a
zoneSensorAvg getSd z s2d = if cnt > 0 then suma / cnt else 0
  where as = zoneSensorData getSd z s2d
        (suma,cnt) = foldl (\(suma',cnt') a -> (suma'+a,cnt'+1)) (0,0) as


zoneSensorData:: (SensorDatum -> Maybe a)
              -> Zone
              -> Map SensorID SensorDatum
              -> [a]
zoneSensorData getSd z s2d =
  catMaybes $! fmap (getSd) $! catMaybes $! fmap (\sid -> Map.lookup sid s2d) zss
  where zss = zoneSensors z


-- | LCD1 interprets special chars in a string,
--   TCP interface.
renderLCD1:: LCDText
          -> Text
renderLCD1 LCDText{..} =
  (if lcdtClear then "\0" else "") <>
    lcdtLine1 <> "\n" <> lcdtLine2
