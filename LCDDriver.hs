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
import           Control.Monad.Base
import           Control.Monad.Logger
import           Data.Conduit
import qualified Data.Conduit.List as CL
import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Time.LocalTime
import           Data.Time.Zones
import           Home.Sensor.AMQP
import           Home.Sensor.Types
import           Home.Sensor.Zone
import           LCDClient
import           MVConduit
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
  DriverState { dsScreen:: !(Cir DriverScreen)
              , dsTimeZone:: !(Cir (Text,TZ))
              , dsSensorData:: !(Map SensorID SensorDatum)
              } deriving (Eq, Show)


driveLCD:: App ()
driveLCD = do
  Args{..} <- getArgs
  (cmdSrc, lcdSink) <- connectToLCD argsHostName argsPort
  let tmrSrc = timerSource

  mvInput <- newEmptyMVar

  _ <- fork $ cmdSrc =$= CL.map ButtonPress $$ mvSink mvInput
  _ <- fork $ tmrSrc =$= CL.map (const Tick) $$ mvSink mvInput

  let amqpUrl = "amqp://" <> argsAmqpUser <> ":" <> argsAmqpPassword <> "@" <> argsAmqpHost <> ":5672/"
  _ <- fork $ sourceSensorData amqpUrl =$= sampleAndHoldSensors 300 =$= CL.map SensorUpdate $$ mvSink mvInput

  allTzs <- liftBase $ allTimeZones
  let iDs = DriverState allDriverScreens allTzs Map.empty

  mvSource mvInput =$= updateStateConduit iDs =$= renderConduit =$= dedupConduit $$ lcdSink


dedupConduit:: (MonadBase IO m, Eq i)
            => Conduit i m i
dedupConduit = ddc Nothing
  where ddc moI = do
          mI <- await
          case mI
            of Nothing -> ddc moI
               Just i  -> if moI == mI
                          then ddc mI
                          else do yield i
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
             => Conduit DriverState m Text
renderConduit =
  awaitForever $ \dS -> do
    $(logDebugSH) dS
    t <- renderState dS
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
           => DriverState
           -> m Text
renderState dS@DriverState{..} = do
  case cirElem dsScreen
    of DSTime -> renderDsTime dS
       DSTemp -> renderDsTemp dS
       DSPressHum  -> renderDsPressHum dS
       DSGeiger  -> renderDsGeiger dS


renderDsGeiger:: MonadBase IO m
              => DriverState
              -> m Text
renderDsGeiger DriverState{..} = do
  let gcCnts = fmap (gcRate) $ zoneSensorData sdGeigerCount Basement dsSensorData
      gcCnt :: Double = maximum $ 0.0 : gcCnts
  return $ "\0" <> (T.pack $ printf "%.1fcpm" (gcCnt * 60.0))


renderDsPressHum:: MonadBase IO m
                => DriverState
                -> m Text
renderDsPressHum DriverState{..} = do
  let press = renderPressure $ zonePressure dsSensorData Basement
      zHum = zoneHumidity dsSensorData
      hBas = renderHumidity . zHum $ Basement
      hFst = renderHumidity . zHum $ FirstFloor
      hSnd = renderHumidity . zHum $ SecondFloor
  return $ "\0" <> press <> " " <> hBas <> "\n  " <> hFst <> "  " <> hSnd



renderDsTemp:: MonadBase IO m
            => DriverState
            -> m Text
renderDsTemp DriverState{..} = do
  let zTemp = zoneTemperature dsSensorData
      tOut = renderTemperature . zTemp $ Outdoor
      tBas = renderTemperature . zTemp $ Basement
      tFst = renderTemperature . zTemp $ FirstFloor
      tSnd = renderTemperature . zTemp $ SecondFloor
  return $ "\0" <> tOut <> " " <> tBas <> "\n" <> tFst <> " " <> tSnd


renderTemperature:: Temperature
                 -> Text
renderTemperature (Celsius t) = T.pack $ printf "%5.1f\223" t


renderHumidity:: Humidity
              -> Text
renderHumidity (Humidity h) = T.pack $ printf "%4.1f%%" h


renderPressure:: Pressure
              -> Text
renderPressure (Pascal p) = T.pack $ printf "%6.0fPa" p


renderDsTime:: MonadBase IO m
           => DriverState
           -> m Text
renderDsTime DriverState{..} = do
  ts <- liftBase $ getCurrentTime
  let (tzName, tz) = cirElem dsTimeZone
      localTs = utcToLocalTimeTZ tz ts
      tsStr = formatTime defaultTimeLocale timeScreenDateFormat localTs
  return $ "\0" <> T.pack tsStr <> "   " <> tzName


nextDriverState:: DriverInput
               -> DriverState
               -> DriverState
nextDriverState !dIn !dS@DriverState{..} =
  case dIn
    of Tick -> dS
       SensorUpdate sensorData -> dS { dsSensorData = sensorData }
       ButtonPress btn ->
         case btn
           of BUp   -> dS { dsScreen = cirPrev dsScreen }
              BDown -> dS { dsScreen = cirNext dsScreen }
              _     -> case cirElem dsScreen
                         of DSTime -> case btn
                                        of BLeft  -> dS { dsTimeZone = cirNext dsTimeZone }
                                           BRight -> dS { dsTimeZone = cirNext dsTimeZone }
                                           _      -> dS
                            _      -> dS



timeScreenDateFormat:: String
timeScreenDateFormat = "%F %H:%M%n%a %b %d"


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
