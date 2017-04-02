{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}



module App (
  App
, Args(..)
, getArgs
, runApp
) where


import           Control.Monad.Base
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           Control.Monad.Reader.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Reader (ReaderT, runReaderT)


data Args = Args { argsVerbosity:: Int
                 , argsLCD1Host:: String -- ^ telnet interface, has buttons
                 , argsLCD2Host:: String -- ^ HTTP interface, display only
                 , argsAmqpUser:: String
                 , argsAmqpPassword:: String
                 , argsAmqpHost:: String
                 } deriving (Eq, Show)


newtype App a =
  App { unApp:: ReaderT Args (LoggingT IO) a
      } deriving ( Applicative
                 , Functor
                 , Monad
                 , MonadBase IO
                 , MonadIO
                 , MonadLogger
                 , MonadLoggerIO
                 , MonadReader Args
                 , MonadThrow
                 , MonadCatch
                 )


instance MonadBaseControl IO App where
  type StM App a = StM (ReaderT Args (LoggingT IO)) a
  liftBaseWith f = App $ liftBaseWith $ \run -> f (run . unApp)
  restoreM = App . restoreM


runApp:: Args
      -> App a
      -> IO a
runApp args@Args{..} app = do
  let llf = case argsVerbosity
              of 0 -> (>LevelWarn)
                 1 -> (>LevelInfo)
                 _ -> (\_ -> True)

  runStderrLoggingT $ filterLogger (\_ ll -> llf ll) $ runReaderT (unApp app) args


getArgs:: App Args
getArgs = ask
