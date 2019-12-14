{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE StrictData        #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

import           Import
import           RIO.Process

-- | Command line arguments
data Options = Options
  { _optionsVerbose      :: Bool
  , _optionsLcd1Host     :: String -- ^ telnet interface, has buttons
  , _optionsLcd2Host     :: String -- ^ HTTP interface, display only
  , _optionsAmqpUser     :: String
  , _optionsAmqpPassword :: String
  , _optionsAmqpHost     :: String
  } deriving (Eq, Show)

makeLenses ''Options


data App = App
  { _appLogFunc        :: LogFunc
  , _appProcessContext :: ProcessContext
  , _appOptions        :: Options
  }

makeLenses ''App

instance HasLogFunc App where
  logFuncL = appLogFunc

instance HasProcessContext App where
  processContextL = appProcessContext


class HasCliOptions a where
  getCliOptions :: SimpleGetter a Options

instance HasCliOptions App where
  getCliOptions = appOptions
