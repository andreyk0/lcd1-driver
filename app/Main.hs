{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell   #-}
module Main (main) where

import           Import
import           LCDDriver
import           Options.Applicative.Simple
import qualified Paths_lcd1_driver
import           RIO.Process
import           System.Environment
import           Types

main :: IO ()
main = do

  maybeAmqpU <- lookupEnv "AMQP_USER"
  maybeAmqpP <- lookupEnv "AMQP_PASSWORD"

  (options, ()) <- simpleOptions
    $(simpleVersion Paths_lcd1_driver.version)
    "Drives LCD via telnet interface"
    "Drives LCD via telnet interface"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )

       <*> strOption
           ( long "lcd1-host"
         <> value "lcd1"
         <> showDefault
         <> help "LCD host with display and buttons, telnet interface." )

       <*> strOption
           ( long "lcd2-host"
         <> value "lcd2"
         <> showDefault
         <> help "LCD host with HTTP interface, display output only." )

       <*> strOption
           ( long "amqp-user"
         <> ( case maybeAmqpU
                 of Nothing -> value "lcd1-driver"
                    Just u  -> value u
             )
         <> showDefault
         <> help "AMQP user, defaults to AMQP_USER env var" )

       <*> strOption
           ( long "amqp-password"
         <> short 'P'
         <> ( case maybeAmqpP
                 of Nothing -> mempty
                    Just p  -> value p
             )
         <> help "AMQP password, defaults to AMQP_PASSWORD env var" )

       <*> strOption
           ( long "amqp-host"
         <> value "amqp"
         <> showDefault
         <> help "AMQP host" )
    )
    empty

  lo <- logOptionsHandle stderr (_optionsVerbose options)
  pc <- mkDefaultProcessContext

  withLogFunc lo $ \lf ->
    let app = App
          { _appLogFunc = lf
          , _appProcessContext = pc
          , _appOptions = options
          }
     in runRIO app driveLCD
