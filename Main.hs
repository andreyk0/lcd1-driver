{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeFamilies               #-}


module Main where


import           App
import           Data.Monoid
import           LCDDriver
import           Options.Applicative
import           System.Environment


parseArgs:: Maybe String -- | AMQP user from AMQP_USER env var
         -> Maybe String -- | AMQP password from AMQP_PASSWORD env var
         -> Parser Args
parseArgs maybeAmqpU maybeAmqpP= Args
  <$> option auto
      ( long "verbosity"
     <> short 'v'
     <> value 0
     <> showDefault
     <> help "Verbosity level.")
  <*> strOption
      ( long "host"
     <> value "lcd1"
     <> showDefault
     <> help "LCD host." )
  <*> option auto
     ( long "port"
    <> value 23
    <> showDefault
    <> help "LCD port." )
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


main:: IO ()
main = do
  maybeAmqpU <- lookupEnv "AMQP_USER"
  maybeAmqpP <- lookupEnv "AMQP_PASSWORD"
  let  opts pAargs = info (helper <*> pAargs) (fullDesc <> header "Drives LCD via telnet interface.")
  args <- execParser $ opts $ parseArgs maybeAmqpU maybeAmqpP
  runApp args driveLCD
