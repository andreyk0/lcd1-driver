{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TemplateHaskell       #-}


module LCDClient (
  Button(..)
, connectToLCD
) where


import           Control.Concurrent.Lifted hiding (yield)
import           Control.Exception.Lifted
import           Control.Monad.Base
import           Control.Monad.Catch (MonadThrow)
import           Control.Monad.Logger
import           Control.Monad.Trans (lift)
import           Control.Monad.Trans.Control
import           Data.ByteString (ByteString)
import           Data.Conduit
import qualified Data.Conduit.Text as CT
import           Data.IORef.Lifted
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           LCDSocket
import           MVConduit
import           Network.Socket (Socket, close)



data Button = BUp | BDown | BLeft | BRight | BSelect
              deriving (Eq, Show)


data LCDData = LCDButtonPress Button
             | LCDHeartBeat
             deriving (Eq, Show)


-- | (Re)connects to LCD host.
--   Provides an infinite source of button presses and a sink of byte strings.
connectToLCD:: (MonadBase IO m, MonadBaseControl IO m, MonadThrow m, MonadLogger m)
            => String -- ^ LCD host name
            -> Int -- ^ LCD port
            -> m ( (Source m Button) , (Sink Text m ()) )
connectToLCD lcdHostName lcdPort = do
  (inMv, outMv) <- connectToLCDMVar lcdHostName lcdPort

  let decodeBtn = do maybeT <- await
                     case maybeT >>= decodeBtnTxt
                       of Just (LCDButtonPress btn) -> yield btn
                          Just LCDHeartBeat         -> return ()
                          _                         -> lift $ $(logError) $ "Failed to decode button '" <> ((T.pack . show) maybeT) <> "'!"
                     decodeBtn

  let srcBtn = mvSource inMv =$= CT.decode CT.ascii =$= CT.lines =$= decodeBtn
  let sinkBs = CT.encode CT.iso8859_1 =$= mvSink outMv

  return $ (srcBtn, sinkBs)



decodeBtnTxt:: Text
            -> Maybe LCDData
decodeBtnTxt ln =
  case (T.strip ln)
    of "U" -> Just $ LCDButtonPress BUp
       "D" -> Just $ LCDButtonPress BDown
       "L" -> Just $ LCDButtonPress BLeft
       "R" -> Just $ LCDButtonPress BRight
       "S" -> Just $ LCDButtonPress BSelect
       "." -> Just $ LCDHeartBeat
       _   -> Nothing


-- | Infinite (re)connect loop.
--   Unbound input/output channels for IO.
connectToLCDMVar:: (MonadBase IO m, MonadBaseControl IO m, MonadLogger m)
                => String -- ^ lcd host name
                -> Int -- ^ lcd port
                -> m (MVar ByteString, MVar ByteString) -- ^ (input,output)
connectToLCDMVar lcdHostName lcdPort = do

  sockRef <- newIORef Nothing

  reconnectMV <- newMVar Nothing

  let trySome :: MonadBaseControl IO m => m a -> m (Either SomeException a)
      trySome = try

  let reconnectLoop:: (MonadBase IO m, MonadBaseControl IO m, MonadLogger m) => m ()
      reconnectLoop = do
        $(logInfo) "Entering reconnect loop"

        r <- takeMVar reconnectMV

        $(logInfo) $ "Reconnecting, old socket: " <> (T.pack . show) r

        case r
          of Nothing -> do c <- try $! liftBase $ connectToLCDSocket 5 lcdHostName lcdPort
                           case c
                             of Left (e::SomeException)  -> do
                                  $(logError) $ (T.pack.show) e
                                  _ <- tryPutMVar reconnectMV Nothing
                                  $(logInfo) $ "Sleeping ..."
                                  threadDelay (5 * 1000000)
                                  reconnectLoop

                                Right s -> do
                                  $(logInfo) $ "Successfully connected to " <> (T.pack lcdHostName)
                                  writeIORef sockRef (Just s)
                                  reconnectLoop

             Just s -> do _ <- trySome $ (liftBase.close) s
                          $(logInfo) $ "Closing socket connection" <> (T.pack . show) s
                          _ <- tryPutMVar reconnectMV Nothing
                          sc <- readIORef sockRef
                          if ((Just s) == sc)
                            then writeIORef sockRef Nothing
                            else return () -- already handled this failed socket
                          reconnectLoop

  _ <- fork reconnectLoop

  inputMVar <- newEmptyMVar
  outputMVar <- newEmptyMVar


  let withSock:: (MonadBase IO m, MonadBaseControl IO m, MonadLogger m) => (Socket -> m a) -> m a
      withSock cont = do
        maybeS <- readIORef sockRef
        case maybeS
          of Nothing -> do $(logInfo) $ "Waiting for connection to " <> (T.pack lcdHostName)
                           threadDelay (5 * 1000000)
                           withSock cont
             Just s  -> do res <- trySome $! cont s
                           case res
                             of Left e -> do $(logError) $ "IO error host:" <> (T.pack lcdHostName) <> " socket: " <> ((T.pack.show) e)
                                             putMVar reconnectMV (Just s)
                                             withSock cont
                                Right b -> return b


  let inputLoop:: (MonadBase IO m, MonadBaseControl IO m, MonadLogger m) => m ()
      inputLoop = do
        b <- withSock $ \s -> liftBase $ readFromLCDSocket 300 s
        putMVar inputMVar b
        inputLoop


  _ <- fork inputLoop


  let outputLoop:: (MonadBase IO m, MonadBaseControl IO m, MonadLogger m) => m ()
      outputLoop = do
        b <- takeMVar outputMVar
        _ <- withSock $ \s -> liftBase $ writeToLCDSocket 5 b s
        outputLoop


  _ <- fork outputLoop


  return (inputMVar, outputMVar)
