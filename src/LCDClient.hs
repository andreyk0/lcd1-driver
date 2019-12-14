{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StrictData          #-}


module LCDClient
  ( Button(..)
  , connectToLCD
  )
where


import           Data.Conduit
import qualified Data.Conduit.Text   as CT
import           Import
import           LCDSocket
import           MVConduit
import qualified Network.Socket      as Socket
import qualified RIO.Text            as T
import qualified UnliftIO.Concurrent as Conc


data Button = BUp | BDown | BLeft | BRight | BSelect
              deriving (Eq, Show)


data LCDData = LCDButtonPress Button
             | LCDHeartBeat
             deriving (Eq, Show)


-- | (Re)connects to LCD host.
--   Provides an infinite source of button presses and a sink of byte strings.
connectToLCD
  :: HasLogFunc env
  => String -- ^ LCD host name
  -> Int -- ^ LCD port
  -> RIO env ( ConduitT () Button (RIO env) ()
             , ConduitT Text Void (RIO env) ()
             )
connectToLCD lcdHostName lcdPort = do
  (inMv, outMv) <- connectToLCDMVar lcdHostName lcdPort

  let decodeBtn = do
        maybeT <- await
        case maybeT >>= decodeBtnTxt of
          Just (LCDButtonPress btn) -> yield btn
          Just LCDHeartBeat         -> return ()
          _ -> logError
              . display
              $  "Failed to decode button '"
              <> (T.pack . show) maybeT
              <> "'!"
        decodeBtn

  let srcBtn = mvSource inMv .| CT.decode CT.ascii .| CT.lines .| decodeBtn
  let sinkBs = CT.encode CT.iso8859_1 .| mvSink outMv

  return (srcBtn, sinkBs)



decodeBtnTxt :: Text -> Maybe LCDData
decodeBtnTxt ln = case T.strip ln of
  "U" -> Just $ LCDButtonPress BUp
  "D" -> Just $ LCDButtonPress BDown
  "L" -> Just $ LCDButtonPress BLeft
  "R" -> Just $ LCDButtonPress BRight
  "S" -> Just $ LCDButtonPress BSelect
  "." -> Just LCDHeartBeat
  _   -> Nothing


-- | Infinite (re)connect loop.
--   Unbound input/output channels for IO.
connectToLCDMVar
  :: HasLogFunc env
  => String -- ^ lcd host name
  -> Int -- ^ lcd port
  -> RIO env (MVar ByteString, MVar ByteString) -- ^ (input,output)
connectToLCDMVar lcdHostName lcdPort = do

  sockRef     <- newIORef Nothing

  reconnectMV <- newMVar Nothing

  let reconnectLoop = do
        logInfo "Entering reconnect loop"

        r <- takeMVar reconnectMV
        logInfo . display $ "Reconnecting, old socket: " <> tshow r

        case r of
          Nothing -> do
            c <- try $! liftIO $ connectToLCDSocket 5 lcdHostName lcdPort
            case c of
              Left (e :: SomeException) -> do
                logError . display $ e
                _ <- tryPutMVar reconnectMV Nothing
                logInfo "Sleeping ..."
                threadDelay (5 * 1000000)
                reconnectLoop

              Right s -> do
                logInfo . display $ "Successfully connected to " <> T.pack lcdHostName
                writeIORef sockRef (Just s)
                reconnectLoop

          Just s -> do
            _ <- tryAny $ (liftIO . Socket.close) s
            logInfo . display $ "Closing socket connection" <> tshow s
            _  <- tryPutMVar reconnectMV Nothing
            sc <- readIORef sockRef
            when (Just s == sc) $ writeIORef sockRef Nothing
            reconnectLoop

  void $ Conc.forkFinally (void reconnectLoop) (logError . displayShow)

  inputMVar  <- newEmptyMVar
  outputMVar <- newEmptyMVar


  let withSock cont = do
        maybeS <- readIORef sockRef
        case maybeS of
          Nothing -> do
            logInfo . display $ "Waiting for connection to " <> T.pack lcdHostName
            threadDelay (5 * 1000000)
            withSock cont
          Just s -> do
            res <- tryAny $! cont s
            case res of
              Left e -> do
                logError . display
                  $  "IO error host:"
                  <> T.pack lcdHostName
                  <> " socket: "
                  <> tshow e
                putMVar reconnectMV (Just s)
                withSock cont
              Right b -> return b


  let inputLoop = do
        b <- withSock $ \s -> liftIO $ readFromLCDSocket 300 s
        putMVar inputMVar b
        inputLoop


  void $ Conc.forkFinally (void inputLoop) (logError . displayShow)


  let outputLoop = do
        b <- takeMVar outputMVar
        _ <- withSock $ \s -> liftIO $ writeToLCDSocket 5 b s
        outputLoop


  void $ Conc.forkFinally (void outputLoop) (logError . displayShow)
  return (inputMVar, outputMVar)
