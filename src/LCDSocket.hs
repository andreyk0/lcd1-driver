{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module LCDSocket
  ( connectToLCDSocket
  , readFromLCDSocket
  , writeToLCDSocket
  )
where


import           Data.ByteString           (ByteString)
import           Import
import           Network.Socket            hiding (recv, recvFrom, send, sendTo)
import           Network.Socket.ByteString


newtype LCDSocketException = LCDSocketException String deriving (Eq, Show)
instance Exception LCDSocketException


-- | Blocking read with the timeout.
readFromLCDSocket
  :: Int -- ^ timeout, seconds
  -> Socket
  -> IO ByteString -- ^ data received
readFromLCDSocket timeoutSecs sock = do
  rRes <- timeout (timeoutSecs * 1000000) $ recv sock 64
  case rRes of
    Nothing ->
      (throwIO . LCDSocketException)
        $  "Failed to receive data from LCD within "
        <> show timeoutSecs
        <> " seconds! Expected at least a heartbeat!"
    Just str -> return str


-- | Blocking write with the timeout.
--   Throws an exception when a write can not be performed within the specified timeout.
writeToLCDSocket
  :: Int -- ^ timeout, seconds
  -> ByteString -- ^ data to write
  -> Socket
  -> IO Int -- ^ how much was written
writeToLCDSocket timeoutSecs str sock = do
  sRes <- timeout (timeoutSecs * 1000000) $ send sock str
  case sRes of
    Nothing ->
      (throwIO . LCDSocketException)
        $  "Failed to send data to LCD within "
        <> show timeoutSecs
        <> " seconds!"
    Just n -> return n


-- | Attempts to connect to an LCD host within a given timeout seconds.
connectToLCDSocket
  :: Int -- ^ timeout,seconds
  -> String -- ^ lcd host name
  -> Int -- ^ lcd port
  -> IO Socket
connectToLCDSocket timeoutSecs lcdHostName lcdPort = do
  addr <- lookupLCD lcdHostName lcdPort
  sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)

  setSocketOption sock KeepAlive 1

  cRes <- timeout (timeoutSecs * 1000000)
    $ onException (connect sock (addrAddress addr)) (close sock)
  case cRes of
    Nothing -> do
      _ <- try (close sock) :: IO (Either SomeException ())
      (throwIO . LCDSocketException)
        $  "Failed to connect to LCD within "
        <> show timeoutSecs
        <> " seconds!"
    Just () -> return sock


-- | Attempts to lookup lcd host by name.
lookupLCD
  :: String -- ^ hostname
  -> Int -- ^ port
  -> IO AddrInfo
lookupLCD lcdHostName lcdPort = do
  addrs <- getAddrInfo (Just $ defaultHints { addrFamily = AF_INET })
                       (Just lcdHostName)
                       (Just (show lcdPort))
  case addrs of
    addr : _ -> return addr
    _ ->
      (throwIO . LCDSocketException)
        $  "Failed to look up LCD host "
        <> lcdHostName
        <> " by name!"
