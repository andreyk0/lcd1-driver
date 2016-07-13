{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}


module MVConduit (
  mvSink
, mvSource
) where


import           Control.Concurrent.MVar.Lifted
import           Control.Monad.Base
import           Data.Conduit
import           Data.Traversable


-- | MVar as an infinite Source
mvSource:: (MonadBase IO m)
        => MVar a
        -> Source m a
mvSource inMv = mvSrc
  where mvSrc = do a <- takeMVar inMv
                   yield a
                   mvSrc


-- | MVar as an infinite Sink
mvSink:: (MonadBase IO m)
      => MVar a
      -> Sink a m ()
mvSink outMv = mvSnk
  where mvSnk = do mA <- await
                   _ <- forM mA (putMVar outMv)
                   mvSnk
