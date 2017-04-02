{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}


module MVConduit (
  mvDup
, mvSink
, mvSource
) where


import Control.Concurrent.MVar.Lifted
import Control.Monad.Base
import Data.Conduit
import Data.Foldable


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
                   forM_ mA (putMVar outMv)
                   mvSnk


-- | Duplicates input and supplies to N sources
mvDup:: forall a m . (MonadBase IO m)
       => Int -- ^ number of sources
       -> m (Sink a m (), [Source m a])
mvDup n = do
  srcMVs <- sequence $ take n $ repeat newEmptyMVar
  let srcs = fmap mvSource srcMVs

  let snk = do mA <- await
               forM_ mA $ \x ->
                 forM_ srcMVs $ \s -> putMVar s x
               snk

  return (snk, srcs)
