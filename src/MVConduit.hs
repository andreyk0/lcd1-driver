{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE ScopedTypeVariables #-}


module MVConduit
  ( mvDup
  , mvSink
  , mvSource
  )
where


import           Control.Monad
import           Data.Conduit
import           Import


-- | MVar as an infinite Source
mvSource :: MonadIO m => MVar a -> ConduitT () a m ()
mvSource inMv = mvSrc
 where
  mvSrc = do
    a <- takeMVar inMv
    yield a
    mvSrc


-- | MVar as an infinite Sink
mvSink :: MonadIO m => MVar a -> ConduitT a Void m ()
mvSink outMv = mvSnk
 where
  mvSnk = do
    mA <- await
    forM_ mA (putMVar outMv)
    mvSnk


-- | Duplicates input and supplies to N sources
mvDup
  :: forall a m
   . MonadIO m
  => Int -- ^ number of sources
  -> m (ConduitT a Void m (), [ConduitT () a m () ])
mvDup n = do
  srcMVs <- replicateM n newEmptyMVar
  let srcs = fmap mvSource srcMVs

  let snk = do
        mA <- await
        forM_ mA $ \x -> forM_ srcMVs $ \s -> putMVar s x
        snk

  return (snk, srcs)
