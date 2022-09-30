{-# LANGUAGE RecordWildCards #-}
module Data.Queue
  ( Queue
  , newQueue
  , arrive
  , collect
  ) where

import Data.IORef
import Control.Concurrent.MVar
import Control.Concurrent (yield)

data Queue a = Queue
  { queueBarrier  :: {-# UNPACK #-}!(MVar ())
  , internalQueue :: {-# UNPACK #-}!(IORef [a])
  } deriving Eq

{-# INLINE newQueue #-}
newQueue :: IO (Queue a)
newQueue = Queue <$> newEmptyMVar <*> newIORef []

{-# INLINE arrive #-}
arrive :: Queue a -> a -> IO Bool
arrive Queue {..} a = do
  q <- atomicModifyIORef' internalQueue $ \q -> (a:q,q)
  tryPutMVar queueBarrier ()

{-# INLINE collect #-}
collect :: Queue a -> IO [a]
collect Queue {..} = do
  takeMVar queueBarrier
  atomicModifyIORef' internalQueue $ \q -> ([],reverse q)
