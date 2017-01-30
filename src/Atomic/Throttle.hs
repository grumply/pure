module Atomic.Throttle (throttle) where

import Ef.Base

import Control.Concurrent
import Data.IORef
import System.IO.Unsafe

throttleify :: IORef Bool -> Int -> IO () -> Bool
throttleify throttler lim io = unsafePerformIO $ do
  b <- readIORef throttler
  unless b $ do
    io
    writeIORef throttler True
    void $ forkIO $ do
      threadDelay (lim * 1000)
      writeIORef throttler False
  return (not b)

{-# NOINLINE throttle #-}
throttle :: MonadIO c => Int -> IO () -> c Bool
throttle lim io = do
  x <- liftIO $
    unsafePerformIO $ do
      throttler <- newIORef False
      return $ return $ throttleify throttler lim io
  x `seq` return x
