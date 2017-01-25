module Atomic.Debounce (debounce,debounce') where

import Ef.Base

import Control.Concurrent
import Data.IORef
import System.IO.Unsafe

debounceify :: IORef (Maybe ThreadId)
            -> Int
            -> IO ()
            -> ()
debounceify bouncer delay m = unsafePerformIO $ do
  mt <- readIORef bouncer

  let
    forkDelay =
      forkIO $ do
        threadDelay (delay * 1000)
        m
        writeIORef bouncer Nothing

  case mt of
    Nothing -> do
      tid <- forkDelay
      writeIORef bouncer (Just tid)
    Just tid -> do
      killThread tid
      tid' <- forkDelay
      writeIORef bouncer (Just tid')

debounceify' :: IORef (Maybe ThreadId)
             -> Int
             -> IO ()
             -> Bool
debounceify' bouncer delay m = unsafePerformIO $ do
  mt <- readIORef bouncer

  let
    forkDelay =
      forkIO $ do
        threadDelay (delay * 1000)
        writeIORef bouncer Nothing

  case mt of
    Nothing -> do
      m
      tid <- forkDelay
      writeIORef bouncer (Just tid)
      return True
    Just tid -> do
      killThread tid
      tid' <- forkDelay
      writeIORef bouncer (Just tid')
      return False

-- | Debounce an IO action with execution on the trailing edge only. Delay is
-- in milliseconds
--
-- > -- note that threadDelay is in microseconds, but debounce is in
-- > -- milliseconds
-- > do let d = debounce 200 (putStrLn "Test")
-- >    d                  -- (1) debounced
-- >    d                  -- (2) debounced
-- >    threadDelay 500000 -- (2) succeeds after 200 milliseconds
-- >    d                  -- (3) debounced
-- >                       -- (3) succeeds after 200 milliseconds
{-# NOINLINE debounce #-}
debounce :: (Monad super, MonadIO super) => Int -> IO () -> super ()
debounce delay io = do
  x <- liftIO $
    unsafePerformIO $ do
      bounce <- newIORef Nothing
      return $ return $ debounceify bounce delay io
  x `seq` return x

-- | Debounce an IO action with execution on the leading edge only. Delay is
-- in milliseconds.
--
-- > -- note that threadDelay is in microseconds, but debounce' is in
-- > -- milliseconds
-- > do let d = debounce' 500 (putStrLn "Test")
-- >    d                   -- call immediately successfull
-- >    d                   -- call debounced
-- >    threadDelay 500000
-- >    d                   -- call immediately successful
{-# NOINLINE debounce' #-}
debounce' :: (Monad super, MonadIO super) => Int -> IO () -> super Bool
debounce' delay io = do
  x <- liftIO $
    unsafePerformIO $ do
      bounce <- newIORef Nothing
      return $ return $ debounceify' bounce delay io
  x `seq` return x
