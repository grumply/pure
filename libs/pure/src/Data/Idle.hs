{-# LANGUAGE CPP, BangPatterns, ScopedTypeVariables #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-state-hack #-}
module Data.Idle (addIdleWork, addIdleWorks, addIdleWorksReverse) where

import Control.Concurrent (MVar,newEmptyMVar,forkIO,takeMVar,putMVar,tryPutMVar,yield,threadDelay)
import Control.Monad (forever,join,void)
import Data.Foldable
import Data.Function (fix)
import Data.IORef (IORef,newIORef,readIORef,writeIORef,atomicModifyIORef')
import Data.List (intersperse)
import System.IO.Unsafe (unsafePerformIO)

#ifdef __GHCJS__
import GHCJS.Foreign.Callback (syncCallback1,OnBlocked(..),releaseCallback)
import GHCJS.Concurrent (synchronously)
#endif

import Data.DOM (JSV,requestIdleCallback)
import Data.Txt (Txt,toTxt)

import Control.Exception (SomeException,catch)

-- |
-- GHCJS: We rely on a poly-filled version of requestIdleCallback
-- which uses the browser's rIC if it is available or a setTimeout(_,1)
-- if rIC is not available. Access to the rIC timeRemaining is not given,
-- but we check if a reschedule is necessary between each action and act
-- accordingly, cleaning up callbacks as we go.
--
-- GHC: All actions are run in a forked thread that yields often.
--
{-# INLINE addIdleWork #-}
addIdleWork :: IO () -> IO Bool
addIdleWork a = idleWorker `seq` do
    atomicModifyIORef' idleWorkQueue $ \as -> (a:as,())
    tryPutMVar idleWorkAwaiting ()

{-# INLINE addIdleWorks #-}
addIdleWorks :: [IO ()] -> IO Bool
addIdleWorks new = idleWorker `seq` do
    atomicModifyIORef' idleWorkQueue $ \as -> (reverse new ++ as,())
    tryPutMVar idleWorkAwaiting ()

{-# INLINE addIdleWorksReverse #-}
addIdleWorksReverse :: [IO ()] -> IO Bool
addIdleWorksReverse new = idleWorker `seq` do
    atomicModifyIORef' idleWorkQueue $ \as -> (new ++ as,())
    tryPutMVar idleWorkAwaiting ()

{-# NOINLINE idleWorkAwaiting #-}
idleWorkAwaiting :: MVar ()
idleWorkAwaiting = unsafePerformIO newEmptyMVar

{-# NOINLINE idleWorkQueue #-}
idleWorkQueue :: IORef [IO ()]
idleWorkQueue = unsafePerformIO (newIORef [])

{-# NOINLINE idleWorker #-}
idleWorker :: ()
idleWorker = unsafePerformIO (void (forkIO worker))
  where

    -- `forever` claims to avoid a space leak 
    -- regardless of optimization level, so it
    -- is used in lieu of my semantic preference 
    -- `fix (work >>)`
    --
    worker = forever work
      where

        -- As above, so below.
        --
        -- Importantly, work will always return so that
        -- the idle worker may continue, though I'm not
        -- sure what situations `work` could run into that 
        -- would throw an exception.
        --
        work = forever await `catch` \(_ :: SomeException) -> return ()
          where
            await :: IO ()
            await = do
              -- Try to yield to any other active threads before
              -- awaiting idle work. I don't know if this will 
              -- actually improve batching, but it seems procedurally 
              -- reasonable.
              yield
              takeMVar idleWorkAwaiting
              as <- atomicModifyIORef' idleWorkQueue $ \as -> ([],as)
              workIdly (reverse as)

workIdly :: [IO ()] -> IO ()
workIdly as = do
#ifndef __GHCJS__
    -- `yield` often on GHC, as this work is low priority. I don't know
    -- if this will improve the nature of idle work but it seems 
    -- procedurally reasonable. Idle work is not meant to be a 
    -- low-priority work queue, but more a deferred resource cleanup 
    -- queue. It is far more important in GHCJS where cleanup is 
    -- deferred from the reconciliation loop to improve rendering
    -- performance and, thus, interactivity.
    yield
    sequence_ (intersperse yield as)
#else
    -- On GHCJS, we use a requestIdleCallback shim to defer idle
    -- work to truly idle periods, as measured in the main JS work 
    -- thread. The idle callback will return work that has not had
    -- a chance to run, and the worker will be re-called with a new
    -- idle work callback request. Thus, we can, hopefully, actually
    -- defer idle work to idle periods! Oddly, this is, theoretically, 
    -- an imporvement over the GHC approach.
    wrapper >>= traverse_ workIdly
  where
    wrapper = do
      barrier <- newEmptyMVar
      let work deadline = synchronously $ worker deadline as >>= putMVar barrier
      callback <- syncCallback1 ContinueAsync work
      requestIdleCallback callback
      result <- takeMVar barrier
      releaseCallback callback
      return result
      where
        worker deadline = go
          where
            -- Swallow /all/ exceptions. I don't know if OOM exceptions, for
            -- example, can even occur in GHCJS, so we swallow any and all
            -- exceptions we can intercept. 
            swallow :: IO () -> IO ()
            swallow action = action `catch` \(_ :: SomeException) -> return ()

            -- Run as many actions from the list as the idle callback allows.
            --
            -- Should complete /at least/ one action per idle period.
            --
            -- Remaining actions are returned for the next idle period.
            --
            -- Note that: as used in, e.g. Data.View.Build, this worker and the 
            -- work queue should remain balanced in almost all situations. Work 
            -- in the idle queue should not outpace the production of idle work. 
            -- This is not necessarily true when used otherwise.
            go :: [IO ()] -> IO (Maybe [IO ()])
            go [] = return Nothing
            go (a:as) = do
              swallow a
              tr <- hasTimeRemaining deadline
              let 
                k | tr = go 
                  | [] <- as = return . const Nothing 
                  | otherwise = return . Just
              k as
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "var tr = $1.timeRemaining(); $r = tr > 0;" hasTimeRemaining :: JSV -> IO Bool
#endif
