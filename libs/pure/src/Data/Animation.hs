{-# LANGUAGE CPP, ScopedTypeVariables, PatternSynonyms, LambdaCase, TupleSections #-}
{-# OPTIONS_GHC -fno-full-laziness -fno-state-hack #-}
module Data.Animation (addAnimation,addAnimations,addAnimationsReverse) where

import Control.Exception (SomeException,catch)
import Control.Concurrent (MVar,newEmptyMVar,forkIO,takeMVar,putMVar,tryPutMVar,yield,myThreadId,tryTakeMVar)
import Control.Monad (void,forever, replicateM_)
import Data.Function (fix)
import Data.IORef (IORef,newIORef,atomicModifyIORef')
import Data.Time (pattern Millisecond,delay)
import System.IO.Unsafe (unsafePerformIO)

import Data.Txt (Txt,toTxt)

#ifdef __GHCJS__
import GHCJS.Foreign.Callback (Callback,syncCallback1,OnBlocked(..),releaseCallback)
import GHCJS.Concurrent
#endif

import Data.DOM (JSV)

-- Note that: while it may seem reasonable to perform actions directly in GHC -
-- rather than batching as we do in GHCJS - it is important to try to maintain 
-- the procedural ordering of effects between the two to try to maintain server-
-- client coherence w.r.t. reconciliation of views. To that end, we do batch in
-- GHC, but we don't delay execution of batches as in GHCJS. Thus, the
-- difference between the two is the density and uniformity of the
-- discretization; GHCJS has 16ms between animation frames, but GHC has a thread
-- always awaiting these reconiliation actions and will perform them as soon as
-- they arrive, RTS permitting.

{-# INLINE addAnimation #-}
addAnimation :: IO () -> IO Bool
addAnimation a = animator `seq` do
    atomicModifyIORef' animationQueue $ \as -> (a:as,())
    tryPutMVar animationsAwaiting ()

{-# INLINE addAnimations #-}
addAnimations :: [IO ()] -> IO Bool
addAnimations new = animator `seq` do
    atomicModifyIORef' animationQueue $ \as -> (reverse new ++ as,())
    tryPutMVar animationsAwaiting ()

{-# INLINE addAnimationsReverse #-}
addAnimationsReverse :: [IO ()] -> IO Bool
addAnimationsReverse new = animator `seq` do
    atomicModifyIORef' animationQueue $ \as -> (new ++ as,())
    b <- tryPutMVar animationsAwaiting ()
    return b

{-# NOINLINE animationsAwaiting #-}
animationsAwaiting :: MVar ()
animationsAwaiting = unsafePerformIO newEmptyMVar

{-# NOINLINE animationQueue #-}
animationQueue :: IORef [IO ()]
animationQueue = unsafePerformIO (newIORef [])

{-# NOINLINE animator #-}
animator :: ()
animator = unsafePerformIO (void (forkIO worker))
  where

    -- `forever` claims to avoid a space leak 
    -- regardless of optimization level, so it
    -- is used in lieu of my semantic preference 
    -- `fix (await >>)`
    --
    worker = forever work
      where

        -- As above, so below.
        --
        -- Importantly, `work` will always return so that
        -- the idle worker may continue, though I'm not
        -- sure what situations `work` could run into that 
        -- would throw an exception.
        --
        work = forever await `catch` \(_ :: SomeException) -> return ()
          where
            await = do
              takeMVar animationsAwaiting
              as <- atomicModifyIORef' animationQueue ([],)
              animate (reverse as)

animate :: [IO ()] -> IO ()
#ifdef __GHCJS__
animate [] = pure ()
animate as = do
    barrier <- newEmptyMVar
    let work _ = synchronously $ sequence_ as >>= putMVar barrier
    callback <- syncCallback1 ContinueAsync work
    requestAnimation callback
    takeMVar barrier
    releaseCallback callback
#else
animate = sequence_
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "$r = window.requestAnimationFrame($1)" request_animation_frame_js :: Callback (JSV -> IO ()) -> IO JSV

{-# INLINE requestAnimation #-}
requestAnimation :: Callback (JSV -> IO ()) -> IO ()
requestAnimation cb = void $ request_animation_frame_js cb
#endif
