{-# language OverloadedStrings #-}
{-# language CPP #-}
module Atomic.Services.Frame where

import Ef.Base

#ifndef __GHCJS__
import Control.Concurrent
#endif

import Data.Micros

import Atomic.Component (Win,getWindow)
import Atomic.Ease
import Atomic.Service

#ifdef __GHCJS__
import GHCJS.DOM.RequestAnimationFrameCallback (newRequestAnimationFrameCallbackAsync)
import GHCJS.DOM.Window (requestAnimationFrame)
#endif

import Unsafe.Coerce

data FrameState = FrameState
  { frames :: Syndicate Double
  , isRunning :: Bool
  }

frameS :: Service '[State () FrameState]
frameS = Service {..}
  where
    key = "atomic.frame"

    build base = do
      frames <- syndicate
      let fs = FrameState frames False
      return (state fs *:* base)

    prime = return ()

getFrameSyndicate :: (MonadIO c)
               => c (Promise (Syndicate Double))
getFrameSyndicate =
  with frameS $ do
    FrameState {..} <- get
    return frames

onFrame :: forall ms c.
           ( MonadIO c
           , '[Evented] <: ms
           )
        => (IO () -> Ef ms c ())
        -> Ef ms c (IO ())
onFrame f = do
  buf <- get
  Just stopper <- demandMaybe =<< with frameS (do
    FrameState {..} <- get
    sub :: Subscription (Ef ms c) Double <- subscribe frames (return buf)
    bhv <- listen sub (\_ -> lift $ f (leaveSyndicate frames sub))
    let stopper = stop bhv >> leaveSyndicate frames sub
    unless isRunning createFrameLoop
    return stopper)
  return stopper

onFPS :: forall ms c.
         (MonadIO c, '[Evented] <: ms)
      => Double
      -> (IO () -> Ef ms c ())
      -> Ef ms c (IO ())
onFPS n f = do
  buf <- get

  let delta = 1000 / n

  Just stopper <- demandMaybe =<< with frameS (do
    FrameState {..} <- get
    sub :: Subscription (Ef ms c) Double <- subscribe frames (return buf)
    let leave = leaveSyndicate frames sub
    bhv <- listen sub (\timestamp -> lift (f leave) >> become (continue leave delta timestamp))
    let stopper = stop bhv >> leaveSyndicate frames sub
    unless isRunning createFrameLoop
    return stopper)


  return stopper
  where
    continue leave delta = go
      where
        go oldTimestamp newTimestamp = do
          let delta_t_milli = newTimestamp - oldTimestamp
          when (delta_t_milli >= delta) $ do
            lift (f leave)
            become (go newTimestamp)

onFrameWithTime :: ( MonadIO c
                   , '[Evented] <: ms
                   )
                => (IO () -> Double -> Ef '[Event Double] (Ef ms c) ())
                -> Ef ms c (IO ())
onFrameWithTime f = do
  buf <- get
  Just stopper <- demandMaybe =<< with frameS (do
    FrameState {..} <- get
    sub :: Subscription (Ef ms c) Double <- subscribe frames (return buf)
    let leave = leaveSyndicate frames sub
    bhv <- listen sub (f leave)
    let stopper = stop bhv >> leaveSyndicate frames sub
    unless isRunning createFrameLoop
    return stopper)
  return stopper

rAF :: Win -> (Double -> IO ()) -> IO Int
rAF win callback = do
#ifdef __GHCJS__
  rafCallback <- newRequestAnimationFrameCallbackAsync callback
  requestAnimationFrame win (Just rafCallback)
#else
  t <- (realToFrac . getMicros) <$> micros
  callback t
  return 0
#endif

createFrameLoop :: ('[State () FrameState,Evented] <: ms) => Ef ms IO ()
createFrameLoop = do
  FrameState {..} <- get
  win <- getWindow
  let createRAF = rAF win buf
      buf :: Double -> IO ()
      buf timestamp = do
        isNull <- nullSyndicate frames
        if isNull
        then void $ with frameS $ modify $ \fs -> (fs { isRunning = False },())
        else void $ do
          publish frames timestamp
#ifdef __GHCJS__
          createRAF
#else
          t <- getMicros <$> micros
          let d = t `mod` 16000
          threadDelay (fromIntegral d)
          createRAF
#endif
#ifdef __GHCJS__
  liftIO createRAF
#else
  liftIO $ forkIO $ void createRAF
#endif
  put $ FrameState frames True
  return ()

ease :: (MonadIO c, '[Evented] <: ms)
     => Ease -> Double -> Double -> Ef ms c Double -> (Double -> Ef ms c ()) -> Ef ms c ()
ease ease durationSec final getter setter = do
  current <- getter
  let delta = final - current
      duration_hr = durationSec * 1000000
  void $ onFrameWithTime (go duration_hr delta current)
  where
    go duration_hr delta start leave begin_hr = go' begin_hr
      where
        go' current_hr = do
          let c = ease duration_hr (current_hr - begin_hr)
              d = c * delta
              n = start + d
          if current_hr - begin_hr > duration_hr then do
            -- Slow animation; jump to destination.
            -- TODO: Make this more dynamic/adaptive.
            lift $ setter (start + delta)
            liftIO leave
            end
          else do
            lift $ setter n
            become go'

ease2 :: (MonadIO c, '[Evented] <: ms)
     => Ease -> Double -> (Double,Double) -> Ef ms c (Double,Double) -> ((Double,Double) -> Ef ms c ()) -> Ef ms c ()
ease2 ease durationSec (final1,final2) getter setter = do
  (current1,current2) <- getter
  let delta1 = final1 - current1
      delta2 = final2 - current2
      duration_hr = durationSec * 1000
  void $ onFrameWithTime (go duration_hr (delta1,delta2) (current1,current2))
  where
    go duration_hr (delta1,delta2) (start1,start2) leave begin_hr = go' begin_hr
      where
        go' current_hr = do
          let c = ease duration_hr (current_hr - begin_hr)
              d1 = c * delta1
              d2 = c * delta2
              n1 = start1 + d1
              n2 = start2 + d2
          -- liftIO $ print (c,duration_hr,current_hr - begin_hr,d1,d2,n1,n2)
          if current_hr - begin_hr > duration_hr then do
            -- Slow animation; jump to destination.
            -- TODO: Make this more dynamic/adaptive.
            lift $ setter (start1 + delta1,start2 + delta2)
            liftIO leave
            end
          else do
            lift $ setter (n1,n2)
            become go'