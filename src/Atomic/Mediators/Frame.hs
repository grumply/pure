{-# language OverloadedStrings #-}
{-# language CPP #-}
module Atomic.Mediators.Frame where

import Ef.Base

#ifndef __GHCJS__
import Control.Concurrent
#endif

import Data.Micros

import Atomic.Construct (Win,getWindow)
import Atomic.Ease
import Atomic.Revent
import Atomic.Mediator
import Atomic.With

#ifdef __GHCJS__
import GHCJS.DOM.RequestAnimationFrameCallback (newRequestAnimationFrameCallbackAsync)
import GHCJS.DOM.Window (requestAnimationFrame)
#endif

import Unsafe.Coerce

data FrameState = FrameState
  { frames :: Network Double
  , isRunning :: Bool
  }

frameS :: Mediator '[State () FrameState]
frameS = Mediator {..}
  where
    key = "atomic.frame"

    build base = do
      frames <- network
      let fs = FrameState frames False
      return (state fs *:* base)

    prime = return ()

getFrameNetwork :: (MonadIO c)
               => c (Promise (Network Double))
getFrameNetwork =
  with frameS $ do
    FrameState {..} <- get
    return frames

onFrame :: ( MonadIO c
           , '[Revent] <: ms
           )
        => (IO () -> Code ms c ())
        -> Code ms c (IO ())
onFrame f = do
  buf <- getReventBuffer
  p <- periodical
  Just frames <- demandMaybe =<< with frameS (do
    FrameState {..} <- get
    subscribe p (lift . const (f (leaveNetwork frames p)))
    joinNetwork frames p buf
    unless isRunning createFrameLoop
    return frames)
  return (leaveNetwork frames p)

onFPS :: (MonadIO c, '[Revent] <: ms)
      => Double
      -> (IO () -> Code ms c ())
      -> Code ms c (IO ())
onFPS n f = do
  buf <- getReventBuffer

  let delta = 1000 / n

  p <- periodical

  Just frames <- demandMaybe =<< with frameS (do
    FrameState {..} <- get
    let leave = leaveNetwork frames p
    subscribe p (\timestamp -> lift (f leave) >> become (continue leave delta timestamp))
    joinNetwork frames p buf
    unless isRunning createFrameLoop
    return frames)


  return (leaveNetwork frames p)
  where
    continue leave delta = go
      where
        go oldTimestamp newTimestamp = do
          let delta_t_milli = newTimestamp - oldTimestamp
          when (delta_t_milli >= delta) $ do
            lift (f leave)
            become (go newTimestamp)

onFrameWithTime :: ( MonadIO c
                   , '[Revent] <: ms
                   )
                => (IO () -> Double -> Code '[Event Double] (Code ms c) ())
                -> Code ms c (IO ())
onFrameWithTime f = do
  buf <- getReventBuffer
  p <- periodical
  Just frames <- demandMaybe =<< with frameS (do
    FrameState {..} <- get
    subscribe p (f (leaveNetwork frames p))
    joinNetwork frames p buf
    unless isRunning createFrameLoop
    return frames)
  return (leaveNetwork frames p)

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

createFrameLoop :: ('[State () FrameState,Revent] <: ms) => Code ms IO ()
createFrameLoop = do
  FrameState {..} <- get
  rb <- getReventBuffer
  win <- getWindow
  let createRAF = rAF win buf
      buf :: Double -> IO ()
      buf timestamp = do
        isNull <- nullNetwork frames
        if isNull
        then void $ with frameS $ modify $ \fs -> (fs { isRunning = False },())
        else void $ do
          syndicate frames timestamp
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

ease :: (MonadIO c, '[Revent] <: ms)
     => Ease -> Double -> Double -> Code ms c Double -> (Double -> Code ms c ()) -> Code ms c ()
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

ease2 :: (MonadIO c, '[Revent] <: ms)
     => Ease -> Double -> (Double,Double) -> Code ms c (Double,Double) -> ((Double,Double) -> Code ms c ()) -> Code ms c ()
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
