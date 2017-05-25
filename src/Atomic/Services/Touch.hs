{-# language OverloadedStrings #-}
{-# language CPP #-}
module Atomic.Services.Touch where

import Ef.Base hiding (Object)

import Data.Txt
import Data.JSON

import Atomic.Component (Win,getWindow)
import Atomic.IsString
import Atomic.Service
import Atomic.Signals

import Data.IORef

#ifdef __GHCJS__
import qualified GHCJS.DOM.EventM as Ev
import qualified GHCJS.DOM.EventTargetClosures as ETC
#endif

import Unsafe.Coerce

import Prelude

data TouchState = TouchState
  { touchStartN    :: Network [(Int,Int)]
  , touchMoveN     :: Network [(Int,Int)]
  , touchN         :: Network [[(Int,Int)]]
  }

touchMove :: EVName Win Obj
touchMove =
#ifdef __GHCJS__
   ETC.unsafeEventName "touchmove"
#else
   "touchmove"
#endif

touchS :: Service '[State () TouchState]
touchS = Service {..}
  where
    key = "atomic.touch"

    build base = do
      touchXN        <- network
      touchYN        <- network
      touchPositionN <- network
      let ms = TouchState
                 touchXRef
                 touchYRef
                 touchPositionN
                 touchXN
                 touchYN
      return (state ms *:* base)

    prime = do
      TouchState {..} <- get

      win <- getWindow

      rb <- getEventQueue

      void $ onWindowNetwork touchMove $ \o -> do
        let mxy = flip parseMaybe o $ \obj -> do
                    x <- obj .: "x"
                    y <- obj .: "y"
                    pure (x,y)

        forM_ mxy $ \(newX,newY) ->  do
          oldX <- liftIO $ readIORef touchXRef
          oldY <- liftIO $ readIORef touchYRef
          liftIO $ do
            syndicate touchPositionN (TouchPosition newX newY)

            when (oldX /= Just newX) $ do
              syndicate touchXN newX
              writeIORef touchXRef (Just newX)

            when (oldY /= Just newY) $ do
              syndicate touchYN newY
              writeIORef touchYRef (Just newY)

getTouchXRef :: (MonadIO c) => c (Promise (IORef (Maybe Int)))
getTouchXRef = with touchS $ do
  TouchState {..} <- get
  return touchXRef

getTouchYRef :: (MonadIO c) => c (Promise (IORef (Maybe Int)))
getTouchYRef = with touchS $ do
  TouchState {..} <- get
  return touchYRef

getTouchX :: (MonadIO c) => c (Promise (Maybe Int))
getTouchX = with touchS $ do
  TouchState {..} <- get
  liftIO $ readIORef touchXRef

getTouchY :: (MonadIO c) => c (Promise (Maybe Int))
getTouchY = with touchS $ do
  TouchState {..} <- get
  liftIO $ readIORef touchYRef

onTouchX :: (MonadIO c, '[Evented] <: ms)
         => (Int -> Code ms c ()) -> Code ms c (IO ())
onTouchX f = do
  buf <- getEventQueue
  p <- periodical
  Just s <- subscribe p (lift . f)
  Just leaveNW <- demandMaybe =<< with touchS (do
    TouchState {..} <- get
    joinNetwork touchXN p buf
    return (leaveNetwork touchXN p))
  return (stop s >> leaveNW)

onTouchY :: (MonadIO c, '[Evented] <: ms)
         => (Int -> Code ms c ()) -> Code ms c (IO ())
onTouchY f = do
  buf <- getEventQueue
  p <- periodical
  Just s <- subscribe p (lift . f)
  Just leaveNW <- demandMaybe =<< with touchS (do
    TouchState {..} <- get
    joinNetwork touchYN p buf
    return (leaveNetwork touchYN p))
  return (stop s >> leaveNW)

onTouchPosition :: (MonadIO c, '[Evented] <: ms)
                => (TouchPosition -> Code ms c ())
                -> Code ms c (IO ())
onTouchPosition f = do
  buf <- getEventQueue
  p <- periodical
  Just s <- subscribe p (lift . f)
  Just leaveNW <- demandMaybe =<< with touchS (do
    TouchState {..} <- get
    joinNetwork touchPositionN p buf
    return (leaveNetwork touchPositionN p))
  return (stop s >> leaveNW)
