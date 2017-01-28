{-# language OverloadedStrings #-}
{-# language CPP #-}
module Atomic.Mediators.Mouse where

import Ef.Base hiding (Object)

import Data.Txt
import Data.JSON

import Atomic.Construct (Win,getWindow)
import Atomic.Revent
import Atomic.Mediator
import Atomic.Signals
import Atomic.With

import Data.IORef

#ifdef __GHCJS__
import qualified GHCJS.DOM.EventM as Ev
import qualified GHCJS.DOM.EventTargetClosures as ETC
#endif

import Unsafe.Coerce

data MousePosition = MousePosition
  { mouseX :: Int
  , mouseY :: Int
  } deriving (Eq,Ord,Show)

-- note we don't add a scroll listener here because it can cause jank on scroll
-- due to a default check for preventDefault in the browser. Instead, scrolling
-- can be inferred by changes on Fusion.Mediators.Window onScrollX/onScrollY.
data MouseState = MouseState
  { mouseXRef      :: IORef (Maybe Int)
  , mouseYRef      :: IORef (Maybe Int)
  , mousePositionN :: Network MousePosition
  , mouseXN        :: Network Int
  , mouseYN        :: Network Int
  }

mouseMove :: EVName Win Obj
mouseMove =
#ifdef __GHCJS__
   ETC.unsafeEventName "mousemove"
#else
   "mousemove"
#endif

mouseS :: Mediator '[State () MouseState]
mouseS = Mediator {..}
  where
    key = "Fusion.mouseS"

    build base = do
      mouseXRef      <- newIORef Nothing
      mouseYRef      <- newIORef Nothing
      mouseXN        <- network
      mouseYN        <- network
      mousePositionN <- network
      let ms = MouseState
                 mouseXRef
                 mouseYRef
                 mousePositionN
                 mouseXN
                 mouseYN
      return (state ms *:* base)

    prime = do
      MouseState {..} <- get

      win <- getWindow

      rb <- getReventBuffer

      void $ onWindowNetwork mouseMove $ \o -> do
        let mxy = flip parseMaybe o $ \obj -> do
                    x <- obj .: "x"
                    y <- obj .: "y"
                    pure (x,y)

        forM_ mxy $ \(newX,newY) ->  do
          oldX <- liftIO $ readIORef mouseXRef
          oldY <- liftIO $ readIORef mouseYRef
          liftIO $ do
            syndicate mousePositionN (MousePosition newX newY)
            when (oldX /= Just newX) $ do
              syndicate mouseXN newX
              writeIORef mouseXRef (Just newX)
            when (oldY /= Just newY) $ do
              syndicate mouseYN newY
              writeIORef mouseYRef (Just newY)

getMouseXRef :: (MonadIO c) => c (Promise (IORef (Maybe Int)))
getMouseXRef = with mouseS $ do
  MouseState {..} <- get
  return mouseXRef

getMouseYRef :: (MonadIO c) => c (Promise (IORef (Maybe Int)))
getMouseYRef = with mouseS $ do
  MouseState {..} <- get
  return mouseYRef

getMouseX :: (MonadIO c) => c (Promise (Maybe Int))
getMouseX = with mouseS $ do
  MouseState {..} <- get
  liftIO $ readIORef mouseXRef

getMouseY :: (MonadIO c) => c (Promise (Maybe Int))
getMouseY = with mouseS $ do
  MouseState {..} <- get
  liftIO $ readIORef mouseYRef

onMouseX :: (MonadIO c, '[Revent] <: ms)
         => (Int -> Code ms c ()) -> Code ms c (Subscription ms c Int,Periodical ms c Int)
onMouseX f = do
  buf <- getReventBuffer
  p <- periodical
  Just s <- subscribe p (lift . f)
  with mouseS $ do
    MouseState {..} <- get
    joinNetwork mouseXN p buf
  return (s,p)

onMouseY :: (MonadIO c, '[Revent] <: ms)
         => (Int -> Code ms c ()) -> Code ms c (Subscription ms c Int,Periodical ms c Int)
onMouseY f = do
  buf <- getReventBuffer
  p <- periodical
  Just s <- subscribe p (lift . f)
  with mouseS $ do
    MouseState {..} <- get
    joinNetwork mouseYN p buf
  return (s,p)

onMousePosition :: (MonadIO c, '[Revent] <: ms)
                => (MousePosition -> Code ms c ())
                -> Code ms c (Subscription ms c MousePosition,Periodical ms c MousePosition)
onMousePosition f = do
  buf <- getReventBuffer
  p <- periodical
  Just s <- subscribe p (lift . f)
  with mouseS $ do
    MouseState {..} <- get
    joinNetwork mousePositionN p buf
  return (s,p)