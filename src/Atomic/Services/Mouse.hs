{-# language OverloadedStrings #-}
{-# language CPP #-}
module Atomic.Services.Mouse where

import Ef.Base hiding (Object)

import Data.Txt
import Data.JSON

import Atomic.Component (Win,getWindow)
import Atomic.Service
import Atomic.Signals

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
-- can be inferred by changes on Fusion.Services.Window onScrollX/onScrollY.
data MouseState = MouseState
  { mouseXRef      :: IORef (Maybe Int)
  , mouseYRef      :: IORef (Maybe Int)
  , mousePositionN :: Syndicate MousePosition
  , mouseXN        :: Syndicate Int
  , mouseYN        :: Syndicate Int
  }

mouseMove :: EVName Win Obj
mouseMove =
#ifdef __GHCJS__
   ETC.unsafeEventName "mousemove"
#else
   "mousemove"
#endif

mouseS :: Service '[State () MouseState]
mouseS = Service {..}
  where
    key = "atomic.mouse"

    build base = do
      mouseXRef      <- newIORef Nothing
      mouseYRef      <- newIORef Nothing
      mouseXN        <- syndicate
      mouseYN        <- syndicate
      mousePositionN <- syndicate
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

      void $ onWindowSyndicate mouseMove $ \o -> do
        let mxy = flip parseMaybe o $ \obj -> do
                    x <- obj .: "x"
                    y <- obj .: "y"
                    pure (x,y)

        forM_ mxy $ \(newX,newY) ->  do
          oldX <- liftIO $ readIORef mouseXRef
          oldY <- liftIO $ readIORef mouseYRef
          liftIO $ do
            publish mousePositionN (MousePosition newX newY)

            when (oldX /= Just newX) $ do
              publish mouseXN newX
              writeIORef mouseXRef (Just newX)

            when (oldY /= Just newY) $ do
              publish mouseYN newY
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

onMouseX :: (MonadIO c, '[Evented] <: ms)
         => (Int -> Ef ms c ()) -> Ef ms c (IO ())
onMouseX f = do
  buf <- get
  Just stopper <- demandMaybe =<< with mouseS (do
    MouseState {..} <- get
    connect_ mouseXN (return buf) (lift . f)
    )
  return stopper

onMouseY :: (MonadIO c, '[Evented] <: ms)
         => (Int -> Ef ms c ()) -> Ef ms c (IO ())
onMouseY f = do
  buf <- get
  Just stopper <- demandMaybe =<< with mouseS (do
    MouseState {..} <- get
    connect_ mouseYN (return buf) (lift . f)
    )
  return stopper

onMousePosition :: (MonadIO c, '[Evented] <: ms)
                => (MousePosition -> Ef ms c ())
                -> Ef ms c (IO ())
onMousePosition f = do
  buf <- get
  Just stopper <- demandMaybe =<< with mouseS (do
    MouseState {..} <- get
    connect_ mousePositionN (return buf) (lift . f)
    )
  return stopper
