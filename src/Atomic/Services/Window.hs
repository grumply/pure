{-# language OverloadedStrings #-}
{-# language CPP #-}
module Atomic.Services.Window where

import Ef.Base hiding (Obj)

import Data.IORef

import Data.Txt
import Data.JSON

import Atomic.Component (Win,getWindow)
import Atomic.Service
import Atomic.Signals

#ifdef __GHCJS__
import qualified GHCJS.DOM.Window as W hiding (getWindow)
import qualified GHCJS.DOM.EventM as Ev
import qualified GHCJS.DOM.EventTargetClosures as ETC
#endif

import Unsafe.Coerce
import System.IO.Unsafe

data WindowDimensions = WindowDimensions
  { windowHeight :: Int
  , windowWidth  :: Int
  } deriving (Eq,Ord,Show)

data ScrollPosition = ScrollPosition
  { windowScrollX :: Int
  , windowScrollY :: Int
  }

data WindowState = WindowState
  { windowDimensionsN :: Syndicate WindowDimensions
  , windowHeightN     :: Syndicate Int
  , windowWidthN      :: Syndicate Int
  , scrollPositionN   :: Syndicate ScrollPosition
  , scrollXN          :: Syndicate Int
  , scrollYN          :: Syndicate Int
  }

scrollEvent :: EVName Win Obj
scrollEvent =
#ifdef __GHCJS__
   ETC.unsafeEventName "scroll"
#else
  "scroll"
#endif

resize :: EVName Win Obj
resize =
#ifdef __GHCJS__
  ETC.unsafeEventName "resize"
#else
  "resize"
#endif

#ifndef __GHCJS__
{-# NOINLINE scrollX_ #-}
scrollX_ :: IORef Int
scrollX_ = unsafePerformIO (newIORef 0)

{-# NOINLINE scrollY_ #-}
scrollY_ :: IORef Int
scrollY_ = unsafePerformIO (newIORef 0)

{-# NOINLINE innerWidth_ #-}
innerWidth_ :: IORef Int
innerWidth_ = unsafePerformIO (newIORef 0)

{-# NOINLINE innerHeight_ #-}
innerHeight_ :: IORef Int
innerHeight_ = unsafePerformIO (newIORef 0)
#endif

getScrollX :: MonadIO c => c Int
getScrollX = do
  win <- getWindow
#ifdef __GHCJS__
  x <- W.getScrollX win
#else
  x <- liftIO $ readIORef scrollX_
#endif
  return x

getScrollY :: MonadIO c => c Int
getScrollY = do
  win <- getWindow
#ifdef __GHCJS__
  y <- W.getScrollY win
#else
  y <- liftIO $ readIORef scrollY_
#endif
  return y

getInnerWidth :: MonadIO c => c Int
getInnerWidth = do
  win <- getWindow
#ifdef __GHCJS__
  iw <- W.getInnerWidth win
#else
  iw <- liftIO $ readIORef innerWidth_
#endif
  return iw

getInnerHeight :: MonadIO c => c Int
getInnerHeight = do
  win <- getWindow
#ifdef __GHCJS__
  ih <- W.getInnerHeight win
#else
  ih <- liftIO $ readIORef innerHeight_
#endif
  return ih

windowS :: Service '[State () WindowState]
windowS = Service {..}
  where
    key = "atomic.window"

    build base = do
      windowDimensionsN <- syndicate
      windowHeightN     <- syndicate
      windowWidthN      <- syndicate
      scrollPositionN   <- syndicate
      scrollXN          <- syndicate
      scrollYN          <- syndicate
      let ws = WindowState
                 windowDimensionsN
                 windowHeightN
                 windowWidthN
                 scrollPositionN
                 scrollXN
                 scrollYN
      return (state ws *:* base)

    prime = do
      WindowState {..} <- get

      win <- getWindow

      initialScrollX <- getScrollX
      scrollX_ <- liftIO $ newIORef initialScrollX
      initialScrollY <- getScrollY
      scrollY_ <- liftIO $ newIORef initialScrollY
      void $ onWindowSyndicate scrollEvent $ \_ -> do
        oldX <- liftIO $ readIORef scrollX_
        newX <- getScrollX
        oldY <- liftIO $ readIORef scrollY_
        newY <- getScrollY
        liftIO $ do
          publish scrollPositionN (ScrollPosition newX newY)
          when (oldX /= newX) $ do
            publish scrollXN newX
            writeIORef scrollX_ newX
          when (oldY /= newY) $ do
            publish scrollYN newY
            writeIORef scrollY_ newY

      initialWidth <- getInnerWidth
      width_ <- liftIO $ newIORef initialWidth
      initialHeight <- getInnerHeight
      height_ <- liftIO $ newIORef initialHeight
      void $ onWindowSyndicate resize $ \_ -> do
        oldWidth <- liftIO $ readIORef width_
        newWidth <- getInnerWidth
        oldHeight <- liftIO $ readIORef height_
        newHeight <- getInnerHeight
        liftIO $ do
          publish windowDimensionsN (WindowDimensions newHeight newWidth)
          when (oldWidth /= newWidth) $ do
            publish windowWidthN newWidth
            writeIORef width_ newWidth
          when (oldHeight /= newHeight) $ do
            publish windowHeightN newHeight
            writeIORef height_ newHeight

onScrollX :: (MonadIO c, '[Revent] <: ms)
          => (Int -> Code ms c ())
          -> Code ms c (IO ())
onScrollX f = do
  buf <- get
  Just stopper <- demandMaybe =<< with windowS (do
    WindowState {..} <- get
    connect_ scrollXN (return buf) (lift . f)
    )
  return stopper

onScrollY :: (MonadIO c, '[Revent] <: ms)
          => (Int -> Code ms c ())
          -> Code ms c (IO ())
onScrollY f = do
  buf <- get
  Just stopper <- demandMaybe =<< with windowS (do
    WindowState {..} <- get
    connect_ scrollYN (return buf) (lift . f)
    )
  return stopper

onScrollPosition :: (MonadIO c, '[Revent] <: ms)
                 => (ScrollPosition -> Code ms c ())
                 -> Code ms c (IO ())
onScrollPosition f = do
  buf <- get
  Just stopper <- demandMaybe =<< with windowS (do
    WindowState {..} <- get
    connect_ scrollPositionN (return buf) (lift . f)
    )
  return stopper

onWindowHeight :: (MonadIO c, '[Revent] <: ms)
               => (Int -> Code ms c ())
               -> Code ms c (IO ())
onWindowHeight f = do
  buf <- get
  Just stopper <- demandMaybe =<< with windowS (do
    WindowState {..} <- get
    connect_ windowHeightN (return buf) (lift . f)
    )
  return stopper

onWindowWidth :: (MonadIO c, '[Revent] <: ms)
              => (Int -> Code ms c ())
              -> Code ms c (IO ())
onWindowWidth f = do
  buf <- get
  Just stopper <- demandMaybe =<< with windowS (do
    WindowState {..} <- get
    connect_ windowWidthN (return buf) (lift . f)
    )
  return stopper

onWindowDimensions :: (MonadIO c, '[Revent] <: ms)
                   => (WindowDimensions -> Code ms c ())
                   -> Code ms c (IO ())
onWindowDimensions f = do
  buf <- get
  Just stopper <- demandMaybe =<< with windowS (do
    WindowState {..} <- get
    connect_ windowDimensionsN (return buf) (lift . f)
    )
  return stopper
