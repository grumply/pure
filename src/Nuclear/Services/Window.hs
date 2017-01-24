{-# language OverloadedStrings #-}
{-# language CPP #-}
module Nuclear.Services.Window where

import Ef.Base hiding (Object)

import Data.IORef

import Data.JSText

import Nuclear.Atom (Win,getWindow)
import Nuclear.Revent
import Nuclear.Service
import Nuclear.Signals
import Nuclear.With

#ifdef __GHCJS__
import qualified GHCJS.DOM.Window as W hiding (getWindow)
import qualified GHCJS.DOM.EventM as Ev
import qualified GHCJS.DOM.EventTargetClosures as ETC
import qualified GHCJS.Marshal as M
import qualified GHCJS.Marshal.Pure as M
import qualified JavaScript.Object.Internal as O
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
  { windowDimensionsN :: Network WindowDimensions
  , windowHeightN     :: Network Int
  , windowWidthN      :: Network Int
  , scrollPositionN   :: Network ScrollPosition
  , scrollXN          :: Network Int
  , scrollYN          :: Network Int
  }

type WindowS = (State () WindowState) ': Service_

scrollEvent :: EVName Win Object
scrollEvent =
#ifdef __GHCJS__
   ETC.unsafeEventName "scroll"
#else
  "scroll"
#endif

resize :: EVName Win Object
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

windowS :: S '[State () WindowState]
windowS = Service {..}
  where
    key = "Fusion.windowS"

    build base = do
      windowDimensionsN <- network
      windowHeightN     <- network
      windowWidthN      <- network
      scrollPositionN   <- network
      scrollXN          <- network
      scrollYN          <- network
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

      rb  <- getReventBuffer

      initialScrollX <- getScrollX
      scrollX_ <- liftIO $ newIORef initialScrollX
      initialScrollY <- getScrollY
      scrollY_ <- liftIO $ newIORef initialScrollY
      void $ onWindowNetwork scrollEvent $ \_ -> do
        oldX <- liftIO $ readIORef scrollX_
        newX <- getScrollX
        oldY <- liftIO $ readIORef scrollY_
        newY <- getScrollY
        liftIO $ do
          syndicate scrollPositionN (ScrollPosition newX newY)
          when (oldX /= newX) $ do
            syndicate scrollXN newX
            writeIORef scrollX_ newX
          when (oldY /= newY) $ do
            syndicate scrollYN newY
            writeIORef scrollY_ newY

      initialWidth <- getInnerWidth
      width_ <- liftIO $ newIORef initialWidth
      initialHeight <- getInnerHeight
      height_ <- liftIO $ newIORef initialHeight
      void $ onWindowNetwork resize $ \_ -> do
        oldWidth <- liftIO $ readIORef width_
        newWidth <- getInnerWidth
        oldHeight <- liftIO $ readIORef height_
        newHeight <- getInnerHeight
        liftIO $ do
          syndicate windowDimensionsN (WindowDimensions newHeight newWidth)
          when (oldWidth /= newWidth) $ do
            syndicate windowWidthN newWidth
            writeIORef width_ newWidth
          when (oldHeight /= newHeight) $ do
            syndicate windowHeightN newHeight
            writeIORef height_ newHeight

onScrollX :: (MonadIO c, '[Revent] <: ms)
          => (Int -> Code ms c ())
          -> Code ms c (Subscription ms c Int,Periodical ms c Int)
onScrollX f = do
  buf <- getReventBuffer
  p <- periodical
  Just s <- subscribe p (lift . f)
  with windowS $ do
    WindowState {..} <- get
    joinNetwork scrollXN p buf
  return (s,p)

onScrollY :: (MonadIO c, '[Revent] <: ms)
          => (Int -> Code ms c ())
          -> Code ms c (Subscription ms c Int,Periodical ms c Int)
onScrollY f = do
  buf <- getReventBuffer
  p <- periodical
  Just s <- subscribe p (lift . f)
  with windowS $ do
    WindowState {..} <- get
    joinNetwork scrollYN p buf
  return (s,p)

onScrollPosition :: (MonadIO c, '[Revent] <: ms)
                 => (ScrollPosition -> Code ms c ())
                 -> Code ms c (Subscription ms c ScrollPosition,Periodical ms c ScrollPosition)
onScrollPosition f = do
  buf <- getReventBuffer
  p <- periodical
  Just s <- subscribe p (lift . f)
  with windowS $ do
    WindowState {..} <- get
    joinNetwork scrollPositionN p buf
  return (s,p)

onWindowHeight :: (MonadIO c, '[Revent] <: ms)
               => (Int -> Code ms c ())
               -> Code ms c (Subscription ms c Int,Periodical ms c Int)
onWindowHeight f = do
  buf <- getReventBuffer
  p <- periodical
  Just s <- subscribe p (lift . f)
  with windowS $ do
    WindowState {..} <- get
    joinNetwork windowHeightN p buf
  return (s,p)

onWindowWidth :: (MonadIO c, '[Revent] <: ms)
              => (Int -> Code ms c ())
              -> Code ms c (Subscription ms c Int,Periodical ms c Int)
onWindowWidth f = do
  buf <- getReventBuffer
  p <- periodical
  Just s <- subscribe p (lift . f)
  with windowS $ do
    WindowState {..} <- get
    joinNetwork windowWidthN p buf
  return (s,p)

onWindowDimensions :: (MonadIO c, '[Revent] <: ms)
                   => (WindowDimensions -> Code ms c ())
                   -> Code ms c (Subscription ms c WindowDimensions,Periodical ms c WindowDimensions)
onWindowDimensions f = do
  buf <- getReventBuffer
  p <- periodical
  Just s <- subscribe p (lift . f)
  with windowS $ do
    WindowState {..} <- get
    joinNetwork windowDimensionsN p buf
  return (s,p)
