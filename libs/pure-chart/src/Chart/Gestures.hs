module Chart.Gestures where

import qualified Base
import Chart.Config
import Chart.Point
import Chart.Viewport
import Data.DOM (Node,preventDefault,JSV)
import Data.IORef
import Data.Point as Point
import Data.Variance
import Prelude hiding (pointer)
import System.IO.Unsafe

gestures :: Exists Gestures => View -> ((Exists Options, Modify Box) => View)
gestures v = let Gestures gs = it in go gs v
  where
    go [] v = v
    go (c:cs) v = prove c (go cs v)

newtype Gestures = Gestures [Config]
instance Default Gestures where
  def = Gestures [pointer True,panner def,zoomer def,resetter True]

-- `pointer` is the exclusive updater of `_foci` in `State Box`
{-# INLINE pointer #-}
pointer :: Bool -> Config
pointer supportTouch = Proof proof
  where
    proof :: Modify Box => View -> View
    proof = mouse . touch
      where
        mouse                = mouseEnters mouseEnter . mouseMoves mouseMove 
        touch | supportTouch = touchStarts touchStart . touchMoves touchMove 
              | otherwise    = id
        
        mouseEnter :: Exists MouseEnter => IO ()
        mouseEnter = let MouseEnter me = it in with me (move [mousePointSVG])

        mouseMove :: Exists MouseMove => IO ()
        mouseMove = let MouseMove me = it in with me (move [mousePointSVG])

        touchStart :: Exists TouchStart => IO ()
        touchStart = let TouchStart te = it in with te (move touchPointsSVG)

        touchMove :: Exists TouchMove => IO ()
        touchMove = let TouchMove te = it in with te (move touchPointsSVG)
        
        move :: [Point SVG] -> IO ()
        move = unsafeSetFoci 

data PannerConfig = PannerConfig
  { supportTouch :: Bool
  , panButton :: Exists MouseEvent => Bool
  , panFingers :: Exists TouchEvent => Bool
  }
instance Default PannerConfig where
  def = PannerConfig True (unmodified Primary) True

{-# INLINE panner #-}
panner :: PannerConfig -> Config
panner PannerConfig {..} = Proof proof
  where
    proof :: Modify Box => View -> View
    proof = mouse . touch 
      where
        mouse                = mouseDowns mouseDown . mouseMoves mouseMove . mouseLeaves mouseLeave . mouseUps mouseUp 
        touch | supportTouch = touchStarts touchStart . touchMoves touchMove . touchEnds touchEnd 
              | otherwise    = id

        {-# NOINLINE panning #-}
        panning :: IORef Bool
        panning = unsafePerformIO (newIORef False)

        {-# NOINLINE lastPosition #-}
        lastPosition :: IORef (Point SVG)
        lastPosition = unsafePerformIO (newIORef 0) 

        mouseDown :: Exists MouseDown => IO ()
        mouseDown = let MouseDown me = it in with me do
          let MouseEvent { eventObject } = it
          preventDefault eventObject
          when panButton do
            writeIORef lastPosition mousePointSVG
            writeIORef panning True

        mouseMove :: Exists MouseMove => IO ()
        mouseMove = let MouseMove me = it in with me do
          p <- readIORef panning
          when p do
            -- It is worrying that this is not correct:
            --
            --  > old <- readIORef lastPosition
            --  > let new = mousePointSVG
            --  > writeIORef lastPosition new
            --  > pan (old - new)
            --
            -- Because the inferred type of `new` is the 
            -- unconstrained `Point SVG`, GHC floats it out?
            --
            -- Even this isn't correct:
            --
            --  > let new = mousePointSVG :: Exists MouseMove => Point SVG
            --
            -- Since the inferred type is still `new :: Point SVG`. 
            --
            -- I consider it sheer luck that 
            --
            -- > new :: Exists MouseMove => Point SVG
            --
            -- isn't reduced to 
            -- 
            -- > new :: Point SVG
            --
            -- since there is a dictionary in scope....
            --
            -- Hopefully, 9.x will fix this, when we can upgrade
            -- to the new JS backend.
            --
            old <- readIORef lastPosition
            let new :: Exists MouseMove => Point SVG
                new = mousePointSVG
            writeIORef lastPosition new
            pan (old - new)


        mouseUp :: IO ()
        mouseUp = writeIORef panning False

        mouseLeave :: IO ()
        mouseLeave = writeIORef panning False

        touchStart :: Exists TouchStart => IO ()
        touchStart = let TouchStart te = it in with te do
          when panFingers do
            preventDefault touchEvent
            writeIORef lastPosition (Point.average touchPointsSVG)
            writeIORef panning True

        touchMove :: Exists TouchMove => IO ()
        touchMove = let TouchMove te = it in with te do
          p <- readIORef panning
          when p do
            preventDefault touchEvent
            t0 <- readIORef lastPosition
            writeIORef lastPosition (Point.average touchPointsSVG) 
            pan (t0 - Point.average touchPointsSVG)

        touchEnd :: IO ()
        touchEnd = writeIORef panning False 

data ZoomerConfig = ZoomerConfig
  { supportTouch :: Bool 
  , zoomWheel    :: Exists WheelEvent => Maybe Double
  , zoomFingers  :: Exists TouchEvent => Bool
  }
instance Default ZoomerConfig where
  def = ZoomerConfig True (Just 1) (touchCount > 1)

{-# INLINE zoomer #-}
zoomer :: ZoomerConfig -> Config
zoomer ZoomerConfig {..} = Proof proof
  where
    proof :: Modify Box => View -> View
    proof = mouse . touch
      where
        mouse                = wheels wheel
        touch | supportTouch = touchStarts touchStart . touchMoves touchMove 
              | otherwise    = id

        wheel :: Exists Wheel => IO ()
        wheel = let Wheel we = it in with we do
          for_ zoomWheel \d -> do
            let WheelEvent { eventObject, deltaY, mouseEvent = MouseEvent { ctrlKey } } = it
            preventDefault eventObject
            zoomPosition (1 + (d * ((if ctrlKey then negate else id) (signum deltaY) / 10)))

        {-# NOINLINE lastDistance #-}
        lastDistance :: IORef Double
        lastDistance = unsafePerformIO (newIORef 0)

        touchStart :: Exists TouchStart => IO ()
        touchStart = let TouchStart te = it in with te do
          when zoomFingers do
            writeIORef lastDistance do
              let mid    = Point.average touchPoints
                  deltas = fmap (distance mid) touchPoints
              fromMaybe 0 (mean (varies id deltas))
            
        touchMove :: Exists TouchMove => IO ()
        touchMove = let TouchMove te = it in with te do
          when zoomFingers do
            preventDefault touchEvent
            prev <- readIORef lastDistance
            let 
              mid    = Point.average touchPoints
              deltas = fmap (distance mid) touchPoints
              new    = fromMaybe 0 (mean (varies id deltas))
            writeIORef lastDistance new
            zoomPosition (new / prev)

{-# INLINE resetter #-}
resetter :: Bool -> Config
resetter supportTouch = Proof proof
  where
    proof :: (Exists Options, Modify Box) => View -> View
    proof = mouse . touch
      where
        mouse                = doubleClicks resets 
        touch | supportTouch = touchStarts touchStart . touchMoves touchMove
              | otherwise    = id

        resets :: Exists DoubleClick => IO ()
        resets = let DoubleClick me = it in with me do
          let MouseEvent { eventObject } = it
          preventDefault eventObject
          put initialBox { _foci = [mousePointSVG] }
          
        {-# NOINLINE lastTouch #-}
        lastTouch :: IORef Time
        lastTouch = unsafePerformIO (newIORef 0)

        -- Since browsers don't have support for double-tap, we have to emulate it.
        -- When a single finger touches, store the target and the time and check if
        -- a previous single touch occurred on the same target within 200ms, and, if
        -- it has, trigger a reset (initialBox + current position). A 'touchmove'
        -- listener overwrites the stored touch, so that 'tap' -> 'move' -> 'tap'
        -- will not trigger a reset.
        touchStart :: Exists TouchStart => IO ()
        touchStart = let TouchStart te = it in with te do
          when (touchCount == 1) do
            preventDefault touchEvent
            t0 <- readIORef lastTouch
            t1 <- time
            writeIORef lastTouch t1
            when (t1 - t0 < Milliseconds 200 0) do
              put initialBox { _foci = touchPointsSVG }

        touchMove :: IO ()
        touchMove = writeIORef lastTouch 0

modified :: Exists MouseEvent => Button -> Bool
modified b =
  let MouseEvent { button, altKey, ctrlKey, metaKey, shiftKey } = it 
  in b == button && Base.or [altKey,ctrlKey,metaKey,shiftKey]

unmodified :: Exists MouseEvent => Button -> Bool
unmodified b = 
  let MouseEvent { button, altKey, ctrlKey, metaKey, shiftKey } = it 
  in b == button && not (Base.or [altKey,ctrlKey,metaKey,shiftKey])

touchEvent :: Exists TouchEvent => JSV
touchEvent = eventObject (it :: TouchEvent)

touchCount :: Exists TouchEvent => Int
touchCount = length (touches it)

