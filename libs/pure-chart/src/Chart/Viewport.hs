{-# language CPP, PatternSynonyms, ExistentialQuantification #-}
module Chart.Viewport where

import qualified Base
import Chart.Config
import Chart.Point
import Data.DOM (Node,Rect(..))
import Data.Ease
import Data.List as List
import Data.Point as Point
import Data.SVG (pattern ViewBox)
import Data.Word
import Unsafe.Coerce
import Prelude hiding (focus,origin,middle,within)

type Viewport = State Box

-- | An initial view box derived from an existential width and height.
initialBox :: Exists Options => Box
initialBox = fix \b -> Box
  { _viewbox = 
      let Point x y = Chart.Config.origin 
          Chart.Config.Margin {..} = Chart.Config.margin
      in (x - left,negate y - top,Chart.Config.width + left + right,Chart.Config.height + top + bottom)
  , _foci = []
  , _zoom = 1
  , _checkpoint = pure b
  }

--------------------------------------------------------------------------------
-- Box accessor and query functions

-- | Witness a `State Box` context, given a dimensional context.
withViewport :: (State Box => View) -> (Exists Options => View)
withViewport = state initialBox

-- | Construct an SVG 'viewBox' attribute from an existential view box.
viewBox :: Exists Box => View -> View
viewBox = let (x,y,w,h) = viewbox in
  ViewBox (toTxt x <<>> toTxt y <<>> toTxt w <<>> toTxt h)

-- | The current view box origin and bounds. 
viewbox :: Exists Box => (Double,Double,Double,Double)
viewbox = _viewbox it

-- | Query for containment of a point within the bounds of a box.
contains :: Box -> Point SVG -> Bool
contains Box { _viewbox = (x,y,w,h) } (Point px py) =
  px > x && px < x + w && py > y && py < y + h

-- | Query for non-containment of a point within the bounds of a box.
excludes :: Box -> Point SVG -> Bool
excludes b = not . (b `contains`)

-- | The current zoom factor from an existenital view box.
zoom :: Exists Box => Double
zoom = _zoom it

-- | The middle point of an existential view box.
middle :: Exists Box => Point SVG
middle = Point.average [Chart.Viewport.origin,bound]

-- | The origin point of an existential view box.
origin :: Exists Box => Point SVG
origin = let (x,y,_,_) = viewbox in Point x y

-- | The extent point of an existential view box.
bound :: Exists Box => Point SVG
bound = let (x,y,w,h) = viewbox in Point x y + Point w h

-- | List of focal points of the view box. This is generally either a single
-- point from a mouse event, or multiple points from a touch event. It could be
-- a list of points added programmatically, though.
foci :: Exists Box => [Point SVG]
foci = _foci it

-- | Focal point of the view box. Either the average of the foci, or the middle
-- of the viewbox.
focus :: Exists Box => Point SVG
focus =
  case length foci of
    0 -> Point.average [Chart.Viewport.origin,bound]
    _ -> Point.average foci

-- | View the checkpointed view box. The initial viewbox peeks itself.
peek :: Exists Box => IO Box
peek = _checkpoint it

--------------------------------------------------------------------------------
-- Low-level, unsafe, Box state modification functions

-- | Set the current viewbox origin and dimensions.
--
-- ## Unsafe
-- `viewbox` should only be updated relative to a focus, so that the focus
-- remains valid. If you're implementing something like cursor click-and-drag
-- to zoom, see `zoomTo`.
unsafeSetViewbox :: Modify Box => (Double,Double,Double,Double) -> IO ()
unsafeSetViewbox vb = modify \b -> b { _viewbox = vb }

-- | Set the current foci.
--
-- ## Unsafe
-- `foci` should only be updated from a touch or mouse event, so that the
-- focus is guaranteed to be within the current view box.
unsafeSetFoci :: Modify Box => [Point SVG] -> IO ()
unsafeSetFoci p = modify \b -> b { _foci = p }

-- | Set the current focus.
--
-- ## Unsafe
-- `focus` should only be updated from a touch or mouse event, so that the
-- focus is guaranteed to be within the current view box.
unsafeSetFocus :: Modify Box => Point SVG -> IO ()
unsafeSetFocus p = unsafeSetFoci [p]

-- | Set the current zoom factor. 
--
-- ## Unsafe 
-- `zoom` should only be updated along with the `viewbox`, as in `zoomPoint`.
-- If you update `zoom`, you are resetting the implicit origin viewbox, and 
-- won't be able to return to it with `reset`.
--
unsafeSetZoom :: Modify Box => Double -> IO ()
unsafeSetZoom z = modify \b -> b { _zoom = z }

-- | Set the current checkpoint.
--
-- ## Unsafe
-- The checkpoint should probably only be updated by wrapping the existing view
-- box, so that it can be returned to. Otherwise, you might lose any existing
-- checkpoint traces. See `checkpoint`, which is written as:
-- 
-- > checkpoint = modify \b -> b { _checkpoint = pure b }
--
unsafeSetCheckpoint :: Modify Box => IO Box -> IO ()
unsafeSetCheckpoint iob = modify \b -> b { _checkpoint = iob }

--------------------------------------------------------------------------------
-- Mid-level Box state modification functions

-- | Set the viewbox to the given point bounds (bottom-left,top-right).
--
-- TODO: we probably need to switch, internally, to (top-left,bottom-right)?
zoomTo :: Modify Box => (Point SVG,Point SVG) -> IO ()
zoomTo (bl,tr) = modify \b -> b { _viewbox = (x,y,w,h) }
  where
    Point x y = Base.min bl tr
    Point bx by = Base.max bl tr
    w = bx - x
    h = by - y

-- | Zoom at a given svg point by a given zoom factor. 
zoomAt :: Point SVG -> Double -> Box -> Box
zoomAt (Point cx cy) f b =
    let
      -- Current viewport
      (x,y,w,h) = _viewbox b
     
      -- Compute relative position of the point within the viewbox
      rx = (cx - x) / w
      ry = (cy - y) / h

      -- Try to guarantee that a zoom will maintain positional invariance of
      -- the given point. It will take some effort to find an approach here
      -- that will work across most or all browsers.
      --
      -- TODO: improve
      precision :: Double -> Double
      precision i = 
        let p = realToFrac . nextFloat . nextFloat . realToFrac $ i
        in i - p

      expanse :: Double -> Double -> Double
      expanse offset span = Base.min (2e35 - offset) span 
     
      w' = expanse x (Base.max (Base.max (1/32) (precision x)) (w / f))
      h' = expanse y (Base.max (Base.max (1/32) (precision y)) (h / f))
      x' = cx - (w' * rx)
      y' = cy - (h' * ry)
      new = (x',y',w',h')

    in
      b { _viewbox = new
        , _zoom = f * _zoom b 
        }

  where
    nextFloat :: Float -> Float
    nextFloat x = 
        let wordRepresentation = unsafeCoerce x :: Word32
            nextWord = wordRepresentation + 1
        in unsafeCoerce nextWord

    prevFloat :: Float -> Float
    prevFloat x = 
        let wordRepresentation = unsafeCoerce x :: Word32
            prevWord = wordRepresentation - 1
        in unsafeCoerce prevWord

-- | Zoom into the middle of the view box. This is likely to be used by zooming
-- controls, like '+' and '-' buttons, rather than user touch or mouse input.
-- zoomCenter :: Modify Box => Double -> IO ()
-- zoomCenter d = modify \b -> with b (zoomAt middle d b)

-- | Zoom into the current focus. This is likely to be used to implement 
-- zooming gestures and events (two-finger pinch events/mouse wheel events).
zoomPosition :: Modify Box => Double -> IO ()
zoomPosition d = modify \b -> with b (zoomAt focus d b)

-- | Checkpoint the current view box so that you may return to it with `pop`.
checkpoint :: Modify Box => IO ()
checkpoint = modify \b -> b { _checkpoint = pure b }

-- | Replace the current view box with a checkpointed view box. The last
-- checkpoint will simply return itself, as the initialBox is defined with
-- `fix`.
pop :: Modify Box => IO ()
pop = modifyIO _checkpoint

-- | Reset to initial view box. The foci remain unchanged.
--
-- To reset the full `Box` state and not keep foci:
--
-- > put initialBox
--
reset :: (Exists Options, Modify Box) => IO ()
reset = modify \b -> initialBox { _foci = _foci b }

-- | Reset the current zoom level to ~1.
-- resetZoom :: State Box => IO ()
-- resetZoom = zoomCenter (1 / Chart.Viewport.zoom)

-- | Pan to the given origin point. 
panTo :: Modify Box => Point SVG -> IO ()
panTo (Point x y) = 
  modify \b -> 
    let (_,_,w,h) = _viewbox b

        clamp :: Double -> Double -> Double
        clamp span x = Base.min (2e35 - span) x 

    in b { _viewbox = (clamp w x,clamp h y,w,h) }

-- | Pan to the given center point. 
panToCenter :: Modify Box => Point SVG -> IO ()
panToCenter (Point x y) =
  modify \b ->
    let 
      (_,_,w,h) = _viewbox b
      x' = x - w / 2
      y' = y - h / 2
    in
      b { _viewbox = (x',y',w,h) }

pan :: Modify Box => Point SVG -> IO ()
pan d = 
  modify \b ->
    with b do
      let 
        (x,y,w,h) = viewbox
        new       = (x + _x d,y + _y d,w,h)
      b { _viewbox = new }

-- | Refocus the view box to a given origin, with, and height. 
refocus :: Modify Box => Origin -> Width -> Height -> IO ()
refocus (O (Point x y)) (W w) (H h) = modify \b -> b { _viewbox = (x,y,w,h) }

--------------------------------------------------------------------------------

cursor :: (Exists Options, Exists Box) => Maybe (Point Chart)
cursor = 
  case length foci of
    0 -> Nothing
    _ -> Just (toChart (Point.average foci))

cursor' :: (Exists Options, Exists Box) => Point Chart
cursor' = toChart Chart.Viewport.focus

cursor0 :: (Exists Options, Exists Box) => Point Chart
cursor0 = 
  case length foci of
    0 -> 0
    _ -> toChart (Point.average foci)

