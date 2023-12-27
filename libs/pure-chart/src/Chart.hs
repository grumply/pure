{-# language CPP, ConstraintKinds, NoMonoLocalBinds #-}
module Chart where

import Chart.Config
import Chart.Gestures
import Chart.Point
import Chart.Transformation
import Chart.Viewport

import qualified Base
import Control.Monad
import Data.Kind
import qualified Data.List as List
import Data.SVG hiding (Transform)
import Data.DOM as DOM (Node(..),JSV,(.#),preventDefault,nullJSV,Rect(..))
import Data.Point
import Data.Variance

import Data.IORef
import Data.Ratio
import System.IO.Unsafe
import Prelude hiding (pointer)
import Unsafe.Coerce

{- |

Reduced to the simplest possible approach, this isn't even really a charting
API, or, even, an API at all. We reduce everything within a chart/graphic to
a 'Graphic':

> type Graphic a = a -> View

Then, a chart is just:

> type Mosaic a = [Graphic a] -> Graphic a

Which reduces to:

> type Mosaic a = [a -> View] -> a -> View

On top of this abstraction, we implement all shapes with `<path>`, rather than
other simple <svg> shapes. This makes it easy to have a standardized API for
combining and transforming shapes without having to special-case anything.

The compilation target is a `Command` type that represents the raw <path> `d`
attribute commands in a 1-to-1 fashion. A `d` attribute is implemented as a
`[Command]`, which we type synonym as `D`. 

To add type-safety, we start with a `SubPath (oc :: OC)` (OC for Open/Closed) 
type that represents either open or closed subpaths as components of a raw path
`d` attribute, using a GADT. Then, we wrap these `SubPath (oc :: OC)` into a 
`SomeSubPath` existential to allow `type Subpaths = [SomeSubPath]` as an
alternative representation of `type Path = [Command]` that carries more 
information about the subpath. This gives us a nicer API for working with
arbitrary path `d` attributes so that some transformation algorithms don't need
to keep track of starting and ending subpaths.

For the intermediate, constructivist representation, we have `Path` that eases 
working with paths, including basic shapes. All `Path`s are guaranteed to have
absolute `Command`s.

On top of `Path` we have `Gather` and `Fragment`. A `Gather` is a list of `Path`
and a `Fragment` is a `Gather` plus a `Transformation`. The transformation can
be updated in O(1) without having to traverse the commands in the paths. 
Combining `Fragment`s forces their transformations to be applied, but, if no
combing of fragments is done, the transformation is applied via CSS.

-}


--------------------------------------------------------------------------------
-- The Graphic API

type Graphic a = a -> View
type Mosaic a = [Graphic a] -> Graphic a

--------------------------------------------------------------------------------
-- Chart

type Charting = (Viewport, Exists Options, Exists Gestures)

-- | Introduce a charting context.
--
-- > withChart def def do
-- >   {...}
--
-- Note: the supplied dimensions are the initial dimensions and the
-- viewport/chart is not reactive to dimensional changes. You may change
-- the dimensions after initialization by modifying the viewport's `Box` state.
--
{-# INLINE charting #-}
charting :: Options -> Gestures -> (Charting => View) -> View
charting o g v = with o (with g (withViewport v))
   
-- | An svg context in Quadrant I of the cartesian plane with zoom and pan.
-- 
-- > myChart :: Chart => View
-- > myChart = chart [ Stroke black . Fill red . path ] (star center 5 10 5)
-- > 
-- > main = run (withChart (W 50) (H 50) def myChart)
--
{-# INLINE chart #-}
chart :: Charting => Mosaic a
chart ps a =
  Svg <| viewBox . gestures . Prelude.Style "cursor" "default" |> 
    [ G <| Transform ("matrix(1, 0, 0, -1, 0, " <> toTxt Chart.Config.height <> ")") |> 
      [ p a | p <- ps ] 
    ]

data ChartText
instance Theme ChartText where
  theme c = is c do
    transform =: Prelude.scaleY(-1)
    "-webkit-user-select" =: none
    "user-select" =: none
    "pointer-events" =: none
    "cursor" =: "pointer"

text :: Charting => Point Chart -> Txt -> View
text (Point x y) t = Text <| X (toTxt x) . Y (toTxt (negate y)) . Themed @ChartText |> [ fromTxt t ]

{-
type Annotation a = Graphic a
type Axis a = Graphic a
-}

topLeft :: Charting => Point Chart
topLeft = 
  let (x,y,_,_) = viewbox 
      Chart.Config.Margin {..} = Chart.Config.margin
  in toChart (Point (x + left) (y + top))

topRight :: Charting => Point Chart
topRight = 
  let (x,y,w,_) = viewbox
      Chart.Config.Margin {..} = Chart.Config.margin
  in toChart (Point (x + w - right) (y + top))

bottomLeft :: Charting => Point Chart
bottomLeft = 
  let (x,y,_,h) = viewbox 
      Chart.Config.Margin {..} = Chart.Config.margin
  in toChart (Point (x + left) (y + h - bottom))

bottomRight :: Charting => Point Chart
bottomRight = 
  let (x,y,w,h) = viewbox 
      Chart.Config.Margin {..} = Chart.Config.margin
  in toChart (Point (x + w - right) (y + h - bottom))

top :: Charting => Point Chart
top = topLeft { _x = 0 } 

bottom :: Charting => Point Chart
bottom = bottomLeft { _x = 0 } 

left :: Charting => Point Chart
left = topLeft { _y = 0 }

right :: Charting => Point Chart
right = topRight { _y = 0 }

width :: Charting => Point Chart
width = Chart.right - Chart.left

height :: Charting => Point Chart
height = Chart.top - Chart.bottom

center :: Charting => Point Chart
center = x + y
  where
    x, y :: Charting => Point Chart
    x = Chart.left + Chart.width / 2
    y = Chart.bottom + Chart.height / 2 
    
marginTop :: Exists Options => Point Chart
marginTop = let Chart.Config.Margin {..} = Chart.Config.margin in Point 0 top

marginRight :: Exists Options => Point Chart
marginRight = let Chart.Config.Margin {..} = Chart.Config.margin in Point right 0

marginBottom :: Exists Options => Point Chart
marginBottom = let Chart.Config.Margin {..} = Chart.Config.margin in Point 0 bottom

marginLeft :: Exists Options => Point Chart
marginLeft = let Chart.Config.Margin {..} = Chart.Config.margin in Point left 0

marginInline :: Exists Options => Point Chart
marginInline = marginLeft + marginRight

marginBlock :: Exists Options => Point Chart
marginBlock = marginTop + marginBottom
