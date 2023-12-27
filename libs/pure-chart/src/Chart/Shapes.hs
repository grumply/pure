module Chart.Shapes where

import qualified Base
import Chart.Commands
import Chart.Path
import Chart.Point
import Data.Point

--------------------------------------------------------------------------------
-- Basic SVG Shapes implemented with D
--
-- By implementing the simple SVG shapes with the `D` type, we can, for
-- instance, analyze the centroid of a `rect` or a `polyline` or `polygon`. We
-- can translate and scale. Ultimately, the cost is slightly higher

type Line = (Point Chart,Point Chart)
line :: Line -> Path
line (start,end) = 
  Path
    [ MoveTo Absolute start
    , LineTo Absolute end
    ]

type Circle = (Point Chart,Double)
circle :: Circle -> Path
circle (center,radius) = ellipse (center,radius,radius)

type Ellipse = (Point Chart,Double,Double)
ellipse :: Ellipse -> Path
ellipse (Point cx cy,rx,ry) = 
  Path
    [ MoveTo Absolute (Point cx (cy - ry))
    , Arc Relative (Point rx ry) 0 True False (Point 0 (2 * ry))
    , Arc Relative (Point rx ry) 0 True False (Point 0 (-2 * ry))
    ]

type Rect = (Point Chart,Double,Double)
rect :: Rect -> Path
rect (origin@(Point x y),width,height) =
  Path 
    [ MoveTo Absolute origin
    , LineTo Absolute (Point (x + width) y)
    , LineTo Absolute (Point (x + width) (y + height))
    , LineTo Absolute (Point x (y + height))
    , LineTo Absolute origin
    , Close
    ]

type Polyline = [Point Chart]
polyline :: Polyline -> Path
polyline [] = Path []
polyline (start:rest) = Path (MoveTo Absolute start : fmap (LineTo Absolute) rest)

type Polygon = [Point Chart]
polygon :: Polygon -> Path
polygon [] = Path []
polygon ps@(start:_) = let Path pl = polyline ps in Path (pl ++ [LineTo Absolute start])

-- | Produces a star shape `D` from:
--   - centroid
--   - inner radius
--   - outer radius
--   - clockwise rotation angle
--   - number of points
--
-- ## Orientation
-- By default, there will always be a point at the top.
-- For a star with one point down, use `pi` as the rotation. 
-- For a star with two points down, use `pi +/- (pi / points)` as the rotation.
-- 
-- ## Minimum Points
-- No stars with fewer than 3 points can be generated.
star :: Point Chart -> Double -> Double -> Double -> Int -> Path
star (Point cx cy) inner outer rot (Base.max 3 -> n) = 
  Path (MoveTo Absolute (point outer (theta 0)) : go 1)
  where
    theta :: Int -> Double
    theta i = fromIntegral i * (pi / fromIntegral n) + (pi / 2) - rot

    point :: Double -> Double -> Point Chart
    point radius t = Point (cx + radius * cos t) (cy + radius * sin t)

    go :: Int -> D
    go x 
      | x == n * 2 = [ Close ]
      | Base.odd x = LineTo Absolute (point inner (theta x)) : go (x + 1)
      | otherwise  = LineTo Absolute (point outer (theta x)) : go (x + 1)

