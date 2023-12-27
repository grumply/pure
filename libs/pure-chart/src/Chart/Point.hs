{-# language CPP #-}
module Chart.Point where

import qualified Base
import Data.Point
import Data.DOM (Node(..),(.#),JSV)
import Chart.Transformation (transformPoint)

--------------------------------------------------------------------------------

-- | Client points are in the viewport's pixel space, in quadrant IV.
data Client

-- | SVG points are in the SVG user coordinate space, in quadrant IV.
data SVG

-- | Chart points are in the natural charting space, in quadrant I.
data Chart

homo :: (Double -> Double -> Double) -> Point d -> Point d -> Point d
homo f (Point ox oy) (Point tx ty) = Point (f ox tx) (f oy ty)

hetero :: (Double -> Double -> Double) -> (Double -> Double -> Double) -> Point d -> Point d -> Point d
hetero fx fy (Point ox oy) (Point tx ty) = Point (fx ox tx) (fy oy ty)

scalePointRelative :: Double -> Double -> Point d -> Point d -> Point d
scalePointRelative sx sy (Point ox oy) (Point x y) = 
  Point (sx * (x - ox) + ox) (sy * (y - oy) + oy)

-- >>> scaleTo' 0.5 0.5 (Point 10 10) (Point 20 20)
-- Point {_x = 15.0, _y = 15.0}
scaleTo' :: Double -> Double -> Point d -> Point d -> Point d
scaleTo' sx sy = hetero (f sx) (f sy)
  where f s o t = o + (t - o) * s

-- >>> scaleTo 0.5 (Point 10 10) (Point 20 20)
scaleTo :: Double -> Point d -> Point d -> Point d
scaleTo s = scaleTo' s s

-- >>> scaleToX 0.5 (Point 10 10) (Point 20 20)
-- Point {_x = 15.0, _y = 20.0}
scaleToX :: Double -> Point d -> Point d -> Point d
scaleToX s = scaleTo' s 0

-- >>> scaleToY 0.5 (Point 10 10) (Point 20 20)
-- Point {_x = 10.0, _y = 15.0}
scaleToY :: Double -> Point d -> Point d -> Point d
scaleToY = scaleTo' 0

-- >>> moveTo' 5 5 (Point 10 10) (Point 20 20)
-- Point {_x = 13.535533905932738, _y = 13.535533905932738}
moveTo' :: Double -> Double -> Point d -> Point d -> Point d
moveTo' xd yd o t 
  | d /= 0 = scaleTo' (xd / d) (yd / d) o t
  | otherwise = o
  where 
    d = distance o t

-- >>> moveTo 5 (Point 10 10) (Point 20 20)
moveTo :: Double -> Point d -> Point d -> Point d
moveTo d = moveTo' d d

-- >>> moveToX 2 (Point 10 10) (Point 20 20)
-- Point {_x = 11.414213562373096, _y = 10.0}
moveToX :: Double -> Point d -> Point d -> Point d
moveToX d = moveTo' d 0

-- >>> moveToY 2 (Point 10 10) (Point 20 20)
-- Point {_x = 10.0, _y = 11.414213562373096}
moveToY :: Double -> Point d -> Point d -> Point d
moveToY = moveTo' 0

toSVGPoint :: Node -> Point Client -> Point SVG
toSVGPoint n (Point cx cy) = do
#ifdef __GHCJS__
  let v = to_svg_point_js n cx cy
  Point 
    (fromMaybe (Base.error "toSVGPoint: invalid x") (v .# "x")) 
    (fromMaybe (Base.error "toSVGPoint: invalid y") (v .# "y"))
#else
  Point cx cy
#endif

#ifdef __GHCJS__
foreign import javascript unsafe 
  "var p = $1.createSVGPoint(); p.x = $2; p.y = $3; $r = p.matrixTransform($1.getScreenCTM().inverse());"
    to_svg_point_js :: Node -> Double -> Double -> JSV
#endif

mousePoint :: Exists MouseEvent => Point Client
mousePoint = 
  let MouseEvent { clientX, clientY } = it 
  in Point clientX clientY

mousePointSVG :: Exists MouseEvent => Point SVG
mousePointSVG = 
  let MouseEvent { currentTarget } = it 
  in toSVGPoint currentTarget mousePoint

touchPoints :: Exists TouchEvent => [Point Client]
touchPoints = fmap point (touches it)
  where point Touch { clientX, clientY } = Point clientX clientY

touchPointsSVG :: Exists TouchEvent => [Point SVG]
touchPointsSVG = 
  let TouchEvent { currentTarget } = it
  in fmap (toSVGPoint currentTarget) touchPoints

scaleFactor :: Node -> Double -> Double -> Point Client
scaleFactor (coerce -> n) w h = fromMaybe (Base.error "invalid scale factor") do
  cw <- n .# "clientWidth" 
  ch <- n .# "clientHeight"
  pure (Point (cw / w) (ch / h))
