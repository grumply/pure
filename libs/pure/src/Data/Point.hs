{-# language DerivingStrategies, DeriveGeneric, DeriveAnyClass #-}
module Data.Point where

import Data.JSON
import Data.Maybe (fromMaybe)
import Data.Variance (mean,varies)
import GHC.Generics

data Point = Point 
  { _x :: {-# UNPACK #-}!Double 
  , _y :: {-# UNPACK #-}!Double
  } deriving stock (Eq,Ord,Generic)
    deriving anyclass (ToJSON,FromJSON)

instance Show Point where
  show (Point x y) = show (x,y)

instance Num Point where
  (Point x1 y1) + (Point x2 y2) = Point (x1 + x2) (y1 + y2)
  (Point x1 y1) - (Point x2 y2) = Point (x1 - x2) (y1 - y2)
  (Point x1 y1) * (Point x2 y2) = Point (x1 * x2) (y1 * y2)
  negate (Point x y) = Point (-x) (-y)
  abs (Point x y) = Point (abs x) (abs y)
  signum (Point x y) = Point (signum x) (signum y)
  fromInteger n = Point (fromInteger n) (fromInteger n)

instance Fractional Point where
  (Point x1 y1) / (Point x2 y2) = Point (x1 / x2) (y1 / y2)
  recip (Point x y) = Point (recip x) (recip y)
  fromRational r = Point (fromRational r) (fromRational r)

instance Floating Point where
  pi = Point pi pi
  exp (Point x y) = Point (exp x) (exp y)
  log (Point x y) = Point (log x) (log y)
  sin (Point x y) = Point (sin x) (sin y)
  cos (Point x y) = Point (cos x) (cos y)
  asin (Point x y) = Point (asin x) (asin y)
  acos (Point x y) = Point (acos x) (acos y)
  atan (Point x y) = Point (atan x) (atan y)
  sinh (Point x y) = Point (sinh x) (sinh y)
  cosh (Point x y) = Point (cosh x) (cosh y)
  asinh (Point x y) = Point (asinh x) (asinh y)
  acosh (Point x y) = Point (acosh x) (acosh y)
  atanh (Point x y) = Point (atanh x) (atanh y)

midpoint :: Point -> Point -> Point
midpoint (Point x1 y1) (Point x2 y2) = Point ((x1 + x2) / 2) ((y1 + y2) / 2)

normalize :: Point -> Point
normalize (Point 0 0) = Point 0 0
normalize p@(Point x y) = let m = magnitude p in Point (x/m) (y/m)

scale :: Double -> Point -> Point
scale s (Point x y) = Point (s * x) (s * y)

magnitude :: Point -> Double
magnitude (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point -> Point -> Double
distance (Point ox oy) (Point tx ty) = sqrt((tx - ox) ^ 2 + (ty - oy) ^ 2)

dot :: Point -> Point -> Double
dot (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

cross :: Point -> Point -> Double
cross (Point x1 y1) (Point x2 y2) = x1 * y2 - y1 * x2

rotate :: Double -> Point -> Point
rotate theta (Point x y) = Point (x * cos theta - y * sin theta) (x * sin theta + y * cos theta)

direction :: Point -> Point -> Point
direction a b = normalize (b - a)

angle :: Point -> Point -> Double
angle a b = acos $ dot a b / (magnitude a * magnitude b)

translate :: Point -> Point -> Point
translate = (+)

point :: Double -> Double -> Point
point = Point

coords :: Point -> (Double,Double)
coords (Point _x _y) = (_x,_y)

x :: Functor f => (Double -> f Double) -> Point -> f Point
x f (Point _x _y) = fmap (`Point` _y) (f _x)

y :: Functor f => (Double -> f Double) -> Point -> f Point
y f (Point _x _y) = fmap (Point _x) (f _y)

xy :: Functor f => ((Double,Double) -> f (Double,Double)) -> Point -> f Point
xy f (Point _x _y) = fmap (uncurry Point) (f (_x,_y))

average :: [Point] -> Point
average ps = Point 
  (fromMaybe 0 (mean (varies (\(Point x _) -> x) ps)))
  (fromMaybe 0 (mean (varies (\(Point _ y) -> y) ps)))

