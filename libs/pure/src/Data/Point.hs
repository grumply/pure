{-# language DerivingStrategies, DeriveGeneric, DeriveAnyClass, PolyKinds, RoleAnnotations #-}
module Data.Point where

import Data.JSON
import Data.Maybe (fromMaybe)
import Data.Variance (mean,varies)
import GHC.Generics

data Point (domain :: k) = Point 
  { _x :: {-# UNPACK #-}!Double 
  , _y :: {-# UNPACK #-}!Double
  } deriving stock (Eq,Ord,Generic)
    deriving anyclass (ToJSON,FromJSON)
type role Point nominal

instance Show (Point d) where
  show (Point x y) = show (x,y)

instance Num (Point d) where
  (Point x1 y1) + (Point x2 y2) = Point (x1 + x2) (y1 + y2)
  (Point x1 y1) - (Point x2 y2) = Point (x1 - x2) (y1 - y2)
  (Point x1 y1) * (Point x2 y2) = Point (x1 * x2) (y1 * y2)
  negate (Point x y) = Point (-x) (-y)
  abs (Point x y) = Point (abs x) (abs y)
  signum (Point x y) = Point (signum x) (signum y)
  fromInteger n = Point (fromInteger n) (fromInteger n)

instance Fractional (Point d) where
  (Point x1 y1) / (Point x2 y2) = Point (x1 / x2) (y1 / y2)
  recip (Point x y) = Point (recip x) (recip y)
  fromRational r = Point (fromRational r) (fromRational r)

instance Floating (Point d) where
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

midpoint :: Point d -> Point d -> Point d
midpoint (Point x1 y1) (Point x2 y2) = Point ((x1 + x2) / 2) ((y1 + y2) / 2)

normalize :: Point d -> Point d
normalize (Point 0 0) = Point 0 0
normalize p@(Point x y) = let m = magnitude p in Point (x/m) (y/m)

scale :: Double -> Point d -> Point d
scale s (Point x y) = Point (s * x) (s * y)

magnitude :: Point d -> Double
magnitude (Point x y) = sqrt (x ^ 2 + y ^ 2)

distance :: Point d -> Point d -> Double
distance (Point ox oy) (Point tx ty) = sqrt((tx - ox) ^ 2 + (ty - oy) ^ 2)

dot :: Point d -> Point d -> Double
dot (Point x1 y1) (Point x2 y2) = x1 * x2 + y1 * y2

cross :: Point d -> Point d -> Double
cross (Point x1 y1) (Point x2 y2) = x1 * y2 - y1 * x2

rotate :: Double -> Point d -> Point d
rotate theta (Point x y) = Point (x * cos theta - y * sin theta) (x * sin theta + y * cos theta)

direction :: Point d -> Point d -> Point d
direction a b = normalize (b - a)

angle :: Point d -> Point d -> Double
angle a b = acos $ dot a b / (magnitude a * magnitude b)

translate :: Point d -> Point d -> Point d
translate = (+)

point :: Double -> Double -> Point d
point = Point

coords :: Point d -> (Double,Double)
coords (Point _x _y) = (_x,_y)

x :: Functor f => (Double -> f Double) -> Point d -> f (Point d)
x f (Point _x _y) = fmap (`Point` _y) (f _x)

y :: Functor f => (Double -> f Double) -> Point d -> f (Point d)
y f (Point _x _y) = fmap (Point _x) (f _y)

xy :: Functor f => ((Double,Double) -> f (Double,Double)) -> Point d -> f (Point d)
xy f (Point _x _y) = fmap (uncurry Point) (f (_x,_y))

average :: [Point d] -> Point d
average ps = Point 
  (fromMaybe 0 (mean (varies (\(Point x _) -> x) ps)))
  (fromMaybe 0 (mean (varies (\(Point _ y) -> y) ps)))

