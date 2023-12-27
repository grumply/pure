module Chart.Transformation where

import qualified Base
import qualified Control.Lens as Lens
import Data.Point

-- | A type for 2d matrix transformations. Instead of a functional approach,
-- we use a reified 2x3 affine transformation, since it is serializable. 
-- Instead of the overhead of the vector library, we just use a simple, 
-- 6-argument unpackable data structure of strict, unpacked doubles.
--
-- Note that the transformation is stored as: `a b c d e f` where
--
-- > | a c e |
-- > | b d f |
-- > | 0 0 1 |
--
data Transformation = Transformation 
  {-# UNPACK #-}!Double {-# UNPACK #-}!Double {-# UNPACK #-}!Double 
  {-# UNPACK #-}!Double {-# UNPACK #-}!Double {-# UNPACK #-}!Double
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (ToJSON,FromJSON)

instance Monoid Transformation where
  mempty = Transformation 1 0 0 1 0 0

-- | The Semigroup instance for Transformations is just matrix multiplication.
instance Semigroup Transformation where
  (<>) (Transformation a b c d e f) (Transformation a' b' c' d' e' f') =
    Transformation 
      (a*a' + c*b')
      (b*a' + d*b')
      (a*c' + c*d')
      (b*c' + d*d')
      (a*e' + c*f' + e)
      (b*e' + d*f' + f)

translation :: Double -> Double -> Transformation
translation = Transformation 1 0 0 1

translationX :: Double -> Transformation
translationX x = translation x 0

translationY :: Double -> Transformation
translationY = translation 0

rotation :: Double -> Transformation
rotation angle = Transformation (cos angle) (sin angle) (-sin angle) (cos angle) 0 0

skew :: Double -> Double -> Transformation
skew x y = Transformation 1 (Base.tan y) (Base.tan x) 1 0 0

skewX :: Double -> Transformation
skewX x = Chart.Transformation.skew x 0

skewY :: Double -> Transformation
skewY = Chart.Transformation.skew 0

scale :: Double -> Double -> Transformation
scale x y = Transformation x 0 0 y 0 0

scaleX :: Double -> Transformation
scaleX x = Chart.Transformation.scale x 1

scaleY :: Double -> Transformation
scaleY = Chart.Transformation.scale 1

flipH :: Transformation
flipH = Chart.Transformation.scaleX (-1)

flipV :: Transformation
flipV = Chart.Transformation.scaleY (-1)

transformPoint :: Transformation -> Point a -> Point a
transformPoint (Transformation a b c d e f) (Point x y) = Point x' y'
  where
    x' = a*x + c*y + e
    y' = b*x + d*y + f

toTransform :: Transformation -> View -> View
toTransform (Transformation a b c d e f) =
  Transform 
    ("matrix(" <> toTxt a 
        <> "," <> toTxt b 
        <> "," <> toTxt c
        <> "," <> toTxt d
        <> "," <> toTxt e
        <> "," <> toTxt f
        <> ")"
    )
