module Chart.Fragment where

import qualified Base
import Chart.Path (Path,transformPath)
import Chart.Gather (paths,Gather)
import Chart.Transformation (Transformation,toTransform)
import qualified Chart.Transformation as T
import Prelude hiding (reify)
import qualified Data.SVG as SVG

-- | A list of absolute paths and a transformation. This allows for composed 
-- transformations without re-traversal.  
data Fragment = Fragment (Maybe Transformation) Gather

-- | Turn a `Fragment` into its fully-transformed `Gather`.
reify :: Fragment -> Gather
reify (Fragment (Just t) ps) = fmap (transformPath t) ps
reify (Fragment _ ps) = ps

-- | Apply the internal transformation to a `Fragment` and reset the 
-- transformation to the identity.
--
-- > rectify = Fragment mempty . reify
--
rectify :: Fragment -> Fragment
rectify = Fragment mempty . reify

instance Monoid Fragment where 
  mempty = Fragment mempty mempty

instance Semigroup Fragment where
  (<>) l r = Fragment mempty (reify l <> reify r)

gather :: Gather -> Fragment
gather = Fragment mempty

fragment :: Fragment -> View
fragment (Fragment t ps) = SVG.G <| maybe id toTransform t |> Chart.Gather.paths ps

-- | Modify the transformation of a fragment.
--
-- For example, to tranlate a fragment by 10 units on both axes:
--
-- > new = transformation (translation 10 10) original
--
transformation :: (Transformation -> Transformation) -> (Fragment -> Fragment)
transformation f (Fragment t p) = Fragment (Just (f (fromMaybe mempty t))) p

-- | Fold a path into a fragment. This is inefficient, as the existing `Gather`
-- must be reified. Use `gather` to batch fold.
fold :: Path -> (Fragment -> Fragment)
fold p (reify -> ps) = Fragment mempty (p:ps)

translation :: Double -> Double -> Fragment -> Fragment
translation x y = transformation (<> T.translation x y)

translationX :: Double -> Fragment -> Fragment
translationX x = transformation (<> T.translationX x)

translationY :: Double -> Fragment -> Fragment
translationY y = transformation (<> T.translationY y)

rotation :: Double -> Fragment -> Fragment
rotation angle = transformation (<> T.rotation angle)

skew :: Double -> Double -> Fragment -> Fragment
skew x y = transformation (<> T.skew x y)

skewX :: Double -> Fragment -> Fragment
skewX x = transformation (<> T.skewX x)

skewY :: Double -> Fragment -> Fragment
skewY y = transformation (<> T.skewY y)

scale :: Double -> Double -> Fragment -> Fragment
scale x y = transformation (<> T.scale x y)

scaleX :: Double -> Fragment -> Fragment
scaleX x = transformation (<> T.scaleX x)

scaleY :: Double -> Fragment -> Fragment
scaleY y = transformation (<> T.scaleY y)

flipH :: Fragment -> Fragment
flipH = transformation (<> T.flipH)

flipV :: Fragment -> Fragment
flipV = transformation (<> T.flipV)

