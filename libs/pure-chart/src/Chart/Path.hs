module Chart.Path where

import Chart.Commands
import Chart.Subpath
import Chart.Transformation
import qualified Control.Lens as Lens
import qualified Data.SVG as SVG

--------------------------------------------------------------------------------
-- Path

-- Note that the `Path` constructor is not exported. To construct a `Path`, use
-- `path`, which forces a path to be fully absolute to allow for efficient
-- path concatenation.
newtype Path = Path { fromPath :: D }
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (ToJSON,FromJSON)

instance Show Path where
  show (Path p) = show (subpaths p)

toPath :: D -> Path
toPath = Path . toAbsolute 

-- mconcat . paths == id
paths :: Path -> [Path]
paths = fmap Path . unconcatOn isMoveTo . fromPath
  where
    isMoveTo (MoveTo _ _) = True
    isMoveTo _ = False

-- >>> unconcatOn (\i -> i `mod` 2 == 0) $ [1,2,3,4,5,6,7::Int]
-- [[1,2],[3,4],[5,6],[7]]
-- >>> concat . unconcatOn (\i -> i `mod` 2 == 0)$ [1,2,3,4,5,6,7::Int]
-- [1,2,3,4,5,6,7]
-- >>> unconcatOn (> 7) $ [1,2,3,4,5,6::Int]
-- [[1,2,3,4,5,6]]
unconcatOn :: (a -> Bool) -> [a] -> [[a]]
unconcatOn p = foldr f []
  where
    f x acc@(group:groups) 
      | p x       = [x] : acc
      | otherwise = (x : group) : groups
    f x []        = [[x]]

instance Monoid Path where mempty = Path []

-- | The semigroup instance for `Path` is not concatenative, but creates
-- overlapping paths. It can be deconstructed with:
-- 
-- > fmap subpath . subpaths . fromPath :: Path -> [D]
--
instance Semigroup Path where 
  (<>) l r = Path (fromPath l <> fromPath r)

path :: Path -> View
path (Path p) = SVG.Path <| SVG.D (draw p)

transformD :: Transformation -> D -> D
transformD t 
  | t == mempty = id
  | otherwise   = fmap (Lens.over points (transformPoint t)) . toAbsolute

transformPath :: Transformation -> Path -> Path
transformPath t (Path p) 
  | t == mempty = Path p
  | otherwise   = Path (fmap (Lens.over points (transformPoint t)) p)

