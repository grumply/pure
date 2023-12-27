module Chart.Commands where

import Chart.Transformation
import Chart.Point
import Data.List
import Data.Point as Point
import Data.Variance (mean,varies)
import qualified Data.Txt as Txt
import qualified Control.Lens as Lens

--------------------------------------------------------------------------------
-- Command/D
-- 
-- This core `Command` and `D` gives us a structure to represent raw path
-- data attributes. This is the compilation target for all other shape 
-- abstractions in this library.
--

data Basis = Relative | Absolute
  deriving stock (Eq,Ord,Generic)
  deriving anyclass (ToJSON,FromJSON)

-- In theory, we don't need `LineTo` or `Quadratic`, but the added traversal and
-- matching complexity is negligible. We don't have Vertical or Horizontal -
-- instead, we use `LineTo`. I guess we don't even really need `Arc`, we could
-- just implement it as multiple `Cubic`s. I would like that deconstruction, but
-- I think the added complexity wouldn't be worth it, and deciding when to use
-- `A` in a `d` attribute would be annoying, and we would likely just avoid it.
-- I guess the convenience of `Arc` for pie charts and others is a reasonable
-- trade-off. We'll see, in time.
data Command
  = MoveTo Basis (Point Chart)
  | LineTo Basis (Point Chart)
  | Cubic Basis (Point Chart) (Maybe (Point Chart)) (Point Chart) 
  | Quadratic Basis (Maybe (Point Chart)) (Point Chart) 
  | Arc Basis (Point Chart) Double Bool Bool (Point Chart)
  | Close
  deriving stock (Eq,Ord,Generic)
  deriving anyclass (ToJSON,FromJSON)

type D = [Command]

point :: Lens.Traversal' Command (Point Chart)
point f = \case
  MoveTo b p           -> MoveTo b <$> f p
  LineTo b p           -> LineTo b <$> f p
  Cubic b p1 p2 p      -> Cubic b p1 p2 <$> f p
  Quadratic b p1 p     -> Quadratic b p1 <$> f p
  Arc b r rot sf las p -> Arc b r rot sf las <$> f p
  Close                -> pure Close

allPoints :: D -> [Point Chart]
allPoints = concatMap (Lens.^.. Chart.Commands.point) . toAbsolute

points :: Lens.Traversal' Command (Point Chart)
points f = \case
  MoveTo b p               -> MoveTo b <$> f p
  LineTo b p               -> LineTo b <$> f p
  Cubic b p1 (Just p2) p   -> Cubic b <$> f p1 <*> (Just <$> f p2) <*> f p
  Cubic b p1 _ p           -> Cubic b <$> f p1 <*> pure Nothing <*> f p
  Quadratic b (Just p1) p  -> Quadratic b <$> (Just <$> f p1) <*> f p
  Quadratic b _ p          -> Quadratic b Nothing <$> f p
  Arc b r rot sf las p     -> Arc b <$> f r <*> pure rot <*> pure sf <*> pure las <*> f p
  Close                    -> pure Close

positions :: Lens.Traversal' Command (Point Chart)
positions f = \case
  MoveTo b p               -> MoveTo b <$> f p
  LineTo b p               -> LineTo b <$> f p
  Cubic b p1 (Just p2) p   -> Cubic b <$> f p1 <*> (Just <$> f p2) <*> f p
  Cubic b p1 _ p           -> Cubic b <$> f p1 <*> pure Nothing <*> f p
  Quadratic b (Just p1) p  -> Quadratic b <$> (Just <$> f p1) <*> f p
  Quadratic b _ p          -> Quadratic b Nothing <$> f p
  Arc b r rot sf las p     -> Arc b r rot sf las <$> f p
  Close                    -> pure Close

bounds :: D -> (Point Chart,Point Chart)
bounds path = (Point (minimum xs) (minimum ys), Point (maximum xs) (maximum ys))
  where (xs,ys) = unzip (fmap (\(Point x y) -> (x,y)) (pathPoints path))

center :: D -> Point Chart
center = Point.average . (\(min,max) -> [min,max]) . bounds

scaleD :: Double -> Double -> D -> D 
scaleD sx sy = fmap (Lens.over points (* Point sx sy)) . toAbsolute

translateD :: Double -> Double -> D -> D
translateD dx dy = fmap (Lens.over positions (+ Point dx dy)) . toAbsolute

scaleDRelative :: Double -> Double -> Point Chart -> D -> D
scaleDRelative sx sy origin = fmap (Lens.over points (scalePointRelative sx sy origin)) . toAbsolute

pathPoints :: D -> [Point Chart]
pathPoints = mapMaybe (Lens.^? Chart.Commands.point) . toAbsolute

-- | Calculate the centroid of a path.
-- 
-- ## Accuracy
-- Be aware that this is a path-relative centroid; if your path starts and ends
-- on the same point or if your path overlaps points, your centroid will be
-- biased to that point.
--
-- ## Alternative
-- Use `center` for a centroid based on the bounding box.
centroid :: D -> Point Chart
centroid = Point.average . pathPoints

-- | Scale a `D` relative to its bounding box.
scaleCenter :: Double -> D -> D
scaleCenter s (toAbsolute -> path) = 
  scaleDRelative s s (Chart.Commands.center path) path

-- | Scale a `D` relative to the its centroid.
scaleCentroid :: Double -> D -> D
scaleCentroid s (toAbsolute -> path) = 
  scaleDRelative s s (centroid path) path

simplify :: D -> D
simplify = go . toAbsolute
  where
    go (MoveTo {} : rest@(MoveTo {} : _)) = go rest
    go (x : rest) = x : go rest
    go [] = []


draw :: D -> Txt
draw = Txt.intercalate " " . fmap fromCommand
  where

    basis Relative = Txt.toLower
    basis _ = id

    x <&>> y = x <> ", " <> y

    co (Point x y) = (toTxt x,toTxt y)

    fromCommand = \case
      Close -> "Z"

      MoveTo b (co -> (x,y)) -> basis b "M" <<>> x <<>> y

      LineTo b (Point x y) -> basis b "L" <<>> toTxt x <<>> toTxt y

      Cubic b p1 Nothing p2 ->
        let
          (x1,y1) = co p1
          (x2,y2) = co p2
        in
          basis b "S" 
            <<>> x1 
            <<>> y1 
            <&>> x2 
            <<>> y2

      Cubic b p1 (Just p2) p3 ->
        let
          (x1,y1) = co p1
          (x2,y2) = co p2
          (x3,y3) = co p3
        in
          basis b "C" 
            <<>> x1 
            <<>> y1 
            <&>> x2 
            <<>> y2 
            <&>> x3 
            <<>> y3

      Quadratic b Nothing p1 ->
        let
          (x1,y1) = co p1
        in
          basis b "T" 
            <<>> x1 
            <<>> y1 

      Quadratic b (Just p1) p2 ->
        let
          (x1,y1) = co p1
          (x2,y2) = co p2
        in
          basis b "Q" 
            <<>> x1 
            <<>> y1 
            <&>> x2 
            <<>> y2

      Arc b r xrot largeArcFlag sweepFlag p ->
        let
          flag    = bool 0 1
          (rx,ry) = co r
          (x,y)   = co p
        in
          basis b "A" 
            <<>> rx 
            <<>> ry 
            <<>> toTxt xrot 
            <<>> flag largeArcFlag 
            <<>> flag sweepFlag 
            <<>> x 
            <<>> y

undraw :: Txt -> D
undraw = catMaybes . unfoldr toCommand
  where
    toBasis = bool Relative Absolute . isUpper

    num (Txt.dropWhile isSpace -> d) = 
      let (n,rest) = Txt.span (\x -> isDigit x || x `elem` ("Ee.-" :: String)) d
      in (fromMaybe 0 (readMaybe (fromTxt n)),rest)
    
    comma = Txt.dropWhile (\x -> isSpace x || x == ',')

    toCommand :: Txt -> Maybe (Maybe Command,Txt)
    toCommand d0 =
      case Txt.uncons (Txt.dropWhile isSpace d0) of
        Nothing -> Nothing
        Just (c,d1)
          | isAlpha c -> let b = toBasis c in
            case toUpper c of
              'Z' -> 
                Just (Just Close,d1)

              'M' -> 
                let 
                  (x,d2) = num d1 
                  (y,d3) = num d2
                in 
                  Just (Just (MoveTo b (Point x y)),d3)

              'L' -> 
                let 
                  (x,d2) = num d1
                  (y,d3) = num d2
                in 
                  Just (Just (LineTo b (Point x y)),d3)

              'H' ->
                let 
                  (h,d2) = num d1
                in 
                  Just (Just (LineTo b (Point h 0)),d2)

              'V' -> 
                let 
                  (v,d2) = num d1
                in 
                  Just (Just (LineTo b (Point 0 v)),d2)

              'C' -> 
                let 
                  (x1,d2) = num d1
                  (y1,d3) = num d2
                  (x2,d4) = num (comma d3)
                  (y2,d5) = num d4
                  (x3,d6) = num (comma d5)
                  (y3,d7) = num d6
                in 
                  Just (Just (Cubic b (Point x1 y1) (Just (Point x2 y2)) (Point x3 y3)),d7)

              'S' -> 
                let 
                  (x1,d2) = num d1
                  (y1,d3) = num d2
                  (x2,d4) = num (comma d3)
                  (y2,d5) = num d4
                in 
                  Just (Just (Cubic b (Point x1 y1) Nothing (Point x2 y2)),d5)

              'Q' -> 
                let 
                  (x1,d2) = num d1
                  (y1,d3) = num d2
                  (x2,d4) = num (comma d3)
                  (y2,d5) = num d4
                in 
                  Just (Just (Quadratic b (Just (Point x1 y1)) (Point x2 y2)),d5)

              'T' -> 
                let 
                  (x1,d2) = num d1
                  (y1,d3) = num d2
                in 
                  Just (Just (Quadratic b Nothing (Point x1 y1)),d3)

              'A' -> 
                let
                  (rx,d2)   = num d1
                  (ry,d3)   = num d2
                  (xrot,d4) = num d3
                  (laf,d5)  = num d4
                  (sf,d6)   = num d5
                  (x,d7)    = num d6
                  (y,d8)    = num d7
                in
                  Just (Just (Arc b (Point rx ry) xrot (laf == 1) (sf == 1) (Point x y)),d8)

              _   -> Nothing

          | otherwise -> Nothing

toAbsolute :: D -> D
toAbsolute = go (Point 0 0)
  where

    go :: Point Chart -> [Command] -> [Command]
    go _ [] = []
    go current (cmd : rest) = do
      case cmd of
        MoveTo Absolute p -> cmd : go p rest
        MoveTo Relative p -> 
          let new = current + p 
          in MoveTo Absolute new : go new rest

        LineTo Absolute p -> cmd : go p rest
        LineTo Relative p -> 
          let new = current + p 
          in LineTo Absolute new : go new rest

        Cubic Absolute p1 p2 p3 -> cmd : go p3 rest
        Cubic Relative p1 p2 p3 -> 
          let new = current + p3 
          in Cubic Absolute p1 p2 new : go new rest

        Quadratic Absolute p1 p2 -> cmd : go p2 rest
        Quadratic Relative p1 p2 -> 
          let new = current + p2 
          in Quadratic Absolute p1 new : go new rest

        Arc Absolute r xrot laf sf p -> cmd : go p rest
        Arc Relative r xrot laf sf p -> 
          let new = current + p 
          in Arc Absolute r xrot laf sf new : go new rest

        Close -> Close : go current rest

-- | Convert a `D` to an origin-relative `D`. 
toRelative :: D -> D
toRelative = toRelativeWith (Point 0 0)

-- | Convert a `D` to a point-relative `D`.
toRelativeWith :: Point Chart -> D -> D
toRelativeWith = go 
  where
    go :: Point Chart -> [Command] -> [Command]
    go _ [] = []
    go current (cmd : rest) =
      case cmd of
        MoveTo Relative p -> cmd : go (current + p) rest
        MoveTo Absolute p -> MoveTo Relative (p - current) : go p rest

        LineTo Relative p -> cmd : go (current + p) rest
        LineTo Absolute p -> LineTo Relative (p - current) : go p rest

        Cubic Relative p1 p2 p3 -> cmd : go (current + p3) rest
        Cubic Absolute p1 p2 p3 -> Cubic Relative p1 p2 (p3 - current) : go p3 rest

        Quadratic Relative p1 p2 -> cmd : go (current + p2) rest
        Quadratic Absolute p1 p2 -> Quadratic Relative p1 (p2 - current ) : go p2 rest

        Arc Relative r xrot laf sf p -> cmd : go (current - p) rest
        Arc Absolute r xrot laf sf p -> Arc Relative r xrot laf sf (p - current) : go p rest

        Close -> Close : go current rest

