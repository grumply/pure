{-# language GADTs #-}
module Chart.Subpath where

import qualified Base
import Chart.Commands
import Chart.Point
import qualified Control.Lens as Lens
import Data.Point
import Prelude hiding (start,Open)

data OC = Open | Closed
data Subpath (x :: OC) where
  OpenSubpath :: Point Chart -> Point Chart -> D -> Subpath Open
  ClosedSubpath :: Point Chart -> D -> Subpath Closed

data SomeSubpath = forall (x :: OC). SomeSubpath (Subpath x) 
instance Show SomeSubpath where
  show (SomeSubpath p) =
    case p of
      OpenSubpath s e path -> "Open path " <> show s <> " -> " <> show e <> ": " <> fromTxt (draw path)
      ClosedSubpath s path -> "Closed path " <> show s <> ": " <> fromTxt (draw path)

rawSubpath :: SomeSubpath -> D
rawSubpath (SomeSubpath sp) =
  case sp of
    OpenSubpath _ _ path -> path
    ClosedSubpath _ path -> path

subpath :: SomeSubpath -> D
subpath (SomeSubpath sp) =
  case sp of
    OpenSubpath s _ path -> MoveTo Absolute s : path
    ClosedSubpath s path -> MoveTo Absolute s : path ++ [Close]

toD :: Subpaths -> D 
toD = concatMap subpath

start :: SomeSubpath -> Point Chart
start (SomeSubpath sp) = 
  case sp of
    OpenSubpath s _ _ -> s
    ClosedSubpath s _ -> s

end :: SomeSubpath -> Point Chart
end (SomeSubpath sp) =
  case sp of
    OpenSubpath _ e _ -> e
    ClosedSubpath e _ -> e

ends :: SomeSubpath -> (Point Chart,Point Chart)
ends ssp = (start ssp,Chart.Subpath.end ssp)

isOpen :: SomeSubpath -> Bool
isOpen (SomeSubpath OpenSubpath {}) = True
isOpen _ = False

isClosed :: SomeSubpath -> Bool
isClosed (SomeSubpath ClosedSubpath {}) = True
isClosed _ = False

type Subpaths = [SomeSubpath]

-- >>> subpaths [MoveTo Absolute (Point 10 10),LineTo Absolute (Point 20 20),Close,LineTo Absolute (Point 30 30)]
-- [Closed path (10.0,10.0): M 10.0 10.0 L 20.0 20.0 Z,Open path (10.0,10.0) -> (30.0,30.0): L 30.0 30.0]
subpaths :: D -> Subpaths
subpaths = go (Point 0 0) (Point 0 0) []
  where
    go :: Point Chart -> Point Chart -> D -> D -> Subpaths
    go _ _ [] [] = []
    go start current xs [] 
      -- convert an implicit close to an explicit close
      | start == current = [ SomeSubpath (ClosedSubpath start (Base.reverse xs)) ]
      | otherwise = [ SomeSubpath (OpenSubpath start current (Base.reverse xs)) ]
    go start current xs (MoveTo b p : rest) = 
      let k = go p p [] rest in
      case xs of
        [] -> k
        -- convert an implicit close to an explicit close
        _ | start == current -> SomeSubpath (ClosedSubpath start (Base.reverse xs)) : k
          | otherwise -> SomeSubpath (OpenSubpath start current (Base.reverse xs)) : k
    go start current xs (Close : rest) = 
      let k = go start start [] rest in
      case xs of
        [] -> k
        _  -> SomeSubpath (ClosedSubpath start (Base.reverse xs)) : k 
    go start current xs (c : rest) = 
      go start (c Lens.^?! Chart.Commands.point) (c : xs) rest

