module Chart.Algorithms where

import Chart.Commands
import Chart.Point
import Chart.Subpath
import Data.List as L
import Data.Point
import Data.Vector as V
import qualified Control.Lens as Lens

-- | Rounds line-line corners.
rounded :: Double -> D -> D
rounded r = L.concatMap rounder . subpaths . toAbsolute
  where
    fromSubpath :: SomeSubpath -> (V.Vector Command,Int,Bool,Point Chart,Point Chart)
    fromSubpath sp = 
      let (s,e) = ends sp 
          c = isClosed sp
          r = rawSubpath sp
          p = V.fromList r
          l = V.length p
      in 
        (p,l,c,s,e)

    rounder :: SomeSubpath -> D
    rounder original@(fromSubpath -> (p,l,closed,start,end))
      | l > 1 = MoveTo Absolute start' : go 0 
      | otherwise = subpath original
      where
        start' 
          | closed
          , LineTo _ next <- V.head p
          , LineTo _ _    <- V.last p 
          = moveTo r start next
          
          | otherwise
          = start
            
        go :: Int -> D
        go i
          | i == l 
          = [ Close | closed ] 

          | i /= l - 1 || closed
          , i /= 0 || closed
          , (Lens.preview Chart.Commands.point -> Just prev) <- p V.! (if i == 0 then l - 1 else i - 1) 
          , LineTo _ curr <- p V.! i
          , LineTo _ next <- p V.! (if i == l - 1 then 0 else i + 1)
          , let cs = moveTo r curr prev 
          , let ce = moveTo r curr next
          , let sc = scaleTo 0.5 cs curr
          , let ec = scaleTo 0.5 curr ce
          = LineTo Absolute cs : Cubic Absolute sc (Just ec) ce : go (i + 1)

          | otherwise 
          = p V.! i : go (i + 1)

-- | Rounds line-line corners.
catmullRom :: Double -> D -> D
catmullRom d = L.concatMap work . subpaths . toAbsolute
  where
    fromSubpath :: SomeSubpath -> (V.Vector Command,Int,Bool,Point Chart,Point Chart)
    fromSubpath sp = 
      let (s,e) = ends sp 
          c = isClosed sp
          r = rawSubpath sp
          p = V.fromList r
          l = V.length p
      in 
        (p,l,c,s,e)

    work :: SomeSubpath -> D
    work original@(fromSubpath -> (p,l,closed,start,end))
      | l > 2 = MoveTo Absolute start : go 0 
      | otherwise = subpath original
      where
            
        go :: Int -> D
        go i
          | i == l - 1
          = [ Close | closed ] 

          | i /= l - 2 || closed 
          , i /= 0 || closed
          , (Lens.preview Chart.Commands.point -> Just (Point ax ay)) <- p V.! (if i == 0 then l - 1 else i - 1) 
          , (Lens.preview Chart.Commands.point -> Just (Point bx by)) <- p V.! i
          , (Lens.preview Chart.Commands.point -> Just (Point cx cy)) <- p V.! (if i == l - 2 then 0 else i + 1) 
          , (Lens.preview Chart.Commands.point -> Just (Point dx dy)) <- p V.! (if i == l - 2 then 1 else i + 2) 
          , let x1 = (negate ax + d * bx + cx) / d
          , let y1 = (negate ay + d * by + cy) / d
          , let x2 = (bx + d * cx - dx) / d
          , let y2 = (by + d * cy - dy) / d
          = Cubic Absolute (Point x1 y1) (Just (Point x2 y2)) (Point cx cy)
          : go (i + 1)

          | otherwise 
          = p V.! i : go (i + 1)

