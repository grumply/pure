{-# language CPP #-}
module Chart.Animation where

import Chart.Config
import Chart.Commands
import Chart.Path
import Chart.Point
import Chart.Subpath
import Chart.Transformation
import Chart.Viewport
import Data.Animation
import Data.Coerce
import Data.DOM hiding (head)
import Data.Ease
import Data.List as L
import Data.Point
import Data.Time
import System.IO.Unsafe

travelWith :: State Box => Ease -> Time -> Box -> IO () -> IO ()
travelWith ease dur destination after = do
  start <- time
  let 
    (sx,sy,sw,sh) = viewbox
    (ex,ey,ew,eh) = _viewbox destination
    (mx,my,mw,mh) = (ex-sx,ey-sy,ew-sw,eh-sh)
    end = start + dur
    tick t = coerce ease dur (t - start)
    loop = 
      void do
        addAnimation do
          modifyIO \b -> do
            now <- time
            let
              t  = tick now
              nx = sx + t * mx
              ny = sy + t * my
              nw = sw + t * mw
              nh = sh + t * mh
            if now >= end then do
              addAnimation after
              pure b { _viewbox = (ex,ey,ew,eh) }
            else do
              loop
              pure b { _viewbox = (nx,ny,nw,nh) }
  loop

travel :: State Box => Ease -> Time -> Box -> IO ()
travel ease dur dest = travelWith ease dur dest def

travels :: State Box => [(Ease,Time,Box)] -> IO ()
travels = go 
  where
    go [] = def
    go ((ease,dur,dest):ds) = travelWith ease dur dest (go ds)

to :: Exists Box => Point Chart -> Box
to p = 
  let (x,y,w,h) = _viewbox it
  in it { _viewbox = (x + _x p,y + _y p,w,h) }

routeWith :: State Box => Ease -> Time -> Path -> IO () -> IO ()
routeWith ease dur (Path d) after = do
  start <- time
  p <- newPath (draw d)
  let 
    (x,y,w,h) = _viewbox it
    end = start + dur
    tick t = coerce ease dur (t - start)
    l = pathLength p
    loop = 
      void do
        addAnimation do
          modifyIO \b -> do
            now <- time
            let
              t  = tick now
              l' = t * l
              Point x y = pointAtLength p l'
            unless (now >= end) loop
            pure b { _viewbox = (x - (w / 2),y - (h / 2),w,h) }
  loop

route :: State Box => Ease -> Time -> Path -> IO ()
route ease dur p = routeWith ease dur p def

routeRelative :: Exists Box => [(Ease,Time,Point Chart)] -> [(Ease,Time,Box)]
routeRelative = go (_viewbox it)
  where
    go _ [] = []
    go (x,y,w,h) ((ease,dur,p):ps) =
      let vb = (x + _x p,y + _y p,w,h) 
      in (ease,dur,it { _viewbox = vb }) : go vb ps

routeAbsolute :: Exists Box => [(Ease,Time,Point Chart)] -> [(Ease,Time,Box)]
routeAbsolute = go (_viewbox it)
  where
    go _ [] = []
    go (x,y,w,h) ((ease,dur,p):ps) =
      let vb = (_x p,_y p,w,h) 
      in (ease,dur,it { _viewbox = vb }) : go vb ps

#ifdef __GHCJS__
foreign import javascript unsafe
  "var p = document.createElementNS('http://www.w3.org/2000/svg', 'path'); p.setAttributeNS(null,'d',$1); $r = p"
    new_path_js :: Txt -> IO Node

foreign import javascript unsafe
  "$r = $1.getTotalLength()"
    path_length_js :: Node -> IO Double
  
foreign import javascript unsafe
  "$r = $1.getPointAtLength($2)"
    point_at_length_js :: Node -> Double -> IO JSV
#endif

newPath :: Txt -> IO Node
newPath d = 
#ifdef __GHCJS__ 
  new_path_js d
#else
  pure (coerce nullJSV)
#endif

pathLength :: Node -> Double
pathLength n =
#ifdef __GHCJS__
  unsafePerformIO (path_length_js n)
#else
  0
#endif

pointAtLength :: Node -> Double -> Point Chart
pointAtLength n l = fromMaybe (Point 0 0) do
#ifdef __GHCJS__
  let v = unsafePerformIO (point_at_length_js n l)
  x <- v .# "x"
  y <- v .# "y"
  pure (Point x y)
#else
  Nothing
#endif

