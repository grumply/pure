module Chart.Config where

import qualified Base
import Chart.Point
import Chart.Transformation
import Data.Point

import Unsafe.Coerce

data Box = Box
  { _viewbox    :: (Double,Double,Double,Double)
  , _foci       :: [Point SVG]
  , _zoom       :: Double
  , _checkpoint :: IO Box -- cool trick
  }

instance Show Box where
  show Box {..} = show (_viewbox,_foci,_zoom)

newtype Origin = O (Point Chart)
  deriving (Num,Fractional,Eq,Ord) via Point Chart

newtype Width  = W Double
  deriving (Num,Fractional,Real,RealFrac,Floating,Eq,Ord) via Double

newtype Height = H Double
  deriving (Num,Fractional,Real,RealFrac,Floating,Eq,Ord) via Double

instance Default Origin where
  def = 0

instance Exists Options => Enum (Point Chart) where
  toEnum n 
    | let p = fromIntegral n
    , p <= maxBound
    , p >= minBound
    = p

    | otherwise 
    = Base.error "Chart.Config.Enum.Point_SVG.toEnum: bad argument"

  fromEnum p@(Point x y)
    | x == y
    , p <= maxBound
    , p >= minBound
    = floor x

    | otherwise 
    = Base.error "Chart.Config.Enum.Point_SVG.fromEnum: bad argument"

instance Exists Options => Bounded (Point Chart) where
  minBound = Chart.Config.origin

  maxBound = 
    let Point x y = Chart.Config.origin
    in Point (x + Chart.Config.width) (y + Chart.Config.height)

data Margin n = Margin
  { top :: n
  , left :: n
  , bottom :: n
  , right :: n
  }

instance (Num n, Ord n) => Num (Margin n) where
  (+) = combineMarginWith (+)
  (-) = combineMarginWith (\x y -> Base.max 0 (x - y))
  (*) = combineMarginWith (*)
  abs = overMargin abs
  signum = overMargin signum
  fromInteger (fromInteger -> i) = Chart.Config.Margin i i i i

instance (Fractional n, Ord n) => Fractional (Margin n) where
  (/) (Chart.Config.Margin a b c d) (Chart.Config.Margin a' b' c' d') = 
    Chart.Config.Margin (a / a') (b / b') (c / c') (d / d')
  fromRational (fromRational -> i) = Chart.Config.Margin i i i i

instance (Num n, Ord n) => Default (Margin n) where
  def = 0

margin2 :: n -> n -> Margin n
margin2 blk inl = Chart.Config.Margin {..}
  where
    top    = blk
    left   = inl
    bottom = blk
    right  = inl

margin3 :: n -> n -> n -> Margin n
margin3 top lr bottom = Chart.Config.Margin {..}
  where 
    left  = lr
    right = lr

margin4 :: n -> n -> n -> n -> Margin n 
margin4 = Chart.Config.Margin

combineMarginWith :: (n -> n -> n) -> Margin n -> Margin n -> Margin n
combineMarginWith combine pl pr = 
  Chart.Config.Margin 
    (combine (Chart.Config.top pl) (Chart.Config.top pr))
    (combine (Chart.Config.left pl) (Chart.Config.left pr))
    (combine (Chart.Config.bottom pl) (Chart.Config.bottom pr))
    (combine (Chart.Config.right pl) (Chart.Config.right pr))

overMargin :: (n -> n) -> Margin n -> Margin n
overMargin f p = 
  Chart.Config.Margin 
    (f (Chart.Config.top p)) 
    (f (Chart.Config.left p)) 
    (f (Chart.Config.bottom p)) 
    (f (Chart.Config.right p))

data Options = Options
  { _margin  :: Margin Double
  , _origin  :: Origin
  , _width   :: Width
  , _height  :: Height
  }

margin :: Exists Options => Margin Double
margin = _margin it

origin :: Exists Options => Point Chart
origin = let O o = _origin it in o

width :: Exists Options => Double
width = let W w = _width it in w

height :: Exists Options => Double
height = let H h = _height it in h

options :: Origin -> Width -> Height -> Margin Double -> Options
options _origin _width _height _margin = Options {..}

type Config = (Exists Options, Modify Box) |- (View -> View)

instance Default Options where
  def = Options 10 0 100 100 
          
toChart :: Exists Options => Point SVG -> Point Chart
toChart = unsafeCoerce . transformPoint (translationY Chart.Config.height <> flipV)

fromChart :: Exists Options => Point Chart -> Point SVG
fromChart = unsafeCoerce . transformPoint (flipV <> translationY (-Chart.Config.height))