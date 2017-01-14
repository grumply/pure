module Data.MicroTime where

import Ef.Base

import Data.JSText
import GHC.Generics

import Data.Ratio

import Data.Time.Clock.POSIX

import Nuclear.ToBS
import Nuclear.FromBS
import Nuclear.ToText

-- microseconds since beginning of 1970
newtype MicroTime = MicroTime { micros :: Integer }
  deriving (Show,Eq,Ord,Generic,ToJSON,FromJSON)
instance ToBS MicroTime
instance FromBS MicroTime
instance ToText MicroTime
instance Num MicroTime where
  (*) (MicroTime t0) (MicroTime t1) = MicroTime (t0 * t1)
  (-) (MicroTime t0) (MicroTime t1) = MicroTime (t0 - t1)
  (+) (MicroTime t0) (MicroTime t1) = MicroTime (t0 + t1)
  abs = MicroTime . abs . micros
  signum = MicroTime . signum . micros
  fromInteger = MicroTime

microtime :: (Monad super, MonadIO super) => super MicroTime
microtime = timeInMicros

timeInMicros :: (Monad super, MonadIO super) => super MicroTime
timeInMicros =
  (MicroTime . posixToMicros) <$> (liftIO getPOSIXTime)

class FromMicroTime a where
  fromMicroTime :: MicroTime -> a

instance FromMicroTime POSIXTime where
  fromMicroTime (MicroTime mt) = posixFromMicros mt

posixToMicros :: POSIXTime -> Integer
posixToMicros =
  numerator
  . toRational
  . (* 1000000)

posixFromMicros :: Integer -> POSIXTime
posixFromMicros =
  fromRational
  . (% 1000000)
