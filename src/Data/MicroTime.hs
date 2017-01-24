{-# language CPP #-}
module Data.MicroTime where

import Ef.Base

import Data.JSText
import Data.JSTime
import GHC.Generics

import Data.Ratio

#ifndef __GHCJS__
import Data.Time.Clock.POSIX
#endif

import Nuclear.ToBS
import Nuclear.FromBS
import Nuclear.ToText

-- microseconds since beginning of 1970 to an accuracy of 1 millisecond in GHCJS and 1 microsecond in GHC
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
#ifdef __GHCJS__
  (MicroTime . (*1000) . fromIntegral) <$> liftIO getTime_millis_js
#else
  (MicroTime . posixToMicros) <$> (liftIO getPOSIXTime)
#endif

class FromMicroTime a where
  fromMicroTime :: MicroTime -> a

#ifndef __GHCJS__
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
#endif
