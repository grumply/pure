{-# language CPP #-}
module Data.Millis where

import Ef.Base

import GHC.Generics

import Data.Ratio

import Data.Txt
import Data.JSON

#ifndef __GHCJS__
import Data.Time.Clock.POSIX
#endif

import Atomic.ToBS
import Atomic.FromBS
import Atomic.ToTxt

#ifdef __GHCJS__
foreign import javascript unsafe
  "$r = new Date().getTime();" getTime_millis_js :: IO Int
#endif

-- milliseconds since beginning of 1970
newtype Millis = Millis { getMillis :: Integer }
  deriving (Show,Eq,Ord,Generic,ToJSON,FromJSON)
instance ToBS Millis
instance FromBS Millis
instance ToTxt Millis where
  toTxt (Millis ms) = toTxt ms
instance Num Millis where
  (*) (Millis t0) (Millis t1) = Millis (t0 * t1)
  (-) (Millis t0) (Millis t1) = Millis (t0 - t1)
  (+) (Millis t0) (Millis t1) = Millis (t0 + t1)
  abs = Millis . abs . getMillis
  signum = Millis . signum . getMillis
  fromInteger = Millis

millis :: MonadIO c => c Millis
millis = timeInMillis

timeInMillis :: MonadIO c => c Millis
timeInMillis =
#ifdef __GHCJS__
  (Millis . fromIntegral) <$> liftIO getTime_millis_js
#else
  (Millis . posixToMillis) <$> (liftIO getPOSIXTime)
#endif

class FromMillis a where
  fromMillis :: Millis -> a

#ifndef __GHCJS__
instance FromMillis POSIXTime where
  fromMillis (Millis jst) = posixFromMillis jst

posixToMillis :: POSIXTime -> Integer
posixToMillis =
  (`div` 1000)
  . numerator
  . toRational
  . (* 1000000)

posixFromMillis :: Integer -> POSIXTime
posixFromMillis =
  fromRational
  . (% 1000000)
  . (* 1000)
#endif
