{-# language CPP #-}
module Data.JSTime where

import Ef.Base

import Data.JSText
import GHC.Generics

import Data.Ratio

#ifndef __GHCJS__
import Data.Time.Clock.POSIX
#endif

import Nuclear.ToBS
import Nuclear.FromBS
import Nuclear.ToText

#ifdef __GHCJS__
foreign import javascript unsafe
  "var d = Date(); d.getTime();" getTime_millis_js :: IO Int
#endif

-- milliseconds since beginning of 1970
newtype JSTime = JSTime { millis :: Integer }
  deriving (Show,Eq,Ord,Generic,ToJSON,FromJSON)
instance ToBS JSTime
instance FromBS JSTime
instance ToText JSTime
instance Num JSTime where
  (*) (JSTime t0) (JSTime t1) = JSTime (t0 * t1)
  (-) (JSTime t0) (JSTime t1) = JSTime (t0 - t1)
  (+) (JSTime t0) (JSTime t1) = JSTime (t0 + t1)
  abs = JSTime . abs . millis
  signum = JSTime . signum . millis
  fromInteger = JSTime

jstime :: (Monad super, MonadIO super) => super JSTime
jstime = timeInMillis

timeInMillis :: (Monad super, MonadIO super) => super JSTime
timeInMillis =
#ifdef __GHCJS__
  (JSTime . fromIntegral) <$> liftIO getTime_millis_js
#else
  (JSTime . posixToMillis) <$> (liftIO getPOSIXTime)
#endif

class FromJSTime a where
  fromJSTime :: JSTime -> a

#ifndef __GHCJS__
instance FromJSTime POSIXTime where
  fromJSTime (JSTime jst) = posixFromMillis jst

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
