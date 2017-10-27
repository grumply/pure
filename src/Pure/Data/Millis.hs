{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DefaultSignatures #-}
module Pure.Data.Millis where

import Pure.Data.Txt
import Pure.Data.JSON
import Pure.Data.Identify

import GHC.Generics

import Data.Hashable
import Data.Ratio

#ifndef __GHCJS__
import Data.Time.Clock.POSIX
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "$r = new Date().getTime();" getTime_millis_js :: IO Int
#endif

-- milliseconds since beginning of 1970
newtype Millis = Millis { getMillis :: Integer }
  deriving (Show,Eq,Ord,Generic,ToJSON,FromJSON)
instance Identify Millis
instance ToTxt Millis where
  toTxt (Millis ms) = toTxt ms
instance Num Millis where
  (*) (Millis t0) (Millis t1) = Millis (t0 * t1)
  (-) (Millis t0) (Millis t1) = Millis (t0 - t1)
  (+) (Millis t0) (Millis t1) = Millis (t0 + t1)
  abs = Millis . abs . getMillis
  signum = Millis . signum . getMillis
  fromInteger = Millis
instance Hashable Millis where
  hashWithSalt salt (Millis ms) = hashWithSalt salt ms
instance Enum Millis where
  toEnum = Millis . toEnum
  fromEnum (Millis ms) = fromEnum ms
instance Real Millis where
  toRational (Millis ms) = toRational ms
instance Integral Millis where
  quotRem (Millis ms) (Millis ms') =
    let (q,r) = quotRem ms ms'
    in (Millis q,Millis r)
  toInteger = getMillis

millis :: IO Millis
millis = timeInMillis

timeInMillis :: IO Millis
timeInMillis =
#ifdef __GHCJS__
  (Millis . fromIntegral) <$> getTime_millis_js
#else
  (Millis . posixToMillis) <$> getPOSIXTime
#endif

class FromMillis a where
  fromMillis :: Millis -> a
  default fromMillis :: Num a => Millis -> a
  fromMillis = fromIntegral . getMillis

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
