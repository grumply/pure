{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Pure.Data.Micros where

import Pure.Data.Txt
import Pure.Data.JSON
import Pure.Data.Identify

import GHC.Generics

import Data.Ratio
import Data.Hashable

#ifndef __GHCJS__
import Data.Time.Clock.POSIX
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "if (performance.now !== 'undefined') { $r = performance.now() + performance.timing.navigationStart } else { $r = new Date().getTime() }" getTime_micros_js :: IO Double
#endif

newtype Micros = Micros { getMicros :: Integer }
  deriving (Show,Eq,Ord,Generic,ToJSON,FromJSON)
instance Identify Micros
instance ToTxt Micros where
  toTxt (Micros us) = toTxt us
instance Num Micros where
  (*) (Micros t0) (Micros t1) = Micros (t0 * t1)
  (-) (Micros t0) (Micros t1) = Micros (t0 - t1)
  (+) (Micros t0) (Micros t1) = Micros (t0 + t1)
  abs = Micros . abs . getMicros
  signum = Micros . signum . getMicros
  fromInteger = Micros
instance Hashable Micros where
  hashWithSalt salt (Micros us) = hashWithSalt salt us

micros :: IO Micros
micros = timeInMicros

timeInMicros :: IO Micros
timeInMicros =
#ifdef __GHCJS__
  (Micros . round . (*1000)) <$> getTime_micros_js
#else
  (Micros . posixToMicros) <$> getPOSIXTime
#endif

class FromMicros a where
  fromMicros :: Micros -> a

instance FromMicros Double where
  fromMicros = fromInteger . getMicros

instance FromMicros Integer where
  fromMicros = getMicros

#ifndef __GHCJS__
instance FromMicros POSIXTime where
  fromMicros (Micros mt) = posixFromMicros mt

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