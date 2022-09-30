{-# LANGUAGE DeriveGeneric, GeneralizedNewtypeDeriving #-}
module Data.Time.GHCJS where

import Data.Txt
import Data.JSON

import GHC.Generics

import Data.Ratio
import Data.Hashable

newtype Micros = Micros { getMicros :: Double }
  deriving (Show,Eq,Ord,Num,Real,Fractional,Floating,RealFrac,Generic,ToJSON,FromJSON)

instance ToTxt Micros where
  toTxt (Micros us) = toTxt us

instance Hashable Micros where
  hashWithSalt salt (Micros us) = hashWithSalt salt us

micros :: IO Micros
micros = Micros <$> getTime_micros_js

newtype Millis = Millis { getMillis :: Double }
  deriving (Show,Eq,Ord,Num,Real,Fractional,Floating,RealFrac,Generic,ToJSON,FromJSON)

instance ToTxt Millis where
  toTxt (Millis ms) = toTxt ms

instance Hashable Millis where
  hashWithSalt salt (Millis ms) = hashWithSalt salt ms

millis :: IO Millis
millis = Millis <$> getTime_millis_js

foreign import javascript unsafe
  "$r = new Date().getTime();" getTime_millis_js :: IO Double

foreign import javascript unsafe
  "$r = 1000 * new Date().getTime();" getTime_micros_js :: IO Double
