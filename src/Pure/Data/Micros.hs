{-# LANGUAGE CPP #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DefaultSignatures #-}
module Pure.Data.Micros where

import Ef.Base

import Pure.Data.Txt
import Pure.Data.JSON
import Pure.Data.Identify

import GHC.Generics

import Data.Ratio
import Data.Hashable

import Data.Time.Clock
import Data.Time.Clock.POSIX

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
instance Enum Micros where
  toEnum = Micros . toEnum
  fromEnum (Micros ms) = fromEnum ms
instance Real Micros where
  toRational (Micros ms) = toRational ms
instance Integral Micros where
  quotRem (Micros ms) (Micros ms') =
    let (q,r) = quotRem ms ms'
    in (Micros q,Micros r)
  toInteger = getMicros

micros :: MonadIO c => c Micros
micros = liftIO timeInMicros

timeInMicros :: IO Micros
timeInMicros =
#ifdef __GHCJS__
  (Micros . round . (*1000)) <$> getTime_micros_js
#else
  posixToMicros <$> getPOSIXTime
#endif

class FromMicros a where
  fromMicros :: Micros -> a
  default fromMicros :: Num a => Micros -> a
  fromMicros = fromIntegral . getMicros

instance FromMicros POSIXTime where
  fromMicros = posixFromMicros

instance FromMicros UTCTime where
  fromMicros = utcTimeFromMicros

posixToMicros :: POSIXTime -> Micros
posixToMicros = Micros .
  numerator
  . toRational
  . (* 1000000)

posixFromMicros :: Micros -> POSIXTime
posixFromMicros =
  fromRational
  . (% 1000000)
  . getMicros

utcTimeToMicros :: UTCTime -> Micros
utcTimeToMicros =
    posixToMicros
  . utcTimeToPOSIXSeconds

utcTimeFromMicros :: Micros -> UTCTime
utcTimeFromMicros =
    posixSecondsToUTCTime
  . posixFromMicros

