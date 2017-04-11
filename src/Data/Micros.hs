{-# language CPP #-}
module Data.Micros where

import Ef.Base

import GHC.Generics

import Data.Ratio

import Data.Hashable
import Data.Txt
import Data.JSON

#ifndef __GHCJS__
import Data.Time.Clock.POSIX
#endif

import Atomic.ToTxt
import Atomic.Identify

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

micros :: MonadIO c => c Micros
micros = timeInMicros

timeInMicros :: MonadIO c => c Micros
timeInMicros =
#ifdef __GHCJS__
  (Micros . round . (*1000)) <$> liftIO getTime_micros_js
#else
  (Micros . posixToMicros) <$> (liftIO getPOSIXTime)
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
