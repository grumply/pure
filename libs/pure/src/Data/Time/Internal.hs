{-# language CPP, ViewPatterns #-}
module Data.Time.Internal where

import Data.Time.Clock 
import Data.Time.Clock.POSIX
import qualified Data.Time.Format as Format
import Data.Time.LocalTime

#ifdef __GHCJS__
import Data.Time.GHCJS
#else
import Data.Time.GHC
#endif
import Data.Txt

import Data.Char
import Data.Fixed
import Data.Maybe
import Data.Ratio

formatTime :: (FromTxt txt, Format.FormatTime t) => String -> t -> txt
formatTime s t = fromTxt $ toTxt (Format.formatTime Format.defaultTimeLocale s t)

posixToMillis :: POSIXTime -> Millis
posixToMillis =
    Millis
  . fromIntegral
  . (`div` 1000)
  . numerator
  . toRational
  . (* 1000000)

posixFromMillis :: Millis -> POSIXTime
posixFromMillis =
  fromRational
  . (% 1000000)
  . (* 1000)
  . round
  . getMillis

utcTimeToMillis :: UTCTime -> Millis
utcTimeToMillis =
    posixToMillis
  . utcTimeToPOSIXSeconds

utcTimeFromMillis :: Millis -> UTCTime
utcTimeFromMillis =
    posixSecondsToUTCTime
  . posixFromMillis

posixToMicros :: POSIXTime -> Micros
posixToMicros =
    Micros
  . fromIntegral
  . numerator
  . toRational
  . (* 1000000)

posixFromMicros :: Micros -> POSIXTime
posixFromMicros =
  fromRational
  . (% 1000000)
  . round
  . getMicros

utcTimeToMicros :: UTCTime -> Micros
utcTimeToMicros =
    posixToMicros
  . utcTimeToPOSIXSeconds

utcTimeFromMicros :: Micros -> UTCTime
utcTimeFromMicros =
    posixSecondsToUTCTime
  . posixFromMicros

millisToDiffTime :: Millis -> DiffTime
millisToDiffTime = picosecondsToDiffTime . round . (* 1e9) . getMillis

diffTimeToMillis :: DiffTime -> Millis
diffTimeToMillis = Millis . fromInteger . (`div` 1000000000) . diffTimeToPicoseconds

microsToDiffTime :: Micros -> DiffTime
microsToDiffTime = picosecondsToDiffTime . round . (* 1e6) . getMicros

diffTimeToMicros :: DiffTime -> Micros
diffTimeToMicros = Micros . fromInteger . (`div` 1000000) . diffTimeToPicoseconds

diffMillis :: Millis -> Millis -> NominalDiffTime
diffMillis (utcTimeFromMillis -> a) (utcTimeFromMillis -> b) = diffUTCTime a b

diffMicros :: Micros -> Micros -> NominalDiffTime
diffMicros (utcTimeFromMicros -> a) (utcTimeFromMicros -> b) = diffUTCTime a b

parseTime :: (ToTxt txt, Format.ParseTime t) => String -> txt -> Maybe t
parseTime f i = Format.parseTimeM True Format.defaultTimeLocale f (fromTxt (toTxt i))