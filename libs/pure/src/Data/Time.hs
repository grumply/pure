{-# language CPP, PatternSynonyms, ViewPatterns, DeriveGeneric, DerivingVia, TypeApplications #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Data.Time (module Data.Time, module Export) where

import Control.Applicative
import Control.Concurrent (yield,threadDelay)
import Control.Monad (forever)
import Data.Coerce
import Data.Default
import Data.JSON
import Data.Ratio
import Data.Txt
import System.IO.Unsafe ( unsafePerformIO )
import GHC.Generics
import Unsafe.Coerce

import Data.Time.Internal as Export

#ifdef __GHCJS__
import Data.Time.GHCJS as Export
#else
import Data.Time.GHC as Export
#endif

import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Time.Format as Format (FormatTime(..),ParseTime(..),defaultTimeLocale,parseTimeM)
import Data.Time.LocalTime (localTimeToUTC,utcToLocalTime,utc,utcToZonedTime,timeZoneMinutes,zonedTimeZone,getZonedTime,getCurrentTimeZone)

import Data.Time.Clock as Export (UTCTime(),NominalDiffTime(),DiffTime())
import Data.Time.LocalTime as Export (LocalTime(),TimeZone())

import Prelude
import System.Timeout

{-
A simple time type; not meant as a replacement for Data.Time from 'time'.
Roughly corresponds with a colloquial use of time.

The intention is a milliseconds-since-unix-epoch Double-based type with
no regard for leap seconds or days, except in general aggregates. This is
not a systems-oriented time type, but a simple display time type. For a
systems-oriented or real-time time type, use `time`'s `UTCTime` and
`DiffTime`.

-}

delay :: Time -> IO ()
delay t
  | t <= 0 = pure ()
  | t == 1 = yield
  | otherwise =
    let (Microseconds us _) = t
    in threadDelay (round us)

timeout :: Time -> IO a -> IO (Maybe a)
timeout (Microseconds us _) = System.Timeout.timeout (round us)

every :: Time -> IO () -> IO ()
every t io = forever (io >> delay t)

every_ :: Time -> IO () -> IO ()
every_ t io = forever (delay t >> io)

newtype Time = Time_ { getTime :: Millis }
  deriving stock Generic
  deriving (Eq,Ord,Num,Real,Fractional,Floating,RealFrac) via Millis

instance ToTxt Time where
  toTxt t = RFC3339 t

instance FromTxt Time where
  fromTxt (RFC3339 t) = t
  fromTxt _ = 0

instance Show Time where
  show = fromTxt . toTxt

instance ToJSON Time where
  toJSON = toJSON . fromTime @Millis

instance FromJSON Time where
  parseJSON = fmap (toTime @Millis) . parseJSON

class IsTime t where
  toTime :: t -> Time
  fromTime :: Time -> t

instance IsTime Time where
  toTime   = id
  fromTime = id

instance IsTime Millis where
  toTime   = coerce
  fromTime = coerce

instance IsTime Micros where
  toTime   = coerce . (/ 1000)
  fromTime = coerce . (* 1000)

instance IsTime DiffTime where
  toTime   = coerce . diffTimeToMillis
  fromTime = millisToDiffTime . coerce

instance IsTime NominalDiffTime where
  toTime   = toTime . (unsafeCoerce :: NominalDiffTime -> DiffTime)
  fromTime = (unsafeCoerce :: DiffTime -> NominalDiffTime) . fromTime

instance IsTime UTCTime where
  toTime = toTime . utcTimeToMillis
  fromTime = utcTimeFromMillis . fromTime

instance IsTime LocalTime where
  toTime = toTime . localTimeToUTC unsafeTimeZone
  fromTime = utcToLocalTime unsafeTimeZone . utcTimeFromMillis . fromTime

{-# complete Time #-}
pattern Time :: IsTime t => t -> Time
pattern Time t <- (fromTime -> t) where
  Time t = toTime t

pattern Nanosecond :: Time
pattern Nanosecond = 1e-6

pattern Microsecond :: Time
pattern Microsecond = 0.001

pattern Millisecond :: Time
pattern Millisecond = 1

pattern Second :: Time
pattern Second = 1000

pattern Minute :: Time
pattern Minute = 60000

pattern Hour :: Time
pattern Hour = 3600000

pattern Day :: Time
pattern Day = 86400000

pattern Week :: Time
pattern Week = 604800000

-- | 30 days, 10 hours, 29 minutes, 6 seconds; the average month in the 400 year
-- gregorian cycle. If you want a 30 day month, e.g., use `Day * 30`.
pattern Month :: Time
pattern Month = 2629746000

-- | 365.2422 days; the average year in the 400 year Gregorian cycle. If you
-- want a 365-day year, use `Day * 365`.
pattern Year :: Time
pattern Year = 31556926080

time :: IO Time
time = coerce <$> millis

{-# INLINE unsafeTime #-}
unsafeTime :: Time
unsafeTime = unsafeLocalizeTime (unsafePerformIO time)

{-# NOINLINE unsafeTimeZone #-}
unsafeTimeZone :: TimeZone
unsafeTimeZone = unsafePerformIO getCurrentTimeZone

{-# NOINLINE __internal_tzm #-}
__internal_tzm :: Time
__internal_tzm = Minutes (unsafePerformIO (fromIntegral . timeZoneMinutes . zonedTimeZone <$> getZonedTime)) 0

unsafeLocalizeTime :: Time -> Time
unsafeLocalizeTime = (+ __internal_tzm)

localizeTime :: Time -> IO Time
localizeTime t = (+) <$> tzm <*> pure t
  where
    tzm = do
      ms <- timeZoneMinutes . zonedTimeZone <$> getZonedTime
      pure (Minutes (fromIntegral ms) 0)

nominalDiffTime :: Time -> Time -> NominalDiffTime
nominalDiffTime a b = diffMillis (fromTime a) (fromTime b)

diffTime :: Time -> Time -> DiffTime
diffTime a b = unsafeCoerce (nominalDiffTime a b)

{-# complete Nanoseconds #-}
pattern Nanoseconds :: Double -> Time
pattern Nanoseconds ns <- (realToFrac . (/ Nanosecond) -> ns) where
  Nanoseconds ns = Nanosecond * realToFrac ns

{-# complete Microseconds #-}
pattern Microseconds :: Double -> Time -> Time
pattern Microseconds us rest <- (fmap (* Microsecond) . properFraction . (/ Microsecond) -> (fromIntegral -> us,rest)) where
  Microseconds us rest = Microsecond * realToFrac us + rest

{-# complete Milliseconds #-}
pattern Milliseconds :: Double -> Time -> Time
pattern Milliseconds ms rest <- (properFraction -> (fromIntegral -> ms,rest)) where
  Milliseconds ms rest = realToFrac ms + rest

{-# complete Seconds #-}
pattern Seconds :: Double -> Time -> Time
pattern Seconds ss rest <- (fmap (* Second) . properFraction . (/ Second) -> (fromIntegral -> ss,rest)) where
  Seconds ss rest = Second * realToFrac ss + rest

{-# complete Minutes #-}
pattern Minutes :: Double -> Time -> Time
pattern Minutes ms rest <- (fmap (* Minute) . properFraction . (/ Minute) -> (fromIntegral -> ms,rest)) where
  Minutes ms rest = Minute * realToFrac ms + rest

{-# complete Hours #-}
pattern Hours :: Double -> Time -> Time
pattern Hours hs rest <- (fmap (* Hour) . properFraction . (/ Hour) -> (fromIntegral -> hs,rest)) where
  Hours hs rest = Hour * realToFrac hs + rest

{-# complete Days #-}
pattern Days :: Double -> Time -> Time
pattern Days ds rest <- (fmap (* Day) . properFraction . (/ Day) -> (fromIntegral -> ds,rest)) where
  Days ds rest = Day * realToFrac ds + rest

{-# complete Weeks #-}
pattern Weeks :: Double -> Time -> Time
pattern Weeks ws rest <- (fmap (* Week) . properFraction . (/ Week) -> (fromIntegral -> ws,rest)) where
  Weeks ws rest = Week * realToFrac ws + rest

{-# complete Months #-}
pattern Months :: Double -> Time -> Time
pattern Months ms rest <- (fmap (* Month) . properFraction . (/ Month) -> (fromIntegral -> ms,rest)) where
  Months ms rest = Month * realToFrac ms + rest

{-# complete Years #-}
pattern Years :: Double -> Time -> Time
pattern Years ys rest <- (fmap (* Year) . properFraction . (/ Year) -> (fromIntegral -> ys,rest)) where
  Years ys rest = Year * realToFrac ys + rest

pattern RFC3339 :: Time -> Txt
pattern RFC3339 t <- (parseRFC3339 -> Just t) where
  RFC3339 t = formatTime "%Y-%m-%dT%H:%M:%SZ" (utcTimeFromMillis (fromTime t :: Millis))

parseRFC3339 :: ToTxt x => x -> Maybe Time
parseRFC3339 txt = toTime . utcTimeToMillis <$> parse
  where
    parse = parseTime "%Y-%m-%dT%H:%M:%SZ" txt
        <|> parseTime "%Y-%m-%dT%H:%M:%S%z" txt
        <|> parseTime "%Y-%m-%dT%H:%M:%S%QZ" txt
        <|> parseTime "%Y-%m-%dT%H:%M:%S%Q%z" txt

pattern Date :: Time -> Txt
pattern Date t <- (fmap toTime . fromDate @Txt @UTCTime -> Just t) where
  Date t = toDate @Txt @UTCTime (fromTime t)

toDate :: (FromTxt txt, Format.FormatTime t) => t -> txt
toDate = formatTime "%Y-%m-%d"

fromDate :: (ToTxt txt, Format.ParseTime t) => txt -> Maybe t
fromDate = parseTime "%Y-%m-%d"

pattern DateTime :: Time -> Txt
pattern DateTime t <- (fmap toTime . fromDateTime @Txt @UTCTime -> Just t) where
  DateTime t = toDateTime @Txt @UTCTime (fromTime t)

toDateTime :: (FromTxt txt, Format.FormatTime t) => t -> txt
toDateTime = formatTime "%Y-%m-%dT%H:%M:%S%03Q"

fromDateTime :: (ToTxt txt, Format.ParseTime t) => txt -> Maybe t
fromDateTime = parseTime "%Y-%m-%dT%H:%M:%S%03Q"

pattern ZonedDateTime :: Time -> Txt
pattern ZonedDateTime t <- (fmap toTime . fromZonedDateTime @Txt @UTCTime -> Just t) where
  ZonedDateTime t = toZonedDateTime @Txt @UTCTime (fromTime t)

toZonedDateTime :: (FromTxt txt, Format.FormatTime t) => t -> txt
toZonedDateTime = formatTime "%Y-%m-%dT%H:%M:%S%03Q%z"

fromZonedDateTime :: (ToTxt txt, Format.ParseTime t) => txt -> Maybe t
fromZonedDateTime = parseTime "%Y-%m-%dT%H:%M:%S%03Q%z"

pattern PrettyDate :: Time -> Txt
pattern PrettyDate t <- (fmap toTime . fromPrettyDate @Txt @UTCTime -> Just t) where
  PrettyDate t = toPrettyDate @Txt @UTCTime (fromTime t)

toPrettyDate :: (FromTxt txt, Format.FormatTime t) => t -> txt
toPrettyDate = formatTime "%b%e, %Y"

fromPrettyDate :: (ToTxt txt, Format.ParseTime t) => txt -> Maybe t
fromPrettyDate = parseTime "%b%e, %Y"

pattern PrettyTime :: Time -> Txt
pattern PrettyTime t <- (fmap toTime . fromPrettyTime @Txt @UTCTime -> Just t) where
  PrettyTime t = toPrettyTime @Txt @UTCTime (fromTime t)

toPrettyTime :: (FromTxt txt, Format.FormatTime t) => t -> txt
toPrettyTime = formatTime "%l:%M%p"

fromPrettyTime :: (ToTxt txt, Format.ParseTime t) => txt -> Maybe t
fromPrettyTime = parseTime "%l:%M%p"

pattern PrettyDateTime :: Time -> Txt
pattern PrettyDateTime t <- (fmap toTime . fromPrettyDateTime @Txt @UTCTime -> Just t) where
  PrettyDateTime t = toPrettyDateTime @Txt @UTCTime (fromTime t)

toPrettyDateTime :: (FromTxt txt, Format.FormatTime t) => t -> txt
toPrettyDateTime = formatTime "%b %d, %Y %l:%M%p"

fromPrettyDateTime :: (ToTxt txt, Format.ParseTime t) => txt -> Maybe t
fromPrettyDateTime = parseTime "%b %d, %Y %l:%M%p"