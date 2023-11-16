{-# language CPP, PatternSynonyms, ViewPatterns, DeriveGeneric, DerivingVia, TypeApplications, OverloadedStrings, CPP #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Data.Time (module Data.Time, module Export) where

import Control.Applicative
import Control.Concurrent (yield,threadDelay)
import Control.Monad (forever,when)
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
import qualified Control.Concurrent.Timeout as Unbounded
import qualified Control.Concurrent.Thread.Delay as Unbounded

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
    let Milliseconds ms _ = t
    in Unbounded.delay (round ms * 1000)

timeout :: Time -> IO a -> IO (Maybe a)
timeout (Microseconds us _) = Unbounded.timeout (round us)

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

{-# INLINE nanoseconds #-}
nanoseconds :: Time -> Double
nanoseconds = realToFrac . (/Nanosecond)

{-# complete Nanoseconds #-}
pattern Nanoseconds :: Double -> Time
pattern Nanoseconds ns <- (nanoseconds -> ns) where
  Nanoseconds ns = Nanosecond * realToFrac ns

{-# INLINE microseconds #-}
microseconds :: Time -> Double
microseconds = realToFrac . (/Microsecond)

{-# complete Microseconds #-}
pattern Microseconds :: Double -> Time -> Time
pattern Microseconds us rest <- (fmap (* Microsecond) . properFraction . (/ Microsecond) -> (fromIntegral -> us,rest)) where
  Microseconds us rest = Microsecond * realToFrac us + rest

{-# INLINE milliseconds #-}
milliseconds :: Time -> Double
milliseconds = realToFrac . (/Millisecond)

to_millis_ :: Time -> (Double,Time)
to_millis_ (Time_ (Millis ms)) = (fromIntegral (floor ms :: Int),Time_ (Millis (ms - fromIntegral (floor ms :: Int))))

{-# complete Milliseconds #-}
pattern Milliseconds :: Double -> Time -> Time
pattern Milliseconds ms rest <- (to_millis_ -> (ms,rest)) where
  Milliseconds ms rest = realToFrac ms + rest

{-# INLINE seconds #-}
seconds :: Time -> Double
seconds = realToFrac . (/Second)

{-# complete Seconds #-}
pattern Seconds :: Double -> Time -> Time
pattern Seconds ss rest <- (fmap (* Second) . properFraction . (/ Second) -> (fromIntegral -> ss,rest)) where
  Seconds ss rest = Second * realToFrac ss + rest

{-# INLINE minutes #-}
minutes :: Time -> Double
minutes = realToFrac . (/Minute)

{-# complete Minutes #-}
pattern Minutes :: Double -> Time -> Time
pattern Minutes ms rest <- (fmap (* Minute) . properFraction . (/ Minute) -> (fromIntegral -> ms,rest)) where
  Minutes ms rest = Minute * realToFrac ms + rest

{-# INLINE hours #-}
hours :: Time -> Double
hours = realToFrac . (/Hour)

{-# complete Hours #-}
pattern Hours :: Double -> Time -> Time
pattern Hours hs rest <- (fmap (* Hour) . properFraction . (/ Hour) -> (fromIntegral -> hs,rest)) where
  Hours hs rest = Hour * realToFrac hs + rest

{-# INLINE days #-}
days :: Time -> Double
days = realToFrac . (/Day)

{-# complete Days #-}
pattern Days :: Double -> Time -> Time
pattern Days ds rest <- (fmap (* Day) . properFraction . (/ Day) -> (fromIntegral -> ds,rest)) where
  Days ds rest = Day * realToFrac ds + rest

{-# INLINE weeks #-}
weeks :: Time -> Double
weeks = realToFrac . (/Week)

{-# complete Weeks #-}
pattern Weeks :: Double -> Time -> Time
pattern Weeks ws rest <- (fmap (* Week) . properFraction . (/ Week) -> (fromIntegral -> ws,rest)) where
  Weeks ws rest = Week * realToFrac ws + rest

{-# INLINE months #-}
months :: Time -> Double
months = realToFrac . (/Month)

{-# complete Months #-}
pattern Months :: Double -> Time -> Time
pattern Months ms rest <- (fmap (* Month) . properFraction . (/ Month) -> (fromIntegral -> ms,rest)) where
  Months ms rest = Month * realToFrac ms + rest

{-# INLINE years #-}
years :: Time -> Double
years = realToFrac . (/Year)

{-# complete Years #-}
pattern Years :: Double -> Time -> Time
pattern Years ys rest <- (fmap (* Year) . properFraction . (/ Year) -> (fromIntegral -> ys,rest)) where
  Years ys rest = Year * realToFrac ys + rest

pattern RFC3339 :: Time -> Txt
pattern RFC3339 t <- (parseRFC3339 -> Just t) where
  RFC3339 t = 
#ifdef __GHCJS__
    to_rfc3339_js t
#else
    formatTime "%Y-%m-%dT%H:%M:%SZ" (utcTimeFromMillis (fromTime t :: Millis))
#endif
    
#ifdef __GHCJS__
foreign import javascript unsafe
  "$r = new Date($1).toISOString()" 
    to_rfc3339_js :: Time -> Txt
#endif

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
  PrettyTime t = toPrettyTime @Txt t

toPrettyTime :: (FromTxt txt, IsTime t) => t -> txt
toPrettyTime = 
#ifdef __GHCJS__
  fromTxt . pretty_time_js . toTime
#else
  formatTime "%l:%M%p" . fromTime @UTCTime . toTime
#endif
  
#ifdef __GHCJS__
foreign import javascript unsafe
  "var now = new Date($1); var hs = now.getHours(); var ms = now.getMinutes(); $r = hs % 12 + ':' + (ms < 10 ? '0' + ms : ms) + (hs > 11 ? 'pm' : 'am');" 
    pretty_time_js :: Time -> Txt
#endif
  

fromPrettyTime :: (ToTxt txt, Format.ParseTime t) => txt -> Maybe t
fromPrettyTime = parseTime "%l:%M%p"

pattern PrettyDateTime :: Time -> Txt
pattern PrettyDateTime t <- (fmap toTime . fromPrettyDateTime @Txt @UTCTime -> Just t) where
  PrettyDateTime t = toPrettyDateTime @Txt @UTCTime (fromTime t)

toPrettyDateTime :: (FromTxt txt, Format.FormatTime t) => t -> txt
toPrettyDateTime = formatTime "%b %d, %Y %l:%M%p"

fromPrettyDateTime :: (ToTxt txt, Format.ParseTime t) => txt -> Maybe t
fromPrettyDateTime = parseTime "%b %d, %Y %l:%M%p"

-- | Given a time, create a pretty `<time> ago` string.
--
-- This method should likely be replaced by something like printf that
-- can take a specification string/DSL and both print and parse durations
-- of various formats. Note that it is also inaccurate w.r.t. leaps, and
-- often fails at boundaries (Weeks 3 (Days 2 0) /= Weeks 3 (Days 2 Second)) 
--
ago :: Time -> Time -> Txt
ago now t
  | Seconds s _ <- now - t, s < 60 = "just now"
  | Minutes m _ <- now - t, m < 5  = "recently"
  | otherwise                      = go
  where
    go
      | ds >= 730  = years 
      | ds >= 60   = months
      | ds >= 14   = weeks
      | hs >= 48   = days  
      | ms >= 120  = hours
      | otherwise  = minutes
      where
        ds = ms / 1440
        hs = ms / 60
        Minutes ms _ = now - t

    ns x nm 
      | x == 0 = ""
      | x == 1 = "1 " <> nm
      | otherwise = toTxt (round x :: Int) <> " " <> nm <> "s"

    years = 
      let Years ys (Months ms _) = now - t
      in if ys < 11 then ns ys "year" <> " " <> ns ms "month" <> " ago"
         else ns ys "year" <> " ago"
    
    months =
      let Months ms (Weeks ws _) = now - t
      in if ms < 13 then ns ms "month" <> " " <> ns ws "week" <> " ago"
         else ns ms "month" <> " ago"

    weeks =
      let Weeks ws (Days ds _) = now - t
      in if ws < 5 then ns ws "week" <> " " <> ns ds "day" <> " ago"
         else ns ws "week" <> " ago"

    days =
      let Days ds (Hours hs _) = now - t
      in if ds < 8 then ns ds "day" <> " " <> ns hs "hour" <> " ago"
         else ns ds "day" <> " ago" 

    hours =
      let Hours hs (Minutes ms _) = now - t
      in if hs < 25 then ns hs "hour" <> " " <> ns ms "minute" <> " ago"
         else ns hs "hour" <> " ago"

    minutes =
      let Minutes ms _ = now - t
      in toTxt (round ms :: Int) <> " minutes ago"

duration :: Time -> Txt
duration t@(abs -> d)
  | Microseconds us _ <- d, us < 1000 = r us "us"
  | Milliseconds ms _ <- d, ms < 1000 = r ms "ms"
  | Seconds ss _      <- d, ss < 60   = r ss "s"
  | Minutes ms _      <- d, ms < 60   = r ms "m"
  | Hours hs _        <- d, hs < 24   = r hs "h"
  | Days ds _         <- d            = r ds "d"
  where
    r :: Double -> Txt -> Txt
    r n t = signed (toTxt @Int (round n) <> t)

    signed :: Txt -> Txt
    signed | signum t < 0 = ("-" <>) | otherwise = id

