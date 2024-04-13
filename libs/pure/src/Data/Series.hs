{-# language MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, BangPatterns, DerivingStrategies, DeriveAnyClass, DeriveGeneric, DerivingVia #-}
module Data.Series 
  ( Series()
  , Policy(..)
  , second
  , Data.Series.seconds
  , secondsFrom
  , minute
  , Data.Series.minutes
  , minutesFrom
  , hour
  , Data.Series.hours
  , hoursFrom
  , Data.Series.day
  , Data.Series.days
  , daysFrom
  , week
  , Data.Series.weeks
  , weeksFrom
  , month
  , Data.Series.months
  , monthsFrom
  , quarter
  , quarters
  , quartersFrom
  , year
  , Data.Series.years
  , yearsFrom
  , new
  , integrate

  , Data.Series.null

  , summarize
  , size
  , variance
  , stdDev
  , Data.Series.mean
  , Data.Series.minimum
  , Data.Series.maximum

  , Summary(..)
  , summarySize
  , summaryVariance
  , summaryStdDev
  , summaryMean
  , summaryMinimum
  , summaryMaximum

  , points
  , summaries

  , findSummary
  , findPoint

  , range
  
  , estimate
  , reconstruct
  ) where

import Control.Applicative ((<|>))
import Data.Coerce (coerce)
import Data.Interval
import Data.JSON (ToJSON(..),FromJSON(..))
import Data.Maybe (fromJust,isNothing)
import Data.Time as Time
import Data.Variance (Variance(),varies,vary,minimum,maximum,populationVariance,populationStdDev,count,mean,sampleVariance,sampleStdDev)
import Data.List as List
import Data.Ord
import GHC.Generics

newtype Summary = Summary { summary :: Variance }
  deriving (Eq,Ord,ToJSON,FromJSON,Show,Monoid,Semigroup) via Variance

-- | Check if a `Series` is empty.
--
-- O(1)
--
null :: Series a -> Bool
null (Root mt _ _ _) = isNothing mt

-- | Get a full-series analysis. 
--
-- O(1)
--
summarize :: Series a -> Summary
summarize (Root _ _ v _) = Summary v

-- | Get the total count of items in and summarized by the series.
--
-- O(1)
--
size :: Series a -> Int
size = count . summary . summarize

-- | Get the mean value of the series.
--
-- Nothing if empty.
--
-- O(1)
--
mean :: Series a -> Maybe Double
mean = Data.Variance.mean . summary . summarize

-- | Get the population variance of the series, assuming the series to be fully
-- representative.
--
-- Nothing if size < 2. 
--
-- O(1)
--
-- If you are sampling events, you might want to consider:
-- 
-- > Data.Variance.sampleVariance . summarize
--
variance :: Series a -> Maybe Double
variance = populationVariance . summary . summarize

-- | Get the population standard deviation of the series, assuming the series to
-- be fully representative.
-- 
-- Nothing if size < 2. 
--
-- O(1)
--
-- If you are sampling events, you might want to consider:
-- 
-- > Data.Variance.sampleStdDev . summarize
--
stdDev :: Series a -> Maybe Double
stdDev = populationStdDev . summary . summarize

-- | Get the series maximum, if the series is non-empty.
--
-- O(1)
--
maximum :: Series a -> Maybe Double
maximum = Data.Variance.maximum . summary . summarize

-- | Get the series minimum, if the series is non-empty.
--
-- O(1)
--
minimum :: Series a -> Maybe Double
minimum = Data.Variance.minimum . summary . summarize

-- | Get a summary size.
--
-- O(1)
--
summarySize :: Summary -> Int
summarySize = coerce count

-- | Get a summary variance.
--
-- Nothing, if empty.
--
-- O(1)
--
summaryVariance :: Summary -> Maybe Double
summaryVariance = coerce sampleVariance 

-- | Get a summary standard deviation.
--
-- Nothing, if empty.
-- 
-- O(1)
--
summaryStdDev :: Summary -> Maybe Double
summaryStdDev = coerce sampleStdDev 

-- | Get a summary mean.
--
-- Nothing, if empty.
--
-- O(1)
--
summaryMean :: Summary -> Maybe Double
summaryMean = coerce Data.Variance.mean

-- | Get a summary maximum.
--
-- Nothing, if empty.
--
-- O(1)
--
summaryMaximum :: Summary -> Maybe Double
summaryMaximum = coerce Data.Variance.maximum

-- | Get a summary minimum.
--
-- Nothing, if empty.
--
-- O(1)
--
summaryMinimum :: Summary -> Maybe Double
summaryMinimum = coerce Data.Variance.minimum

-- | Extract the un-summarized time-tagged values from a series. 
--
-- Note that this extraction method must reverse the list of points. This is
-- equivalent to Okasaki's functional dequeue approach, where the piper must be
-- paid for the exceptional run-time performance of the insert method.
--
-- O(n); n = unsummarized points
--
points :: Series a -> [(Time,a)]
points (Root _ _ _ (Series _ v _ _)) = List.reverse v

-- | Extract the summarized time-tagged `Summary`s from a series. 
--
-- Note that this extraction method must reverse each level of summaries
-- before concatenation. This is the most expensive common operation on a
-- `Series`.
--
-- Complexity should be linear in the count of results.
--
-- O(n); n = results
--
summaries :: Series a -> [(Time,Time,Summary)]
summaries (Root _ _ _ (Series mt _ _ (Just sv))) = build (extract sv)
  where
    build :: [(Time,Summary)] -> [(Time,Time,Summary)]
    build tvs = zipWith go tvs (tail (fmap (Just . fst) tvs) <> [Nothing])
      where
        go :: (Time,Summary) -> Maybe Time -> (Time,Time,Summary)
        go (start,v) (Just end) = (start,end,v)
        go (start,v) Nothing    = (start,fromJust mt,v)

    extract :: Series Summary -> [(Time,Summary)]
    extract (Series _ b _ mv) = maybe [] extract mv <> List.reverse b
summaries _ = []

-- | Approximately reconstruct a series at a given sampling rate from the first
-- recorded point to the latest. For summarized ranges, the same value is used
-- for all points (the summary mean).
reconstruct :: (Real a, Fractional a) => Time -> Series a -> [(Time,a)]
reconstruct g sa = concatMap go (summaries sa) ++ 
  case (sStart (sSeries sa),sLatest sa) of
    (Just start,Just end) -> go (start,end,Summary (varies snd (points sa)))
    _ -> []
  where
    go (start,end,s) = 
      [ (t,u) 
      | t <- if start == end then [start] else [start,start+g..end-g] 
      , Just u <- [fmap realToFrac (summaryMean s)] 
      ]

-- | Find the summary associated with a given time. Returns `Nothing` in
-- the case that the time does not fall within a summary.
--
-- O(summaries)
--
findSummary :: Time -> Series a -> Maybe (Time,Time,Summary)
findSummary t = wrapping . summaries
  where
    wrapping ((s,e,v):ss)
      | s <= t && t <= e = Just (s,e,v)
      | otherwise        = wrapping ss
    wrapping _           = Nothing

-- | Estimate the value at a given time. If the series does not cover the time,
-- `Nothing` is returned.
--
-- O(summaries)
--
estimate :: Time -> Series a -> Maybe Double
estimate t s = do
  (_,_,summary) <- findSummary t s 
  summaryMean summary

-- | Find the point that is closest to the given time.
--
-- O(points)
--
-- Note: `findPoint` should be avoided. Instead, `findSummary` should be used as
--       it is more representative of the expected use of a `Series`.
-- 
findPoint :: Time -> Series a -> Maybe (Time,a)
findPoint p = closest . points
  where
    closest []  = Nothing
    closest pts = Just (minimumBy (comparing (abs . subtract p . fst)) pts)

-- | Extract the summaries and points that overlap the given interval. The
-- summaries are expected to be non-overlapping and non-overlapping
-- with the list of points, assuming that the series was correctly constructed
-- with strictly-increasing times.
--
-- See `summarizeRange` to further reduce the result.
--
range :: Interval Time -> Series a -> ([(Time,Time,Summary)],[(Time,a)])
range i sa = (filter summaryInRange (summaries sa),filter pointInRange (points sa))
  where
    summaryInRange (s,e,v) = intersecting (interval (including s) (including e)) i
    pointInRange (t,a) = intersecting (interval (including t) (including t)) i

-- | Summarize a full range query result.
summarizeRange :: Real a => ([(Time,Time,Summary)],[(Time,a)]) -> (Time,Time,Summary)
summarizeRange (l,r) = 
  (List.minimum (fmap (\(s,_,_) -> s) l)
  ,List.maximum (fmap fst r)
  ,mconcat (Summary (varies snd r) : fmap (\(_,_,s) -> s) l)
  )

-- Note that this structure always contains a `Root a -> Series a -> Maybe (Series Summary)`;
-- the `sBuffer` is specialized to `[(Time,Summary)]` after the first `Series`.
data Series a 
  = Root 
    { sLatest   :: !(Maybe Time)
    , sTriggers :: !Policy
    , sVariance :: {-# UNPACK #-}!Variance
    , sSeries   :: !(Series a)
    }
  | Series
    { sStart    :: !(Maybe Time)
    , sBuffer   :: ![(Time,a)]
    , sCount    :: {-# UNPACK #-}!Int
    , sSummary  :: !(Maybe (Series Summary))
    }
  deriving stock (Generic,Eq,Ord,Show)

instance ToJSON a => ToJSON (Series a) where
  toJSON (Root l ts v s) = toJSON (l,ts,v,s)
  toJSON (Series t b c (Just s)) = toJSON (t,b,c,s)
  toJSON (Series t b c _) = toJSON (t,b,c)

instance FromJSON a => FromJSON (Series a) where
  parseJSON v = root <|> with <|> without
    where
      root = do
        (l,ts,v,s) <- parseJSON v
        pure (Root l ts v s)

      with = do
        (t,b,c,s) <- parseJSON v
        pure (Series t b c (Just s))
      
      without = do
        (t,b,c) <- parseJSON v
        pure (Series t b c Nothing)

-- | Construct a new series, given a Policy. 
--
-- O(1)
--
-- Warning: Partial - a valid policy is required.
--
-- A valid policy depends on the Policy type:
--   Clock: Clock policies must be non-empty and composed of strictly-increasing [Granularity].
--   Hybrid: Hybrid policies must be non-empty and composed of strictly-increasing [Granularity].
--   Elapsed: Elapsed policies must be non-empty and composed of strictly-increasing [Time].
--   Fixed: All fixed policies are valid. 
--
-- Note that an empty `Fixed` policy for `Series a` is equivalent to `(Maybe a,Maybe Time,Variance)`
-- with worse constant factors.
--   
new :: Policy -> Series a
new p = case p of
  (Clock ts) | not (validTimes (fmap period ts)) -> error "Clock policies must be non-empty and composed of strictly-increasing [Granularity]."
  (Hybrid its) | not (coerce validTimes (fmap (period . fst) its)) -> error "Hybrid policies must be non-empty and composed of strictly-increasing [Granularity]."
  (Elapsed ts) | not (validTimes ts) -> error "Elapsed policies must be non-empty and composed of strictly-increasing [Time]."
  _ -> Root Nothing p mempty (Series Nothing [] 0 Nothing)
  where
    period g = case g of
      Data.Series.Seconds _ n -> Time.Seconds (fromIntegral n) 0   
      Data.Series.Minutes _ n -> Time.Minutes (fromIntegral n) 0
      Data.Series.Hours _ n -> Time.Hours (fromIntegral n) 0
      Data.Series.Days _ n -> Time.Days (fromIntegral n) 0
      Data.Series.Weeks _ n -> Time.Weeks (fromIntegral n) 0
      Data.Series.Months _ n -> Time.Months (fromIntegral n) 0
      Data.Series.Quarters _ n -> Time.Months (fromIntegral n * 3) 0
      Data.Series.Years _ n -> Time.Years (fromIntegral n) 0

validTimes :: [Time] -> Bool
validTimes as = nonEmpty as && strictlyIncreasing as
  where
    nonEmpty = not . List.null

    strictlyIncreasing (a0:a1:as) = a0 < a1 && strictlyIncreasing (a1:as)
    strictlyIncreasing _ = True

data Granularity 
  = Seconds Time Int
  | Minutes Time Int
  | Hours Time Int
  | Days Time Int
  | Weeks Time Int
  | Months Time Int
  | Quarters Time Int
  | Years Time Int
  deriving stock (Generic,Eq,Ord,Show)
  deriving anyclass (ToJSON,FromJSON)

second = Data.Series.seconds 1
seconds = secondsFrom 0
secondsFrom = Data.Series.Seconds
minute = Data.Series.minutes 1
minutes = minutesFrom 0
minutesFrom = Data.Series.Minutes
hour = Data.Series.hours 1
hours = hoursFrom 0
hoursFrom = Data.Series.Hours
day = Data.Series.days 1
days = daysFrom 0
daysFrom = Data.Series.Days
week = Data.Series.weeks 1
weeks = weeksFrom 0
weeksFrom = Data.Series.Weeks
month = Data.Series.months 1
months = monthsFrom 0
monthsFrom = Data.Series.Months
quarter = Data.Series.quarters 1
quarters = quartersFrom 0
quartersFrom = Data.Series.Quarters
year = Data.Series.years 1
years = yearsFrom 0
yearsFrom = Data.Series.Years

-- | The `Policy` specifies how series events are aggregated.
--
-- Policy should be considered to affect only the space characteristics
-- and not the time characteristics of event integration/insert.
--
-- The clock policy gives wall-clock-delimited naturality: the `minute` 
-- granularity will aggregate at wall-clock minutes, the `hour` granularity
-- at wall-clock hours, etc....
--
-- The custom policy gives elapsed time aggregation. The expectation is that
-- the query logic will accomodate the mismatch between the wall-clock and
-- elapsed-time summaries. Note that the custom policy is expected to be
-- /slightly/ more performant since the matching logic only uses subtraction.
--
-- The fixed policy specifies that a certain number of events or summaries are 
-- aggregated at each level. If the aggregation gets to the empty list, that
-- tells the aggregator to roll off old summaries.
--
-- The current result of this approach is a bit odd, as there is 1 summary kept
-- in the final tier that definitely shouldn't be kept. For the moment, I 
-- consider it to be a minor issue.
--
-- The hybrid approach combines the two with either a fixed size or wall-clock 
-- aggregation, whichever threshold is reached first.
-- 
-- Note that there is an implicit limitation to the interface here: it must be
-- serializable. If that were not a requirement, we would simply use the type
-- of `shouldAggregate`, itself, along with a `depth` argument.
--
data Policy = Clock [Granularity] | Fixed [Int] | Hybrid [(Granularity,Int)] | Elapsed [Time]
  deriving stock (Generic,Eq,Ord,Show)
  deriving anyclass (ToJSON,FromJSON)

{-# INLINE peel #-}
peel :: Policy -> Policy
peel (Clock (t:ts)) = Clock ts
peel (Fixed (i:is)) = Fixed is
peel (Hybrid (ti:tis)) = Hybrid tis
peel x = x

{-# INLINE shouldAggregate #-}
shouldAggregate :: Policy -> Time -> Time -> Int -> Maybe Bool
shouldAggregate (Fixed (i:_)) _ _ count = Just (count >= i)
shouldAggregate (Elapsed (t:_)) start current _ = Just (current - start >= t)
shouldAggregate (Clock (g:_)) start current _ = 
  let 
    t :: Int -> Double -> Int
    t n = floor . (/ fromIntegral n) 
    d :: Int -> Int -> Int
    d n = (`div` n)
  in
    case g of
      Data.Series.Seconds from n ->
        let Time.Seconds s _ = current - from
            Time.Seconds s' _ = start - from
        in Just (t n s > t n s')
      Data.Series.Minutes from n ->
        let Time.Minutes m _ = current - from
            Time.Minutes m' _ = start - from
        in Just (t n m > t n m')
      Data.Series.Hours from n ->
        let Time.Hours h _ = current - from
            Time.Hours h' _ = start - from
        in Just (t n h > t n h')
      Data.Series.Days from n ->
        let Time.Days d _ = current - from
            Time.Days d' _ = start - from
        in Just (t n d > t n d')
      Data.Series.Weeks from n ->
        let Time.Weeks w _ = current - from
            Time.Weeks w' _ = start - from
        in Just (t n w > t n w')
      Data.Series.Months from n ->
        let Time.Gregorian y m _ = current - from
            Time.Gregorian y' m' _ = start - from
        in Just (d n (y * 12 + m) > d n (y' * 12 + m'))
      Data.Series.Quarters from n ->
        let Time.Gregorian y m _ = current - from
            Time.Gregorian y' m' _ = start - from
        in Just (d (3 * n) (y * 12 + m) > d (3 * n) (y' * 12 + m'))
      Data.Series.Years from n ->
        let Time.Gregorian y _ _ = current - from
            Time.Gregorian y' _ _ = start - from
        in Just (d n y > d n y')
shouldAggregate (Hybrid ((t,i):_)) start current count
  | count >= i = Just True
  | otherwise  = shouldAggregate (Clock [t]) start current count
shouldAggregate _ _ _ _ = Nothing


-- | Integrate a value into a series. It is expected that integrated points
-- have strictly-increasing `Time` values. This can be guaranteed in a multi-
-- threaded context by putting the `Series` behind an MVar and using a custom
-- `integrateIO`.
--
-- Note: complexity is amortized O(1) and should be largely unaffected (up to
--       minor constants) by the chosen Policy.
-- 
{-# INLINE integrate #-}
integrate :: forall a. Real a => Time -> a -> Series a -> Series a
integrate !now !a (Root _ ls v s) = Root (Just now) ls (vary id a v) (go ls s)
  where
    go :: Policy -> Series a -> Series a
    go p (Series Nothing _ _ s) = Series (Just now) [(now,a)] 1 s
    go p (Series (Just start) b c s) =
      case shouldAggregate p start now c of
        Just True -> 
          let
            b' = [(now,a)]
            c' = 1
            !s' = summarize (peel p) start (Summary (varies snd b)) s
          in
            Series (Just now) b' c' (Just s')

        Just False ->
          let
            b' = (now,a):b
            !c' = c + 1
          in
            Series (Just start) b' c' s
        
        Nothing -> 
          Series (Just start) ((now,a) : init b) c s

    summarize :: Policy -> Time -> Summary -> Maybe (Series Summary) -> Series Summary
    summarize _ t !v Nothing = Series (Just t) [(t,v)] 1 Nothing
    summarize p t v (Just (Series (Just start) b c s)) =
      case shouldAggregate p start now c of
        Just True ->
          let
            b' = [(t,v)]
            c' = 1
            !s' = summarize (peel p) start (mconcat (fmap snd b)) s
          in
            Series (Just now) b' c' (Just s')

        Just False ->
          let
            b' = (t,v) : b
            !c' = c + 1
          in
            Series (Just start) b' c' s

        Nothing ->
          Series (Just start) ((t,v) : init b) c s