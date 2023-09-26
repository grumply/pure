{-# language MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, BangPatterns, DerivingStrategies, DeriveAnyClass, DeriveGeneric, DerivingVia #-}
module Data.Series 
  ( Series()
  , Policy(..)
  , second
  , minute
  , hour
  , new
  , integrate

  , Data.Series.null

  , analyze
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
  ) where

import Control.Applicative ((<|>))
import Data.Coerce (coerce)
import Data.JSON (ToJSON(..),FromJSON(..))
import Data.Maybe (fromJust,isNothing)
import Data.Time
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
analyze :: Series a -> Variance
analyze (Root _ _ v _) = v

-- | Get the total count of items in and summarized by the series.
--
-- O(1)
--
size :: Series a -> Int
size = count . analyze

-- | Get the mean value of the series.
--
-- Nothing if empty.
--
-- O(1)
--
mean :: Series a -> Maybe Double
mean = Data.Variance.mean . analyze

-- | Get the population variance of the series, assuming the series to be fully
-- representative.
--
-- Nothing if size < 2. 
--
-- O(1)
--
-- If you are sampling events, you might want to consider:
-- 
-- > Data.Variance.sampleVariance . analyze
--
variance :: Series a -> Maybe Double
variance = populationVariance . analyze

-- | Get the population standard deviation of the series, assuming the series to
-- be fully representative.
-- 
-- Nothing if size < 2. 
--
-- O(1)
--
-- If you are sampling events, you might want to consider:
-- 
-- > Data.Variance.sampleStdDev . analyze
--
stdDev :: Series a -> Maybe Double
stdDev = populationStdDev . analyze

-- | Get the series maximum, if the series is non-empty.
--
-- O(1)
--
maximum :: Series a -> Maybe Double
maximum = Data.Variance.maximum . analyze

-- | Get the series minimum, if the series is non-empty.
--
-- O(1)
--
minimum :: Series a -> Maybe Double
minimum = Data.Variance.minimum . analyze

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
    extract (Series _ b _ (Just v)) = extract v <> List.reverse b
    extract (Series _ _ _ Nothing) = []
summaries _ = []

-- | Find the summary associated with a given time. Returns `Nothing` in
-- the case that the time does not fall within a summary.
--
-- O(n)
--
findSummary :: Time -> Series a -> Maybe (Time,Time,Summary)
findSummary t = wrapping . summaries
  where
    wrapping ((s,e,v):ss)
      | s <= t && t <= e = Just (s,e,v)
      | otherwise        = wrapping ss
    wrapping _           = Nothing

-- | Find the point that is closest to the given time.
--
-- O(n)
--
-- Note: `findPoint` should be avoided. Instead, `findSummary` should be used as
--       it is more representative of the expected use of a `Series`.
-- 
findPoint :: Time -> Series a -> Maybe (Time,a)
findPoint p = closest . points
  where
    closest []  = Nothing
    closest pts = Just (minimumBy (comparing (abs . subtract p . fst)) pts)

-- | Extract the summaries and points that overlap the given open-ended range of
-- times. The summaries are expected to be non-overlapping and non-overlapping
-- with the list of points, assuming that the series was correctly constructed
-- with strictly-increasing times.
range :: Maybe Time -> Maybe Time -> Series a -> ([(Time,Time,Summary)],[(Time,a)])
range mstart mend sa = (filter summaryInRange (summaries sa),filter pointInRange (points sa))
  where
    summaryInRange (s,e,v) =
      case (mstart,mend) of
        (Just start,Just end) -> start <= s && e <= end
        (Just start,_)        -> start <= s
        (_,Just end)          ->               e <= end
        _                     -> True

    pointInRange (t,a) =
      case (mstart,mend) of
        (Just start,Just end) -> start <= t && t <= end
        (Just start,_)        -> start <= t
        (_,Just end)          ->               t <= end
        _                     -> True

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
new p@(Clock ts) | not (coerce validTimes ts) = 
  error "Clock policies must be non-empty and composed of strictly-increasing [Granularity]."
new p@(Hybrid its) | not (coerce validTimes (fmap fst its)) = 
  error "Hybrid policies must be non-empty and composed of strictly-increasing [Granularity]."
new p@(Elapsed ts) | not (validTimes ts) = 
  error "Elapsed policies must be non-empty and composed of strictly-increasing [Time]."
new p = Root Nothing p mempty (Series Nothing [] 0 Nothing)

validTimes :: [Time] -> Bool
validTimes as = nonEmpty as && strictlyIncreasing as
  where
    nonEmpty = not . List.null

    strictlyIncreasing (a0:a1:as) = a0 < a1 && strictlyIncreasing (a1:as)
    strictlyIncreasing _ = True

newtype Granularity = Granularity Time
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (ToJSON,FromJSON)

second = Granularity Second
minute = Granularity Minute
hour = Granularity Hour
day = Granularity Day

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
-- /slightly/ more performant since the matching logic only u
-- only subtraction.
--
-- The fixed policy specifies that a certain number of events or summaries are 
-- aggregated at each level. If the aggregation gets to the empty list, that
-- tells to aggregator to roll off old summaries.
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
-- The constructor `Granularity` is not exposed; we expose a fixed set of 
-- granularity options. I'm reluctant to expose others, but could be convinced 
-- to do so. Each additional granularity incurs a cost in aggregation logic. 
-- Until I figure out a clean way to allow arbitrary `Time` values, I would 
-- prefer to keep this set small. The current set is `second`, `minute`, `hour`,
-- `day`. I originally had `week`, `month`, `year`, and `decade`, but because 
-- they do not calendar-align (Month in Data.Time is the 400-year Gregorian
-- average month of ~30.5 days and Year is the average year of ~365.2 days, and
-- switching to `UTCTime` for accuracy is even more costly, I removed them... My
-- assumption was that the client-side query logic can necessarily aggregate on
-- the expected boundaries. I'm sure there are still issues here, but
-- performance was my primary importance.
data Policy = Clock [Granularity] | Fixed [Int] | Hybrid [(Granularity,Int)] | Elapsed [Time]
  deriving stock Generic
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
shouldAggregate (Clock (Granularity t:_)) start current _ = 
  case t of
    Second ->
      let Seconds s _ = current
          Seconds s' _ = start
      in Just (s > s')
    Minute ->
      let Minutes m _ = current
          Minutes m' _ = start
      in Just (m > m')
    Hour ->
      let Hours h _ = current
          Hours h' _ = start
      in Just (h > h')
    Day ->
      let Days d _ = current
          Days d' _ = start
      in Just (d > d')
shouldAggregate (Hybrid ((t,i):_)) start current count
  | count >= i = Just True
  | otherwise  = shouldAggregate (Clock [t]) start current count
shouldAggregate _ _ _ _ = Nothing


-- | Integrate a value into a series. It is expected that integrated points
-- have strictly-increasing `Time` values. This can be guaranteed in a multi-
-- threaded context by putting the `Series` behind an MVar and using 
-- `integrateIO`.
--
-- Note: complexity is amortized O(1) and should be largely unaffected (up to
--       minor constants) by the chosen Policy.
-- 
integrate :: forall a. Real a => Time -> a -> Series a -> Series a
integrate now a (Root _ ls v s) = Root (Just now) ls (vary id a v) (go ls s)
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
    summarize _ t v Nothing = Series (Just t) [(t,v)] 1 Nothing
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