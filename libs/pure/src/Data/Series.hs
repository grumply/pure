{-# language MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, BangPatterns, DerivingStrategies, DeriveAnyClass, DeriveGeneric #-}
module Data.Series (Series(),Policy(..),new,newWith,integrate,Data.Series.null,variance,points,summaries,summary,point,range,second,minute,hour,day,week,month,year,decade) where

import Control.Applicative ((<|>))
import Data.JSON (ToJSON(..),FromJSON(..))
import Data.Maybe (fromJust,isNothing)
import Data.Time
import Data.Variance (Variance,varies,vary)
import Data.List as List
import Data.Ord
import GHC.Generics

null :: Series a -> Bool
null (Root mt _ _ _) = isNothing mt

variance :: Series a -> Variance
variance (Root _ _ v _) = v

points :: Series a -> [(Time,a)]
points (Root _ _ _ (Series _ v _ _ _)) = List.reverse v

summaries :: Series a -> [(Time,Time,Variance)]
summaries (Root _ _ _ (Series mt _ _ _ (Just sv))) = build (extract sv)
  where    
    build :: [(Time,Variance)] -> [(Time,Time,Variance)]
    build tvs = zipWith go tvs (tail (fmap (Just . fst) tvs) <> [Nothing])
      where
        go :: (Time,Variance) -> Maybe Time -> (Time,Time,Variance)
        go (start,v) (Just end) = (start,end,v)
        go (start,v) Nothing    = (start,fromJust mt,v)

    extract :: Series Variance -> [(Time,Variance)]
    extract (Series _ b _ _ (Just v)) = extract v <> List.reverse b
    extract (Series _ _ _ _ Nothing) = []
    
summaries _ = []

summary :: Time -> Series a -> Maybe (Time,Time,Variance)
summary t = closest . summaries
  where
    closest ((s,e,v):ss)
      | s <= t && t <= e = Just (s,e,v)
      | otherwise        = closest ss
    closest _            = Nothing

point :: Time -> Series a -> Maybe (Time,a)
point p = closest . points
  where
    closest []  = Nothing
    closest pts = Just (minimumBy (comparing (abs . subtract p . fst)) pts)

range :: Maybe Time -> Maybe Time -> Series a -> ([(Time,Time,Variance)],[(Time,a)])
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
    , sLast     :: ![(Time,a)]
    , sCount    :: {-# UNPACK #-}!Int
    , sSummary  :: !(Maybe (Series Variance))
    }

instance ToJSON a => ToJSON (Series a) where
  toJSON (Root l ts v s) = toJSON (l,ts,v,s)
  toJSON (Series t b l c (Just s)) = toJSON (t,b,l,c,s)
  toJSON (Series t b l c _) = toJSON (t,b,l,c)

instance FromJSON a => FromJSON (Series a) where
  parseJSON v = root <|> with <|> without
    where
      root = do
        (l,ts,v,s) <- parseJSON v
        pure (Root l ts v s)

      with = do
        (t,b,l,c,s) <- parseJSON v
        pure (Series t b l c (Just s))
      
      without = do
        (t,b,l,c) <- parseJSON v
        pure (Series t b l c Nothing)

new :: Policy -> Series a
new = newWith Nothing

newWith :: Maybe Time -> Policy -> Series a
newWith mt p = Root mt p mempty (Series Nothing [] [] 0 Nothing)

newtype Granularity = Granularity Time
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

second = Granularity Second
minute = Granularity Minute
hour = Granularity Hour
day = Granularity Day
week = Granularity Week
month = Granularity Month
year = Granularity Year
decade = Granularity (Years 10 0)

data Policy = Natural [Granularity] | Fixed [Int] | Hybrid [(Granularity,Int)]
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

{-# INLINE peel #-}
peel :: Policy -> Policy
peel (Natural (t:ts)) = Natural ts
peel (Fixed (i:is)) = Fixed is
peel (Hybrid (ti:tis)) = Hybrid tis
peel x = x

{-# INLINE shouldAggregate #-}
shouldAggregate :: Policy -> Time -> Time -> Int -> Maybe Bool
shouldAggregate (Natural (Granularity t:_)) start current _ = 
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
    Week ->
      let Weeks w _ = current
          Weeks w' _ = start
      in Just (w > w')
    Month ->
      let Months m _ = current
          Months m' _ = start
      in Just (m > m')
    Year ->
      let Years y _ = current
          Years y' _ = start
      in Just (y > y')
    Years 10 0 ->
      let Years y _ = current
          Years y' _ = start
      in Just (round y `div` 10 > round y' `div` 10)
  -- Just (current - start >= t)
shouldAggregate (Fixed (i:_)) _ _ count = Just (count >= i)
shouldAggregate (Hybrid ((t,i):_)) start current count = 
  count >= i || shouldAggregate (Natural [t]) start current count
  -- Just (current - start >= t || count >= i)
shouldAggregate _ _ _ _ = Nothing

-- amortized O(1)
integrate :: forall a. Real a => Time -> a -> Series a -> Series a
integrate now a (Root _ ls v s) = Root (Just now) ls (vary id a v) (go ls s)
  where
    go :: Policy -> Series a -> Series a
    go p (Series Nothing _ _ _ s) = Series (Just now) [(now,a)] [] 1 s
    go p (Series (Just start) b l c s) =
      case shouldAggregate p start now c of
        Just True -> 
          let
            b' = [(now,a)]
            c' = 1
            !s' = summarize (peel p) start (varies snd b) s
          in
            Series (Just now) b' b c' (Just s')

        Just False ->
          let
            b' = (now,a):b
            !c' = c + 1
          in
            Series (Just start) b' l c' s
        
        Nothing -> 
          Series (Just start) ((now,a) : init b) l c s

    summarize :: Policy -> Time -> Variance -> Maybe (Series Variance) -> Series Variance
    summarize _ t v Nothing = Series (Just t) [(t,v)] [] 1 Nothing
    summarize p t v (Just (Series (Just start) b l c s)) =
      case shouldAggregate p start now c of
        Just True ->
          let
            b' = [(t,v)]
            c' = 1
            !s' = summarize (peel p) start (mconcat (fmap snd b)) s
          in
            Series (Just now) b' l c' (Just s')

        Just False ->
          let
            b' = (t,v) : b
            !c' = c + 1
          in
            Series (Just start) b' l c' s

        Nothing ->
          Series (Just start) ((t,v) : init b) l c s