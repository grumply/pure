{-# language TypeOperators, DefaultSignatures, RecordWildCards, FlexibleInstances, TypeSynonymInstances, DataKinds, FlexibleContexts, UndecidableInstances, DeriveGeneric, DeriveAnyClass, BangPatterns #-}
module Data.Variance
  (Variance
  ,minimum,maximum,mean,count
  ,stdDev,sampleStdDev,populationStdDev
  ,variance,sampleVariance,populationVariance
  ,vary,varies
  ,Vary(..),Variances
  ,lookupVariance,variances
  ) where

import Data.JSON
import Data.Txt hiding (count,minimum,maximum)

import qualified Data.Foldable as Foldable
import Data.Functor.Sum
import Data.Functor.Const
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Identity
import Data.Int
import Data.Word
import Data.Maybe
import GHC.Generics as G
import GHC.Natural
import GHC.TypeLits

import Data.HashMap.Strict as HM
import Data.Map as Map
import Data.Txt.Trie as Trie
import qualified Data.Vector.Generic as V

import Prelude hiding (minimum,maximum)

data Variance
  = Variance
    { vCount   :: {-# UNPACK #-} !Double
    , vMean    :: {-# UNPACK #-} !Double
    , vMean2   :: {-# UNPACK #-} !Double
    , vMinimum :: {-# UNPACK #-} !Double
    , vMaximum :: {-# UNPACK #-} !Double
    } deriving (Vary,Eq,Ord,Generic,ToJSON,FromJSON,Show)

instance Monoid Variance where
  {-# INLINE [1] mempty #-} 
  mempty = Variance 0 0 0 0 0

instance Semigroup Variance where
  {-# INLINE [1] (<>) #-}
  (<>) v1 v2
    | vCount v1 == 0 = v2
    | vCount v2 == 0 = v1
    | otherwise =
      let
        c1 = vCount v1
        c2 = vCount v2
        m1 = vMean v1
        m2 = vMean v2

        c  = c1 + c2

        m = (c1 * m1 + c2 * m2) / c

        d  = m1 - m2
        m2' = v v1 + v v2 + d ^^ 2 * c1 * c2 / c

        min_ = min (vMinimum v1) (vMinimum v2)
        max_ = max (vMaximum v1) (vMaximum v2)

        v c
          | vCount c < 2 = 0
          | otherwise    = vMean2 c

      in
        Variance c m m2' min_ max_
        

count :: Variance -> Int
count = round . vCount

mean :: Variance -> Maybe Double
mean v
  | vCount v == 0 = Nothing
  | otherwise     = Just (vMean v)

minimum :: Variance -> Maybe Double
minimum v
  | vCount v == 0 = Nothing
  | otherwise     = Just (vMinimum v)

maximum :: Variance -> Maybe Double
maximum v
  | vCount v == 0 = Nothing
  | otherwise     = Just (vMaximum v)

{-# INLINE [1] vary #-}
vary :: Real b => (a -> b) -> a -> Variance -> Variance
vary f !a Variance {..} =
  let b = realToFrac (f a)
      count = vCount + 1
      delta = b - vMean
      mean = vMean + delta / count
      mean2 = vMean2 + delta * (b - mean)
      mx = if vCount == 0 then b else max b vMaximum
      mn = if vCount == 0 then b else min b vMinimum
  in
    Variance count mean mean2 mn mx

{-# RULES
"vary f a mempty" forall f a. vary f a mempty = let b = realToFrac (f a) in Variance 1 b 0 b b
"vary f a (Variance 0 0 0 0 0)" forall f a. vary f a (Variance 0 0 0 0 0) = let b = realToFrac (f a) in Variance 1 b 0 b b
  #-}

{-# INLINE [1] varies #-}
varies :: (Foldable f, Real b) => (a -> b) -> f a -> Variance
varies f = Foldable.foldl' (\v (!a) -> vary f a v) mempty

{-# INLINE [1] variance #-}
variance :: Variance -> Maybe Double
variance = sampleVariance

{-# INLINE [1] sampleVariance #-}
sampleVariance :: Variance -> Maybe Double
sampleVariance Variance {..}
  | vCount < 2  = Nothing
  | otherwise   = Just $ vMean2 / (vCount - 1)

{-# INLINE [1] populationVariance #-}
populationVariance :: Variance -> Maybe Double
populationVariance Variance {..}
  | vCount < 2  = Nothing
  | otherwise   = Just $ vMean2 / vCount

{-# INLINE [1] stdDev #-}
stdDev :: Variance -> Maybe Double
stdDev = sampleStdDev

{-# INLINE [1] sampleStdDev #-}
sampleStdDev :: Variance -> Maybe Double
sampleStdDev = fmap sqrt . sampleVariance

{-# INLINE [1] populationStdDev #-}
populationStdDev :: Variance -> Maybe Double
populationStdDev = fmap sqrt . populationVariance

newtype Variances = Variances (HashMap String Variance)
 deriving (Show,Eq,Generic)

instance Semigroup Variances where
  {-# INLINE [1] (<>) #-}
  (<>) (Variances v1) (Variances v2)
    | HM.null v1 = Variances v2
    | HM.null v2 = Variances v1
    | otherwise = Variances $ HM.unionWith (<>) v1 v2

instance Monoid Variances where
  {-# INLINE [1] mempty #-}
  mempty = Variances mempty

{-# INLINE [1] lookupVariance #-}
lookupVariance :: String -> Variances -> Maybe Variance
lookupVariance s (Variances v) = HM.lookup s v

{-# INLINE [1] variances #-}
variances :: (Foldable f, Vary a) => f a -> Variances
variances = Foldable.foldl' (flip (varied "")) (Variances mempty)

class Vary a where
  varied :: String -> a -> Variances -> Variances
  default varied :: (Generic a, GVary (Rep a)) => String -> a -> Variances -> Variances
  varied nm = gUpdateVariance nm . from

instance {-# OVERLAPPING #-} (Vary a,Vary b) => Vary (a,b)
instance {-# OVERLAPPING #-} (Vary a,Vary b,Vary c) => Vary (a,b,c)
instance {-# OVERLAPPING #-} (Vary a,Vary b,Vary c,Vary d) => Vary (a,b,c,d)
instance {-# OVERLAPPING #-} (Vary a,Vary b,Vary c,Vary d,Vary e) => Vary (a,b,c,d,e)
instance {-# OVERLAPPING #-} (Vary a,Vary b,Vary c,Vary d,Vary e,Vary f) => Vary (a,b,c,d,e,f)
instance {-# OVERLAPPING #-} (Vary a,Vary b,Vary c,Vary d,Vary e,Vary f,Vary g) => Vary (a,b,c,d,e,f,g)

instance {-# OVERLAPPING #-} (Vary a,Vary b) => Vary (Either a b)

instance {-# OVERLAPPING #-} (Vary (f a), Vary (g a)) => Vary (Sum f g a)
instance {-# OVERLAPPING #-} (Vary (f a), Vary (g a)) => Vary (Product f g a)
instance {-# OVERLAPPING #-} (Vary a) => Vary (Const a b)
instance {-# OVERLAPPING #-} (Vary a) => Vary (Identity a)
instance {-# OVERLAPPING #-} (Vary (f ( g a))) => Vary (Compose f g a)

{-# INLINE [1] gUpdateRealVariance #-}
gUpdateRealVariance :: (Real a) => String -> a -> Variances -> Variances
gUpdateRealVariance nm a (Variances hm) =
  Variances (HM.alter (Just . maybe (vary id a mempty) (vary id a)) nm hm)

instance {-# OVERLAPPABLE #-} Vary a where
  varied _ _ = id

instance {-# OVERLAPPING #-} Vary Double where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Float where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Int where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Int8 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Int16 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Int32 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Int64 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Integer where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Natural where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Word where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Word8 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Word16 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Word32 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} Vary Word64 where
  varied = gUpdateRealVariance

instance {-# OVERLAPPING #-} (Vary a) => Vary (TxtTrie a) where
  varied nm a m = varied nm (Trie.toList a) m

instance {-# OVERLAPPING #-} (Vary a) => Vary (Map String a) where
  varied nm a m = varied nm (Map.toList a) m

instance {-# OVERLAPPING #-} (Vary a) => Vary (Map Txt a) where
  varied nm a m = varied nm (Map.toList a) m

instance {-# OVERLAPPING #-} (Vary a) => Vary (HashMap String a) where
  varied nm a hm = varied nm (HM.toList a) hm

instance {-# OVERLAPPING #-} (Foldable f, Vary a) => Vary (f (String,a)) where
  varied nm a v =
    let x | nm == ""  = nm
          | otherwise = nm ++ "."
    in
      Foldable.foldl'
        (\m (k,v) ->
          let n = x ++ k
          in varied n v m
        )
        v
        a

instance {-# OVERLAPPING #-} (Vary a) => Vary (HashMap Txt a) where
  varied nm a hm = varied nm (HM.toList a) hm

instance {-# OVERLAPPING #-} (Foldable f, Vary a) => Vary (f (Txt,a)) where
  varied nm a v =
    let x | nm == ""  = nm
          | otherwise = nm ++ "."
    in
      Foldable.foldl'
        (\m (k,v) ->
          let n = x ++ fromTxt k
          in varied n v m
        )
        v
        a

instance {-# OVERLAPPING #-} (Vary a, V.Vector v a) => Vary (v a) where
  varied nm a v =
    let x | nm == ""  = nm
          | otherwise = nm ++ "."
    in
      V.ifoldl' (\m i v ->
          let n = x ++ show i
          in varied n v m
        )
        v
        a

class GVary a where
  gUpdateVariance :: String -> a x -> Variances -> Variances

instance ( Datatype d
         , GVary a
         ) => GVary (D1 d a) where
  gUpdateVariance base (M1 d) hm =
    gUpdateVariance base d hm

instance ( Constructor c
         , GVary a
         ) => GVary (C1 c a) where
  gUpdateVariance base (M1 c) hm =
    gUpdateVariance base c hm

instance {-# OVERLAPPING #-}
         ( Selector ('MetaSel 'Nothing u s l)
         , GUnlabeledFieldVary a
         ) => GVary (S1 ('MetaSel 'Nothing u s l) a) where
  gUpdateVariance base m@(M1 s) hm =
    gUpdateUnlabeledFieldVariance base 1 s hm

instance {-# OVERLAPPABLE #-}
         ( Selector s
         , GVary a
         ) => GVary (S1 s a)  where
  gUpdateVariance base m@(M1 s) hm =
    let sn = selName m
        x | sn == ""   = base
          | base == "" = sn
          | otherwise  = base ++ "." ++ sn
    in gUpdateVariance x s hm

instance (Vary a) => GVary (K1 r a) where
  gUpdateVariance base (K1 a) hm =
    varied base a hm

instance (GVary a, GVary b) => GVary (a :+: b) where
  gUpdateVariance base (L1 a) = gUpdateVariance base a
  gUpdateVariance base (R1 b) = gUpdateVariance base b

instance {-# OVERLAPPING #-}
         ( Selector ('MetaSel 'Nothing u s l)
         , GRecordVary (S1 ('MetaSel 'Nothing u s l) a :*: sb)
         ) => GVary (S1 ('MetaSel 'Nothing u s l) a :*: sb) where
  gUpdateVariance base ss hm = gUpdateRecordVariance base 1 ss hm

instance {-# OVERLAPPABLE #-}
         ( GVary a
         , GVary b
         ) => GVary (a :*: b) where
  gUpdateVariance base (a :*: b) hm = gUpdateVariance base b (gUpdateVariance base a hm)

class GUnlabeledFieldVary a where
  gUpdateUnlabeledFieldVariance :: String -> Int -> a x -> Variances -> Variances

instance {-# OVERLAPPING #-} (GVary a) => GUnlabeledFieldVary (S1 ('MetaSel 'Nothing u s l) a) where
  gUpdateUnlabeledFieldVariance base index m@(M1 s) hm =
    let sn = show index
        x | base == "" = sn
          | otherwise  = base ++ "." ++ sn
    in gUpdateVariance x s hm

instance {-# OVERLAPPABLE #-} (GVary (S1 s a)) => GUnlabeledFieldVary (S1 s a) where
  gUpdateUnlabeledFieldVariance base _ = gUpdateVariance base

instance (Vary a) => GUnlabeledFieldVary (K1 r a) where
  gUpdateUnlabeledFieldVariance base index (K1 a) hm =
    let x | base == "" = show index
          | otherwise  = base ++ "." ++ show index
    in varied x a hm

class GRecordVary a where
  gUpdateRecordVariance :: String -> Int -> a x -> Variances -> Variances

instance {-# OVERLAPPABLE #-}
         ( GUnlabeledFieldVary s
         ) => GRecordVary s where
  gUpdateRecordVariance base index s hm = gUpdateUnlabeledFieldVariance base index s hm

instance {-# OVERLAPPING #-}
         ( Selector sa
         , GVary a
         , GRecordVary sb
         ) => GRecordVary (S1 sa a :*: sb) where
   gUpdateRecordVariance base index (sa :*: sb) hm =
     let x | base == "" = show index
           | otherwise  = base ++ "." ++ show index
     in gUpdateRecordVariance base (index + 1) sb (gUpdateVariance x sa hm)