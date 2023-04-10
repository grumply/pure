{-# LANGUAGE TypeOperators, DefaultSignatures, RecordWildCards, FlexibleInstances, DataKinds, FlexibleContexts, UndecidableInstances, DeriveGeneric, DeriveAnyClass, MonoLocalBinds #-}
module Data.Covariance
  (Covariance,covary,covaries
  ,count,meanX,meanY,minimumX,maximumX,minimumY,maximumY
  ,covariance,sampleCovariance,populationCovariance
  ,varianceX,sampleVarianceX,populationVarianceX,varianceY,sampleVarianceY,populationVarianceY
  ,stdDevX,sampleStdDevX,populationStdDevX,stdDevY,sampleStdDevY,populationStdDevY
  ,correlation,sampleCorrelation,populationCorrelation
  ,Covary(..),Covariances
  ,lookupCovariance
  ,covaried,covariances
  ) where

import Control.Applicative ((<|>))
import Control.Arrow (first)
import qualified Data.Foldable as Foldable
import Data.Functor.Sum
import Data.Functor.Const
import Data.Functor.Compose
import Data.Functor.Product
import Data.Functor.Identity
import Data.Int
import Data.JSON (ToJSON,FromJSON)
import Data.List
import Data.Word
import Data.Maybe
import Data.Txt (Txt,fromTxt)
import Data.Txt.Trie as Trie
import Data.Variance (Vary(),varies,sampleVariance,populationVariance,sampleStdDev,populationStdDev)
import GHC.Generics as G
import GHC.Natural
import GHC.TypeLits

import Data.HashMap.Strict as HM
import Data.Map as Map

import qualified Data.Vector.Generic as V

data Covariance = Covariance
  { cCount    :: {-# UNPACK #-} !Double
  , cMeanx    :: {-# UNPACK #-} !Double
  , cMeany    :: {-# UNPACK #-} !Double
  , cMeanx2   :: {-# UNPACK #-} !Double
  , cMeany2   :: {-# UNPACK #-} !Double
  , cMinimumX :: {-# UNPACK #-} !Double
  , cMaximumX :: {-# UNPACK #-} !Double
  , cMinimumY :: {-# UNPACK #-} !Double
  , cMaximumY :: {-# UNPACK #-} !Double
  , cC        :: {-# UNPACK #-} !Double
  } deriving (Vary,Eq,Ord,Generic,ToJSON,FromJSON,Show)

instance Monoid Covariance where
  {-# INLINE [1] mempty #-}
  mempty = Covariance 0 0 0 0 0 0 0 0 0 0

instance Semigroup Covariance where
  {-# INLINE [1] (<>) #-}
  (<>) c1 c2
    | cCount c1 == 0 = c2
    | cCount c2 == 0 = c1
    | otherwise =
      let
        n1 = cCount c1
        n2 = cCount c2

        mx1 = cMeanx c1
        mx2 = cMeanx c2

        my1 = cMeany c1
        my2 = cMeany c2

        dx = mx1 - mx2
        dy = my1 - my2

        count = n1 + n2

        meanx = (n1 * mx1 + n2 * mx2) / count

        meany = (n1 * my1 + n2 * my2) / count

        meanx2 = vX c1 + vX c2 + dx ^^ 2 * n1 * n2 / count
        meany2 = vY c1 + vY c2 + dy ^^ 2 * n1 * n2 / count

        c = cC c1 + cC c2 + (mx1 - mx2) * (my1 - my2) * (n1 * n2 / count)

        vX c
          | cCount c < 2 = 0
          | otherwise    = cMeanx2 c

        vY c
          | cCount c < 2 = 0
          | otherwise    = cMeany2 c


        minx = min (cMinimumX c1) (cMinimumX c2)
        maxx = max (cMaximumX c1) (cMaximumX c2)
        miny = min (cMinimumY c1) (cMinimumY c2)
        maxy = max (cMaximumY c1) (cMaximumY c2)

        cov = Covariance count meanx meany meanx2 meany2 minx maxx miny maxy c
       in
         cov `seq` cov


count :: Covariance -> Int
count = round . cCount

meanX :: Covariance -> Maybe Double
meanX c
  | cCount c == 0 = Nothing
  | otherwise     = Just (cMeanx c)

meanY :: Covariance -> Maybe Double
meanY c
  | cCount c == 0 = Nothing
  | otherwise     = Just (cMeany c)

minimumX :: Covariance -> Maybe Double
minimumX c
  | cCount c == 0 = Nothing
  | otherwise     = Just (cMinimumX c)

maximumX :: Covariance -> Maybe Double
maximumX c
  | cCount c == 0 = Nothing
  | otherwise     = Just (cMaximumX c)

minimumY :: Covariance -> Maybe Double
minimumY c
  | cCount c == 0 = Nothing
  | otherwise     = Just (cMinimumY c)

maximumY :: Covariance -> Maybe Double
maximumY c
  | cCount c == 0 = Nothing
  | otherwise     = Just (cMaximumY c)

{-# RULES
"covary f g a mempty == Convariance {...}"
    forall f g a.
    covary f g a mempty =
      let x = realToFrac (f a)
          y = realToFrac (g a)
      in Covariance 1 x y 0 0 x x y y 0
"covary f g a (Covariance 0 0 0 0 0 0 0 0 0 0) == Covariance {...}"
    forall f g a.
    covary f g a (Covariance 0 0 0 0 0 0 0 0 0 0) =
      let x = realToFrac (f a)
          y = realToFrac (g a)
      in Covariance 1 x y 0 0 x x y y 0
  #-}

{- Are these always better?
"sampleVarianceX (covaries f g as) == sampleVariance (varies f as)" forall f g as. sampleVarianceX (covaries f g as) = sampleVariance (varies f as)
"populationVarianceX (covaries f g as) == populationVariance (varies f as)" forall f g as. populationVarianceX (covaries f g as) = populationVariance (varies f as)
"sampleVarianceY (covaries f g as) == sampleVariance (varies g as)" forall f g as. sampleVarianceY (covaries f g as) = sampleVariance (varies g as)
"populationVarianceY (covaries f g as) == populationVariance (varies g as)" forall f g as. populationVarianceY (covaries f g as) = populationVariance (varies g as)
"sampleStdDevX (covaries f g as) == sampleStdDev (varies f as)" forall f g as. sampleStdDevX (covaries f g as) = sampleStdDev (varies f as)
"populationStdDevX (covaries f g as) == populationStdDev (varies f as)" forall f g as. populationStdDevX (covaries f g as) = populationStdDev (varies f as)
"sampleStdDevY (covaries f g as) == sampleStdDev (varies g as)" forall f g as. sampleStdDevY (covaries f g as) = sampleStdDev (varies g as)
"populationStdDevY (covaries f g as) == populationStdDev (varies g as)" forall f g as. populationStdDevY (covaries f g as) = populationStdDev (varies g as)
  -}

{-# INLINE [1] covary #-}
covary :: (Real x, Real y) => (a -> x) -> (a -> y) -> a -> Covariance -> Covariance
covary f g a Covariance {..} =
  let
    x = realToFrac (f a)
    y = realToFrac (g a)

    dx = x - cMeanx
    dy = y - cMeany

    count = cCount + 1
    meanx = cMeanx + dx / count
    meany = cMeany + dy / count
    meanx2 = cMeanx2 + dx * (x - meanx)
    meany2 = cMeany2 + dy * (y - meany)
    c = cC + dx * (y - meany) -- NOTE: this looks instictively wrong, but isn't
    minx = min cMinimumX x
    maxx = max cMaximumX x
    miny = min cMinimumY y
    maxy = max cMaximumY y

    cov = Covariance count meanx meany meanx2 meany2 minx maxx miny maxy c
   in
     cov `seq` cov

{-# INLINE [1] covaries #-}
covaries :: (Foldable f, Real x, Real y) => (a -> x) -> (a -> y) -> f a -> Covariance
covaries f g = Foldable.foldl' (flip (covary f g)) mempty

{-# INLINE [1] covariance #-}
covariance :: Covariance -> Maybe Double
covariance = sampleCovariance

{-# INLINE [1] sampleCovariance #-}
sampleCovariance :: Covariance -> Maybe Double
sampleCovariance Covariance {..}
  | cCount < 2  = Nothing
  | otherwise   = Just $ cC / (cCount - 1)

{-# INLINE [1] populationCovariance #-}
populationCovariance :: Covariance -> Maybe Double
populationCovariance Covariance {..}
  | cCount < 2  = Nothing
  | otherwise   = Just $ cC / cCount

{-# INLINE [1] varianceX #-}
varianceX :: Covariance -> Maybe Double
varianceX = sampleVarianceX

{-# INLINE [1] sampleVarianceX #-}
sampleVarianceX :: Covariance -> Maybe Double
sampleVarianceX Covariance {..}
  | cCount < 2  = Nothing
  | otherwise   = Just $ cMeanx2 / (cCount - 1)

{-# INLINE [1] populationVarianceX #-}
populationVarianceX :: Covariance -> Maybe Double
populationVarianceX Covariance {..}
  | cCount < 2  = Nothing
  | otherwise   = Just $ cMeanx2 / cCount

{-# INLINE [1] varianceY #-}
varianceY :: Covariance -> Maybe Double
varianceY = sampleVarianceY

{-# INLINE [1] sampleVarianceY #-}
sampleVarianceY :: Covariance -> Maybe Double
sampleVarianceY Covariance {..}
  | cCount < 2  = Nothing
  | otherwise   = Just $ cMeany2 / (cCount - 1)

{-# INLINE [1] populationVarianceY #-}
populationVarianceY :: Covariance -> Maybe Double
populationVarianceY Covariance {..}
  | cCount < 2  = Nothing
  | otherwise   = Just $ cMeany2 / cCount

{-# INLINE [1] stdDevX #-}
stdDevX :: Covariance -> Maybe Double
stdDevX = sampleStdDevX

{-# INLINE [1] sampleStdDevX #-}
sampleStdDevX :: Covariance -> Maybe Double
sampleStdDevX = fmap sqrt . sampleVarianceX

{-# INLINE [1] populationStdDevX #-}
populationStdDevX :: Covariance -> Maybe Double
populationStdDevX = fmap sqrt . populationVarianceX

{-# INLINE [1] stdDevY #-}
stdDevY :: Covariance -> Maybe Double
stdDevY = sampleStdDevY

{-# INLINE [1] sampleStdDevY #-}
sampleStdDevY :: Covariance -> Maybe Double
sampleStdDevY = fmap sqrt . sampleVarianceY

{-# INLINE [1] populationStdDevY #-}
populationStdDevY :: Covariance -> Maybe Double
populationStdDevY = fmap sqrt . populationVarianceY

{-# INLINE [1] correlation #-}
correlation :: Covariance -> Maybe Double
correlation = sampleCorrelation

{-# INLINE [1] sampleCorrelation #-}
sampleCorrelation :: Covariance -> Maybe Double
sampleCorrelation c = do
  cov <- sampleCovariance c
  sdx <- sampleStdDevX c
  sdy <- sampleStdDevY c
  pure $
    if sdx == 0 || sdy == 0
    then 0
    else cov / (sdx * sdy)

{-# INLINE [1] populationCorrelation #-}
populationCorrelation :: Covariance -> Maybe Double
populationCorrelation c = do
  cov <- populationCovariance c
  sdx <- populationStdDevX c
  sdy <- populationStdDevY c
  pure $
    if sdx == 0 || sdy == 0
    then 0
    else cov / (sdx * sdy)

newtype Covariances = Covariances (HashMap (String,String) Covariance)
  deriving (Show,Eq,Generic)

instance Semigroup Covariances where
  {-# INLINE [1] (<>) #-}
  (<>) (Covariances c1) (Covariances c2)
    | HM.null c1 = Covariances c2
    | HM.null c2 = Covariances c1
    | otherwise  = Covariances $ HM.unionWith (<>) c1 c2

instance Monoid Covariances where
  {-# INLINE [1] mempty #-}
  mempty = Covariances mempty

{-# INLINE [1] lookupCovariance #-}
lookupCovariance :: String -> String -> Covariances -> Maybe Covariance
lookupCovariance x y (Covariances c) = HM.lookup (x,y) c <|> HM.lookup (y,x) c

{-# INLINE [1] covariances #-}
covariances :: (Foldable f, Covary a) => f a -> Covariances
covariances = Foldable.foldl' (flip covaried) (Covariances mempty)

{-# INLINE [1] covaried #-}
covaried :: Covary a => a -> Covariances -> Covariances
covaried = updateCovariances . flip (extract "") mempty
  where
    {-# INLINE [1] updateCovariances #-}
    updateCovariances :: [(String,Double)] -> Covariances -> Covariances
    updateCovariances cvs hm = hm <> Covariances (HM.fromList (fmap analyze pairs))
      where
        analyze ((x,xd),(y,yd)) = ((x,y),covary fst snd (xd,yd) mempty)
        pairs = [(x,y) | (x:ys) <- tails cvs, y <- ys]

instance {-# OVERLAPPING #-} (Covary a,Covary b) => Covary (a,b)
instance {-# OVERLAPPING #-} (Covary a,Covary b,Covary c) => Covary (a,b,c)
instance {-# OVERLAPPING #-} (Covary a,Covary b,Covary c,Covary d) => Covary (a,b,c,d)
instance {-# OVERLAPPING #-} (Covary a,Covary b,Covary c,Covary d,Covary e) => Covary (a,b,c,d,e)
instance {-# OVERLAPPING #-} (Covary a,Covary b,Covary c,Covary d,Covary e,Covary f) => Covary (a,b,c,d,e,f)
instance {-# OVERLAPPING #-} (Covary a,Covary b,Covary c,Covary d,Covary e,Covary f,Covary g) => Covary (a,b,c,d,e,f,g)

instance {-# OVERLAPPING #-} (Covary a,Covary b) => Covary (Either a b)

instance {-# OVERLAPPING #-} (Covary (f a), Covary (g a)) => Covary (Sum f g a)
instance {-# OVERLAPPING #-} (Covary (f a), Covary (g a)) => Covary (Product f g a)
instance {-# OVERLAPPING #-} (Covary a) => Covary (Const a b)
instance {-# OVERLAPPING #-} (Covary a) => Covary (Identity a)
instance {-# OVERLAPPING #-} (Covary (f ( g a))) => Covary (Compose f g a)

gCovaryReal :: (Real a) => String -> a -> [(String,Double)] -> [(String,Double)]
gCovaryReal nm a xs = (nm,realToFrac a) : xs

class Covary a where
  extract :: String -> a -> [(String,Double)] -> [(String,Double)]
  default extract :: (Generic a, GCovary (Rep a)) => String -> a -> [(String,Double)] -> [(String,Double)]
  extract nm = gCovary nm . from

instance {-# OVERLAPPABLE #-} Covary a where
  extract _ _ = id

instance {-# OVERLAPPING #-} Covary Double where
  extract = gCovaryReal

instance {-# OVERLAPPING #-} Covary Float where
  extract = gCovaryReal

instance {-# OVERLAPPING #-} Covary Int where
  extract = gCovaryReal

instance {-# OVERLAPPING #-} Covary Int8 where
  extract = gCovaryReal

instance {-# OVERLAPPING #-} Covary Int16 where
  extract = gCovaryReal

instance {-# OVERLAPPING #-} Covary Int32 where
  extract = gCovaryReal

instance {-# OVERLAPPING #-} Covary Int64 where
  extract = gCovaryReal

instance {-# OVERLAPPING #-} Covary Integer where
  extract = gCovaryReal

instance {-# OVERLAPPING #-} Covary Natural where
  extract = gCovaryReal

instance {-# OVERLAPPING #-} Covary Word where
  extract = gCovaryReal

instance {-# OVERLAPPING #-} Covary Word8 where
  extract = gCovaryReal

instance {-# OVERLAPPING #-} Covary Word16 where
  extract = gCovaryReal

instance {-# OVERLAPPING #-} Covary Word32 where
  extract = gCovaryReal

instance {-# OVERLAPPING #-} Covary Word64 where
  extract = gCovaryReal

instance {-# OVERLAPPING #-} (Covary a) => Covary (TxtTrie a) where
  extract nm a m = extract nm (Trie.toList a) m

instance {-# OVERLAPPING #-} (Covary a) => Covary (Map String a) where
  extract nm a m = extract nm (Map.toList a) m

instance {-# OVERLAPPING #-} (Covary a) => Covary (Map Txt a) where
  extract nm a m = extract nm (Map.toList a) m

instance {-# OVERLAPPING #-} (Covary a) => Covary (HashMap String a) where
  extract nm a xs =
    let x | nm == "" = nm
          | otherwise = nm ++ "."
    in
      Foldable.foldl'
        (\m (k,v) ->
          let n = x ++ k
          in extract n v m
        )
        xs
        (HM.toList a)

instance {-# OVERLAPPING #-} (Covary a) => Covary (HashMap Txt a) where
  extract nm a xs =
    let x | nm == "" = nm
          | otherwise = nm ++ "."
    in
      Foldable.foldl'
        (\m (k,v) ->
          let n = x ++ fromTxt k
          in extract n v m
        )
        xs
        (HM.toList a)

instance {-# OVERLAPPING #-} (Covary a, Foldable f) => Covary (f (String,a)) where
  extract nm a xs =
    let x | nm == "" = nm
          | otherwise = nm ++ "."
    in
      Foldable.foldl'
        (\m (k,v) ->
          let n = x ++ k
          in extract n v m
        )
        xs
        a

instance {-# OVERLAPPING #-} (Covary a, Foldable f) => Covary (f (Txt,a)) where
  extract nm a xs =
    let x | nm == "" = nm
          | otherwise = nm ++ "."
    in
      Foldable.foldl'
        (\m (k,v) ->
          let n = x ++ fromTxt k
          in extract n v m
        )
        xs
        a

instance {-# OVERLAPPING #-} (Covary a, V.Vector v a) => Covary (v a) where
  extract nm a xs =
    let x | nm == "" = nm
          | otherwise = nm ++ "."
    in
      V.ifoldl' (\m i v ->
        let n = x ++ show i
        in extract n v m
      )
      xs
      a

class GCovary a where
  gCovary :: String -> a x -> [(String,Double)] -> [(String,Double)]

instance ( Datatype d
         , GCovary a
         ) => GCovary (D1 d a) where
  gCovary base (M1 d) xs =
    gCovary base d xs

instance ( Constructor c
         , GCovary a
         ) => GCovary (C1 c a) where
  gCovary base (M1 c) xs =
    gCovary base c xs

instance {-# OVERLAPPING #-}
         ( Selector ('MetaSel 'Nothing u s l)
         , GUnlabeledFieldCovary a
         ) => GCovary (S1 ('MetaSel 'Nothing u s l) a) where
  gCovary base m@(M1 s) xs =
    gUnlabeledFieldCovary base 1 s xs

instance {-# OVERLAPPABLE #-}
         ( Selector s
         , GCovary a
         ) => GCovary (S1 s a) where
  gCovary base m@(M1 s) xs =
    let sn = selName m
        x | sn == ""   = base
          | base == "" = sn
          | otherwise  = base ++ "." ++ sn
    in gCovary x s xs

instance Covary a => GCovary (K1 r a) where
  gCovary base (K1 a) = extract base a

instance (GCovary a, GCovary b) => GCovary (a :+: b) where
  gCovary base (L1 a) = gCovary base a
  gCovary base (R1 b) = gCovary base b

instance {-# OVERLAPPING #-}
         ( Selector ('MetaSel 'Nothing u s l)
         , GRecordCovary (S1 ('MetaSel 'Nothing u s l) a :*: sb)
         ) => GCovary (S1 ('MetaSel 'Nothing u s l) a :*: sb) where
  gCovary base ss xs = gRecordCovary base 1 ss xs

instance {-# OVERLAPPABLE #-}
         ( GCovary a
         , GCovary b
         ) => GCovary (a :*: b) where
  gCovary base (a :*: b) xs = gCovary base b (gCovary base a xs)

class GUnlabeledFieldCovary a where
  gUnlabeledFieldCovary :: String -> Int -> a x -> [(String,Double)] -> [(String,Double)]

instance {-# OVERLAPPING #-} (GCovary a) => GUnlabeledFieldCovary (S1 ('MetaSel 'Nothing u s l) a) where
  gUnlabeledFieldCovary base index m@(M1 s) xs =
    let sn = show index
        x | base == "" = sn
          | otherwise  = base ++ "." ++ sn
    in gCovary x s xs

instance {-# OVERLAPPABLE #-} (GCovary (S1 s a)) => GUnlabeledFieldCovary (S1 s a) where
  gUnlabeledFieldCovary base _ = gCovary base

instance Covary a => GUnlabeledFieldCovary (K1 r a) where
  gUnlabeledFieldCovary base index (K1 a) xs =
    let x | base == "" = show index
          | otherwise  = base ++ "." ++ show index
    in extract x a xs

class GRecordCovary a where
  gRecordCovary :: String -> Int -> a x -> [(String,Double)] -> [(String,Double)]

instance {-# OVERLAPPABLE #-}
         ( GUnlabeledFieldCovary s
         ) => GRecordCovary s where
  gRecordCovary base index s xs = gUnlabeledFieldCovary base index s xs

instance {-# OVERLAPPING #-}
         ( Selector sa
         , GCovary a
         , GRecordCovary sb
         ) => GRecordCovary (S1 sa a :*: sb) where
  gRecordCovary base index (sa :*: sb) xs =
    let x | base == "" = show index
          | otherwise  = base ++ "." ++ show index
    in gRecordCovary base (index + 1) sb (gCovary x sa xs)
