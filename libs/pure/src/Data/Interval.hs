{-# language BlockArguments, DerivingStrategies, RecordWildCards, KindSignatures, MultiParamTypeClasses, FlexibleInstances, DeriveGeneric, DeriveFunctor, OverloadedStrings, FlexibleContexts, DerivingVia, TypeApplications, ViewPatterns, RankNTypes, BangPatterns,ScopedTypeVariables, CPP #-}
module Data.Interval 
  (Measure(..)
  ,Complement(..)
  ,Closure(..)
  ,clusive
  ,endpoint
  ,exclusive
  ,excluding
  ,inclusive
  ,including
  ,Interval()
  ,unsafeInterval
  ,interval
  ,point
  ,puncture

  ,empty
  ,unit
  ,degenerate
  ,proper
  
  ,closure
  
  ,proscriptive
  ,prescriptive
  ,open
  ,closed
  ,semiopen
  ,semiclosed
  ,leftOpen
  ,rightOpen
  
  ,nonempty
  ,contains
  ,within
  ,difference
  ,intersecting
  ,disjoint
  ,intersection
  
  ,Partition()
  ,simple
  
  ) where  

import Control.Applicative
import Control.Monad
import Data.Bifunctor (first,second)
import Data.Bool (bool)
import Data.JSON
import Data.List as List
import Data.Maybe (fromMaybe,isJust,isNothing)
import Data.Ratio
import Data.Txt (Txt)
import GHC.Exts (IsString(..))
import GHC.Generics
import Data.List.NonEmpty
import Test.QuickCheck (Arbitrary(..),frequency,oneof,listOf1)
import Data.Bool (bool)

-- $setup
-- >>> import Test.QuickCheck.Gen hiding (scale)
-- >>> import Test.QuickCheck.Property
-- >>> :set -XNoMonomorphismRestriction
-- >>> :set -XViewPatterns

{-

## Closure

### Arbitrary Instance

>>> canGenerateSentinels s maxAttempts = ioProperty . fmap (\as -> and (List.map (`elem` as) s)) . sequence . replicate maxAttempts $ generate arbitrary

>>> floatingSentinels = [  "(-1)", "[-1]", "(1)", "[1]", "(-Infinity)", "[-Infinity]", "(Infinity)", "[Infinity]" ] :: [Closure Double] 

Will produce inclusive and exclusive zeros, as well as inclusive and exclusive positive and negative infinities:
prop> canGenerateSentinels floatingSentinels 1000000
*** Failed! Falsified (after 1 test):

## Closure Independence

All closures are either inclusive or exclusive:
prop> \(a :: Closure Int) -> isInclusive a || isExclusive a
+++ OK, passed 100 tests.

But not both:
prop> \(a :: Closure Int) -> isInclusive a && not (isExclusive a) || isExclusive a && not (isInclusive a)
+++ OK, passed 100 tests.



-}

class Complement f a where
  complement :: f a -> f a 

-- | A measure of a structure `f` of `a`s.
class Measure (f :: * -> *) (a :: *) where
  measure :: f a -> a

-- | A closure is the interception of two concepts:
data Closure a = Excluding a | Including a
  deriving stock (Generic,Eq,Functor)

instance {-# OVERLAPPABLE #-} Arbitrary a => Arbitrary (Closure a) where 
  arbitrary = boolToClosure <$> arbitrary <*> arbitrary
    where
      boolToClosure = bool Excluding Including

instance {-# OVERLAPPING #-} Arbitrary (Closure Double) where
  arbitrary = oneof 
    [ pat <$> val
    | val <- [pure (-1/0),pure (1/0),arbitrary]
    , pat <- [Excluding,Including]
    ]

instance {-# OVERLAPPING #-} Arbitrary (Closure Float) where
  arbitrary = oneof 
    [ pat <$> val
    | val <- [pure (-1/0),pure (1/0),arbitrary]
    , pat <- [Excluding,Including]
    ]

instance Show a => Show (Closure a) where
  show (Excluding a) = "(" <> show a <> ")"
  show (Including a) = "[" <> show a <> "]"

instance Complement Closure a where
  complement (Excluding a) = Including a
  complement (Including a) = Excluding a

-- 
-- Comparability 
-- prop> \(a,b :: Closure Int) -> a <= b || b <= a
-- +++ OK, passed 100 tests.
--
-- Transitivity
-- prop> \(a,b,c :: Closure Int) -> if a <= b && b <= c then a <= c else True
-- +++ OK, passed 100 tests.
--
-- Reflexivity
-- prop> \(a :: Closure Int) -> a <= a
-- +++ OK, passed 100 tests.
--
-- Antisymmetry
-- prop> \(a,b :: Closure Int) -> if a <= b && b <= a then a == b else True
-- +++ OK, passed 100 tests.
-- 
instance Ord a => Ord (Closure a) where
  compare a b
    | endpoint a == endpoint b =
      case (a,b) of
        (Including {},Excluding {}) -> GT
        (Excluding {},Including {}) -> LT
        _                           -> EQ

    | otherwise = 
      compare (endpoint a) (endpoint b)


-- | Num instance with multiplication by 0 as a portal between clusivities.
-- This does not define a ring; multiplication does not distribute over addition.
--
-- ## Associativity of (+)
-- prop> \(a,b,c :: Closure Int) -> (a + b) + c == a + (b + c) 
-- +++ OK, passed 100 tests.
-- 
-- Commutativity of (+)
-- prop> \(a,b :: Closure Int) -> a + b == b + a 
-- +++ OK, passed 100 tests.
--
-- Additive identity
-- prop> \(a :: Closure Int) -> a + negate 0 == a
-- +++ OK, passed 100 tests.
--
-- Additive inverse
-- prop> \(a :: Closure Int) -> negate a + a == 0
-- +++ OK, passed 100 tests.
--
-- Associativity of (*)
-- prop> \(a,b,c :: Closure Int) -> (a * b) * c == a * (b * c)
-- +++ OK, passed 100 tests.
--
-- Multiplicative identity
-- prop> \(a :: Closure Int) -> a * Excluding 0 == a
-- +++ OK, passed 100 tests.
-- 
-- prop> \(a :: Closure Int) -> a - a + a == a
-- +++ OK, passed 100 tests.
--
-- prop> \(a :: Closure Int) -> a + a - a == a
-- +++ OK, passed 100 tests.
--
-- prop> \(a :: Closure Int) -> abs a * signum a == a
-- +++ OK, passed 100 tests.
--
-- >>> let a = Including 3 :: Closure Int
-- >>> let b = Excluding 0 
-- >>> ("a",a)
-- >>> ("b",b)
-- >>> ("a+b",a + b)
-- >>> ("a*a",a * a)
-- >>> ("a*b",a * b)
-- >>> ("a*(a+b)",a * (a + b))
-- >>> ("(a*a)+(a*b)",(a * a) + (a * b))
-- ("a",[3])
-- ("b",(0))
-- ("a+b",[3])
-- ("a*a",[9])
-- ("a*b",[3])
-- ("a*(a+b)",[9])
-- ("(a*a)+(a*b)",[12])

instance (Num a, Eq a) => Num (Closure a) where
  (Including a) + (Including b) = Including (a + b)
  (Including a) + (Excluding b) = Including (a - b)
  (Excluding a) + (Including b) = Including (b - a)
  (Excluding a) + (Excluding b) = Excluding (a + b)

  (Including 0) * (Including a) = Excluding a 
  (Including a) * (Including 0) = Excluding a 
  (Including a) * (Including b) = Including (a * b)
  (Including 0) * (Excluding a) = Including a
  (Including a) * (Excluding 0) = Including a
  (Including a) * (Excluding b) = Excluding (a * b)
  (Excluding 0) * (Including a) = Including a
  (Excluding a) * (Including 0) = Including a
  (Excluding a) * (Including b) = Excluding (a * b)
  (Excluding 0) * (Excluding a) = Excluding a
  (Excluding a) * (Excluding 0) = Excluding a
  (Excluding a) * (Excluding b) = Including (a * b)

  (Including a) - (Including b) = Including (a - b)
  (Including a) - (Excluding b) = Including (a + b)
  (Excluding a) - (Including b) = Including (b + a)
  (Excluding a) - (Excluding b) = Excluding (a - b)

  abs (Including a) = Including a
  abs (Excluding a) = Including a

  negate (Including a) = Excluding a
  negate (Excluding a) = Including a
  
  signum (Excluding 0) = Including 0
  signum (Including 0) = Excluding 0
  signum (Excluding a) = Excluding 1
  signum (Including a) = Including 1
  
  fromInteger = Including . fromInteger

-- | Fractional instance with division by zero as a portal between closures.
-- 
-- >>> let c = bool Excluding Including
-- prop> \(b1,c b1 -> a,flip c 0 -> z) -> a * z / z == a
-- +++ OK, passed 100 tests.
--
--
-- prop> \(a,b :: Closure Double) -> isInfinite (endpoint a) || isInfinite (endpoint b) || a * b / b == a
--
instance (Fractional a, Eq a) => Fractional (Closure a) where
  Including a / Including 0 = Excluding a
  Including a / Excluding 0 = Including a
  Excluding a / Including 0 = Including a
  Excluding a / Excluding 0 = Excluding a

  Including a / Including b = Including (a / b)
  Including a / Excluding b = Excluding (a / b)
  Excluding a / Including b = Excluding (a / b)
  Excluding a / Excluding b = Including (a / b)

  fromRational = Including . fromRational


instance (Read a, Ord a) => IsString (Closure a) where
  fromString = read

-- | A catamorphism for `Closure`. Like `maybe`, `either`, `bool`, etc....
--
-- > clusive (error "Impossible") id (Including a) == a
-- > clusive id (error "Impossible") (Excluding a) == a
--
clusive :: (a -> b) -> (a -> b) -> Closure a -> b
clusive exclusive _ (Excluding a) = exclusive a
clusive _ inclusive (Including a) = inclusive a

endpoint :: Closure a -> a
endpoint = clusive id id

-- | An intuitive measure of closure: 0 for exclosure, and 1 for inclosure.
instance Num n => Measure Closure n where
  measure = clusive (const 0) (const 1)

-- | A closure accessor. For exclusive closures, returns a Just.
exclusive :: Closure a -> Maybe a
exclusive = clusive Just (const Nothing)

-- | Test if a closure is exclusive.
isExclusive :: Closure a -> Bool
isExclusive = isJust . exclusive

-- | A closure accessor. For inclusive closures, returns a Just.
inclusive :: Closure a -> Maybe a
inclusive = clusive (const Nothing) Just

-- | Test if a closure is inclusive.
isInclusive :: Closure a -> Bool
isInclusive = isJust . inclusive

-- | A convenience constructor for exclusive closure.
excluding :: a -> Closure a
excluding = Excluding

-- | A convenience constructor for inclusive closure.
including :: a -> Closure a
including = Including

-- | An interval is the product of a lower and upper closure bound.
data Interval a = Empty | Interval 
  { lo :: Closure a 
  , hi :: Closure a
  } deriving stock (Generic,Eq,Functor)

instance (Ord a, Arbitrary a) => Arbitrary (Interval a) where
  arbitrary = frequency [ (1,pure Empty) , (9,interval <$> arbitrary <*> arbitrary) ]

instance Read (a -> Closure a) where
  readsPrec _ ('[':rest) = [(Including,rest)]
  readsPrec _ (']':rest) = [(Including,rest)]
  readsPrec _ ('(':rest) = [(Excluding,rest)]
  readsPrec _ (')':rest) = [(Excluding,rest)]
  readsPrec _ _ = []

newtype Elems a = Elems [a] deriving Show
instance Read a => Read (Elems a) where
  readsPrec n str = let (es,rest) = go [] str in [(Elems (List.reverse es),rest)]
    where
      go acc str
        | [(a,rest)] <- readsPrec n str
        = case rest of
            ',' : more -> go (a:acc) more
            rest -> (a:acc,rest)

        | otherwise
        = (acc,str)

instance Read a => Read (Closure a) where
  readsPrec n str0
    | [(left,str1)] <- readsPrec n str0
    , [(a :: a,str2)] <- readsPrec n str1
    , [(right,str3)] <- readsPrec n str2
    , isInclusive (left a) == isInclusive (right a :: Closure a)
    = [(left a,str3)]

    | otherwise
    = []


-- prop> \(a :: Interval Int) -> read (show a) == a
-- +++ OK, passed 100 tests.
-- 
instance (Read a, Ord a) => Read (Interval a) where
  readsPrec _ ( '∅'  : rest) = [(Empty,rest)]
  readsPrec n str0 
    | [(left,str1)] <- readsPrec n str0
    , [(Elems [lo,hi :: a],str2)] <- readsPrec n str1
    , [(right,str3)] <- readsPrec n str2
    = [(interval (left lo) (right hi),str3)]
  readsPrec _ _ = []
  

-- >>> "(-Infinity,1)" == interval (excluding (-1/0)) (excluding 1)
-- True

instance (Read a, Ord a) => IsString (Interval a) where
  fromString = read

infix 3 ...
(...) :: Ord a => a -> a -> Interval a
lo ... hi = interval (including lo) (including hi)

-- | Unsafely construct an interval. Prefer `interval` which is order invariant.
unsafeInterval :: Closure a -> Closure a -> Interval a
unsafeInterval = Interval

-- | The unit interval: `[0,1]`.
unit :: (Num a, Ord a) => Interval a
unit = interval (including 0) (including 1)

-- | The complete interval for bounded types.
universe :: (Bounded a, Ord a) => Interval a
universe = interval (including minBound) (including maxBound)

-- | A degenerate interval is any interval `[a,a]`.
degenerate :: Eq a => Interval a -> Bool
degenerate Interval {..} | endpoint lo == endpoint hi, Including {} <- lo, Including {} <- hi = True
degenerate _ = False

-- | A proper interval is any interval `a,b` where `a /= b`.
proper :: Ord a => Interval a -> Bool
proper Interval {..} = endpoint lo < endpoint hi
proper _ = False

-- | A smart constructor for intervals. Invariant w.r.t. argument order. All
-- intervals should be constructed via `interval` or its derivatives; the
-- constructor, `Interval`, is not exported.
interval :: Ord a => Closure a -> Closure a -> Interval a
interval a b 
  | endpoint a <= endpoint b = case (a,b) of
    (Including x,Including y) | x <= y -> Interval a b
    (Excluding x,Excluding y) | x <= y -> Interval a b
    (Including x,Excluding y) | x <= y -> Interval a b
    (Excluding x,Including y) | x <= y -> Interval a b
    _                                  -> Interval b a
  | otherwise = interval b a

-- | The singleton interval.
--
-- > interval (including a) (including a)
--
point :: Ord a => a -> Interval a
point a = interval (including a) (including a)

-- | The empty interval centered at a point.
--
-- > interval (excluding a) (excluding a)
--
puncture :: Ord a => a -> Interval a
puncture a = interval (excluding a) (excluding a)

-- | The closure of an interval, i.e.,
--
-- > closure () == ()
-- > closure (a,b) == [a,b]
-- > closure (a,b] == [a,b]
-- > closure [a,b) == [a,b]
-- > closure [a,b] == [a,b]
--
-- prop> \(i :: Interval Int) -> i `Data.Interval.within` closure i
-- *** Failed! Falsified (after 1 test):
-- NOW [0,0)
closure :: Interval a -> Interval a
closure Empty = Empty
closure Interval {..} = 
  let inc = clusive including including 
  in Interval (inc lo) (inc hi)

opening :: Interval a -> Interval a
opening Empty = Empty
opening Interval {..} = 
  let exc = clusive excluding excluding
  in Interval (exc lo) (exc hi)

-- | Check if two intervals are intersecting or have equivalent endpoints with 
-- alternate closures.
--
-- For example:
--
-- [a,b] (b,c) are non-intersecting, but they can be safely merged to [a,c)
--
continuous :: Ord a => Interval a -> Interval a -> Bool
continuous Empty _ = True
continuous _ Empty = True
continuous a b
  | intersecting a b = True

  | endpoint (hi a) == endpoint (lo b)
  = case (hi a,lo b) of
      (Including _, _) -> True
      (_, Including _) -> True
      _                -> False

  | endpoint (lo a) == endpoint (hi b)
  = case (lo a,hi b) of
      (Including _, _) -> True
      (_, Including _) -> True
      _                -> False

  | otherwise = False
     
--
-- ## Associativity of (+)
-- prop> \(a,b,c :: Interval Int) -> (a + b) + c == a + (b + c) 
-- 
-- Commutativity of (+)
-- prop> \(a,b :: Interval Int) -> a + b == b + a 
-- +++ OK, passed 100 tests.
--
-- Additive identity
-- prop> \(a :: Interval Int) -> a + Empty == a
-- +++ OK, passed 100 tests.
--
-- Additive inverse
-- prop> \(a :: Interval Int) -> negate a + a == Empty
--
-- Associativity of (*)
-- prop> \(a,b,c :: Interval Int) -> (a * b) * c == a * (b * c)
--
-- Multiplicative identity
-- prop> \(a :: Interval Int) -> a * 0 == a
-- 
-- prop> \(a :: Interval Int) -> a - a + a == a
--
-- prop> \(a :: Interval Int) -> a + a - a == a
--
-- prop> \(a :: Interval Int) -> abs a * signum a == a
--

-- prop> \(a,b,c :: Interval Int) -> (a + b) + c == a + (b + c) 
-- *** Failed! Falsified (after 8 tests):
-- ([-3,-2],(-4,2),(-7,-6])
--
-- >>> let (a :: Interval Int,b :: Interval Int,c :: Interval Int) = ("[-1,0)","(-1,-1)","[-1,1)")
-- >>> a + b
-- >>> b + c
-- >>> (a + b) + c
-- >>> a + (b + c)
-- (-1,0]
-- [0,0)
-- [-1,0]
-- [-1,0)
--
instance (Num a, Ord a) => Num (Interval a) where
  Empty + r = r
  l + Empty = l
  (Interval a b) + (Interval c d) = interval (a + c) (b + d)
  
  (*) a b = let Partition (p :| ps) = Partition (a :| []) * Partition (b :| []) in mconcat (p:ps)
  (-) a b = let Partition (p :| ps) = Partition (a :| []) - Partition (b :| []) in mconcat (p:ps)
  negate Empty = Empty
  negate (Interval a b) = interval (negate a) (negate b)
  abs Empty = Empty
  abs (Interval a b) = interval (abs a) (abs b)
  fromInteger = point . fromInteger

instance Show a => Show (Interval a) where
  show Empty = "∅" -- "()"
  show Interval {..} = Prelude.init (show lo) <> "," <> Prelude.tail (show hi)

-- | Ordering of intervals of orderables. Potentially non-intuitive.
--
-- > compare a,b   a,b   == EQ
-- > compare (0,_  (1,_  == LT
-- > compare (1,_  (0,_  == GT
-- > compare [0,_  (0,_  == LT
-- > compare (0,_  [0,_  == GT
-- > compare (0,1) (0,1] == LT
-- > compare (0,1] (0,1) == GT
-- > compare [0,1) [0,1] == LT
--
instance Ord a => Ord (Interval a) where
  compare Empty Empty = EQ
  compare Empty _ = LT
  compare _ Empty = GT
  compare a b = cmp LT GT (cmp GT LT EQ (hi a) (hi b)) (lo a) (lo b)
    where
      cmp lt gt eq a b =
        case (a,b) of
          (Including x,Including y)
            | x == y    -> eq
            | otherwise -> compare x y
          (Excluding x,Excluding y)
            | x == y    -> eq
            | otherwise -> compare x y
          (Including x,Excluding y)
            | x == y    -> lt
            | otherwise -> compare x y
          (Excluding x,Including y)
            | x == y    -> gt
            | otherwise -> compare x y

-- | The measure of an interval is its range.
--
-- > measure ()  == 0
-- > measure a,b == b - a
--
-- Note the implication:
--
-- > measure (a,a) == 0
-- > measure (a,a] == 0
-- > measure [a,a) == 0
-- > measure [a,a] == 0
--
instance Num a => Measure Interval a where
  measure Empty = 0
  measure (Interval a b) = endpoint b - endpoint a

-- | The monoidal identity is the empty interval. Monoidal append is a 
-- least-upper-bound of intervals.
--
-- > mempty <> a,b = a,b
-- > a,b <> mempty = a,b
--
instance Ord a => Monoid (Interval a) where
  mempty = Empty
  mappend = (<>)

-- | Implemented as a least-upper-bound of intervals.
--
-- >  a,b  <>  a,b  = a,b
-- > (a,b) <> (c,d) = (min a c,max b d)
-- > [a,b] <> [c,d] = [min a c,max b d]
-- > (a,b] <> (c,d] = (min a c,max b d]
-- > [a,b) <> [c,d) = [min a c,max b d)
-- > (a,b] <> (a,b) = (a,b]
-- > [a,b) <> (a,b) = [a,b)
--
instance Ord a => Semigroup (Interval a) where
  (<>) = lub
    where
      lub Empty b = b
      lub a Empty = a
      lub a b = interval (min (lo a) (lo b)) (max (hi a) (hi b))

-- | A proscriptive interval is empty, semiopen, or open.
proscriptive :: Interval a -> Bool
proscriptive Empty = True
proscriptive i = semiopen i || open i

-- | A prescriptive interval is closed.
prescriptive :: Interval a -> Bool
prescriptive = closed

-- | A semi-open interval has one inclusive and one exclusive bound. The empty
-- interval is not semi-open.
semiopen :: Interval a -> Bool
semiopen Interval {..} | Including {} <- lo, Excluding {} <- hi = True
semiopen Interval {..} | Excluding {} <- lo, Including {} <- hi = True
semiopen _ = False

-- | A semi-closed interval has one inclusive and one exclusive bound.
-- 
-- > semiclosed = semiopen
--
semiclosed :: Interval a -> Bool
semiclosed = semiopen

-- | An open interval has exclusive bounds. The empty interval is open.
open :: Interval a -> Bool
open Empty = True
open Interval {..} | Excluding _ <- lo, Excluding _ <- hi = True 
open _ = False

-- | A closed interval has inclusive bounds. The empty interval is not closed.
closed :: Interval a -> Bool
closed Interval {..} | Including _ <- lo, Including _ <- hi = True
closed _ = False

-- | A left-open interval excludes its lower bound but includes its upper bound.
leftOpen :: Interval a -> Bool
leftOpen (Interval Excluding {} Including {}) = True
leftOpen _ = False

-- | A right-open interval includes its lower bound but excludes its upper bound.
rightOpen :: Interval a -> Bool
rightOpen (Interval Including {} Excluding {}) = True
rightOpen _ = False

-- | Test if the measure of an interval is greater than zero.
nonempty :: Eq a => Interval a -> Bool
nonempty Empty = False
nonempty (Interval (Excluding a) (Excluding b)) | a == b = False
nonempty _ = True

-- | Query a closure with respect to an interval.
--
-- > ()   `contains` excluding _ == True
-- > ()   `contains` including _ == False
-- > (a,_ `contains` excluding a == True
-- > _,b) `contains` excluding b == True
-- > [a,_ `contains` excluding a == False
-- > [a,_ `contains` including a == True
-- > _,b] `contains` excluding b == False
-- > _,b] `contains` including b == True
--
-- TODO: test this.
--
-- prop> \(x :: Int) -> Empty `contains` Including x == False
-- prop> \(x :: Int) -> universe `contains` Including x == True
-- prop> \(x :: Int) -> universe `contains` Excluding x == False
-- prop> \(x :: Float) -> universe `contains` Including (signum x / 0) == False
-- prop> \(x :: Float) -> universe `contains` Including (signum x / 0) == False
--
contains :: Ord a => Interval a -> Closure a -> Bool
contains Empty Excluding {} = True
contains Empty _ = False
contains Interval {..} a =
  case a of
    Including a -> 
      case (lo,hi) of
        (Including x,Including y) -> x <= a && a <= y
        (Including x,Excluding y) -> x <= a && a <  y
        (Excluding x,Including y) -> x <  a && a <= y
        (Excluding x,Excluding y) -> x <  a && a <  y
    Excluding a ->
      case (lo,hi) of
        (Including x,Including y) -> a <  x || y <  a
        (Including x,Excluding y) -> a <  x || y <= a
        (Excluding x,Including y) -> a <= x || y <  a
        (Excluding x,Excluding y) -> a <= x || y <= a


-- | Test `intersection a b == a`.
-- 
-- > [a,b] `within` (a,maxBound] == False
-- > (a,b) `within` [a,maxBound] == True
--
within :: Ord a => Interval a -> Interval a -> Bool
within a b = intersection a b == a

overlapping :: Ord a => Interval a -> Interval a -> Bool
overlapping = intersecting

-- | Test if the intersection of two intervals is non-empty.
intersecting :: Ord a => Interval a -> Interval a -> Bool
intersecting a b =
  case intersection a b of
    Interval {..} 
      | endpoint lo < endpoint hi 
      -> True

      | endpoint lo == endpoint hi
      , (Including {},Including {}) <- (lo,hi) 
      -> True

    _ -> False

-- | Test if the intersection of two intervals is empty.
--
-- prop> \(i :: Interval Int) -> if nonempty i then disjoint i Empty && disjoint Empty i else True
-- +++ OK, passed 100 tests.
disjoint :: Ord a => Interval a -> Interval a -> Bool
disjoint a b = not (intersecting a b)

-- | Intersection of two intervals.
--
-- prop> \(i :: Interval Int) -> i `intersection` i == i
-- +++ OK, passed 100 tests.
-- prop> \(i :: Interval Int) -> Empty `intersection` i == Empty
-- +++ OK, passed 100 tests.
-- prop> \(i :: Interval Int) -> i `intersection` Empty == Empty
-- +++ OK, passed 100 tests.
--
intersection :: Ord a => Interval a -> Interval a -> Interval a
intersection Empty _ = Empty
intersection _ Empty = Empty
intersection a b 
  | endpoint lower <= endpoint upper 
  = interval lower upper

  | otherwise = Empty
  where
    lower =
      case compare (endpoint (lo a)) (endpoint (lo b)) of
        EQ | Including {} <- lo a, Excluding {} <- lo b -> lo b
        LT -> lo b
        _  -> lo a
     
    upper =
      case compare (endpoint (hi a)) (endpoint (hi b)) of
        EQ | Excluding {} <- hi a, Including {} <- hi b
           -> hi b
        GT -> hi b
        _  -> hi a

-- >>> complement (universe @Int)
-- /Users/sean/work/synchronizing-dynamics/libs/pure/src/Data/Interval.hs:439:10-43: No instance nor default method for class operation fromInteger

-- | The first interval minus the second. If the first contains the second, the
-- result will be two non-empty intervals.
--
-- prop> \(a :: Interval Int) -> uncurry (<>) (difference universe (complement a)) == a
--
difference :: Ord a => Interval a -> Interval a -> (Interval a,Interval a)
difference Empty b = (Empty,Empty)
difference a Empty = (a,Empty)
difference a b | disjoint a b = (a,Empty)
difference a b = (x,y)
  where
    (x,upper) = split a (lo b)
    y = shrink upper (hi b)

    split ~(Interval a b) (Including lo)
      | endpoint a < lo = (Interval a (Excluding lo),Interval (Including lo) b)
      | endpoint a > lo = (Empty,Interval a b)
      | otherwise = (Empty,Interval (Excluding lo) b)
    split ~(Interval a b) ~(Excluding lo)
      | endpoint a < lo = (Interval a (Including lo),Interval (Excluding lo) b)
      | endpoint a > lo = (Empty,Interval a b)
      | otherwise = (Empty,Interval (Including lo) b)
    
    shrink ~(Interval a b) (Including hi) 
      | endpoint b <= hi = Empty
      | otherwise = Interval (Excluding hi) b
    shrink ~(Interval a b) ~(Excluding hi)
      | endpoint b < hi = Empty
      | endpoint b == hi = Interval (Including hi) (Including hi)
      | otherwise = Interval (Including hi) b

-- | A partition of `a` as a set of non-intersecting intervals.
newtype Partition a = Partition (NonEmpty (Interval a))
  deriving stock (Generic,Eq,Functor)
  deriving (ToJSON,FromJSON,Ord) via (NonEmpty (Interval a))

#ifdef __GHCJS__
instance ToJSON a => ToJSON (NonEmpty a) where
  toJSON (a :| as) = toJSON (a:as)
instance FromJSON a => FromJSON (NonEmpty a) where
  parseJSON v = do
    (a:as) <- parseJSON v
    pure (a :| as)
#endif

instance (Ord a, Arbitrary a) => Arbitrary (Partition a) where
  arbitrary = do
    ~(i:is) <- listOf1 arbitrary
    pure (Partition (i :| is)) 

instance Show a => Show (Partition a) where
  show (Partition (x :| xs)) = show (x : xs)

-- prop> \(a :: Partition a) -> read (show a) == a
-- +++ OK, passed 100 tests.
--
instance (Ord a, Read a) => Read (Partition a) where
  readsPrec n str 
    | [(i:is,rest)] <- readsPrec n str
    = [ (Partition (i :| is), rest) ]
  
    | otherwise
    = []

-- | Construct a partition from a single interval.
--
-- > simple x == Partition (x :| [])
--
simple :: Interval a -> Partition a    
simple i = Partition (i :| [])

instance Num a => Measure Partition a where
  measure (Partition is) = sum (fmap measure is) 

instance (Num a, Ord a, Bounded a) => Complement Interval a where
  complement i = Interval (including minBound) (including maxBound) - i

instance (Num a, Ord a, Bounded a) => Complement Partition a where
  complement a = Partition (interval (including minBound) (including maxBound) :| []) - a

instance (Num a, Ord a) => Num (Partition a) where
  -- let u = Parition [Empty] 
  -- let x = Partition _
  -- u + x == x
  (+) (Partition (a :| as)) (Partition (b :| bs))
    | ~(x:xs) <- go (a:as) (b:bs) = Partition (x :| xs)
    where
      go [] bs = bs
      go as [] = as
      go (a:as) (b:bs) 
        | continuous a b = go (a <> b : as) bs
        | a < b          = a : go as (b:bs)
        | otherwise      = b : go (a:as) bs

  (-) (Partition (a :| as)) (Partition (b :| bs))
    | ~(x:xs) <- go (a:as) (b:bs) = Partition (x :| xs)
    where
      go [] bs = []
      go as [] = as
      go (a:as) (b:bs)
        | intersecting a b 
        , (x,y) <- difference a b
        = case (x,y) of
            (Empty,_) -> y : go as (b:bs)
            (_,Empty) -> x : go as (b:bs)
            _ -> x : y : go as (b:bs) 
        | a < b = a : go as (b:bs)
        | otherwise = go (a:as) bs

  -- let u = Partition [interval (including minBound) (including maxBound)]
  -- let x = Partition _
  -- u * x == x
  (*) (Partition (a :| as)) (Partition (b :| bs)) 
    | ~(x:xs) <- go (a:as) (b:bs) = Partition (x :| xs)
    where
      go [] bs = []
      go as [] = as
      go (a:as) (b:bs)
        | i <- intersection a b 
        , nonempty i
        = i : go as (b:bs)

        | a < b = go as (b:bs)

        | otherwise = go (a:as) bs

  abs = id
  negate (Partition (a :| as)) = 
    let (b : bs) = fmap negate (a:as)
    in Partition (b :| bs)
  signum = id

  fromInteger i = Partition ( interval (including (fromInteger i)) (including (fromInteger i)) :| [] )

{-

instance Divisible m => Divisible (ListT m) where
  divide :: (a -> (b, c)) -> f b -> f c -> f a
  divide f (ListT l) (ListT r) = ListT $ divide (funzip . map f) l r
  conquer :: f a
  conquer = ListT conquer

instance Divisible m => Decidable (ListT m) where
  lose :: (a -> Void) -> f a
  lose _ = ListT conquer
  choose :: (a -> Either b c) -> f b -> f c -> f a
  choose f (ListT l) (ListT r) = ListT $ divide ((lefts &&& rights) . map f) l r
-}
instance ToJSON a => ToJSON (Closure a) where
  toJSON (Excluding a) = toJSON ("excluding" :: Txt,a)
  toJSON (Including a) = toJSON ("including" :: Txt,a)

instance FromJSON a => FromJSON (Closure a) where
  parseJSON v = do
    (e,a) <- parseJSON v
    case e :: Txt of
      "excluding" -> pure (Excluding a)
      "including" -> pure (Including a)
      _           -> fail "Unknown Closure"

instance ToJSON a => ToJSON (Interval a) where
  toJSON Interval {..} = object [ "lo" .= lo , "hi" .= hi ]

instance (FromJSON a, Ord a) => FromJSON (Interval a) where
  parseJSON = withObject "Interval" \o -> do
    lo <- o .: "lo"
    hi <- o .: "hi"
    pure (interval lo hi)

instance Bounded Float where
  minBound = (-1)/0
  maxBound = 1/0

instance Bounded Double where
  minBound = (-1)/0
  maxBound = 1/0

 -- ## Constructive Correctness
--
-- ### Inclusive Intervals
-- prop> Interval 1 3 == "[1,3]"
-- +++ OK, passed 1 test.
--
-- ### Order Independence
-- prop> Interval 3 1 == "[1,3]"
-- +++ OK, passed 1 test.
--
-- ### Singleton Intervals
-- prop> Interval 1 1 == "[1,1]"
-- +++ OK, passed 1 test.
--
-- ### Degenerate Intervals
-- prop> Interval (Excluding 1) (Excluding 1) == "(1,1)"
-- +++ OK, passed 1 test.
--
-- ### Preferring Exclusions
-- prop> Interval (Excluding 1) 1 == "(1,1)" && "(1,1)" == Interval 1 (Excluding (1 :: Int))
-- +++ OK, passed 1 test.
--
-- ### Non-constructable Intervals
-- prop> \(a :: Int) -> Interval (Including a) (Excluding a)  == Interval (Excluding a) (Excluding a)
-- +++ OK, passed 100 tests.
--
-- prop> "[0,0)" == ("(0,0)" :: Interval Int)
-- +++ OK, passed 1 test.
--
-- # Interval
--
-- ## Ord Tests
--
--
--
-- ## Num Tests
--
-- ### Associativity of (+)
-- prop> \(a,b,c :: Interval Int) -> (a + b) + c == a + (b + c) 
-- *** Failed! Falsified (after 48 tests):
-- ((0,18),(-46,-27],(23,25])
-- 
-- ### Commutativity of (+)
-- prop> \(a,b :: Interval Int) -> a + b == b + a 
-- *** Failed! Falsified (after 2 tests):
-- ((-1,1],(0,1])
--
-- ### Additive identity
-- prop> \(a :: Interval Int) -> a + Empty == a
-- +++ OK, passed 100 tests.
--
-- ### Additive inverse
-- prop> \(a :: Interval Int) -> empty (a + negate a)
-- *** Failed! Falsified (after 3 tests):
-- [-2,0)
--
-- >>> ("(-1,-1)" :: Interval Int) + negate "(-1,-1)"
-- (0,1)
--
-- >>> ("(-1,1]" :: Interval Int) + "(-1,1]"
-- (-1,2]
--
-- ### Associativity of (*)
-- prop> \(a,b,c :: Interval Int) -> (a * b) * c == a * (b * c)
-- *** Failed! Falsified (after 4 tests):
-- ([-3,2],[-2,-1],[-2,0])
--
-- ### Multiplicative identity
-- prop> \(a :: Interval Int) -> a * 1 == a
-- *** Failed! Falsified (after 2 tests):
-- (-1,0)
-- 
-- prop> \(a :: Interval Int) -> a - a + a == a
-- *** Failed! Falsified (after 3 tests):
-- [0,1)
--
-- prop> \(a :: Interval Int) -> a + a - a == a
-- *** Failed! Falsified (after 3 tests):
-- [-1,1]
--
-- prop> \(a :: Interval Int) -> abs a * signum a == a
-- *** Failed! Falsified (after 2 tests):
-- [0,1)
--
-- ### Distributivity of multiplication over addition
--
-- prop> \(a,b,c :: Interval Int) -> a * (b + c) == (a * b) + (a * c)
-- *** Failed! Falsified (after 6 tests):
-- ((-2,0],(-1,4),[-2,1])
--
-- ## Semigroup
--
-- prop> \(a, b :: Interval Int) -> let c = a <> b in a `within` c && b `within` c
-- *** Failed! Falsified (after 3 tests):
-- ((-1,-1),(-1,0))
--
-- ## contains
--
-- prop> \(x :: Int) -> Empty `contains` Including x == False
-- +++ OK, passed 100 tests.
-- prop> \(x :: Int) -> universe `contains` Including x == True
-- +++ OK, passed 100 tests.
-- prop> \(x :: Int) -> universe `contains` Excluding x == False
-- +++ OK, passed 100 tests.
-- prop> \(x :: Float) -> universe `contains` Including (signum x / 0) == False
-- *** Failed! Falsified (after 2 tests and 1 shrink):
-- -0.1
-- prop> \(x :: Float) -> universe `contains` Including (signum x / 0) == False
-- *** Failed! Falsified (after 2 tests and 3 shrinks):
-- 0.1
--
-- ## intersection
--
-- prop> \(i :: Interval Int) -> i `intersection` i == i
-- +++ OK, passed 100 tests.
-- prop> \(i :: Interval Int) -> Empty `intersection` i == Empty
-- +++ OK, passed 100 tests.
-- prop> \(i :: Interval Int) -> i `intersection` Empty == Empty
-- +++ OK, passed 100 tests.
-- prop> intersection "[0,0]" "[0,1]" == ("[0,0]" :: Interval Int)
-- +++ OK, passed 1 test.
-- prop> intersection "(0,0)" "[0,0]" == ("(0,0)" :: Interval Int)
-- +++ OK, passed 1 test.
--
-- ## complement
--
-- >>> complement (universe @Int)
-- [0,0]
--
-- ## disjoin
--
-- prop> \(i :: Interval Int) -> if nonempty i then disjoint i Empty && disjoint Empty i else True
-- +++ OK, passed 100 tests.
--
-- ## difference
--
-- prop> \(a :: Interval Int) -> uncurry (<>) (difference universe (complement a)) == a
-- *** Failed! Falsified (after 1 test):
-- [0,0]
--
-- ## within
--
-- prop> \(i :: Interval Int) -> Empty `within` i && i `within` universe
-- +++ OK, passed 100 tests.
--
-- ## complement
--
-- prop> \(i :: Interval Int) -> complement (complement i) == i
-- *** Failed! Falsified (after 1 test):
-- ∅
--
-- >>> universe - ("(-1,1)" :: Interval Int)
-- [-9223372036854775807,9223372036854775806]
-- >>> universe - ("[-9223372036854775807,9223372036854775806]" :: Interval Int)
-- [-1,1]
--

{-
-- | An Interval is either `Empty`, or low and high `Extent`s.
data Interval a = Empty | Interval 
  { lo :: Extent a 
  , hi :: Extent a
  } deriving stock (Generic,Eq,Functor)

-- | A point is the singleton Interval centered around the given value.
point :: Ord a => a -> Interval a
point a = let x = including a in Interval x x

-- | A puncture is an empty Interval centered around the given value.
puncture :: Ord a => a -> Interval a
puncture a = let x = excluding a in Interval x x

-- | A universal Interval is inclusive of both the minBound and maxBound.
universe :: (Ord a, Bounded a) => Interval a
universe = Interval (including minBound) (including maxBound)

-- | The width of an Interval is the distance between its ends.
width :: Num a => Interval a -> a
width Empty = 0
width (Interval lo hi) = end hi - end lo

-- | A proscriptive Interval is empty, semiopen, or open.
proscriptive :: Interval a -> Bool
proscriptive Empty = True
proscriptive i = semiopen i || open i

-- | A prescriptive Interval is closed.
prescriptive :: Interval a -> Bool
prescriptive = closed

-- | A semi-open Interval has one exclusive and one inclusive bound. The empty
-- Interval is not semi-open.
semiopen :: Interval a -> Bool
semiopen Interval {..} | Including {} <- lo, Excluding {} <- hi = True
semiopen Interval {..} | Excluding {} <- lo, Including {} <- hi = True
semiopen _ = False

-- | A semi-closed Interval has one inclusive and one exclusive bound.
-- 
-- > semiclosed = semiopen
--
semiclosed :: Interval a -> Bool
semiclosed = semiopen

-- | An open Interval has exclusive bounds. The empty Interval is open.
open :: Interval a -> Bool
open Empty = True
open Interval {..} | Excluding _ <- lo, Excluding _ <- hi = True 
open _ = False

-- | A closed Interval has inclusive bounds. The empty Interval is closed.
closed :: Interval a -> Bool
closed Empty = True
closed Interval {..} | Including _ <- lo, Including _ <- hi = True
closed _ = False

-- | A left-open Interval excludes its lower bound but includes its upper bound.
leftOpen :: Interval a -> Bool
leftOpen (Interval Excluding {} Including {}) = True
leftOpen _ = False

-- | A right-open Interval includes its lower bound but excludes its upper bound.
rightOpen :: Interval a -> Bool
rightOpen (Interval Including {} Excluding {}) = True
rightOpen _ = False

-- | Test if the measure of an Interval is greater than zero.
nonempty :: Eq a => Interval a -> Bool
nonempty Empty = False
nonempty (Interval (Excluding a) (Excluding b)) | a == b = False
nonempty _ = True

instance (Ord a, Arbitrary a) => Arbitrary (Interval a) where
  arbitrary = frequency [ (1,pure Empty) , (9,Interval <$> arbitrary <*> arbitrary) ]

instance (Ord a, Num a) => Ord (Interval a) where
  compare = compare `on` width

instance (Num a, Ord a, Bounded a) => Complement Interval a where
  complement i = universe - i

-- | Intersection of two Intervals. 
intersection :: Ord a => Interval a -> Interval a -> Interval a
intersection Empty _ = Empty
intersection _ Empty = Empty
intersection a b 
  | end lower <= end upper 
  = Interval lower upper

  | otherwise = Empty
  where
    lower =
      case compare (end (lo a)) (end (lo b)) of
        EQ | Including {} <- lo a, Excluding {} <- lo b -> lo b
        LT -> lo b
        _  -> lo a
     
    upper =
      case compare (end (hi a)) (end (hi b)) of
        EQ | Excluding {} <- hi a, Including {} <- hi b
           -> hi b
        GT -> hi b
        _  -> hi a

-- | Test if the intersection of two Intervals is non-empty.
intersecting :: Ord a => Interval a -> Interval a -> Bool
intersecting a b =
  case intersection a b of
    Interval {..} 
      | end lo < end hi 
      -> True

      | end lo == end hi
      , (Including {},Including {}) <- (lo,hi) 
      -> True

    _ -> False

-- | Test if the intersection of two Intervals is empty.
disjoint :: Ord a => Interval a -> Interval a -> Bool
disjoint a b = not (intersecting a b)

-- | The first Interval minus the second. If the first contains the second, the
-- result will be two non-empty Intervals.
difference :: Ord a => Interval a -> Interval a -> (Interval a,Interval a)
difference Empty b = (Empty,Empty)
difference a Empty = (a,Empty)
difference a b | disjoint a b = (a,Empty)
difference a b = (x,y)
  where
    (x,upper) = split a (lo b)
    y = shrink upper (hi b)

    split ~(Interval a b) (Including lo)
      | end a < lo = (Interval a (Excluding lo),Interval (Including lo) b)
      | end a > lo = (Empty,Interval a b)
      | otherwise = (Empty,Interval (Excluding lo) b)
    split ~(Interval a b) ~(Excluding lo)
      | end a < lo = (Interval a (Including lo),Interval (Excluding lo) b)
      | end a > lo = (Empty,Interval a b)
      | otherwise = (Empty,Interval (Including lo) b)
    
    shrink ~(Interval a b) (Including hi) 
      | end b <= hi = Empty
      | otherwise = Interval (Excluding hi) b
    shrink ~(Interval a b) ~(Excluding hi)
      | end b < hi = Empty
      | end b == hi = Interval (Including hi) (Including hi)
      | otherwise = Interval (Including hi) b

-- | Query an Interval for a extent.
contains :: Ord a => Interval a -> Extent a -> Bool
contains Empty Excluding {} = True
contains Empty _ = False
contains Interval {..} a =
  case a of
    Including a -> 
      case (lo,hi) of
        (Including x,Including y) -> x <= a && a <= y
        (Including x,Excluding y) -> x <= a && a <  y
        (Excluding x,Including y) -> x <  a && a <= y
        (Excluding x,Excluding y) -> x <  a && a <  y
    Excluding a ->
      case (lo,hi) of
        (Including x,Including y) -> a <  x || y <  a
        (Including x,Excluding y) -> a <  x || y <= a
        (Excluding x,Including y) -> a <= x || y <  a
        (Excluding x,Excluding y) -> a <= x || y <= a

-- | Test if the first Interval is contained in the second.
within :: Ord a => Interval a -> Interval a -> Bool
within a b = intersection a b == a

-- | Test if two Intervals overlap.
--
-- > overlapping = intersecting
--
overlapping :: Ord a => Interval a -> Interval a -> Bool
overlapping = intersecting

-- | Implemented as a least-upper-bound, or hull, of Intervals.
instance Ord a => Semigroup (Interval a) where
  (<>) = lub
    where
      lub Empty b = b
      lub a Empty = a
      lub a b = Interval lower upper
        where
          lower =
            case compare (end (lo a)) (end (lo b)) of
              EQ | isInclusive (lo a) 
                 -> lo a
              LT -> lo a
              _  -> lo b
          
          upper =
            case compare (end (hi a)) (end (hi b)) of
              EQ | isInclusive (hi a) 
                 -> hi a
              GT -> lo a
              _  -> lo b


-- >>> "[0,1]" + "[2,3]" :: Interval Int
-- [-1,2]
--
-- prop>  \(a,b :: Interval Int) -> a + b == b + a
-- *** Failed! Falsified (after 3 tests):
-- ((-1,1],[0,2))
-- 
instance (Num a, Ord a) => Num (Interval a) where
  (+) = intersection

  Empty * _ = Empty
  _ * Empty = Empty
  l * r     = Interval (lo l * lo r) (hi l * hi r)
  
  (-) = difference

  negate Empty = Empty
  negate (Interval a b) = Interval (negate a) (negate b)

  abs Empty = Empty
  abs (Interval a b) = Interval (abs a) (abs b)

  signum Empty = 0
  signum i = Interval (excluding 0) (including (width i))

  fromInteger (fromInteger -> i) = Interval (Excluding 0) (Including i)

interval :: Ord a => Extent a -> Extent a -> Interval a
interval a b  =
  case compare (end a) (end b) of
    EQ | isExclusive a -> Interval a a
       | isExclusive b -> Interval b b
    LT -> Interval a b
    _  -> Interval b a

instance Bounded Float where
  minBound = (-1)/0
  maxBound = 1/0

instance Bounded Double where
  minBound = (-1)/0
  maxBound = 1/0


-}

