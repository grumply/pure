{-# LANGUAGE ViewPatterns, BangPatterns, ScopedTypeVariables, MagicHash, UnboxedTuples #-}
module Data.Random.Generator (module Data.Random.Generator, Seed, newSeed, initialSeed) where

import Data.Random.Internal

import Control.Applicative
import Control.Monad
import Control.Monad.Zip
import Control.Arrow ((&&&))
import Data.Bits ((.|.),(.&.),xor)
import Data.List
import Data.Int
import Data.Word

-- Inspired by Max Goldstein's elm-random-pcg library: https://github.com/mgold/elm-random-pcg
-- On GHC and 64-bit word size, Data.Random uses the 64-bit RXS M XS pcg variant.
-- On GHCJS or GHC and 32-bit word size, Data.Random uses the 32-bit RXS M XS pcg variant.
--
-- For Doubles, the code produced by GHCJS is quite a bit slower than the browser-based Math.random().
-- For non-Doubles, especially Ints, the code produced by GHCJS is competetive with or better than the 
-- browser-based Math.random(). 
--
-- For best performance, use the primitive Generators (int,intR,double,doublrR,bool,oneIn)
-- along with (list,sample,sampleVector) rather than the System.Random.Random typeclass methods.

newtype Generator a = Generator { generate :: Seed -> (Seed,a) }

instance Functor Generator where
    {-# INLINE fmap #-}
    fmap f (Generator g) = Generator (fmap (\(seed,a) -> let !b = f a in (seed,b)) g)

instance Applicative Generator where
    {-# INLINE pure #-}
    pure a = Generator (\seed -> (seed,a))
    {-# INLINE (<*>) #-}
    (<*>) (Generator gfs) (Generator gas) = Generator $ \seed0 ->
        let
            (!seed1,!f) = gfs seed0
            (!seed2,!a) = gas seed1
            !b = f a
        in
            (seed2,b)

instance Monad Generator where
    {-# INLINE return #-}
    return = pure
    {-# INLINE (>>=) #-}
    ga >>= agb = Generator $ \seed -> 
        let (!seed',!a) = generate ga seed
            (!seed'',!b) = generate (agb a) seed'
        in (seed'',b)

instance Bounded a => Bounded (Generator a) where
  minBound = pure minBound
  maxBound = pure maxBound

instance Eq a => Eq (Generator a) where
  (==) a b = e1 && e2
    where
      -- extremely unlikely that this fails
      e1 = snd (generate a (initialSeed 1000)) == snd (generate b (initialSeed 1000))
      e2 = snd (generate a (initialSeed 1001)) == snd (generate b (initialSeed 1001))

instance Ord a => Ord (Generator a) where
  compare a b = compare a1 a2
    where
      -- arbitrary; required for Num
      a1 = snd (generate a (initialSeed 1000))
      a2 = snd (generate b (initialSeed 1000))

instance Num a => Num (Generator a) where
  (+) = liftA2 (+)
  (-) = liftA2 (-)
  (*) = liftA2 (*)
  negate = liftA negate
  abs = liftA abs
  signum = liftA signum
  fromInteger = pure . fromInteger

instance Enum a => Enum (Generator a) where
  toEnum = pure . toEnum
  fromEnum _ = 0

instance MonadZip Generator where
  mzip = liftA2 (,)

instance Integral a => Integral (Generator a) where
  quot = liftA2 quot
  rem = liftA2 rem
  div = liftA2 div
  mod = liftA2 mod
  quotRem a b = munzip (liftA2 quotRem a b)
  divMod a b = munzip (liftA2 divMod a b)
  toInteger _ = 0

instance Real a => Real (Generator a) where
  toRational _ = 0

instance Fractional a => Fractional (Generator a) where
  (/) = liftA2 (/)
  recip = liftA recip
  fromRational = pure . fromRational

instance Floating a => Floating (Generator a) where
  pi = pure pi
  exp = liftA exp
  log = liftA log
  sqrt = liftA sqrt
  (**) = liftA2 (**)
  logBase = liftA2 logBase
  sin = liftA sin
  cos = liftA cos
  tan = liftA tan
  asin = liftA asin
  acos = liftA acos
  atan = liftA atan
  sinh = liftA sinh
  cosh = liftA cosh
  tanh = liftA tanh
  asinh = liftA asinh
  acosh = liftA acosh
  atanh = liftA atanh

{-# INLINE int #-}
int :: Generator Int
int = Generator (pcg_next &&& pcg_peel)

{-# INLINE advance #-}
advance :: Int -> Seed -> Seed
advance = flip pcg_advance

{-# INLINE retract #-}
-- retract n . advance n == id
retract :: Int -> Seed -> Seed
retract steps = flip pcg_advance (negate steps)

{-# INLINE word32 #-}
word32 :: Generator Word32
word32 = pure fromIntegral <*> int
