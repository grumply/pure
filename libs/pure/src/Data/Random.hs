{-# LANGUAGE CPP, FlexibleContexts, TypeFamilies, ScopedTypeVariables, BangPatterns, MagicHash, UnboxedTuples, TypeApplications, ViewPatterns #-}
module Data.Random (module Data.Random, module Data.Random.Generator, System.Random.RandomGen(..), System.Random.Random(..)) where

import Control.Monad
import Control.Monad.ST
import Data.Bits
import Data.Foldable as F
import Data.Int
import qualified Data.List as List
import Data.Proxy
import Data.Traversable
import Data.Word

import Data.Random.Generator
import Data.Random.Internal

import Control.Monad.Primitive

import qualified Data.Vector as V
import qualified Data.Vector.Mutable as MV
import qualified Data.Vector.Generic as G
import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Unboxed as U

import qualified System.Random

import GHC.Prim
import GHC.Word

#ifndef __GHCJS__
#import "MachDeps.h"
#endif

instance System.Random.RandomGen Seed where
    {-# INLINE genWord64 #-}
    genWord64 seed =
      let (!seed',!i) = generate (fromIntegral <$> int) seed
      in (i,seed')
    {-# INLINE next #-}
    next seed = 
        let (!seed',!i) = generate int seed
        in (i,seed')
    {-# INLINE split #-}
    split seed =
        let (!seed',!randSeed) = generate independentSeed seed
        in (randSeed,seed')

-- | Variate taken from Bryan O'Sullivan's mwc-random with changes made for
-- uniformR when WORD_SIZE_IN_BITS == 32, for performance. 
--
-- Some concessions were made to keep bounded generation non-conditional and efficient.
--
-- All bounded non-fractional generators produce in the range (lo,hi].
-- All bounded fractional generators produce in the range
-- Unbounded non-fractional generators produce in the range [minBound..maxBound].
-- Unbounded fractional generators produce in the range (0,1].
-- 
class Variate a where
    uniform :: Generator a
    uniformR :: a -> a -> Generator a

{-# RULES
    "uniformR x x" forall x. uniformR x x = pure x
#-}

instance Variate Int8 where
    -- | Produces in the range [minBound..maxBound]
    uniform = fromIntegral <$> int
    {-# INLINE uniform #-}
    -- | Produces in the range [lo + 1..hi]
    uniformR a b = fmap (a +) (uniformRange_internal (b - a))
    {-# INLINE uniformR #-}

instance Variate Int16 where
    -- | Produces in the range [minBound..maxBound]
    uniform = fromIntegral <$> int
    {-# INLINE uniform #-}
    -- | Produces in the range [lo + 1..hi]
    uniformR a b = fmap (a +) (uniformRange_internal (b - a))
    {-# INLINE uniformR #-}

instance Variate Int32 where
    -- | Produces in the range [minBound..maxBound]
    uniform = fromIntegral <$> int
    {-# INLINE uniform #-}
    -- | Produces in the range [lo + 1..hi]
    uniformR a b = fmap (a +) (uniformRange_internal (b - a))
    {-# INLINE uniformR #-}

instance Variate Int64 where
    -- | Produces in the range [minBound..maxBound]
#if (defined __GHCJS__ || WORD_SIZE_IN_BITS == 32)
    uniform = wordsTo64Bit <$> uniform <*> uniform
#else
    uniform = fmap fromIntegral int
#endif
    {-# INLINE uniform #-}
    -- | Produces in the range [lo + 1..hi]
    uniformR a b = fmap (a +) (uniformRange_internal (b - a))
    {-# INLINE uniformR #-}

instance Variate Int where
    -- | Produces in the range [minBound..maxBound]
    uniform = int
    {-# INLINE uniform #-}
    -- | Produces in the range [lo + 1..hi]
    uniformR a b = fmap (a +) (uniformRange_internal (b - a))
    {-# INLINE uniformR #-}

instance Variate Word8 where
    -- | Produces in the range [minBound..maxBound]
    uniform = fromIntegral <$> int 
    {-# INLINE uniform #-}
    -- | Produces in the range [lo + 1..hi]
    uniformR a b = fmap (a +) (uniformRange_internal (b - a))
    {-# INLINE uniformR #-}

instance Variate Word16 where
    -- | Produces in the range [minBound..maxBound]
    uniform = fromIntegral <$> int 
    {-# INLINE uniform #-}
    -- | Produces in the range [lo + 1..hi]
    uniformR a b = fmap (a +) (uniformRange_internal (b - a))
    {-# INLINE uniformR #-}

instance Variate Word32 where
    -- | Produces in the range [minBound..maxBound]
    uniform = fromIntegral <$> int 
    {-# INLINE uniform #-}
    -- | Produces in the range [lo + 1..hi]
    uniformR a b = fmap (a +) (uniformRange_internal (b - a))
    {-# INLINE uniformR #-}

instance Variate Word64 where
    -- | Produces in the range [minBound..maxBound]
#if (defined __GHCJS__ || WORD_SIZE_IN_BITS == 32)
    uniform = wordsTo64Bit <$> uniform <*> uniform
#else
    uniform = fromIntegral <$> int
#endif
    {-# INLINE uniform #-}
    -- | Produces in the range [lo + 1..hi]
    uniformR a b = fmap (a +) (uniformRange_internal (b - a))
    {-# INLINE uniformR #-}

instance Variate Word where
    -- | Produces in the range [minBound..maxBound]
    uniform = fromIntegral <$> int
    {-# INLINE uniform #-}
    -- | Produces in the range [lo + 1..hi]
    uniformR a b = fmap (a +) (uniformRange_internal (b - a))
    {-# INLINE uniformR #-}

instance Variate Bool where
    uniform = wordToBool <$> uniform
    {-# INLINE uniform #-}
    uniformR a b 
      | a < b = pure a
      | otherwise = uniform
    {-# INLINE uniformR #-}

instance Variate Float where
    -- | Produces in the range [2**(-33)..1]
    uniform = wordToFloat <$> uniform
    {-# INLINE uniform #-}
    -- | Produces in the range [lo..hi]
    uniformR a b = (\w -> a + (b - a) * wordToFloat w) <$> uniform
    {-# INLINE uniformR #-}

instance Variate Double where
#if ( defined __GHCJS__ || WORD_SIZE_IN_BITS == 32 )
    -- | Produces in the range [2**(-53)..1]
    uniform = wordsToDouble <$> uniform <*> uniform
    {-# INLINE uniform #-}
    -- | Produces in the range [lo..hi]
    uniformR a b = (\w1 w2 -> a + (b - a) * wordsToDouble w1 w2) <$> uniform <*> uniform
    {-# INLINE uniformR #-}
#else
    -- | Produces in the range [2**(-53)..1]
    uniform = word64ToDouble <$> uniform
    {-# INLINE uniform #-}
    -- | Produces in the range [lo..hi]
    uniformR a b = fmap (\w -> a + (b - a) * wordsToDouble (fromIntegral $ w `unsafeShiftR` 32) (fromIntegral w)) (uniform @Word64)
    {-# INLINE uniformR #-}
#endif

{-# INLINE boolean #-}
boolean :: Generator Bool
boolean = oneIn 2

{-# INLINE oneIn #-}
oneIn :: Int -> Generator Bool
oneIn n = (== 1) <$> uniformR 0 n

{-# INLINE sample #-}
sample :: Foldable f => f a -> Generator (Maybe a)
sample = sampleVector . V.fromList . F.toList

{-# INLINE sampleVector #-}
sampleVector :: G.Vector v a => v a -> Generator (Maybe a)
sampleVector v =
    let !l = G.length v
    in if l == 0 then pure Nothing else Generator (go l)
  where
    {-# INLINE go #-}
    go len seed =
        let (!seed',!l) = fmap (subtract 1) (generate (uniformR 0 len) seed)
            x = G.unsafeIndex v l
        in (seed',Just x)

{-# INLINE shuffle #-}
shuffle :: [a] -> Seed -> [a]
shuffle as = V.toList . shuffleVector (V.fromList as)

{-# INLINE shuffleVector #-}
shuffleVector :: G.Vector v a => v a -> Seed -> v a
shuffleVector v0 seed = G.modify (flip shuffleMVector seed) v0

{-# INLINE shuffleMVector #-}
shuffleMVector :: (PrimMonad m, GM.MVector v a) => v (PrimState m) a -> Seed -> m ()
shuffleMVector v = go (GM.length v)
  where
    {-# INLINE go #-}
    go !i !seed
      | i <= 1    = return ()
      | otherwise = do
        let destination = i - 1
            (seed',source) = fmap (subtract 1) (generate (uniformR 0 i) seed)
        GM.unsafeSwap v source destination
        go destination seed' 

{-# INLINE choose #-}
choose :: forall a. (Bounded a,Enum a) => Generator a
choose = toEnum <$> fmap (subtract 1) (uniformR 0 (succ (fromEnum (maxBound :: a))))

-- Based on Stephen Canon's insight: 
-- https://github.com/apple/swift/pull/39143
--
-- Generates in the range 0<..upper
--
-- This unconditional bounded range generator solves all previous performance
-- problems with bounded integral generation and makes all exported numeric 
-- generators (<= WORD_SIZE) approximately performance-equivalent. Lovely!
{-# INLINE uniformRange_internal #-}
uniformRange_internal :: Integral a => a -> Generator a
uniformRange_internal (fromIntegral -> W# upper) = do
    word1 :: Word <- uniform
    word2 :: Word <- uniform
    pure $ fromIntegral $ W#
      (case (# word1, word2 #) of
        (# W# w1, W# w2 #) -> 
          case timesWord2# upper w1 of
            (# result, fraction #) -> 
              case timesWord2# upper w2 of
                (# x, _ #) -> 
                  case plusWord2# fraction x of
                    (# _, carry #) -> 
                      plusWord# result (int2Word# (gtWord# carry (int2Word# 0#)))
      )

{-
uniformRange_internal :: forall a.  ( Integral a, Bounded a, Variate a) => a -> a -> Generator a
uniformRange_internal x1 x2 = Generator go
  where
    {-# INLINE go #-}
    go seed
        | n == 0    = generate uniform seed
        | otherwise = loop seed
        where
            (# i, j #) | x1 < x2   = (# x1, x2 #)
                       | otherwise = (# x2, x1 #)
            n = 1 + isub j i :: Word64
            buckets = maxBound `div` n
            maxN    = buckets * n
            {-# INLINE loop #-}
            loop seed = 
                let (!seed',!x) = generate uniform seed
                in if x < maxN 
                    then 
                        let !br = iadd i (fromIntegral (x `div` buckets))
                        in (seed',br)
                    else loop seed'
-}

{-# INLINE independentSeed #-}
independentSeed :: Generator Seed
independentSeed = Generator go 
  where
    {-# INLINE go #-}
    go seed0 = 
        let !gen = uniformR (-1) maxBound
            (!seed1,(!state,!b,!c)) = generate ((,,) <$> gen <*> gen <*> gen) seed0
            !incr = (b `xor` c) .|. 1
            !seed' = pcg_next (Seed state incr)
        in (seed',seed1)

{-# INLINE independent #-}
independent :: (Seed -> a) -> Generator a
independent f = do
  seed' <- independentSeed
  pure (f seed')

{-# INLINE list #-}
list :: Generator a -> Generator [a]
list gen = go <$> independentSeed
  where go seed = let (seed',r) = generate gen seed in r : go seed'

{-# INLINE vector #-}
vector :: forall v a. (G.Vector v a) => Int -> Generator a -> Generator (v a)
vector n g = 
    -- Since the state isn't recoverable from G.unfoldrN, 
    -- we use an independent seed.
    G.unfoldrN n go <$> independentSeed
  where
    {-# INLINE go #-}
    go :: Seed -> Maybe (a,Seed)
    go seed = 
        let (seed',a) = generate g seed
        in Just (a,seed')

-- structural `pred` for Double
-- `uniform` for Double produces in the range (0,1]. 
-- To produce a Double in the interval [0,1):
--
-- > sub <$> uniform
--
{-# INLINE sub #-}
sub :: Double -> Double
sub = subtract (2**(-53))

-- structural `succ` for Double
{-# INLINE sup #-}
sup :: Double -> Double
sup = (+ (2**(-53)))

-- structural `pred` for Float
{-# INLINE subFloat #-}
subFloat :: Float -> Float
subFloat = subtract (2**(-33))

-- structural `succ` for Float
{-# INLINE supFloat #-}
supFloat :: Float -> Float
supFloat = (+ (2**(-33)))


-- Distributions ported from Bryan O'Sullivan's mwc-random

{-# INLINE normal #-}
normal :: Double -> Double -> Generator Double
normal mean stdDev = (\x -> mean + stdDev * x) <$> standard

-- modified Doornik's ziggurat algorithm as implemented in mwc-random by Bryan O'Sullivan
{-# INLINE standard #-}
standard :: Generator Double
standard = loop
  where
    loop = do
      u  <- (subtract 1 . (*2)) `liftM` uniform
      ri <- uniform
      let i  = fromIntegral ((ri :: Word32) .&. 127)
          bi = U.unsafeIndex blocks i
          bj = U.unsafeIndex blocks (i+1)
      case () of
        _| abs u < U.unsafeIndex ratios i -> return $ u * bi
         | i == 0                         -> normalTail (u < 0)
         | otherwise                      -> do
             let x  = u * bi
                 xx = x * x
                 d  = exp (-0.5 * (bi * bi - xx))
                 e  = exp (-0.5 * (bj * bj - xx))
             c <- uniform
             if e + c * (d - e) < 1
               then return x
               else loop
    normalTail neg  = tailing
      where tailing  = do
              x <- ((/rNorm) . log) `liftM` uniform
              y <- log              `liftM` uniform
              if y * (-2) < x * x
                then tailing
                else return $! if neg then x - rNorm else rNorm - x

{-# INLINE exponential #-}
exponential :: Double -> Generator Double
exponential b = (\x -> - log x / b) <$> uniform

{-# INLINE truncatedExp #-}
truncatedExp :: Double -> (Double,Double) -> Generator Double
truncatedExp scale (a,b) = truncateExp <$> uniform
    where
        truncateExp p = a - log ( (1 - p) + p * exp (-scale * (b - a))) / scale

{-# INLINE gamma #-}
gamma :: Double -> Double -> Generator Double
gamma a b
  | a <= 0    = error "Data.Random.gamma: negative alpha parameter"
  | otherwise = mainloop
  where
    mainloop = do
      T x v <- innerloop
      u     <- uniform
      let cont =  u > 1 - 0.331 * sqr (sqr x)
              && log u > 0.5 * sqr x + a1 * (1 - v + log v)
      case () of
        _ | cont      -> mainloop
          | a >= 1    -> return $ a1 * v * b
          | otherwise -> do y <- uniform
                            return $ y ** (1 / a) * a1 * v * b
    innerloop = do
      x <- standard
      case 1 + a2*x of
        v | v <= 0    -> innerloop
          | otherwise -> return $ T x (v*v*v)

    a' = if a < 1 then a + 1 else a
    a1 = a' - 1/3
    a2 = 1 / sqrt(9 * a1)

{-# INLINE chiSquare #-}
chiSquare :: Int -> Generator Double
chiSquare n
  | n <= 0    = error "Data.Random.chiSquare: number of degrees of freedom must be positive"
  | otherwise = (\x -> 2 * x) <$> gamma (0.5 * fromIntegral n) 1 

{-# INLINE geometric0 #-}
geometric0 :: Double -> Generator Int
geometric0 p
  | p == 1          = pure 0
  | p >  0 && p < 1 = (\q -> floor $ log q / log (1 - p)) <$> uniform
  | otherwise       = error "Data.Random.geometric0: probability out of [0,1] range"

{-# INLINE geometric1 #-}
geometric1 :: Double -> Generator Int
geometric1 p = (+ 1) <$> geometric0 p

{-# INLINE beta #-}
beta :: Double -> Double -> Generator Double
beta a b = do
  x <- gamma a 1
  y <- gamma b 1
  return (x / (x+y))

{-# INLINE dirichlet #-}
dirichlet :: (Traversable t) => t Double -> Generator (t Double)
dirichlet t = do
  t' <- mapM (flip gamma 1) t
  let total = foldl' (+) 0 t'
  return $ fmap (/total) t'

{-# INLINE bernoulli #-}
bernoulli :: Double -> Generator Bool
bernoulli p = (< p) <$> uniform

{-# INLINE categorical #-}
categorical :: (G.Vector v Double) => v Double -> Generator Int
categorical v
    | G.null v = error "Data.Random.categorical: empty weights"
    | otherwise = do
        let cv  = G.scanl1' (+) v
        p <- (G.last cv *) `liftM` uniform
        return $ 
          case G.findIndex (>=p) cv of
            Just i  -> i
            Nothing -> error "Data.Random.categorical: bad weights"

{-# INLINE logCategorical #-}
logCategorical :: (G.Vector v Double) => v Double -> Generator Int
logCategorical v
  | G.null v  = error "Data.Random.logCategorical: empty weights"
  | otherwise = categorical (G.map (exp . subtract m) v)
  where
    m = G.maximum v

{-# INLINE sqr #-}
sqr :: Double -> Double
sqr x = x * x