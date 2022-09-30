{-# LANGUAGE MagicHash, BangPatterns, CPP, ViewPatterns, TypeFamilies, FlexibleContexts #-}
module Data.Random.Internal where

import Data.Bits
import GHC.Base
import Data.Word
import Data.Int
import qualified System.Random

import qualified Data.Vector.Unboxed as U

#ifndef __GHCJS__
#import "MachDeps.h"
{-# INLINE newSeed #-}
newSeed :: IO Seed
newSeed = do
   gen <- System.Random.newStdGen
   let (i,_) = System.Random.random gen
   return (initialSeed i)
#else
foreign import javascript unsafe 
  "$r = Math.floor(Math.random()*0xFFFFFFFF)" random_seed_js :: IO Int
newSeed :: IO Seed
newSeed = initialSeed <$> random_seed_js
#endif

data Seed = Seed {-# UNPACK #-}!Int {-# UNPACK #-}!Int 
  deriving (Eq,Show)

{-# INLINE halfInt32Count #-}
halfInt32Count :: Double
halfInt32Count = 0.5 * realToFrac (toInteger (maxBound :: Int32) - toInteger (minBound :: Int32) + 1)

{-# INLINE uncheckedIShiftRL #-}
uncheckedIShiftRL :: Int -> Int -> Int 
uncheckedIShiftRL (I# n) (I# i) = I# (uncheckedIShiftRL# n i)

{-# INLINE toWord #-}
toWord :: Int -> Word
toWord = fromIntegral

{-# INLINE pcg_advance_lcg #-}
pcg_advance_lcg :: Int -> Int -> Int -> Int -> Int
pcg_advance_lcg (toWord -> state) (toWord -> delta) (toWord -> cur_mult) (toWord -> cur_plus) = fromIntegral (go cur_mult cur_plus 1 0 delta)
  where
    go :: Word -> Word -> Word -> Word -> Word -> Word
    go !cur_mult !cur_plus !acc_mult !acc_plus !delta
      | delta > 0 =
        if (delta .&. 1 /= 0) 
            then go (cur_mult * cur_mult) ((cur_mult + 1) * cur_plus) (acc_mult * cur_mult) (acc_plus * cur_mult + cur_plus) (delta `div` 2)
            else go (cur_mult * cur_mult) ((cur_mult + 1) * cur_plus) acc_mult acc_plus (delta `div` 2)
      | otherwise = acc_mult * state + acc_plus

#if ( defined __GHCJS__ || WORD_SIZE_IN_BITS == 32 )

{-# INLINE pcg_advance #-}
pcg_advance :: Seed -> Int -> Seed
pcg_advance (Seed state incr) delta = Seed (pcg_advance_lcg state delta 1664525 incr) incr

{-# INLINE pcg_next #-}
pcg_next :: Seed -> Seed
-- for information about constants, see: https://en.wikipedia.org/wiki/Linear_congruential_generator
pcg_next (Seed state0 incr) = Seed (state0 * 1664525 + incr) incr

{-# INLINE pcg_peel #-}
-- pcg_output_rxs_m_xs_32_32
pcg_peel :: Seed -> Int
pcg_peel (Seed state _) =
    let
        !word = 
            xor state (uncheckedIShiftRL state (uncheckedIShiftRL state 28 + 4)) * 277803737

        !result = xor (uncheckedIShiftRL word 22) word

    in
        result

{-# INLINE initialSeed #-}
-- for information about constants, see: https://en.wikipedia.org/wiki/Linear_congruential_generator
initialSeed :: Int -> Seed
initialSeed x =
    let 
        (Seed state0 incr) = pcg_next (Seed 0 1013904223)

        !state1 = state0 + x

        !result = pcg_next (Seed state1 incr)

    in 
        result

#elif (WORD_SIZE_IN_BITS == 64)

{-# INLINE pcg_advance #-}
pcg_advance :: Seed -> Int -> Seed
pcg_advance (Seed state incr) delta = Seed (pcg_advance_lcg state delta 6364136223846793005 incr) incr

{-# INLINE pcg_next #-}
-- for information about constants, see: https://en.wikipedia.org/wiki/Linear_congruential_generator
pcg_next :: Seed -> Seed
pcg_next (Seed state0 incr) = Seed (state0 * 6364136223846793005 + incr) incr

{-# INLINE pcg_peel #-}
-- pcg_output_rxs_m_xs_64_64
pcg_peel :: Seed -> Int
pcg_peel (Seed state _) =
    let
        !word = 
            xor state (uncheckedIShiftRL state (uncheckedIShiftRL state 59 + 5)) * 12605985483714917081

        !result = xor (uncheckedIShiftRL word 43) word

    in
        result

{-# INLINE initialSeed #-}
-- for information about constants, see: https://en.wikipedia.org/wiki/Linear_congruential_generator
initialSeed :: Int -> Seed
initialSeed x =
    let 
        (Seed state0 incr) = pcg_next (Seed 0 1442695040888963407)

        !state1 = state0 + x

        !result = pcg_next (Seed state1 incr)

    in 
        result

#else

-- *****************************************************************************
#warning "No implementation for word sizes other than 32 or 64."
-- *****************************************************************************

#endif

-- The following utility methods come from mwc-random by Bryan O'Sullivan
wordToFloat :: Word32 -> Float
wordToFloat x      = (fromIntegral i * m_inv_32) + 0.5 + m_inv_33
    where m_inv_33 = 1.16415321826934814453125e-10
          m_inv_32 = 2.3283064365386962890625e-10
          i        = fromIntegral x :: Int32
{-# INLINE wordToFloat #-}

wordsToDouble :: Word32 -> Word32 -> Double
wordsToDouble x y  = (fromIntegral u * m_inv_32 + (0.5 + m_inv_53) +
                     fromIntegral (v .&. 0xFFFFF) * m_inv_52)
    where m_inv_52 = 2.220446049250313080847263336181640625e-16
          m_inv_53 = 1.1102230246251565404236316680908203125e-16
          m_inv_32 = 2.3283064365386962890625e-10
          u        = fromIntegral x :: Int32
          v        = fromIntegral y :: Int32
{-# INLINE wordsToDouble #-}

word64ToDouble :: Word64 -> Double
word64ToDouble x = (fromIntegral u * m_inv_32 + (0.5 + m_inv_53) +
                   fromIntegral (v .&. 0xFFFFF) * m_inv_52)
    where m_inv_52 = 2.220446049250313080847263336181640625e-16
          m_inv_53 = 1.1102230246251565404236316680908203125e-16
          m_inv_32 = 2.3283064365386962890625e-10
          u        = fromIntegral (shiftR x 32) :: Int32
          v        = fromIntegral x :: Int32
{-# INLINE word64ToDouble #-}

isub :: (Integral a, Integral b) => a -> a -> b
isub x y = fromIntegral x - fromIntegral y
{-# INLINE isub #-}

iadd :: (Integral a, Integral b) => a -> b -> a
iadd m x = m + fromIntegral x
{-# INLINE iadd #-}

wordsTo64Bit :: (Integral a) => Word32 -> Word32 -> a
wordsTo64Bit x y =
    fromIntegral ((fromIntegral x `shiftL` 32) .|. fromIntegral y :: Word64)
{-# INLINE wordsTo64Bit #-}

wordToBool :: Word32 -> Bool
wordToBool i = (i .&. 1) /= 0
{-# INLINE wordToBool #-}

data T = T {-# UNPACK #-}!Double {-# UNPACK #-}!Double 

{-# NOINLINE blocks #-}
blocks :: U.Vector Double
blocks = (`U.snoc` 0) . U.cons (v/f) . U.cons rNorm . U.unfoldrN 126 go $! T rNorm f
  where
    go (T b g) = let !u = T h (exp (-0.5 * h * h))
                     h  = sqrt (-2 * log (v / b + g))
                 in Just (h, u)
    v = 9.91256303526217e-3
    f = exp (-0.5 * rNorm * rNorm)

rNorm :: Double
rNorm = 3.442619855899

{-# NOINLINE ratios #-}
ratios :: U.Vector Double
ratios = U.zipWith (/) (U.tail blocks) blocks
