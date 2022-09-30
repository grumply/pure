{-# language DeriveAnyClass, BlockArguments, BangPatterns, NamedFieldPuns, ScopedTypeVariables, RecordWildCards, ViewPatterns, OverloadedStrings #-}
module Data.Bloom
  ( Bloom(..), 
    new, 
    add, 
    test, 
    update,
    size, 
    encode,
    decode, 
    union, 
    intersection,
    approximateSize,
    maximumSize
  ) where

import Data.Txt as Txt (Txt,ToTxt(..),foldl')
import Data.JSON hiding ((!),encode,decode)

import Control.Monad.IO.Class
import Data.Array.MArray
import Data.Array.IO
import Data.Array.Unboxed
import Data.Array.Unsafe

import Control.Concurrent.MVar
import Control.Monad (foldM,unless,void,when)
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Function
import Data.IORef
import Data.List as List (foldl')
import Data.Word

data Bloom = Bloom 
  { epsilon :: {-# UNPACK #-}!Double
  , hashes  :: {-# UNPACK #-}!Int 
  , buckets :: {-# UNPACK #-}!Int 
  , count   :: {-# UNPACK #-}!(IORef Int)
  , bits    :: {-# UNPACK #-}!(IOUArray Int Bool)
  }

{-# INLINE encode #-}
encode :: MonadIO m => Bloom -> m Value
encode Bloom { epsilon, hashes, buckets, count, bits } = liftIO do
  c <- readIORef count
  uarr :: UArray Int Bool <- unsafeFreeze bits 
  let 
    ints =
      flip fmap [0 .. (buckets `div` 32) - 1] $ \int ->
        -- writing only 32-bit Ints here
        let b = int * 32
            set :: Int -> Int -> Int
            set i a
              | uarr ! (b + i) = setBit a i
              | otherwise      = a
        in List.foldl' (flip set) 0 [0..31]
  pure $!
    object
      [ "epsilon" .= epsilon
      , "hashes"  .= hashes 
      , "buckets" .= buckets
      , "count"   .= c
      , "ints"    .= ints 
      ]

-- >>> let k = "ABCD" :: String
-- >>> b <- new 0.01 10000 
-- >>> add k b
-- >>> Just b' <- encode b >>= decode
-- >>> test k b'
-- True
{-# INLINE decode #-}
decode :: MonadIO m => Value -> m (Maybe Bloom)
decode v
  | Just (!epsilon,!hashes,!buckets,!c,ints) <- fields = liftIO do 
    count <- newIORef c
    bits <- newArray (0,buckets) False
    for_ (zip [0..] ints) $ \(off,int) -> do
      for_ [0..31] $ \b ->
        when (testBit int b) $
          writeArray bits (off * 32 + b) True
    pure (Just Bloom {..})
  | otherwise = 
    pure Nothing
  where
    fields :: Maybe (Double,Int,Int,Int,[Int])
    fields = parse v $ withObject "Bloom" $ \o -> do
      epsilon <- o .: "epsilon"
      hashes  <- o .: "hashes"
      buckets <- o .: "buckets"
      count   <- o .: "count"
      ints    <- o .: "ints" 
      pure (epsilon,hashes,buckets,count,ints)

-- | Produce a Bloom filter from a desired false positive rate (0.01 => 1%) and
-- an upper bound of entries.
--
-- >>> Bloom 7 2075680 _ <- new 0.01 216553
{-# INLINE new #-}
new :: MonadIO m => Double -> Int -> m Bloom
new epsilon (fromIntegral -> bs) = liftIO do
  let clusters, leftovers :: Int
      (clusters,leftovers) = ceiling (negate bs * ln epsilon / ln2 ^ (2 :: Int)) `quotRem` 32

      buckets :: Int
      buckets = 32 * (clusters + signum leftovers)

      hashes :: Double
      hashes = fromIntegral buckets / bs * ln2

  count <- newIORef 0
  bits <- newArray (0,buckets) False
  pure (Bloom epsilon (ceiling hashes) buckets count bits)
  where
    ln = logBase (exp 1)
    ln2 = ln 2

-- | A simple Txt-based hash using FNV-1a and the Kirsch-Mitzenmacher 
-- optimization to compute the desired number of hashes. Returns a number
-- of hashes necessary to maintain the desired false positive rate, assuming
-- the filter is non-full.
--
-- >>> b@(Bloom hs bs _) <- new 0.9 5
-- >>> (hs,bs,hash "abcd" b,hash "bcde" b)
-- (5,32,[0,29,26,23,20],[0,5,10,15,20])
{-# INLINE hash #-}
hash :: Bloom -> Txt -> [Int]
hash (Bloom _ hashes buckets _ _) val =
  let
    -- We want the list of hashes to be lazy, but
    -- we know that at least 1 will always be materialized,
    -- so go ahead and make these constants strict.
    !h = fnv64 val
    !hi = fromIntegral (shiftR h 32)
    !lo = fromIntegral (h .&. 0x00000000FFFFFFFF)
  in
    fmap (\i -> (hi + lo * i) `mod` buckets) [1..hashes]

{-# INLINE add #-}
add :: (MonadIO m, ToTxt a) => Bloom -> a -> m ()
add bloom v = void (update bloom v)

{-# INLINE update #-}
-- returns True if the filter was updated, false if the value already existed
update :: (MonadIO m, ToTxt a) => Bloom -> a -> m Bool
update bloom@Bloom { count, bits } (toTxt -> val) = liftIO do
  b <- and <$> sequence (fmap (readArray bits) (hash bloom val))
  unless b do
    atomicModifyIORef' count $ \c -> 
      let !c' = c + 1 
      in (c',())
    for_ (hash bloom val) $ \bucket -> 
      writeArray bits bucket True
  pure (not b)

{-|

Test if a Txt value is in the filter.

-- >>> let good = toTxt ("ABCD" :: String)
-- >>> let bad  = toTxt ("BCDE" :: String)
-- >>> new 0.01 10 >>= \b -> add good b >> (,) <$> test good b <*> test bad b
-- (True,False)

-- >>> let good = toTxt ("ABCD" :: String)
-- >>> let bad  = toTxt ("BCDE" :: String)
-- >>> new 0.0 2 >>= \b -> add good b >> (,) <$> test good b <*> test bad b
-- (True,True)
-}
{-# INLINE test #-}
test :: (MonadIO m, ToTxt a) => Bloom -> a -> m Bool
test bloom@(Bloom _ _ _ _ bits) (toTxt -> val) = liftIO do
  and <$> sequence (fmap (readArray bits) (hash bloom val))

-- >>> let ks = [ [x,y] | let abcs = ['a'..'z'], x <- abcs, y <- abcs ]
-- >>> b <- new 0.01 2000
-- >>> for_ ks (`add` b)
-- >>> s <- size b
-- >>> (length ks,s)
-- (676,678)
{-# INLINE approximateSize #-}
approximateSize :: MonadIO m => Bloom -> m Int
approximateSize Bloom { hashes, buckets, bits } = liftIO do
  let 
    count :: Int -> Int -> IO Int
    count !c bucket = do
      b <- readArray bits bucket
      pure (c + fromEnum b)

  c <- foldM count (0 :: Int) [0..buckets - 1]

  let
    bs :: Double
    bs = fromIntegral buckets

    hs :: Double
    hs = fromIntegral hashes

    n :: Double
    n = negate bs / hs * logBase (exp 1) (1 - (fromIntegral c / bs))

  pure $! round n

{-# INLINE size #-}
size :: MonadIO m => Bloom -> m Int
size Bloom { count } = liftIO do
  readIORef count

-- >>> l <- new 0.01 200
-- >>> r <- new 0.01 200
-- >>> let k1 = "abc" :: String
-- >>> let k2 = "bcd" :: String
-- >>> add k1 l
-- >>> add k2 r
-- >>> Just b <- union l r
-- >>> t1 <- test k1 b
-- >>> t2 <- test k2 b
-- >>> (t1,t2)
-- (True,True)
{-# INLINE union #-}
union :: MonadIO m => Bloom -> Bloom -> m (Maybe Bloom)
union (Bloom el hsl bsl _ bitsl) (Bloom er hsr bsr _ bitsr)
  | el == er && hsl == hsr && bsl == bsr = liftIO do 
    l :: UArray Int Bool <- unsafeFreeze bitsl
    r :: UArray Int Bool <- unsafeFreeze bitsr
    bits <- newListArray (0,bsl) (zipWith (||) (elems l) (elems r))
    c <- newIORef 0
    let b = Bloom el hsl bsl c bits
    count <- approximateSize b
    writeIORef c count
    pure (Just b)
  | otherwise = 
    pure Nothing

-- >>> l <- new 0.01 200
-- >>> r <- new 0.01 200
-- >>> let k1 = "abc" :: String
-- >>> let k2 = "bcd" :: String
-- >>> add k1 l
-- >>> add k1 r
-- >>> add k2 r
-- >>> Just b <- intersection l r
-- >>> t1 <- test k1 b
-- >>> t2 <- test k2 b
-- >>> (t1,t2)
-- (True,False)
{-# INLINE intersection #-}
intersection :: MonadIO m => Bloom -> Bloom -> m (Maybe Bloom)
intersection (Bloom el hsl bsl _ bitsl) (Bloom er hsr bsr _ bitsr)
  | el == er && hsl == hsr && bsl == bsr = liftIO do 
    l :: UArray Int Bool <- unsafeFreeze bitsl
    r :: UArray Int Bool <- unsafeFreeze bitsr
    bits <- newListArray (0,bsl) (zipWith (&&) (elems l) (elems r))
    c <- newIORef 0
    let b = Bloom el hsl bsl c bits
    count <- approximateSize b
    writeIORef c count
    pure (Just b)
  | otherwise = 
    pure Nothing

maximumSize :: Bloom -> Int
maximumSize Bloom { hashes = fromIntegral -> k, buckets = fromIntegral -> m, epsilon = p } =
  ceiling(m / (negate k / log(1 - exp(log(p) / k))))

-- FNV-1a 
{-# INLINE fnv64 #-}
fnv64 :: Txt -> Word64
fnv64 = Txt.foldl' h 0xcbf29ce484222325
  where
    {-# INLINE h #-}
    h :: Word64 -> Char -> Word64
    h !i c = 
      let i' = i `xor` fromIntegral (ord c) 
      in i' * 0x100000001b3


