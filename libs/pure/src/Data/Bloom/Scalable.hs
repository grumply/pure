{-# language BlockArguments, BangPatterns, RecordWildCards, ViewPatterns, NamedFieldPuns, OverloadedStrings #-}
module Data.Bloom.Scalable 
  ( Bloom(..), 
    fromBloom,
    fromBloomWith,
    bloom,
    new, 
    newWith,
    add, 
    test, 
    update,
    size, 
    encode,
    decode,
  ) where

import qualified Data.Bloom as Bloom

import Data.Txt as Txt (Txt,ToTxt(..),foldl',fnv64)
import Data.JSON hiding ((!),encode,decode)

import Control.Monad.IO.Class
import Data.Array.MArray
import Data.Array.IO
import Data.Array.Unboxed
import Data.Array.Unsafe

import Control.Concurrent.MVar
import Control.Monad (foldM,unless,when,join,void)
import Data.Bits
import Data.Char
import Data.Foldable
import Data.Function
import Data.IORef
import Data.List as List
import Data.Maybe
import Data.Traversable
import Data.Word

data Bloom = Bloom
  { epsilon :: {-# UNPACK #-}!Double
  , hashes  :: {-# UNPACK #-}!Int 
  , buckets :: {-# UNPACK #-}!Int 
  , scale   :: {-# UNPACK #-}!Double
  , tighten :: {-# UNPACK #-}!Double
  , quota   :: {-# UNPACK #-}!(IORef (Int,Int))
  , blooms  :: {-# UNPACK #-}!(IORef [Bloom.Bloom])
  }

{-# INLINE fromBloom #-}
fromBloom :: MonadIO m => Bloom.Bloom -> m Bloom
fromBloom b = 
  let e = exp 1
  in fromBloomWith b e (1 - 1/e)

{-# INLINE fromBloomWith #-}
fromBloomWith :: MonadIO m => Bloom.Bloom -> Double -> Double -> m Bloom
fromBloomWith b@Bloom.Bloom {..} scale tighten = liftIO do
  sz     <- Bloom.size b
  let mx = Bloom.maximumSize b
  quota  <- newIORef (mx,sz)
  blooms <- newIORef [b]
  pure Bloom {..}

{-# INLINE encode #-}
encode :: MonadIO m => Bloom -> m Value
encode Bloom {..} = liftIO do
  (s,c) <- readIORef quota
  bs <- readIORef blooms >>= traverse Bloom.encode
  pure $
    object
      [ "epsilon"  .= epsilon
      , "hashes"   .= hashes 
      , "buckets"  .= buckets
      , "scale"    .= scale
      , "tighten"  .= tighten
      , "quota"    .= (s,c)
      , "blooms"   .= bs 
      ]

{-# INLINE decode #-}
decode :: MonadIO m => Value -> m (Maybe Bloom)
decode v
  | Just (epsilon,hashes,buckets,scale,tighten,q,bs) <- fields = liftIO do 
    traverse Bloom.decode bs >>= \blooms ->
      if all isJust blooms then do
        quota  <- newIORef q
        blooms <- newIORef (catMaybes blooms)
        pure (Just Bloom {..})
      else
        pure Nothing
  | otherwise = 
    pure Nothing
  where
    fields :: Maybe (Double,Int,Int,Double,Double,(Int,Int),[Value])
    fields = parse v $ withObject "Bloom" $ \o -> do
      epsilon <- o .: "epsilon"
      hashes  <- o .: "hashes"
      buckets <- o .: "buckets"
      scale   <- o .: "scale" 
      tighten <- o .: "tighten"
      quota   <- o .: "quota"
      blooms  <- o .: "blooms"
      pure (epsilon,hashes,buckets,scale,tighten,quota,blooms)

{-# INLINE bloom #-}
bloom :: Double -> IO Bloom
bloom epsilon = new epsilon (max 100 (round (10 * (1 / epsilon))))

{-# INLINE new #-}
new :: Double -> Int -> IO Bloom
new epsilon size = 
  let e = exp 1
  in newWith epsilon size e (1 - (1 / e))

{-# INLINE newWith #-}
-- A smaller scaling factor increases the number of filters and lookups,
-- but keeps the false positive rate lower. A reasonable default is e.
-- A smaller tighten factor increases the size of each new filter. A
-- reasonable default is (1 - (1/e))
newWith :: MonadIO m => Double -> Int -> Double -> Double -> m Bloom
newWith epsilon size scale tighten = liftIO do
  b@(Bloom.Bloom _ hashes buckets _ _) <- Bloom.new epsilon size
  quota <- newIORef (size,0)
  blooms <- newIORef [b]
  pure Bloom {..}

{-# INLINE add #-}
add :: (MonadIO m, ToTxt a) => Bloom -> a -> m ()
add bs a = void (update bs a)

{-# INLINE update #-}
update :: (MonadIO m, ToTxt a) => Bloom -> a -> m Bool
update bs@Bloom {..} val = liftIO do
  b <- test bs val
  unless b do
    bs <- readIORef blooms
    let Bloom.Bloom e _ _ _ _ = head bs
    added <- Bloom.update (head bs) val
    let 
      grow n = do
        b <- Bloom.new (tighten / scale * e) n
        atomicModifyIORef' blooms $ \bs -> (b:bs,())

    when added do
      join $ atomicModifyIORef' quota $ \(total,current) ->
        let current' = current + 1 in
        if current' == total then
          let total' = round (fromIntegral total * scale)
          in ((total',current'),grow total')
        else
          ((total,current'),pure ())
   
  pure (not b)

{-# INLINE test #-}
test :: (MonadIO m, ToTxt a) => Bloom -> a -> m Bool
test Bloom {..} (toTxt -> val) = liftIO do
  bs <- readIORef blooms
  or <$> traverse go bs
  where
    go :: Bloom.Bloom -> IO Bool
    go b@Bloom.Bloom { hashes, buckets, bits } = 
      and <$> traverse (readArray bits) (hash b val)

{-# INLINE size #-}
size :: MonadIO m => Bloom -> m Int
size Bloom { quota } = liftIO do
  snd <$> readIORef quota

{-# INLINE hash #-}
hash :: Bloom.Bloom -> Txt -> [Int]
hash (Bloom.Bloom _ hashes buckets _ _) val =
  let
    -- We want the list of hashes to be lazy, but
    -- we know that at least 1 will always be materialized,
    -- so go ahead and make these constants strict.
    !h = fnv64 val
    !hi = fromIntegral (shiftR h 32)
    !lo = fromIntegral (h .&. 0x00000000FFFFFFFF)
  in
    fmap (\i -> (hi + lo * i) `mod` buckets) [1..hashes]



