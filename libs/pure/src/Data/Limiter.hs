{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Limiter (Limiter,Limiter_(..),Tokens,limiter,limit,minDelay,refund) where

import Data.Time
import Data.JSON

import Control.Concurrent
import GHC.Generics

import Control.Monad.IO.Class

-- Inspired by: https://medium.com/smyte/rate-limiter-df3408325846

-- In multi-threaded contexts, be sure that all rate-limited actions use the same number of tokens.
-- Or be sure that you understand when this could be a problem (starvation). Also understand
-- that relying heavily on minDelay can lead to high contention - if needed, consider adding jitter
-- with Data.Random

type Tokens = Int

type Limiter = MVar Limiter_
data Limiter_ = Limiter
  { bValue        :: {-# UNPACK #-}!Tokens
  , bMax          :: {-# UNPACK #-}!Tokens
  , bLastUpdate   :: {-# UNPACK #-}!Micros
  , bRefillTime   :: {-# UNPACK #-}!Micros
  , bRefillAmount :: {-# UNPACK #-}!Tokens
  } deriving (Eq,Generic,ToJSON,FromJSON)

refund :: MonadIO m => Int -> Limiter -> m ()
refund r l = liftIO $
  modifyMVar_ l $ \Limiter { bValue = old, .. } ->
    let bValue = old + r
    in pure Limiter {..}

limiter :: MonadIO m => Int -> Int -> Int -> Micros -> m Limiter
limiter bMax bValue bRefillAmount bRefillTime = liftIO $ do
  bLastUpdate <- micros
  newMVar Limiter {..}

{-# INLINE reduce #-}
reduce :: MonadIO m => Int -> Limiter -> m Bool
reduce tokens bucket = liftIO $ modifyMVar bucket $ \Limiter {..} -> do
  now  <- micros
  let last = round $ getMicros bLastUpdate
      val  = bValue
      refillTime :: Int
      refillTime = round $ getMicros bRefillTime
      refillCount = (round (getMicros now) - last) `div` refillTime
      value = min bMax (abs {- protect from overflow -} $ val + refillCount * bRefillAmount)
      lu    = min now (Micros $ fromIntegral $ last + refillCount * refillTime)
      limited = tokens > value
  return (Limiter { bValue = if not limited then value - tokens else value, bLastUpdate = lu, .. },not limited)

{-# INLINE limit #-}
limit :: MonadIO m => Int -> Limiter -> m a -> m (Maybe a)
limit tokens bucket f = do
  b <- Data.Limiter.reduce tokens bucket
  if b
    then Just <$> f
    else return Nothing

{-# INLINE minDelay #-}
minDelay :: MonadIO m => Int -> Limiter -> m Micros
minDelay tokens bucket = liftIO $ do
  Limiter {..} <- readMVar bucket
  if bValue > tokens
    then return 0
    else do
      now <- micros
      let lu = bLastUpdate
          needed = tokens - bValue
          rounds = needed `div` bRefillAmount
      return (bRefillTime * fromIntegral rounds - (now - lu))