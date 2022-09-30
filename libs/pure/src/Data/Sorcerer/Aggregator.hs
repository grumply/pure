{-# language RankNTypes, LambdaCase, BangPatterns, RecordWildCards, TypeApplications, ScopedTypeVariables, DataKinds, KindSignatures, TypeOperators, MultiParamTypeClasses, FlexibleInstances, GADTs, AllowAmbiguousTypes #-}
module Data.Sorcerer.Aggregator
  ( Aggregator(..)
  , toAggregator
  , persist
  , integrate
  , BuildAggregators(..)
  ) where

import Data.Sorcerer.Aggregable as Aggregable
import Data.Sorcerer.Aggregate as Aggregate
import Data.Sorcerer.JSON
import Data.Sorcerer.Streamable as Streamable

import Control.Concurrent
import Control.Monad
import Data.Foldable (for_)
import Data.Maybe
import Data.Typeable
import Unsafe.Coerce

import System.Directory
import System.FilePath

class BuildAggregators (ev :: *) (ags :: [*]) where
  buildAggregators :: Stream ev -> TransactionId -> IO [Aggregator ev]

instance BuildAggregators ev '[] where
  buildAggregators _ _ = pure []

instance (Typeable ev, Typeable x, ToJSON ev, FromJSON x, ToJSON x, FromJSON ev, Streamable ev, Aggregable ev x, BuildAggregators ev xs) => BuildAggregators ev (x : xs) where
  buildAggregators s tid = do
    ag <- toAggregator @ev @x s tid
    ags <- buildAggregators @ev @xs s tid
    pure (ag : ags)

data Aggregator ev = forall ag. (Typeable ag, ToJSON ag, Aggregable ev ag) => Aggregator
  { path         :: FilePath
  , maggregate   :: Maybe ag
  , latest       :: !TransactionId
  , shouldWrite  :: !Bool
  , shouldCommit :: !Bool
  }

-- This and integrate are a little messy and could definitely use some type management.
-- There are constraints that can be better propagated and unsafe coercions that should
-- be unnecessary.
toAggregator :: forall ev ag. (Typeable ev, Typeable ag, ToJSON ev, FromJSON ag, ToJSON ag, FromJSON ev, Streamable ev, Aggregable ev ag) => Stream ev -> TransactionId -> IO (Aggregator ev)
toAggregator s latest = do
  let path = dropExtension (Streamable.stream s) <> "/" <> Aggregable.aggregate @ev @ag
  readAggregate path >>= \case
    Nothing
      | latest == 0 -> pure (Aggregator path (Nothing :: Maybe ag) 0 False False)

      | otherwise -> do
        evs0 :: [ev] <- events s
        let
          mkAggregatorEvent tid ev = (tid,Write ev)
          evs = zipWith mkAggregatorEvent [1 ..] evs0
        foldM integrate (Aggregator path (Nothing :: Maybe ag) 0 True True) evs

    Just (Aggregate tid mag :: Aggregate ag)
      | tid == latest -> pure (Aggregator path mag latest False False)

      | otherwise -> do
        evs0 :: [ev] <- events s
        let
          mkAggregatorEvent tid ev = (tid,Write ev)
          evs = zipWith mkAggregatorEvent [tid + 1 ..] (Prelude.drop tid evs0)
        foldM integrate (Aggregator path mag tid True False) evs

integrate :: forall ev. (Typeable ev) => Aggregator ev -> (TransactionId,Event) -> IO (Aggregator ev)
integrate ag@Aggregator {..} (tid,e) =
  case e of
    Write ev | Just (e :: ev) <- cast ev -> do
      let !mmag = update e maggregate
          !maggregate' = fromMaybe maggregate mmag
      pure (Aggregator path maggregate' tid True (shouldWrite || isJust mmag))

    Read cb -> do
      for_ (cast cb) $ \f -> f maggregate :: IO ()
      pure ag

    Transact ev cb | Just (e :: ev) <- cast ev -> do
      let !mmag = update e maggregate
          !maggregate' = fromMaybe maggregate mmag
      for_ (cast cb) $ \f -> f maggregate mmag :: IO ()
      pure (Aggregator path maggregate' tid True (shouldWrite || isJust mmag))

persist :: Aggregator ev -> IO (Aggregator ev)
persist Aggregator {..} = do
  let
    fp = path
    temp = fp <> ".temp"
  createDirectoryIfMissing True (takeDirectory fp)
  if shouldWrite then do
    writeAggregate temp latest maggregate
    renameFile temp fp
  else if shouldCommit then
    commitTransaction fp latest
  else
    pure ()
  pure (Aggregator path maggregate latest False False)
 