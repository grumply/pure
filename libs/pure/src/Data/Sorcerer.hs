{-# language PatternSynonyms, RankNTypes, BlockArguments, KindSignatures, DataKinds, TypeApplications, ScopedTypeVariables, FlexibleContexts, AllowAmbiguousTypes, ScopedTypeVariables #-}
module Data.Sorcerer 
  ( sorcerer
  , read
  , read'
  , unsafeRead
  , write
  , writeMany
  , transact
  , observe
  , events
  , Streamable(..)
  , Aggregable(..)
  , pattern Update
  , pattern Delete
  , pattern Ignore
  , pattern Added
  , pattern Updated
  , pattern Deleted
  , pattern Ignored
  , Hashable(..) -- commonly used for Streamable
  ) where

import Data.Sorcerer.Aggregable
import qualified Data.Sorcerer.Aggregate as Ag
import Data.Sorcerer.Aggregator
import Data.Sorcerer.Dispatcher
import Data.Sorcerer.JSON
import Data.Sorcerer.Manager
import Data.Sorcerer.Streamable

import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.Map.Strict as Map
import Data.Hashable

import Control.Concurrent
import Control.Monad.IO.Class
import Data.IORef
import Data.Typeable
import System.Directory
import System.FilePath
import System.IO.Unsafe
import Unsafe.Coerce

import Prelude hiding (read)

-- sorcerer @SomeEvent @'[SomeAggregate1,SomeAggregate2]
-- It is safe to call sorcerer multiple times, but the sets of aggregates need be distinct.
sorcerer :: forall (ev :: *) (ags :: [*]). (Typeable ev, ToJSON ev, Ord (Stream ev), BuildAggregators ev ags) => IO ()
sorcerer = atomicModifyIORef' builders $ \bs ->
  let
    ty = typeOf (undefined :: ev)
    insert Nothing = Just (buildAggregators @ev @ags)
    insert (Just existing) = Just $ \s tid -> do
      xs <- existing s tid 
      ys <- buildAggregators @ev @ags s tid
      pure (xs ++ ys)
  in
    (unsafeCoerce (Map.alter insert ty (unsafeCoerce bs)),())

-- Get the current value of an aggregate. If there are awaiting writes, the read
-- will be queued after them. 
{-# INLINE read #-}
read :: forall ev ag. (Typeable ev, Typeable ag, ToJSON ev, FromJSON ev, Streamable ev, Ord (Stream ev), Aggregable ev ag) => Stream ev -> IO (Maybe ag)
read s = liftIO do
  mv <- newEmptyMVar
  dispatchWith (startManagerWithBuilders @ev) s (Read (\_ -> putMVar mv))
  takeMVar mv

{-# INLINE read' #-}
read' :: forall ev ag. (Typeable ev, Typeable ag, ToJSON ev, FromJSON ev, Streamable ev, Ord (Stream ev), Aggregable ev ag) => Stream ev -> IO (Int,Maybe ag)
read' s = liftIO do
  mv <- newEmptyMVar
  dispatchWith (startManagerWithBuilders @ev) s (Read (curry (putMVar mv)))
  takeMVar mv

{-# INLINE unsafeRead #-}
-- Get the last-written value of an aggregate. May not be up-to-date.
unsafeRead :: forall ev ag. (FromJSON ag, Streamable ev, Aggregable ev ag) => Stream ev -> IO (Maybe (Int,Maybe ag))
unsafeRead s = do
  let fp = dropExtension (stream s) </> aggregate @ev @ag
  mag <- Ag.readAggregate fp
  pure $
    case mag of
      Just (Ag.Aggregate i mag) -> Just (i,mag)
      _ -> Nothing

-- Write an event to an event stream.
{-# INLINE write #-}
write :: forall ev. (Typeable ev, Streamable ev, ToJSON ev, FromJSON ev, Ord (Stream ev)) => Stream ev -> ev -> IO ()
write s ev = dispatchWith (startManagerWithBuilders @ev) s (Write ev)

{-# INLINE writeMany #-}
writeMany :: forall ev. (Typeable ev, Streamable ev, ToJSON ev, FromJSON ev, Ord (Stream ev)) => Stream ev -> [ev] -> IO ()
writeMany s evs = dispatchManyWith (startManagerWithBuilders @ev) s (fmap Write evs)

-- Transactional version of write s ev >> read' s.
{-# INLINE transact #-}
transact :: forall ev ag. (Typeable ag, Typeable ev, Streamable ev, Aggregable ev ag, ToJSON ev, FromJSON ev, Ord (Stream ev)) => Stream ev -> ev -> IO (Maybe (Maybe ag))
transact s ev = do
  mv <- newEmptyMVar
  dispatchWith (startManagerWithBuilders @ev) s (Transact ev (\_ -> putMVar mv))
  takeMVar mv

-- Transactional version of read' s >>= \ag -> write s ev >> read' s >>= \ag' -> pure (ag,ag').
{-# INLINE observe #-}
observe :: forall ev ag. (Typeable ag, Typeable ev, Streamable ev, Aggregable ev ag, ToJSON ev, FromJSON ev, Ord (Stream ev)) => Stream ev -> ev -> IO (Maybe ag,Maybe (Maybe ag))
observe s ev = do
  mv <- newEmptyMVar
  dispatchWith (startManagerWithBuilders @ev) s (Transact ev (\before after -> putMVar mv (before,after)))
  takeMVar mv

pattern Added :: a -> (Maybe a,Maybe (Maybe a))
pattern Added a <- (Nothing,Just (Just a))

pattern Updated :: a -> a -> (Maybe a,Maybe (Maybe a))
pattern Updated a a' <- (Just a,Just (Just a'))

pattern Deleted :: a -> (Maybe a,Maybe (Maybe a))
pattern Deleted a <- (Just a,Just Nothing)

pattern Ignored :: (Maybe a,Maybe (Maybe a)) 
pattern Ignored <- (_,Nothing)
