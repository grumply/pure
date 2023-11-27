{-# language LambdaCase, RankNTypes, ScopedTypeVariables, TypeApplications, FlexibleContexts, AllowAmbiguousTypes #-}
module Data.Sorcerer.Sorcery 
  ( listenStream, listenStreams
  , subscribeStream, subscribeStreams
  , unsubscribeStream, unsubscribeStreams
  , unlisten
  ) where

import Data.Sorcerer.Dispatcher
import Data.Sorcerer.Streamable

import Control.Concurrent
import Control.Monad
import Data.Exists
import Data.Foldable
import Data.Typeable
import Data.Unique
import Data.View hiding (Listener)
import Unsafe.Coerce

listenStream :: (Typeable ev, Ord (Stream ev)) => Stream ev -> (ev -> IO ()) -> IO (StreamListener ev)
listenStream s f = do
  u <- newUnique
  addStreamListener (Left u) s (traverse_ g)
  where
    g = \case
      Write ev -> f (unsafeCoerce ev)
      Transact ev _ -> f (unsafeCoerce ev)
      _ -> pure ()

listenStreams :: Typeable ev => (ev -> IO ()) -> IO (Listener ev)
listenStreams f = do
  u <- newUnique
  addListener (Left u) (traverse_ g)
  where
    g = \case
      Write ev -> f (unsafeCoerce ev)
      Transact ev _ -> f (unsafeCoerce ev)
      _ -> pure ()

subscribeStream :: (Typeable ev, Ord (Stream ev), Effect eff) => Stream ev -> (ev -> eff) -> IO (StreamListener ev)
subscribeStream s f = do
  mtid <- myThreadId
  addStreamListener (Right mtid) s (traverse_ g)
  where
    g = \case
      Write ev -> effect' (f (unsafeCoerce ev)) (pure ()) >>= \b -> unless b (unsubscribeStream s)
      Transact ev _ -> effect' (f (unsafeCoerce ev)) (pure ()) >>= \b -> unless b (unsubscribeStream s)
      _ -> pure ()

subscribeStreams :: forall ev eff. (Typeable ev, Effect eff) => (ev -> eff) -> IO (Listener ev)
subscribeStreams f = do
  mtid <- myThreadId
  addListener (Right mtid) (traverse_ g)
  where
    g = \case
      Write ev -> effect' (f (unsafeCoerce ev)) (pure ()) >>= \b -> unless b (unsubscribeStreams @ev)
      Transact ev _ -> effect' (f (unsafeCoerce ev)) (pure ()) >>= \b -> unless b (unsubscribeStreams @ev)
      _ -> pure ()

unsubscribeStream :: (Typeable ev, Ord (Stream ev)) => Stream ev -> IO ()
unsubscribeStream s = do
  mtid <- myThreadId
  unlisten (StreamListener (s,Right mtid))

unsubscribeStreams :: forall ev. Typeable ev => IO ()
unsubscribeStreams = do
  mtid <- myThreadId
  unlisten (Listener (Right mtid) :: Listener ev)
