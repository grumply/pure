{-# language LambdaCase, PatternSynonyms #-}
module Data.Sorcerer.Queue 
  ( newQueue
  , newEmptyQueue
  , withEmptyQueue
  , isEmptyQueue
  , arrive
  , arriveMany
  , collect
  , Queue
  ) where

import Control.Concurrent
import Control.Monad (join,void,unless)
import qualified Data.List as List (reverse)

--------------------------------------------------------------------------------
-- Modified version of Neil Mitchell's `The Flavor of MVar` Queue from:
-- http://neilmitchell.blogspot.com/2012/06/flavours-of-mvar_04.html
-- Modified for amortized O(1) read and O(1) write.
newtype Queue a = Queue (MVar (Either [a] (MVar [a])))

{-# INLINE newQueue #-}
newQueue :: [a] -> IO (Queue a)
newQueue xs = Queue <$> newMVar (Left (List.reverse xs))

{-# INLINE newEmptyQueue #-}
newEmptyQueue :: IO (Queue a)
newEmptyQueue = newQueue []

{-# INLINE withEmptyQueue #-}
withEmptyQueue :: Queue a -> IO () -> IO Bool
withEmptyQueue (Queue q_) io =
  withMVar q_ $ \case
    Pushing [] -> io >> pure True
    Pulling b -> do
      ma <- tryReadMVar b
      case ma of
        Nothing -> io >> pure True
        Just _  -> pure False
    _ -> pure False

{-# INLINE isEmptyQueue #-}
isEmptyQueue :: Queue a -> IO Bool
isEmptyQueue = flip withEmptyQueue (pure ())

{-# INLINE arrive #-}
arrive :: Queue a -> a -> IO ()
arrive (Queue q_) x =
  modifyMVar_ q_ $ \case
    Pushing xs -> return (Pushing (x:xs))
    Pulling b -> do 
      push b x
      pure (Pulling b)

{-# INLINE arriveMany #-}
arriveMany :: Queue a -> [a] -> IO ()
arriveMany (Queue q_) as = do
  modifyMVar_ q_ $ \case
    Pushing xs -> do
      return (Pushing (List.reverse as ++ xs))
    Pulling b -> do
      pushMany b as
      pure (Pulling b)

{-# INLINE collect #-}
collect :: Queue a -> IO [a]
collect (Queue q_) = do
  join $ modifyMVar q_ $ \case

    Pulling b -> do
      mxs <- tryPull b
      case mxs of
        Nothing -> return (Pulling b,yield >> pull b)
        Just xs -> return (Pushing [],return (List.reverse xs))

    Pushing [] -> do 
      b <- puller
      return (Pulling b,yield >> pull b)

    Pushing xs -> 
      return (Pushing [],return (List.reverse xs))

pattern Pulling :: b -> Either a b
pattern Pulling a = Right a

pattern Pushing :: a -> Either a b
pattern Pushing a = Left a

{-# complete Pushing, Pulling #-}

puller = newEmptyMVar
tryPull = tryTakeMVar
pull = takeMVar
push b x = do
  pushed <- tryPutMVar b [x]
  unless pushed (modifyMVar_ b (pure . (x:)))
pushMany b xs = do
  pushed <- tryPutMVar b (List.reverse xs)
  unless pushed (modifyMVar_ b (pure . (List.reverse xs ++)))