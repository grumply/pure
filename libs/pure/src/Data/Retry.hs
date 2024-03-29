{-# language AllowAmbiguousTypes, TypeApplications, LambdaCase, ScopedTypeVariables, RecordWildCards, FlexibleContexts, RankNTypes #-}
module Data.Retry (retry,Status(..),Policy(..),recover,recoverWith,recoverWithIO,recovering,retrying,retryingIO,limitRetries,limitTries,limitDelay,limitDuration,constant,exponential,jittered,fibonacci,logRetry,logFailure,onRetry,onFailure) where

import Control.Exception
import Data.Foldable
import Data.JSON
import Data.Random (newSeed,uniformR,generate)
import Data.Time
import Data.View (Exists,with,caught)

import GHC.Stack
import System.IO.Unsafe

import Data.Log as Log hiding (Policy)

{-

Inspired by `retry` with simplified dynamics via throw. 

>>> start <- time
>>> retrying (exponential Second & limitRetries 5) (time >>= \now -> let Seconds elapsed _ = now - start in print elapsed >> retry)
0.0
1.0
3.0
7.0
15.0
31.0
Nothing

-}

data Retry = Retry deriving Show
instance Exception Retry

retry :: a
retry = throw Retry

data Status = Status
  { retries :: Int
  , start   :: Time
  , current :: Time
  }

instance ToJSON Status where
  toJSON Status {..} = 
    toJSON (retries,RFC3339 start,RFC3339 current)
  
instance FromJSON Status where
  parseJSON value = do
    (retries,RFC3339 start,RFC3339 current) <- parseJSON value
    pure Status {..}

newInitialStatus :: IO Status
newInitialStatus = time >>= \t -> pure (Status 0 t t)

newtype Policy = Policy (Status -> IO (Maybe Time))

instance Monoid Policy where
  mempty = constant 0
  mappend = (<>)

instance Semigroup Policy where
  (<>) (Policy p1) (Policy p2) = Policy p3
    where
      p3 status = do
        md1 <- p1 status
        case md1 of
          Nothing -> pure Nothing
          Just d1 -> do
            md2 <- p2 status
            case md2 of
              Nothing -> pure Nothing
              Just d2 -> pure (Just (max d1 d2))

-- | Retry the action if an exception of the given type is caught. 
-- Requires type application.
recover :: forall e a. Exception e => a -> a
recover = flip caught (\(_ :: e) -> retry) 

-- | Analyze exceptions of type `e`, and determine if the action should be
-- retried. If it shouldn't be retried, the exception is re-thrown.
recoverWith :: Exception e => (e -> Bool) -> a -> a
recoverWith f = flip caught (\e -> if f e then retry else throw e)

-- | Analyze exceptions of type `e`, and determine, effectfully, if the
-- action should be retried. If it shouldn't be retried, the exception is
-- re-thrown.
recoverWithIO :: Exception e => (e -> IO Bool) -> IO a -> IO a 
recoverWithIO f = handle (\e -> f e >>= \b -> if b then retry else throw e)

-- | Recover any exception.
recovering :: IO a -> IO a
recovering = recover @SomeException

-- | With a given policy, try the given action. If the policy fails, Nothing is returned.
retryingIO :: Policy -> (Exists Status => IO a) -> IO (Maybe a)
retryingIO (Policy policy) io = newInitialStatus >>= go
  where
    go status@Status {..} =
      handle (\Retry -> pure Nothing) (Just <$> with status io) >>= \case
        Just a -> pure (Just a)
        _ -> do
          current <- time
          md <- policy status { current = current }
          case md of
            Nothing -> pure Nothing
            Just t  -> delay t >> go Status { retries = retries + 1, .. }

-- | With a given policy, try the given action. If the policy fails, Nothing is returned.
retrying :: Policy -> (Exists Status => a) -> Maybe a
retrying (Policy policy) a = unsafePerformIO (newInitialStatus >>= go)
  where
    go status@Status {..} =
      handle (\Retry -> pure Nothing) (Just <$> with status (evaluate a)) >>= \case
        Just a -> pure (Just a)
        _ -> do
          current <- time
          md <- policy status { current = current }
          case md of
            Nothing -> pure Nothing
            Just t  -> delay t >> go Status { retries = retries + 1, .. }

-- | Stop retrying after `n` retries.
limitRetries :: Int -> Policy -> Policy
limitRetries n (Policy p) = Policy p'
  where
    p' status@Status {..}
      | retries >= n = pure Nothing
      | otherwise = p status

-- | Stop trying after `n` tries.
limitTries :: Int -> Policy -> Policy
limitTries n = limitRetries (n - 1)

-- | Set a maximum delay on a policy.
limitDelay :: Time -> Policy -> Policy
limitDelay t (Policy p) = Policy p'
  where
    p' status = do
      md <- p status
      pure $ 
        case md of
          Just d | d > t -> Just t
          x -> x

-- | Stop retrying after the given amount of time has elapsed.
limitDuration :: Time -> Policy -> Policy
limitDuration t (Policy p) = Policy p'
  where
    p' status@Status {..}
      | current < start + t = p status
      | otherwise           = pure Nothing

onRetry :: (Status -> Time -> IO ()) -> Policy -> Policy
onRetry f (Policy p) = Policy p'
  where
    p' status = do
      mt <- p status
      for_ mt (f status)
      pure mt

onFailure :: (Status -> IO ()) -> Policy -> Policy
onFailure f (Policy p) = Policy p'
  where
    p' status = do
      mt <- p status
      maybe (f status) (const (pure ())) mt
      pure mt

logRetry :: Logging a => Level -> (Status -> Time -> a) -> Policy -> Policy
logRetry lvl f = onRetry (\status time -> Log.log lvl (f status time))

logFailure :: Logging a => Level -> (Status -> a) -> Policy -> Policy
logFailure lvl f = onFailure (Log.log lvl . f)

-- | Constant delay policy.
constant :: Time -> Policy
constant t = Policy (\_ -> pure (Just t))

-- | Exponential delay policy.
exponential :: Time -> Policy
exponential (Milliseconds ms _) = Policy policy
  where
    policy Status {..} = do
      let d = ms * 2 ** fromIntegral retries
      pure (Just (Milliseconds d 0))

-- | Exponential delay policy with jittering.
jittered :: Time -> Policy
jittered (Milliseconds ms _) = Policy policy
  where
    policy Status {..} = do
      let d = ms * 2 ** fromIntegral retries
      s <- newSeed
      let (_,r) = generate (uniformR 0 d) s
      pure (Just (Milliseconds (d + r) 0))

-- | Fibonacci delay policy.
fibonacci :: Time -> Policy
fibonacci (Milliseconds ms _) = Policy policy
  where
    policy Status {..} = do
      let d = ms + fromIntegral (fibs !! retries)
      pure (Just (Milliseconds d 0))

{-# NOINLINE fibs #-}
fibs :: [Int]
fibs = 0 : 1 : zipWith (+) fibs (tail fibs)
