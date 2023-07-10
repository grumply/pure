{-# language RankNTypes, ConstraintKinds, AllowAmbiguousTypes, MagicHash, ScopedTypeVariables, TupleSections, BangPatterns #-}
module Control.React where

import Control.Concurrent
import Data.Exists
import Data.IORef
import Data.Maybe
import Data.Time
import GHC.Exts
import Prelude hiding (read)
import System.IO.Unsafe

type Event a = Exists a
type Behavior a b = Event a => b

{-# INLINE behavior #-}
behavior :: forall a b. Event a => Behavior a b -> b
behavior = id

{-# INLINE event #-}
event :: a -> (Event a => b) -> b
event = with

{-# INLINE read #-}
read :: Event a => a
read = it

{-# INLINE one #-}
one :: a -> a
one a = unsafePerformIO (readIORef ref)
  where
    {-# NOINLINE ref #-}
    ref = unsafePerformIO (newIORef a)

{-# INLINE occs #-}
occs :: a -> [a]
occs a = unsafePerformIO (atomicModifyIORef' ref (dup . (a:)))
  where
    {-# NOINLINE ref #-}
    ref = unsafePerformIO (newIORef [])

    dup x = (x,x)

{-# INLINE ambs #-}
ambs :: a -> b -> [Either a b]
ambs a b = changes (occs (a,b))
  where
    changes ((a1,b1):(a2,b2):xys)
      | isTrue# (reallyUnsafePtrEquality# a1 a2) = Right b1 : changes ((a2,b2):xys)
      | otherwise = Left a1 : changes ((a2,b2):xys)
    changes xs = []

{-# INLINE improving #-}
improving :: a -> b -> Maybe (Either a b)
improving a b = listToMaybe (ambs a b)

{-# INLINE amb #-}
amb :: a -> a -> Maybe a
amb l r = fmap (either id id) (improving l r)



