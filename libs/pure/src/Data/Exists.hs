{-# LANGUAGE RankNTypes, TypeApplications, TypeFamilies, FlexibleContexts, TypeOperators, DataKinds, ScopedTypeVariables, ConstraintKinds, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -O0 #-}
module Data.Exists where

import Control.Monad
import Data.Type.Equality
import Data.IORef
import Unsafe.Coerce
import GHC.Magic
import System.IO.Unsafe

class Exists a where
  it :: a

newtype Witness a r = Witness (Exists a => r)

-- using `inline` on the `unsafeCoerce` seems to avoid an issue with GHC 
-- floating `it` values out at some optimization levels on GHC < 8.11 (16893
-- fixed in 8.11). I suspect that the `inline` /slightly/ delays the coercion -
-- just enough to avoid the issue. Hopefully it is sufficient for all cases
-- since the inline `with` can produce /much/ better core than the noinline
-- version and `with` is used quite a lot. I suspect that there are still
-- issues, but hopefully they're minimized enough for this approach to work
-- until I can switch to 9.* with the new JS backend and the newer coercion
-- mechanisms (and, perhaps, switch to magicDict).
{-# INLINE with #-}
with :: forall a r. a -> (Exists a => r) -> r
with a w = inline (unsafeCoerce (Witness w :: Witness a r)) a

{-# INLINE using #-}
-- | Like `contramap` for existentials; existential refinement. 
--
-- NOTE: Be careful that `Exists a` is not resolvable by the calling context.
--       Homomorphisms are disallowed as a special case because it is guaranteed
--       that the existential constraint would be incorrectly resolved - the 
--       transformation would be ignored.
--
using :: (b == a) ~ False => (b -> a) -> (Exists a => r) -> (Exists b => r)
using f = with (f it)

newtype Handler eff = Handler { runHandler :: eff -> IO () -> IO Bool }

type Effect eff = Exists (Handler eff)

{-# INLINE effect' #-}
effect' :: Effect eff => eff -> IO () -> IO Bool
effect' = runHandler it

{-# INLINE effect #-}
effect :: Effect eff => eff -> IO ()
effect eff = void (effect' eff (pure ()))

{-# INLINE reinterpret #-}
-- | The covariant `reinterpret` allows you to adapt producers to be more specific or varied.
-- 
-- Related: see `refine`.
reinterpret :: forall msg msg' a. (msg -> msg') -> (Effect msg => a) -> (Effect msg' => a)
reinterpret f = with (Handler (effect' . f)) 

-- {-# INLINE also #-}
-- also :: forall msg a. (msg -> IO ()) -> (Effect msg => a) -> (Effect msg => a)
-- also f = with (Handler (\m io -> effect' m (io >> f m)))

{-# INLINE (#) #-}
infixr 9 #
(#) :: forall a b x. (a -> b) -> (Effect a => x) -> (Effect b => x)
(#) = Data.Exists.reinterpret

{-# RULES
  "Data.Exists.reinterpret id" forall x. Data.Exists.reinterpret id x = x
  #-}

type Producer a = Effect a

{-# INLINE yield #-}
yield :: Producer a => a -> IO ()
yield = effect

{-# INLINE stream #-}
stream :: forall a b. (a -> IO ()) -> (Producer a => b) -> b
stream f b = 
  case unsafeDupablePerformIO (writeIORef ref f) of
    () -> with (Handler (\a after -> readIORef ref >>= \f -> f a >> after >> pure True)) b
  -- with (Handler (\a after -> f a >> after >> pure True)) b
  where
    {-# NOINLINE ref #-}
    ref :: IORef (a -> IO ())
    ref = unsafePerformIO (newIORef undefined)

{-# INLINE consume #-}
consume :: (a -> IO ()) -> (Producer a => b) -> b
consume = stream

{-# INLINE events #-}
events :: forall a b. (Exists a => IO ()) -> (Producer a => b) -> b
events f = stream @a (`with` f)

{-# INLINE discard #-}
discard :: forall a b. (Producer a => b) -> b
discard = stream @a (\_ -> pure ())

