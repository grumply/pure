{-# LANGUAGE RankNTypes, TypeApplications, TypeFamilies, FlexibleContexts, TypeOperators, DataKinds, ScopedTypeVariables, ConstraintKinds, AllowAmbiguousTypes, BangPatterns #-}
{- This -O0 is absolutely ciritical to maintaining safety. 
   Without it, the mutable reference in `stream` can float 
   out and be shared!
-}
{-# OPTIONS_GHC -O0 #-}
module Data.Consumer where

import Control.Monad
import Data.Exists
import Data.Type.Equality
import Data.IORef
import Unsafe.Coerce
import GHC.Magic
import System.IO.Unsafe
import Control.Parallel

newtype Handler eff = Handler { runHandler :: eff -> IO () -> IO Bool }

type Effect eff = Exists (Handler eff)
type Producer a = Effect a

{-
`stream` inverts callback control to prevent the capturing of a consumer, 
resulting in `yield` being dynamic, but appearing static. With a more
traditional stream consumer, like

> stream' f = with (Handler (\a after -> f a >> ...)) 

the handler must be updated any time `f` changes. Thus, if you were to write
something like 

> bad f = stream' (\() -> f >> pure ()) (static (... yield () ...))

any change to `f` will not be propagated to a live `yield` because we have made
the View static. This is generally not desired, and you cannot know if user 
code constructs a static View. The solution is to make the existential that
`yield` sees mutable so that it can be changed at the source rather than having
to walk down to every instance of `yield` to update it. 

This results in two major benefits:

1. Updating a callback doesn't induce reconciliation, which can /greatly/ 
   improve performance of web applications, making many styles of application
   updatable in constant time, independent of the View size.

2. Prevents inadvertent, or purposeful, capture of the consuming function.

With this approach, when used with `Web` - especially the use in `become`, we
are able to piggyback statefulness of Views onto Haskell's function syntax in
the same way one would with recursive workers, while maintaining good
performance in the reconciler.

Unfortunately, we have to work exceptionally hard to prevent GHC from sharing
the internal mutable reference. This prevents the rather powerful optimization
of floating the `stream` up/out, which would allow more efficient updates during
reconciliation. If we had a stronger guarantee that the mutable reference that
`stream` uses couldn't be shared, we could re-enable the let-floating (which is
prevented by the over-broad -O0 at the top of this module).

Ultimately, this approach is fragile, and a better solution is warranted.

-}

-- | Substitute any call to `yield a` or `effect' a x` in `b` with a call to
-- `f a [>> x]`. Asynchronously updates during reconciliation; `yield` should
-- always see a current version of `f`.
{-# INLINE stream #-}
stream :: forall a b. (a -> IO ()) -> (Producer a => b) -> b
stream f b = 
  -- with (Handler (\a after -> f a >> after >> pure True)) b
  unsafeDupablePerformIO (writeIORef ref f) `pseq`
    with (Handler (\a after -> readIORef ref >>= \f -> f a >> after >> pure True)) b
  where
    {-# NOINLINE ref #-}
    ref :: IORef (a -> IO ())
    ref = unsafePerformIO (newIORef undefined)

