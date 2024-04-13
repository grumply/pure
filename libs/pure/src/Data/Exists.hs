{-# LANGUAGE RankNTypes, TypeApplications, TypeFamilies, FlexibleContexts, TypeOperators, DataKinds, ScopedTypeVariables, ConstraintKinds, AllowAmbiguousTypes #-}
{-# OPTIONS_GHC -O2 #-}
module Data.Exists where

import Control.Monad
import Data.Type.Equality
import Data.IORef
import Data.Void (Void,absurd)
import Unsafe.Coerce
import GHC.Magic
import System.IO.Unsafe

class Exists a where
  it :: a
instance Exists () where it = ()
instance Exists Void where it = absurd (it :: Void)

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

