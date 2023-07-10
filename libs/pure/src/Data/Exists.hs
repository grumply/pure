{-# language RankNTypes, ConstraintKinds, FlexibleContexts, ScopedTypeVariables, AllowAmbiguousTypes #-}
module Data.Exists (Exists(..), using, with, may, try, unite) where

import Control.Concurrent (newEmptyMVar,tryPutMVar,readMVar)
import qualified Data.Try as Try
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)
import GHC.Exts (inline,magicDict)
import Data.Proxy (Proxy(..))

class Exists a where
  it :: a

{-
data Witness a r = Witness (Exists a => Proxy a -> r)

-- This configuration is not preferable, but it avoids a constraint satisfaction 
-- propagation issue. Using the `unsafeCoerce` approach for `using` along with
-- `GHC.Exts.inline` was able to completely remove dictionaries from the generated 
-- core, but calls to `using` were (sometimes) allowed to satisfy non-local 
-- `Exists` constraints! 
--
-- See: https://gitlab.haskell.org/ghc/ghc/-/issues/21575
--
{-# NOINLINE withWitness #-}
withWitness :: forall a r. (Exists a => Proxy a -> r) -> a -> Proxy a -> r
withWitness w a p = magicDict (Witness w) a p

{-# INLINE using' #-}
using' :: forall a r. a -> (Exists a => r) -> r
using' a w = withWitness (\_ -> w) a Proxy
-}

newtype Witness a r = Witness (Exists a => r)

-- This approach is likely to produce bugs, but the performance improvement over
-- the magicDict version can be massive. Keep `using` in mind if anything
-- inexplicable happens.
{-# INLINABLE using #-}
using :: forall a r. a -> (Exists a => r) -> r
using a w = unsafeCoerce (Witness w :: Witness a r) a

{-# INLINE with #-}
with :: a -> (Exists a => r) -> r
with = using

{-# INLINE may #-}
may :: forall a b. Exists (Maybe a) => b -> (Exists a => b) -> b
may nothing just = maybe nothing (\a -> using a just) (it :: Maybe a)

{-# INLINE try #-}
try :: forall a b. Exists (Try.Try a) => b -> b -> (Exists a => b) -> b
try trying failed done = Try.try trying failed (\a -> using a done) (it :: Try.Try a)

{-# INLINE unite #-}
unite :: forall a b c. Exists (Either a b) => (Exists a => c) -> (Exists b => c) -> c
unite left right = either (\a -> using a left) (\b -> using b right) (it :: Either a b)


