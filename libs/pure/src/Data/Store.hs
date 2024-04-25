{-# language TypeApplications, ScopedTypeVariables, DerivingVia, RankNTypes, FlexibleContexts, AllowAmbiguousTypes #-}
module Data.Store (Storable(..),Memory(),Vault,Label,unsafeToLabel,Store,withStore,withUniverse,observe,store,replace) where

import Data.Marker (Marker(),markIO)
import Data.Key (Key(),toKey)
import Data.Map (Map)
import qualified Data.Map.Lazy as Map
import Data.Typeable (TypeRep,Typeable,typeRep,Proxy(..))
import Data.Txt (ToTxt,FromTxt)
import Data.View (Exists(..),with,State,Modify,state,put,modify,zoom,View)
import qualified Data.View as View
import GHC.Exts (Any(..))
import Unsafe.Coerce

{- | 

'Universe' is a dependent map of type representations to maps of uniquely-keyed 
values with optimizations for efficient observation and a class-based
abstraction for extension to other backends. This module implements an in-memory
backend which does not require any serialization of the stored types.

-}

-- | A @Universe@ represents a shared data root, allowing ad-hoc sharing of data
-- between independent contexts. Only one @Universe@ can be available evaluation
-- root.
type Universe = Map TypeRep Vault

newtype Arbitrary = Arbitrary (forall a. a) 
type Vault = Map Key Arbitrary

-- | A @Label@ is a phantom-typed key, implemented as a newtype around a
-- 'Marker'.
newtype Label a = Label (Marker a) 
  deriving (Eq,Ord,Show,ToTxt,FromTxt) via Marker a

unsafeToLabel :: Marker a -> Label a
unsafeToLabel = unsafeCoerce

-- | A 'Store' is a @Map@ of uniquely-keyed values. Internally, a 'Store' is
-- stored in a 'Universe' as a @Map Key Any@ keyed by the 'TypeRep' for @a@,
-- allowing interdependence of independent contexts via communication through a
-- shared data root, @Universe@.
type Store a = Map (Label a) a

-- | A class of storage backends for reactive contexts. 
--
-- # Justification 
-- The extension here, the type class, permits implementation of other backends
-- (on-disk, remote, S3, DB-backed, etc....). This is why they're suffixed 
-- `...Impl`, so that the individual modules implementing a backend can export a
-- non-suffixed version (like below, for `Memory`).
--
-- # Custom Implementations
-- Note that, if defining a custom implementation, there needs to be a means of
-- witnessing all changes to a 'Universe'. For shared on-disk implementations, 
-- this might be implemented via fsnotify, or the like. For remote
-- implementations, say on S3, this might require S3 event notifications and
-- synchronization. It should be possible to rely on the laziness of
-- `Data.Map.Lazy` to only synchronize what is actively being 'observe'd: 
-- thunk the update and allow it to be performed by the observer.
--
-- # Reactivity
-- 'observe' is reactive, since it is derived of `Exists (Store a)` which is
-- implicitly reactive. An 'observe' call will, therefore, imply scope-level
-- reactivity. This can be useful when, for example, writing 'Server.Handler's:
-- the 'Handler' will, implicitly, witness changes via 'observe'.
class Storable backend where
  withUniverseImpl :: Universe -> (State Universe => View) -> View
  storeImpl :: forall a. Modify (Store a) => a -> IO (Label a)
  replaceImpl :: Modify (Store a) => Label a -> a -> IO ()
  observeImpl :: Exists (Store a) => Label a -> Maybe a

-- I'm not actually sure why this can't be a member of `Storable`. GHC complains
-- about a missing instance for `State (Store a)`. Strange....
withStore :: forall a x. Typeable a => (State (Store a) => x) -> (State Universe => x)
withStore = zoom (fromVault . Map.findWithDefault mempty tr) (Map.insert tr . toVault)
  where
    tr :: TypeRep
    tr = typeRep (Proxy :: Proxy a)

    toVault :: Store a -> Vault
    toVault = unsafeCoerce

    fromVault :: Vault -> Store a
    fromVault = unsafeCoerce

data Memory
instance Storable Memory where
  withUniverseImpl = state 

  storeImpl a = do
    l <- unsafeToLabel <$> markIO 
    modify (Map.insert l a)
    pure l

  replaceImpl l a = modify (Map.adjust (const a) l)

  observeImpl = (it Map.!?) 

withUniverse :: Universe -> (State Universe => View) -> View
withUniverse = withUniverseImpl @Memory

store :: forall a. Modify (Store a) => a -> IO (Label a)
store = storeImpl @Memory

replace :: forall a. Modify (Store a) => Label a -> a -> IO ()
replace = replaceImpl @Memory

observe :: forall a. Exists (Store a) => Label a -> Maybe a
observe = observeImpl @Memory