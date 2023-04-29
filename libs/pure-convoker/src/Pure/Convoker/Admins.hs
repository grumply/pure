{-# language CPP, ConstraintKinds, KindSignatures #-}
module Pure.Convoker.Admins where

import Control.Log (Logging)
import Pure.Auth (Username)
import Pure.Conjurer
import Control.Component
import Data.JSON

import Data.Hashable

import Control.Monad (liftM2)
import Data.List as List
import Data.Maybe
import Data.Typeable (Typeable)
import GHC.Generics
import GHC.Exts

import Effect.Websocket as WS

data Admins (ctx :: *)
newtype instance Resource (Admins ctx) = RawAdmins
  { admins :: [Username]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

newtype instance Product (Admins ctx) = Admins
  { admins :: [Username] 
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data instance Preview (Admins ctx) = NoAdminsPreview
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

data instance Context (Admins ctx) = AdminsContext
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (Hashable,Pathable,ToJSON,FromJSON)

data instance Name (Admins ctx) = AdminsName
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (Hashable,Pathable,ToJSON,FromJSON)

instance Producible (Admins ctx) where
  produce _ _ RawAdmins {..} _ = pure Admins {..}

instance Previewable (Admins ctx) where
  preview _ _ _ _ = pure NoAdminsPreview

instance Nameable (Admins ctx) where
  toName _ = AdminsName

instance Processable (Admins ctx)

instance (Typeable domain, Logging) => Ownable (Admins domain) where
  isOwner un _ _ = isAdmin @domain un

data instance Amend (Admins ctx) = AddAdmin Username | RemoveAdmin Username
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)
instance Amendable (Admins ctx) where

  amend (AddAdmin un) RawAdmins {..} | un `notElem` admins = 
    Just RawAdmins { admins = un : admins }

  amend (RemoveAdmin un) RawAdmins {..} | un `elem` admins, List.length admins > 1, List.last admins /= un = 
    Just RawAdmins { admins = List.filter (/= un) admins }

  amend _ _ = 
    Nothing

data instance Action (Admins ctx) = NoAdminsAction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

data instance Reaction (Admins ctx) = NoAdminsReaction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance (Typeable domain, Logging) => DefaultPermissions (Admins domain) where
  permissions Nothing = readPermissions
  permissions (Just un) = readPermissions { canAmend = canAmend' }
    where
      canAmend' _ _ _ = isAdmin @domain un

tryCreateAdmins :: forall (ctx :: *). Typeable ctx => [Username] -> IO Bool
tryCreateAdmins admins = fmap isJust do
  tryCreate (fullPermissions @(Admins ctx)) (callbacks @(Admins ctx) Nothing) AdminsContext (RawAdmins @ctx admins)

tryAddAdmin :: forall ctx. Typeable ctx => Permissions (Admins ctx) -> Callbacks (Admins ctx) -> Username -> IO Bool
tryAddAdmin permissions callbacks un = fmap isJust do
  tryAmend permissions callbacks AdminsContext AdminsName 
    (AddAdmin un)

tryRemoveAdmin :: forall ctx. Typeable ctx => Permissions (Admins ctx) -> Callbacks (Admins ctx) -> Username -> IO Bool
tryRemoveAdmin permissions callbacks un = fmap isJust do
  tryAmend permissions callbacks AdminsContext AdminsName
    (RemoveAdmin un)

isAdmin :: forall (domain :: *). (Typeable domain, Logging) => Username -> IO Bool
isAdmin un = 
#ifdef __GHCJS__
  WS.req @domain WS.Cached (readingAPI @(Admins domain)) (readProduct @(Admins domain)) (AdminsContext,AdminsName) >>= \case
    Nothing -> pure False
    Just Admins {..} -> pure (un `elem` admins)
#else
  tryReadProduct (readPermissions @(Admins domain)) (callbacks @(Admins domain) Nothing) AdminsContext AdminsName >>= \case
    Just Admins {..} -> pure (un `elem` admins)
    _ -> pure False
#endif

defaultIsOwner :: forall (domain :: *) a. 
  ( Typeable domain
  , Ownable a
  , Logging
  ) => Username -> Context a -> Maybe (Name a) -> IO Bool
defaultIsOwner un ctx nm = liftM2 (||) (isOwner un ctx nm) (isAdmin @domain un)

adminPermissions :: forall (domain :: *) resource. 
  ( Typeable domain
  , Logging
  ) => Username -> Permissions resource
adminPermissions un = Permissions {..}
  where
    canRead     ctx nm      = isAdmin @domain un
    canCreate   ctx nm res  = isAdmin @domain un
    canUpdate   ctx nm      = isAdmin @domain un
    canAmend    ctx nm amnd = isAdmin @domain un
    canInteract ctx nm actn = isAdmin @domain un
    canDelete   ctx nm      = isAdmin @domain un
    canList     ctx         = isAdmin @domain un
    canEnum                 = isAdmin @domain un

