module Pure.Convoker.Mods where

import Pure.Convoker.Admins hiding (Add,Remove)

import Pure.Auth (Username)
import Pure.Conjurer
import Data.JSON
import Effect.Websocket

import Data.Hashable

import Control.Monad (liftM2)
import Data.List as List
import Data.Maybe
import Data.Typeable
import GHC.Generics

data Mods (domain :: *) (a :: *)

data instance Resource (Mods domain a) = RawMods
  { mods :: [Username]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data instance Product (Mods domain a) = Mods
  { mods :: [Username]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data instance Preview (Mods domain a) = NoModsPreview
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance (Typeable domain, Pathable (Context a), Hashable (Context a), Ord (Context a), Typeable a ) => Ownable (Mods domain a) where
  isOwner un ctx _ = liftM2 (||) (isMod @domain ctx un) (isAdmin @domain un)

data instance Context (Mods domain a) = ModsContext (Context a)
  deriving stock Generic
deriving instance Eq (Context a) => Eq (Context (Mods domain a))
deriving instance Ord (Context a) => Ord (Context (Mods domain a))
deriving instance Hashable (Context a) => Hashable (Context (Mods domain a))
deriving instance (Typeable a, Pathable (Context a)) => Pathable (Context (Mods domain a))
deriving instance ToJSON (Context a) => ToJSON (Context (Mods domain a))
deriving instance FromJSON (Context a) => FromJSON (Context (Mods domain a))

data instance Name (Mods domain a) = ModsName
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (Hashable,Pathable,ToJSON,FromJSON)

instance Processable (Mods domain a)

instance Producible (Mods domain a) where
  produce _ _ RawMods {..} _ = pure Mods {..}

instance Previewable (Mods domain a) where
  preview _ _ _ _ = pure NoModsPreview

instance Nameable (Mods domain a) where
  toName _ = ModsName

data instance Action (Mods domain a) = NoModsAction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)
data instance Reaction (Mods domain a) = NoModsReaction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance Amendable (Mods domain a) where
  data Amend (Mods domain a) = AddMod Username | RemoveMod Username
    deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

  amend (AddMod un) RawMods {..} | un `notElem` mods = 
    Just RawMods { mods = un : mods }
  
  amend (RemoveMod un) RawMods {..} | un `elem` mods, List.length mods > 1, List.last mods /= un =
    Just RawMods { mods = List.filter (/= un) mods }

  amend _ _ = 
    Nothing

tryCreateMods :: 
  ( Typeable domain
  , Typeable a
  , Processable (Mods domain a)
  , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
  ) => Permissions (Mods domain a) -> Callbacks (Mods domain a) -> Context a -> [Username] -> IO Bool
tryCreateMods permissions callbacks ctx mods = fmap isJust do
  tryCreate permissions callbacks (ModsContext ctx) (RawMods mods)

tryAddMod :: 
  ( Typeable domain
  , Typeable a
  , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
  ) => Permissions (Mods domain a) -> Callbacks (Mods domain a) -> Context a -> Username -> IO Bool
tryAddMod permissions callbacks ctx mod = fmap isJust do
  tryAmend permissions callbacks (ModsContext ctx) ModsName 
    (AddMod mod)

tryRemoveMod :: 
  ( Typeable domain
  , Typeable a
  , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
  ) => Permissions (Mods domain a) -> Callbacks (Mods domain a) -> Context a -> Username -> IO Bool
tryRemoveMod permissions callbacks ctx mod = fmap isJust do
  tryAmend permissions callbacks (ModsContext ctx) ModsName 
    (RemoveMod mod)

modPermissions :: forall domain a. 
  ( Typeable domain
  , Typeable a
  , Pathable (Context (Mods domain a)), Hashable (Context (Mods domain a)), Ord (Context a)
  ) => Username -> Permissions (Mods domain a)
modPermissions un = readPermissions { canAmend = canAmend' }
  where
    canAmend' ctx _ _ = isMod @domain ctx un

isMod :: forall (domain :: *) a. 
  (Typeable domain
  , Pathable (Context a), Hashable (Context a), Ord (Context a)
  , Typeable a
  ) => Context a -> Username -> IO Bool
isMod ctx un =
  tryReadProduct (fullPermissions @(Mods domain a)) (callbacks @(Mods domain a) (Just un)) (ModsContext @domain ctx) (ModsName @domain) >>= \case
    Just Mods {..} | un `elem` mods -> pure True
    _ -> isAdmin @domain un