module Pure.Conjurer.Permissions where

import Pure.Conjurer.Context
import Pure.Conjurer.Interactions
import Pure.Conjurer.Name
import Pure.Conjurer.Resource

import Pure.Auth (Username)

class Ownable resource where
  isOwner :: Username c -> Context resource -> Maybe (Name resource) -> IO Bool

data Permissions resource = Permissions
  { canCreate   :: Context resource -> Name resource -> Resource resource -> IO Bool
  , canUpdate   :: Context resource -> Name resource -> IO Bool
  , canAmend    :: Context resource -> Name resource -> Amend resource -> IO Bool
  , canInteract :: Context resource -> Name resource -> Action resource -> IO Bool
  , canDelete   :: Context resource -> Name resource -> IO Bool
  , canRead     :: Context resource -> Name resource -> IO Bool
  , canList     :: Context resource -> IO Bool
  , canEnum     :: IO Bool
  }

noPermissions :: Permissions resource
noPermissions = Permissions
  { canCreate   = \_ _ _ -> pure False
  , canUpdate   = \_ _ -> pure False
  , canAmend    = \_ _ _ -> pure False
  , canInteract = \_ _ _ -> pure False
  , canDelete   = \_ _ -> pure False
  , canRead     = \_ _ -> pure False
  , canList     = \_   -> pure False
  , canEnum     = pure False
  }

fullPermissions :: Permissions resource
fullPermissions = Permissions
  { canCreate   = \_ _ _ -> pure True
  , canUpdate   = \_ _ -> pure True
  , canAmend    = \_ _ _ -> pure True
  , canInteract = \_ _ _ -> pure True
  , canDelete   = \_ _ -> pure True
  , canRead     = \_ _ -> pure True
  , canList     = \_   -> pure True
  , canEnum     = pure True
  }

readPermissions :: Permissions resource
readPermissions = Permissions
  { canCreate   = \_ _ _ -> pure False
  , canUpdate   = \_ _ -> pure False
  , canAmend    = \_ _ _ -> pure False
  , canInteract = \_ _ _ -> pure False
  , canDelete   = \_ _ -> pure False
  , canRead     = \_ _ -> pure True
  , canList     = \_   -> pure True
  , canEnum     = pure True
  }

defaultPermissions :: Ownable resource => Maybe Username -> Permissions resource
defaultPermissions = \case
  Nothing -> readPermissions
  Just un -> readPermissions
    { canCreate   = \ctx nm _ -> isOwner un ctx (Just nm)
    , canUpdate   = \ctx nm   -> isOwner un ctx (Just nm)
    , canAmend    = \ctx nm _ -> isOwner un ctx (Just nm)
    , canInteract = \ctx nm _ -> isOwner un ctx (Just nm)
    }
      -- canDelete = \ctx nm _ -> isOwner un ctx (Just nm)
      -- Omit as a default. I prefer delete not work and the programmer have to
      -- figure out why rather than being surprised that resources are disappearing!

class DefaultPermissions x where
  permissions :: Maybe (Username c) -> Permissions x
  default permissions :: Ownable x => Maybe (Username c) -> Permissions x
  permissions = defaultPermissions

instance {-# OVERLAPPABLE #-} Ownable x => DefaultPermissions x
