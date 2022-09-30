{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module Pure.Conjurer.Resource (Stream(..),Resource(..),ResourceMsg(..),Name(..),Nameable(..),Processable(..),Amendable(..)) where

import Pure.Conjurer.Context
import Pure.Conjurer.Name
import Pure.Conjurer.Pathable
import Pure.Conjurer.Rep

import Data.JSON
import Data.Txt
import Data.Sorcerer as Sorcerer

import Data.Hashable

import Data.Typeable
import GHC.Generics

data family Resource a :: *

-- Processable is necessary in cases where constructing a resource requires
-- server-side logic, like seeding with a server-generated UUID, etc...
-- `process` is run before product creation, update, and preview.
class Processable a where
  process :: Resource a -> IO (Maybe (Resource a))
  process = pure . Just

  processPreview :: Resource a -> IO (Maybe (Resource a))
  processPreview = process

instance {-# OVERLAPPABLE #-} Processable a

class Amendable a where
  data Amend a :: *
  -- NOTE: Resources are not processed after amend!
  amend :: Amend a -> Resource a -> Maybe (Resource a)
  amend _ = Just

instance {-# OVERLAPPABLE #-} Typeable a => ToJSON (Amend a) where
  toJSON _ = 
    let ty = show $ typeOf (undefined :: Amend a)
    in error $ "No derived or explicit implementation of ToJSON (" <> ty <> ")"

instance {-# OVERLAPPABLE #-} Typeable a => FromJSON (Amend a) where
  parseJSON _ = 
    let ty = show $ typeOf (undefined :: Amend a)
    in error $ "No derived or explicit implementation of FromJSON (" <> ty <> ")"

class Nameable a where
  toName :: Resource a -> Name a

data ResourceMsg a
  = CreateResource (Resource a)
  | SetResource (Resource a)
  | AmendResource (Amend a)
  | DeleteResource
  deriving stock Generic

deriving instance (ToJSON (Amend a), ToJSON (Resource a))
  => ToJSON (ResourceMsg a)

deriving instance (FromJSON (Amend a), FromJSON (Resource a))
  => FromJSON (ResourceMsg a)

instance 
  ( Typeable a
  , ToJSON (ResourceMsg a), FromJSON (ResourceMsg a)
  , Hashable (Context a), Pathable (Context a)
  , Hashable (Name a), Pathable (Name a)
  ) => Streamable (ResourceMsg a) 
  where
    data Stream (ResourceMsg a) = ResourceStream (Context a) (Name a)
      deriving stock Generic
      
    stream (ResourceStream ctx nm) = 
      "conjurer/resource/" 
        ++ fromTxt (rep @a)
        ++ fromTxt (toPath ctx)
        ++ fromTxt (toPath nm)
        ++ ".stream"

deriving instance (Eq (Context a), Eq (Name a))
  => Eq (Stream (ResourceMsg a))
deriving instance (Ord (Context a), Ord (Name a))
  => Ord (Stream (ResourceMsg a))
deriving instance (Hashable (Context a), Hashable (Name a)) 
  => Hashable (Stream (ResourceMsg a))
  
instance 
  ( Typeable a
  , Amendable a
  , FromJSON (Amend a), ToJSON (Amend a)
  , Hashable (Context a), Pathable (Context a)
  , Hashable (Name a), Pathable (Name a)
  , FromJSON (Resource a), ToJSON (Resource a)
  ) => Aggregable (ResourceMsg a) (Resource a)
  where
    update (CreateResource r) Nothing = Sorcerer.Update r
    update (SetResource r) (Just _) = Sorcerer.Update r
    update (AmendResource c) (Just r) = maybe Ignore Sorcerer.Update (amend c r)
    update DeleteResource (Just _) = Delete
    update _ _ = Ignore

    aggregate = "resource.aggregate"