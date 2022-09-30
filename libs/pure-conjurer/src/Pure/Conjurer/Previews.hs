module Pure.Conjurer.Previews (Stream(..),Previews(..),PreviewsMsg(..),Name(..),Preview(..),defaultPreviewsUpdate) where

import Pure.Conjurer.Context
import Pure.Conjurer.Pathable
import Pure.Conjurer.Previewable
import Pure.Conjurer.Rep
import Pure.Conjurer.Resource

import Data.JSON
import Data.Txt
import Data.Sorcerer as Sorcerer

import Data.Hashable

import Data.List as List
import Data.Typeable
import GHC.Generics

-- TODO: pull this out of the default pages and make it optional.
--       This could be expensive when there are many resources
--       for a given context.

data Previews a = Previews
  { previews :: [(Name a,Preview a)] 
  } deriving stock Generic

deriving instance (ToJSON (Name a), ToJSON (Preview a))
  => ToJSON (Previews a)

deriving instance (FromJSON (Name a), FromJSON (Preview a))
  => FromJSON (Previews a)

data PreviewsMsg a
  = SetPreviewItem (Name a) (Preview a)
  | DeletePreviewItem (Name a)
  deriving stock Generic

deriving instance (ToJSON (Name a), ToJSON (Preview a)) 
  => ToJSON (PreviewsMsg a)

deriving instance (FromJSON (Name a), FromJSON (Preview a)) 
  => FromJSON (PreviewsMsg a)

instance 
  ( Typeable a
  , Pathable (Context a), Hashable (Context a)
  , ToJSON (Name a), FromJSON (Name a)
  , ToJSON (Preview a), FromJSON (Preview a)
  ) => Streamable (PreviewsMsg a) 
  where
    data Stream (PreviewsMsg a) = PreviewsStream (Context a)
      deriving stock Generic

    stream (PreviewsStream ctx) = 
      "conjurer/previews/" 
        ++ fromTxt (rep @a)
        ++ fromTxt (toPath ctx)
        ++ "/previews.stream"

deriving instance (Eq (Context a))
  => Eq (Stream (PreviewsMsg a))
deriving instance (Ord (Context a))
  => Ord (Stream (PreviewsMsg a))
deriving instance (Hashable (Context a))
  => Hashable (Stream (PreviewsMsg a))

-- Overlappable in case the need arises to, for instance, limit the length of a previews.
instance {-# OVERLAPPABLE #-} 
  ( Typeable a 
  , Eq (Name a), ToJSON (Name a), FromJSON (Name a)
  , Pathable (Context a), Hashable (Context a)
  , ToJSON (Preview a), FromJSON (Preview a)
  ) => Aggregable (PreviewsMsg a) (Previews a) 
  where
    update = defaultPreviewsUpdate id 
    aggregate = "previews.aggregate"

defaultPreviewsUpdate 
  :: Eq (Name x) 
  => (forall a. [a] -> [a]) 
  -> PreviewsMsg x -> Maybe (Previews x) -> Maybe (Maybe (Previews x))
defaultPreviewsUpdate f = update
  where
    update (SetPreviewItem nm p) Nothing = 
      Sorcerer.Update Previews 
        { previews = f [(nm,p)] }

    update (SetPreviewItem nm p) (Just l) = 
      Sorcerer.Update Previews 
        { previews = f $ (nm,p) : List.filter (((/=) nm) . fst) (previews l) }

    update (DeletePreviewItem nm) (Just l) = 
      Sorcerer.Update l 
        { previews = List.filter ((/= nm) . fst) (previews l) }

    update _ _ = Ignore