module Pure.Conjurer.Producible (Stream(..),Product(..),ProductMsg(..),Producible(..)) where

import Pure.Conjurer.Context
import Pure.Conjurer.Pathable
import Pure.Conjurer.Rep
import Pure.Conjurer.Resource

import Data.JSON
import Data.Txt
import Data.Sorcerer as Sorcerer
import Data.Theme

import Data.Hashable

import Data.Typeable
import GHC.Generics

import Prelude

data family Product a :: *

instance Theme Product

class Producible a where
  produce :: Context a -> Name a -> Resource a -> Maybe (Product a) -> IO (Product a)
  default produce :: Typeable a => Context a -> Name a -> Resource a -> Maybe (Product a) -> IO (Product a)
  produce _ _ _ _ = 
    let 
      tc = fromTxt (rep @a)
      err = "Producible " <> tc 
         <> " => produce :: Context " <> tc
         <> " -> Name " <> tc
         <> " -> Resource " <> tc 
         <> " -> Maybe (Product " <> tc <> ")" 
         <> " -> IO (Product " <> tc <> "): Not implemented."
    in 
      pure (error err)

  producePreview :: Context a -> Name a -> Resource a -> Maybe (Product a) -> IO (Product a)
  producePreview = produce

data ProductMsg a
  = SetProduct (Product a)
  | DeleteProduct
  deriving stock Generic

deriving instance ToJSON (Product a) 
  => ToJSON (ProductMsg a)

deriving instance FromJSON (Product a) 
  => FromJSON (ProductMsg a)

instance 
  ( Typeable a
  , ToJSON (ProductMsg a), FromJSON (ProductMsg a)
  , Hashable (Context a), Pathable (Context a)
  , Hashable (Name a), Pathable (Name a)
  ) => Streamable (ProductMsg a) 
  where
    data Stream (ProductMsg a) = ProductStream (Context a) (Name a)
      deriving stock Generic

    stream (ProductStream ctx nm) = 
      "conjurer/product/" 
        ++ fromTxt (rep @a)
        ++ fromTxt (toPath ctx)
        ++ fromTxt (toPath nm)
        ++ ".stream"

deriving instance (Eq (Context a), Eq (Name a)) 
  => Eq (Stream (ProductMsg a))
deriving instance (Ord (Context a), Ord (Name a)) 
  => Ord (Stream (ProductMsg a))
deriving instance (Hashable (Context a), Hashable (Name a)) 
  => Hashable (Stream (ProductMsg a))
  
instance 
  ( Typeable a
  , Hashable (Context a), Pathable (Context a)
  , Hashable (Name a), Pathable (Name a)
  , FromJSON (Product a), ToJSON (Product a)
  ) => Aggregable (ProductMsg a) (Product a) 
  where
    update (SetProduct p) _ = Sorcerer.Update p
    update DeleteProduct (Just _) = Delete
    update _ _ = Ignore

    aggregate = "product.aggregate"