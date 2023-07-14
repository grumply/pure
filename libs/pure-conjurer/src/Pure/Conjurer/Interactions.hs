module Pure.Conjurer.Interactions where

import Pure.Conjurer.Context
import Pure.Conjurer.Name
import Pure.Conjurer.Rep
import Pure.Conjurer.Producible

import Pure.Auth (Username)
import Data.Txt (FromTxt(..))
import Data.JSON (ToJSON(..),FromJSON(..))
import Data.Default

import Data.Typeable

import Prelude

data family Action a
data family Reaction a

instance {-# OVERLAPPABLE #-} Typeable a => ToJSON (Action a) where
  toJSON _ = 
    let ty = show $ typeOf (undefined :: Action a)
    in error $ "No derived or explicit implementation of ToJSON (" <> ty <> ")"

instance {-# OVERLAPPABLE #-} Typeable a => FromJSON (Action a) where
  parseJSON _ = 
    let ty = show $ typeOf (undefined :: Action a)
    in error $ "No derived or explicit implementation of FromJSON (" <> ty <> ")"

instance {-# OVERLAPPABLE #-} Typeable a => ToJSON (Reaction a) where
  toJSON _ = 
    let ty = show $ typeOf (undefined :: Reaction a)
    in error $ "No derived or explicit implementation of ToJSON (" <> ty <> ")"

instance {-# OVERLAPPABLE #-} Typeable a => FromJSON (Reaction a) where
  parseJSON _ = 
    let ty = show $ typeOf (undefined :: Reaction a)
    in error $ "No derived or explicit implementation of FromJSON (" <> ty <> ")"

data Interactions a = Interactions
  { interact :: Context a -> Name a -> Product a -> Action a -> IO (Reaction a)
  }

class Typeable a => DefaultInteractions a where
  interactions :: Maybe (Username c) -> Interactions a
  interactions _ = 
    let 
      tc = fromTxt (rep @a)

      err = "Interactions from DefaultInteractions " <> tc 
         <> " => interact :: Context " <> tc
         <> " -> Name " <> tc
         <> " -> Product " <> tc 
         <> " -> Action " <> tc 
         <> " -> IO (Reaction " <> tc <> "): Not implemented."
      
    in 
      Interactions (\_ _ _ _ -> error err)

instance {-# OVERLAPPABLE #-} Typeable x => DefaultInteractions x
