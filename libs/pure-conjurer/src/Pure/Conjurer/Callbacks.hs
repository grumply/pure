module Pure.Conjurer.Callbacks where

import Pure.Conjurer.Context
import Pure.Conjurer.Interactions
import Pure.Conjurer.Name
import Pure.Conjurer.Previewable
import Pure.Conjurer.Producible
import Pure.Conjurer.Resource
import Pure.Conjurer.Rep

import Pure.Auth (Username)
import Data.Default
import Data.Txt
import Data.JSON

import Data.Typeable

data Callbacks resource = Callbacks
  { onCreate   :: Context resource -> Name resource -> Resource resource -> Product resource -> Preview resource -> [(Name resource,Preview resource)] -> IO ()
  , onUpdate   :: Context resource -> Name resource -> Resource resource -> Product resource -> Preview resource -> [(Name resource,Preview resource)] -> IO ()
  , onDelete   :: Context resource -> Name resource -> Resource resource -> Product resource -> Preview resource -> [(Name resource,Preview resource)] -> IO ()
  , onAmend    :: Context resource -> Name resource -> Resource resource -> Product resource -> Preview resource -> [(Name resource,Preview resource)] -> Amend resource -> IO ()
  , onInteract :: Context resource -> Name resource -> Product resource -> Action resource -> Reaction resource -> IO ()
  , onResource :: Context resource -> Name resource -> Resource resource -> IO ()
  , onRead     :: Context resource -> Name resource -> Product resource  -> IO ()
  , onPreview  :: Context resource -> Name resource -> Preview resource -> IO ()
  , onList     :: Context resource -> [(Name resource,Preview resource)] -> IO ()
  }

emptyCallbacks :: forall a. Typeable a => Callbacks a
emptyCallbacks = Callbacks
  { onCreate   = def
  , onUpdate   = def
  , onDelete   = def
  , onAmend    = def
  , onInteract = def
  , onResource = def
  , onRead     = def
  , onPreview  = def
  , onList     = def
  } 

class DefaultCallbacks x where
  callbacks :: Typeable x => Maybe (Username c) -> Callbacks x
  callbacks _ = emptyCallbacks

instance {-# OVERLAPPABLE #-} Typeable x => DefaultCallbacks x

instance Typeable a => Default (Callbacks a) where
  def = emptyCallbacks
