module Pure.Conjurer.Listable (Listable(..),ShouldPreloadPreviews,Listing,toListWith,listing) where

import Pure.Conjurer.API
import Pure.Conjurer.Context
import Pure.Conjurer.Name
import Pure.Conjurer.Pathable
import Pure.Conjurer.Previewable
import Pure.Conjurer.Producible
import Pure.Conjurer.Resource
import Pure.Conjurer.Rootable
import Pure.Conjurer.Routable

import Control.Log (Logging)
import Data.Events
import Data.Exists
import Data.HTML
import Data.Maybe (fromMaybe)
import Data.Scroll
import Data.Theme 
import Data.JSON
import Control.Component hiding (root)
import Data.Router as Router
import Data.View
import qualified Data.Websocket as WS
import Effect.View
import Effect.Websocket hiding (sync)

import Control.Concurrent
import Control.Monad
import Data.Typeable

data Listing a
instance Theme Listing
instance {-# OVERLAPPABLE #-} Typeable a => Theme (Listing a)

class Listable _role resource where
  toList :: Context resource -> View
  default toList 
    :: ( Typeable _role
       , Typeable resource
       , Routable resource
       , Viewable (Preview resource)
       , FromJSON (Preview resource)
       , ToJSON (Context resource), FromJSON (Context resource), Ord (Context resource)
       , ToJSON (Name resource), FromJSON (Name resource), Ord (Name resource)
       , Eq (Context resource)
       , Pathable (Context resource), Pathable (Name resource)
       , Theme (Listing resource)
       , FromJSON (Product resource)
       , Logging
       ) => Context resource -> View
  toList = toListWith @_role Cached True 
      
type ShouldPreloadPreviews = Bool

toListWith 
  :: forall _role resource.
    ( Typeable _role
    , Typeable resource
    , Routable resource
    , Viewable (Preview resource)
    , FromJSON (Preview resource)
    , ToJSON (Context resource), FromJSON (Context resource), Ord (Context resource)
    , ToJSON (Name resource), FromJSON (Name resource), Ord (Name resource)
    , FromJSON (Product resource)
    , Eq (Context resource)
    , Theme (Listing resource)
    , Logging
    ) 
  => Policy -> ShouldPreloadPreviews -> Context resource -> View
toListWith policy shouldPreloadPreviews ctx =
  request @_role policy (readingAPI @resource) (readListing @resource) ctx do
    with @[(Context resource, Name resource, Preview resource)] (maybe [] (fmap (\(n,p) -> (ctx,n,p))) it) do
      listing @_role @resource policy shouldPreloadPreviews <| Themed @Listing . Themed @(Listing resource) 

listing 
  :: forall _role resource.
    ( Typeable _role
    , Typeable resource
    , Routable resource
    , Viewable (Preview resource)
    , FromJSON (Preview resource)
    , ToJSON (Context resource), FromJSON (Context resource), Ord (Context resource)
    , ToJSON (Name resource), FromJSON (Name resource), Ord (Name resource)
    , FromJSON (Product resource)
    , Eq (Context resource)
    , Exists [(Context resource,Name resource,Preview resource)]
    , Logging
    ) 
  => Policy -> ShouldPreloadPreviews -> View
listing policy shouldPreloadPreviews =
  Ul <||> 
    [ Li <| OnClickWith intercept (\_ -> storeScrollPosition >> goto r) . Href r . preload |> 
      [ toView (p :: Preview resource) ] 
    | (ctx,nm,p) <- it 
    , let 
        r = toReadRoute ctx nm
        preload
          | shouldPreloadPreviews = OnMouseDown load . OnTouchStart load
          | otherwise             = id
          where
            load _ = void $ forkIO $ void $
              req @_role policy (readingAPI @resource) 
                (readProduct @resource) 
                (ctx,nm)
    ]

instance {-# OVERLAPPABLE #-}
  ( Typeable _role
  , Typeable resource
  , Routable resource
  , Viewable (Preview resource)
  , FromJSON (Preview resource)
  , ToJSON (Context resource), FromJSON (Context resource), Ord (Context resource)
  , ToJSON (Name resource), FromJSON (Name resource), Ord (Name resource)
  , Eq (Context resource)
  , Pathable (Context resource), Pathable (Name resource)
  , Theme (Listing resource)
  , FromJSON (Product resource)
  , Logging
  ) => Listable _role resource
