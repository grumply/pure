module Pure.Conjurer.Creatable (Creatable(..),Creating) where

import Pure.Conjurer.API
import Pure.Conjurer.Context
import Pure.Conjurer.Formable
import Pure.Conjurer.Name
import Pure.Conjurer.Pathable
import Pure.Conjurer.Producible
import Pure.Conjurer.Previewable
import Pure.Conjurer.Resource
import Pure.Conjurer.Rootable
import Pure.Conjurer.Routable
import Pure.Conjurer.Updatable (Previewing)

import Control.Component hiding (root)
import Data.Default
import Data.HTML
import Data.JSON
import Data.Theme
import Data.Router as Router
import Data.View
import Pure.Auth (Authentication,guarded,basic)
import Effect.Websocket

import Data.Foldable
import Data.Typeable

data Creating a
instance Theme Creating
instance {-# OVERLAPPABLE #-} Typeable a => Theme (Creating a)

class Creatable _role resource where
  toCreate :: Context resource -> View
  default toCreate 
    :: ( Typeable resource, Typeable _role
       , Routable resource
       , ToJSON (Resource resource), FromJSON (Resource resource), Default (Resource resource), Ord (Resource resource)
       , ToJSON (Context resource), FromJSON (Context resource), Ord (Context resource)
       , ToJSON (Name resource), FromJSON (Name resource), Ord (Name resource)
       , FromJSON (Preview resource)
       , FromJSON (Product resource)
       , Formable (Resource resource)
       , Pure (Preview resource)
       , Pure (Product resource)
       , Theme (Creating resource)
       , Theme (Previewing resource)
       , Authentication _role
       ) => Context resource -> View
  toCreate ctx =
    guarded @_role Null (basic @_role) do
      Div <| Themed @Creating . Themed @(Creating resource) |>
        [ form onSubmit onPreview def
        ]
    where
      onPreview resource = do
        r <- req @_role Uncached (publishingAPI @resource) (previewResource @resource) (ctx,resource)
        case r of
          Nothing -> pure "Failed to preview."
          Just (ctx,nm,pre,pro,res) -> pure do
            Div <| Themed @Previewing . Themed @(Previewing resource) |>
              [ View pre
              , View pro
              ]

      onSubmit resource = do
        mi <- req @_role Uncached (publishingAPI @resource) (createResource @resource) (ctx,resource)
        for_ mi $ \nm -> do
          flush @_role (readingAPI @resource) (readPreview @resource) (ctx,nm)
          flush @_role (readingAPI @resource) (readProduct @resource) (ctx,nm)
          Router.goto (toReadRoute ctx nm)

instance {-# OVERLAPPABLE #-}
  ( Typeable resource, Typeable _role
  , Routable resource
  , ToJSON (Resource resource), FromJSON (Resource resource), Default (Resource resource), Ord (Resource resource)
  , ToJSON (Context resource), FromJSON (Context resource), Ord (Context resource)
  , ToJSON (Name resource), FromJSON (Name resource), Ord (Name resource)
  , FromJSON (Preview resource)
  , FromJSON (Product resource)
  , Formable (Resource resource)
  , Pure (Preview resource)
  , Pure (Product resource)
  , Theme (Creating resource)
  , Theme (Previewing resource)
  , Authentication _role
  ) => Creatable _role resource
