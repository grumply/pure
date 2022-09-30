module Pure.Conjurer.Updatable (Updatable(..),Previewing,Updating,cachingToUpdate) where

import Pure.Conjurer.API
import Pure.Conjurer.Context
import Pure.Conjurer.Formable
import Pure.Conjurer.Pathable
import Pure.Conjurer.Previewable
import Pure.Conjurer.Producible
import Pure.Conjurer.Resource
import Pure.Conjurer.Rootable
import Pure.Conjurer.Routable

import Pure.Auth (Authentication,guarded,basic)
import Data.JSON
import Data.HTML
import Data.Theme
import Data.View
import Control.Component hiding (root,Update)
import Data.Router as Router
import qualified Data.Websocket as WS
import Effect.Async
import Effect.Websocket hiding (sync)
import Effect.Router as Router (goto)

import Control.Concurrent
import Control.Monad
import Data.Typeable

data Updating a
data Previewing a
instance Theme Updating
instance {-# OVERLAPPABLE #-} Typeable a => Theme (Updating a)
instance Theme Previewing
instance {-# OVERLAPPABLE #-} Typeable a => Theme (Previewing a)

class Updatable _role resource where
  toUpdate :: Context resource -> Name resource -> View
  default toUpdate 
    :: ( Typeable resource, Typeable _role
       , Routable resource
       , ToJSON (Context resource), FromJSON (Context resource), Ord (Context resource)
       , ToJSON (Name resource), FromJSON (Name resource), Ord (Name resource)
       , ToJSON (Resource resource), FromJSON (Resource resource), Ord (Resource resource)
       , FromJSON (Preview resource)
       , FromJSON (Product resource)
       , Formable (Resource resource)
       , Pure (Preview resource)
       , Pure (Product resource)
       , Theme (Updating resource)
       , Theme (Previewing resource)
       , Authentication _role
       ) => Context resource -> Name resource -> View
  toUpdate ctx nm =
    guarded @_role Null (basic @_role) do
      request @_role Uncached (publishingAPI @resource) (readResource @resource) (ctx,nm) do
        case await of
          Just x -> Div <| Themed @Updating . Themed @(Updating resource) |> [ form onSubmit onPreview x ]
          _ -> "Not Found"
    where
      onPreview resource =
        req @_role Uncached (publishingAPI @resource) (previewResource @resource) (ctx,resource) >>= \case
          Nothing -> pure "Failed to preview."
          Just (ctx,nm,pre,pro,res) -> pure do
            Div <| Themed @Previewing . Themed @(Previewing resource) |>
              [ View pre
              , View pro
              ]

      onSubmit resource = do
        did <- req @_role Uncached (publishingAPI @resource) (updateResource @resource) (ctx,nm,resource)
        flush @_role (readingAPI @resource) (readPreview @resource) (ctx,nm)
        flush @_role (readingAPI @resource) (readProduct @resource) (ctx,nm)
        when did do
          Router.goto (toReadRoute ctx nm)

cachingToUpdate 
  :: forall _role resource.
    ( Typeable resource, Typeable _role
    , Routable resource
    , ToJSON (Context resource), FromJSON (Context resource), Ord (Context resource)
    , ToJSON (Name resource), FromJSON (Name resource), Ord (Name resource)
    , ToJSON (Resource resource), FromJSON (Resource resource), Ord (Resource resource)
    , FromJSON (Preview resource)
    , FromJSON (Product resource)
    , Formable (Resource resource)
    , Pure (Preview resource)
    , Pure (Product resource)
    , Theme (Updating resource)
    , Theme (Previewing resource)
    , Authentication _role
    ) => Context resource -> Name resource -> View
cachingToUpdate ctx nm =
  guarded @_role Null (basic @_role) do
    async (req @_role Uncached (publishingAPI @resource) (readResource @resource) (ctx,nm)) do
      case await of
        Just x -> Div <| Themed @Updating . Themed @(Updating resource) |> [ form onSubmit onPreview x ]
        _ -> "Not Found"
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
      did <- req @_role Uncached (publishingAPI @resource) (updateResource @resource) (ctx,nm,resource)
      when did do
        flush @_role (readingAPI @resource) (readPreview @resource) (ctx,nm)
        flush @_role (readingAPI @resource) (readProduct @resource) (ctx,nm)
        Router.goto (toReadRoute ctx nm)

instance {-# OVERLAPPABLE #-}
  ( Typeable _role
  , Typeable resource
  , ToJSON (Context resource), FromJSON (Context resource), Pathable (Context resource), Ord (Context resource)
  , ToJSON (Name resource), FromJSON (Name resource), Pathable (Name resource), Ord (Name resource)
  , ToJSON (Resource resource), FromJSON (Resource resource), Ord (Resource resource)
  , FromJSON (Preview resource)
  , FromJSON (Product resource)
  , Formable (Resource resource)
  , Pure (Preview resource)
  , Pure (Product resource)
  , Theme (Updating resource)
  , Theme (Previewing resource)
  , Authentication _role
  ) => Updatable _role resource