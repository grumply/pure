module Pure.Conjurer.Routable where

import Pure.Conjurer.Context
import Pure.Conjurer.Name
import Pure.Conjurer.Pathable
import Pure.Conjurer.Rootable

import Data.Txt
import Data.Router

import Control.Monad
import Control.Monad.IO.Class
import Data.Typeable

class Routable resource where
  createRoute :: (Context resource -> rt) -> Routing rt ()
  default createRoute 
    :: ( Rootable resource, Pathable (Context resource)
       ) => (Context resource -> rt) -> Routing rt ()
  createRoute f =
    void do
      path (root @resource) do
        path "/new" do
          mctx <- fromPath
          case mctx of
            Just ctx -> dispatch (f ctx)
            Nothing  -> continue

  toCreateRoute :: Context resource -> Txt
  default toCreateRoute 
    :: ( Rootable resource, Pathable (Context resource)
       ) => Context resource -> Txt
  toCreateRoute ctx = root @resource <> "/new" <> toPath ctx

  readRoute :: (Context resource -> Name resource -> rt) -> Routing rt ()
  default readRoute 
    :: ( Rootable resource, Pathable (Context resource), Pathable (Name resource)
       ) => (Context resource -> Name resource -> rt) -> Routing rt ()
  readRoute f =
    void do
      path (root @resource) do
        mctx <- fromPath
        mnm  <- fromPath
        case (,) <$> mctx <*> mnm of
          Just (ctx,nm) -> dispatch (f ctx nm)
          Nothing       -> continue

  toReadRoute :: Context resource -> Name resource -> Txt
  default toReadRoute 
    :: ( Rootable resource, Pathable (Context resource), Pathable (Name resource)
       ) => Context resource -> Name resource -> Txt
  toReadRoute ctx nm = root @resource <> toPath ctx <> toPath nm

  updateRoute :: (Context resource -> Name resource -> rt) -> Routing rt ()
  default updateRoute 
    :: ( Rootable resource, Pathable (Context resource), Pathable (Name resource)
       ) => (Context resource -> Name resource -> rt) -> Routing rt ()
  updateRoute f =
    void do
      path (root @resource) do
        path "/update" do
          mctx <- fromPath
          mn   <- fromPath
          case (,) <$> mctx <*> mn of
            Just (ctx,nm) -> dispatch (f ctx nm)
            Nothing       -> continue

  toUpdateRoute :: Context resource -> Name resource -> Txt
  default toUpdateRoute 
    :: ( Rootable resource, Pathable (Context resource), Pathable (Name resource)
       ) => Context resource -> Name resource -> Txt
  toUpdateRoute ctx nm = root @resource <> "/update" <> toPath ctx <> toPath nm 

  listRoute :: (Context resource -> rt) -> Routing rt ()
  default listRoute 
    :: ( Rootable resource, Pathable (Context resource)
       ) => (Context resource -> rt) -> Routing rt ()
  listRoute f =
    void do
      path (root @resource) do
        mctx <- fromPath 
        case mctx of
          Just ctx -> dispatch (f ctx)
          Nothing  -> continue

  toListRoute :: Context resource -> Txt
  default toListRoute 
    :: ( Rootable resource, Pathable (Context resource)
       ) => Context resource -> Txt
  toListRoute ctx = root @resource <> toPath ctx