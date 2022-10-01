module Pure.Conjurer.Readable (Readable(..),Reading,toReadWith) where

import Pure.Conjurer.API
import Pure.Conjurer.Context
import Pure.Conjurer.Pathable
import Pure.Conjurer.Producible
import Pure.Conjurer.Resource
import Pure.Conjurer.Rootable

import qualified Data.View
import Data.JSON
import Control.Component hiding (root)
import Control.State
import Data.Router as Router
import Data.Theme
import qualified Data.Websocket as WS
import Data.View hiding (get)
import Data.HTML
import Effect.Async
import Effect.Websocket

import Control.Concurrent
import Data.Typeable

import Prelude hiding (Read)

data Reading
instance Theme Reading

class Readable _role resource where
  toRead :: Context resource -> Name resource -> View
  default toRead 
    :: ( Typeable resource, Typeable _role
       , Viewable (Product resource)
       , FromJSON (Context resource), ToJSON (Context resource), Ord (Context resource)
       , FromJSON (Name resource), ToJSON (Name resource), Ord (Name resource)
       , FromJSON (Product resource)
       , Eq (Context resource)
       , Eq (Name resource)
       , Pathable (Context resource)
       ) => Context resource -> Name resource -> View
  toRead = toReadWith @_role Cached $ \_ _ -> 
    maybe "Not Found" (\x -> Div <| Themed @Reading |> [ toView x ])

toReadWith
  :: forall _role resource.
    ( Typeable _role
    , Typeable resource
    , FromJSON (Context resource), ToJSON (Context resource), Ord (Context resource)
    , FromJSON (Name resource), ToJSON (Name resource), Ord (Name resource)
    , FromJSON (Product resource)
    , Eq (Context resource)
    , Eq (Name resource)
    ) 
  => Policy -> (Context resource -> Name resource -> Maybe (Product resource) -> View) -> Context resource -> Name resource -> View
toReadWith policy f ctx nm = request @_role policy (readingAPI @resource) (readProduct @resource) (ctx,nm) (f ctx nm await)

instance {-# OVERLAPPABLE #-}
  ( Typeable resource, Typeable _role
  , Viewable (Product resource)
  , FromJSON (Context resource), ToJSON (Context resource), Ord (Context resource)
  , FromJSON (Name resource), ToJSON (Name resource), Ord (Name resource)
  , FromJSON (Product resource)
  , Eq (Context resource)
  , Eq (Name resource)
  , Pathable (Context resource)
  ) => Readable _role resource