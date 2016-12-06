{-# language UndecidableInstances #-}
{-# language OverlappingInstances #-}
module Nuclear.API.Implementation where

import Ef.Base
import Ef.Type.Nat

import Nuclear.Request
import Nuclear.Message
import Nuclear.Endpoint

import Nuclear.API.Interface

import Data.Proxy

data Endpoints (hndlr :: [* -> *] -> (* -> *) -> * -> *) (self :: [* -> *]) (super :: * -> *) (endpoints :: [*])
  where
    EndpointsNull
      :: Endpoints hndlr self super '[]

    EndpointsCons
      :: hndlr self super endpoint
      -> Endpoints hndlr self super endpoints
      -> Endpoints hndlr self super (endpoint ': endpoints)

instance EmptyDefault (Endpoints hndlr self super) where
  none = EndpointsNull

instance EmptyDefault (API f) where
  none = APINull

instance Build (hndlr self super :: * -> *) (Endpoints hndlr self super :: [*] -> *) where
  (<:>) = EndpointsCons

instance ( Appended (x ': xs) (y ': ys) ~ zs
         , Appended (x ': xs) (y ': ys) ~ (xy ': xys)
         , Append (Endpoints hndlr self super) xs (y ': ys) xys
         )
    => Append (Endpoints hndlr self super) (x ': xs) (y ': ys) zs
  where
    (<++>) (EndpointsCons x xs) ys = EndpointsCons x (xs <++> ys)

-- instance endpoints ~ (endpoint ': xs)
class GetHandler' (hndlr :: [* -> *] -> (* -> *) -> * -> *) (endpoint :: *) (endpoints :: [*]) (n :: Nat) where
  getHandler' :: Index n
               -> Endpoints hndlr self super endpoints
               -> hndlr self super endpoint

instance endpoints ~ (endpoint ': xs)
    => GetHandler' hndlr endpoint endpoints 'Z
  where
    getHandler' _ (EndpointsCons h _) = h

instance ( index ~ Offset endpoint endpoints
         , GetHandler' hndlr endpoint endpoints index
         )
    => GetHandler' hndlr endpoint (x ': endpoints) ('S n)
  where
    getHandler' _ (EndpointsCons _ es) =
      let index = Index :: Index (Offset endpoint endpoints)
      in getHandler' index es

class GetHandler hndlr endpoint endpoints where
  getHandler :: Endpoints hndlr self super endpoints
              -> hndlr self super endpoint

instance ( index ~ Offset endpoint endpoints
         , GetHandler' hndlr endpoint endpoints index
         )
    => GetHandler hndlr endpoint endpoints
  where
    getHandler es =
      let index = Index :: Index index
      in getHandler' index es

class (Removed endpoint endpoints ~ endpoints')
    => DeleteHandler hndlr endpoint endpoints endpoints'
  where
    deleteHandler :: Proxy endpoint
                   -> Endpoints hndlr self super endpoints
                   -> Endpoints hndlr self super endpoints'

instance (Removed endpoint (endpoint ': endpoints) ~ endpoints)
    => DeleteHandler hndlr endpoint (endpoint ': endpoints) endpoints
  where
  deleteHandler _ (EndpointsCons _ hs) = hs

instance ( DeleteHandler hndlr endpoint endpoints endpoints''
         , Removed endpoint (x ': endpoints) ~ endpoints'
         , endpoints' ~ (x ': endpoints'')
         ) => DeleteHandler hndlr endpoint (x ': endpoints) endpoints' where
  deleteHandler p (EndpointsCons mh mhs) = EndpointsCons mh (deleteHandler p mhs)

data ActiveEndpoints self super endpoints
  where
    ActiveEndpointsNull
      :: ActiveEndpoints self super '[]

    ActiveEndpointsCons
      :: Proxy endpoint
      -> Endpoint self super
      -> ActiveEndpoints self super endpoints
      -> ActiveEndpoints self super (endpoint ': endpoints)

class EnactEndpoints api_ty hndlr self super endpoints endpoints' where
  -- we take api_ty's endpoints to be our fixed basis
  enactEndpoints :: api_ty endpoints
                 -> Endpoints hndlr self super endpoints'
                 -> Narrative self super (ActiveEndpoints self super endpoints)

instance (Monad super) => EnactEndpoints api_ty hndlr self super '[] '[] where
  enactEndpoints _ _ = return ActiveEndpointsNull

data ActiveAPI self super messages requests =
  ActiveAPI (ActiveEndpoints self super messages) (ActiveEndpoints self super requests)

type family Equal a b :: Bool
  where

    Equal a a = 'True

    Equal a b = 'False
