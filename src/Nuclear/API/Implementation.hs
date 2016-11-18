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

emptyH = EndpointsNull

infixr 5 <@>
(<@>) :: hndlr self super endpoint -> Endpoints hndlr self super endpoints -> Endpoints hndlr self super (endpoint ': endpoints)
(<@>) = EndpointsCons
-- class EndpointCons (f :: [* -> *] -> (* -> *) -> [*] -> *) (g :: [* -> *] -> (* -> *) -> * -> *) (self :: [* -> *]) (super :: * -> *) (x :: *) (xs :: [*]) where
  -- (<@>) :: g self super x -> f self super xs -> f self super (x ': xs)

-- class GetEndpoint' hndlr self super (endpoint :: k) (endpoints :: [k]) (n :: Nat) where
--   getEndpoint' :: Index n -> Endpoints hndlr self super endpoints -> hndlr self super endpoint

-- instance endpoints ~ (endpoint ': xs)
class GetEndpoint' (hndlr :: [* -> *] -> (* -> *) -> * -> *) (endpoint :: *) (endpoints :: [*]) (n :: Nat) where
  getEndpoint' :: Index n
               -> Endpoints hndlr self super endpoints
               -> hndlr self super endpoint

instance endpoints ~ (endpoint ': xs)
    => GetEndpoint' hndlr endpoint endpoints 'Z
  where
    getEndpoint' _ (EndpointsCons h _) = h

instance ( index ~ Offset endpoint endpoints
         , GetEndpoint' hndlr endpoint endpoints index
         )
    => GetEndpoint' hndlr endpoint (endpoint ': endpoints) ('S n)
  where
    getEndpoint' _ (EndpointsCons _ es) =
      let index = Index :: Index (Offset endpoint endpoints)
      in getEndpoint' index es

class GetEndpoint hndlr endpoint endpoints where
  getEndpoint :: Endpoints hndlr self super endpoints
              -> hndlr self super endpoint

instance ( index ~ Offset endpoint endpoints
         , GetEndpoint' hndlr endpoint endpoints index
         )
    => GetEndpoint hndlr endpoint endpoints
  where
    getEndpoint es =
      let index = Index :: Index index
      in getEndpoint' index es

class (Remove endpoint endpoints ~ endpoints') => DeleteEndpoint hndlr endpoint endpoints endpoints' where
  deleteEndpoint :: Proxy endpoint -> Endpoints hndlr self super endpoints -> Endpoints hndlr self super endpoints'

instance (Remove endpoint (endpoint ': endpoints) ~ endpoints) => DeleteEndpoint hndlr endpoint (endpoint ': endpoints) endpoints where
  deleteEndpoint _ (EndpointsCons _ hs) = hs

instance ( Remove endpoint endpoints ~ endpoints'
         , DeleteEndpoint hndlr endpoint endpoints endpoints'
         , Remove endpoint (x ': endpoints) ~ endpoints''
         , endpoints'' ~ (x ': endpoints')
         ) => DeleteEndpoint hndlr endpoint (x ': endpoints) endpoints'' where
  deleteEndpoint p (EndpointsCons mh mhs) = EndpointsCons mh (deleteEndpoint p mhs)

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

data Implementation msg req self super messages requests =
  Implementation (Endpoints msg self super messages) (Endpoints req self super requests)

impl :: Endpoints msg self super messages -> Endpoints req self super requests -> Implementation msg req self super messages requests
impl = Implementation

emptyImpl = Implementation emptyH emptyH

data ActiveAPI self super messages requests =
  ActiveAPI (ActiveEndpoints self super messages) (ActiveEndpoints self super requests)

type family Equal a b :: Bool
  where

    Equal a a = 'True

    Equal a b = 'False
