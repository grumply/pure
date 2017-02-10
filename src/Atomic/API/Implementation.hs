{-# language UndecidableInstances #-}
{-# language OverlappingInstances #-}
module Atomic.API.Implementation where

import Ef.Base
import Ef.Type.Nat

import Atomic.Request
import Atomic.Message
import Atomic.Endpoint

import Atomic.API.Interface

import Data.Proxy
import Data.Typeable

data Endpoints (hndlr :: [* -> *] -> (* -> *) -> * -> *) (ms :: [* -> *]) (c :: * -> *) (es :: [*])
  where
    EndpointsNull
      :: Endpoints hndlr ms c '[]

    EndpointsCons
      :: hndlr ms c e
      -> Endpoints hndlr ms c es
      -> Endpoints hndlr ms c (e ': es)

instance EmptyDefault (Endpoints hndlr ms c) where
  none = EndpointsNull

instance EmptyDefault (API f) where
  none = APINull

instance Build (hndlr ms c :: * -> *) (Endpoints hndlr ms c :: [*] -> *) where
  (<:>) = EndpointsCons

instance (Appended '[] ys ~ ys) => TListAppend (Endpoints hndlr ms c) '[] ys ys where
  (<++>) _ ys = ys

instance ( Removed (y ': ys) x ~ (y ': ys)
         , Appended (x ': xs) (y ': ys) ~ (x ': zs)
         , TListAppend (Endpoints hndlr ms c) xs (y ': ys) zs
         )
    => TListAppend (Endpoints hndlr ms c) (x ': xs) (y ': ys) (x ': zs)
  where
    (<++>) (EndpointsCons x xs) ys = EndpointsCons x (xs <++> ys)


-- instance es ~ (e ': xs)
class GetHandler' (hndlr :: [* -> *] -> (* -> *) -> * -> *) (e :: *) (es :: [*]) (n :: Nat) where
  getHandler' :: Index n
               -> Endpoints hndlr ms c es
               -> hndlr ms c e

instance es ~ (e ': xs)
    => GetHandler' hndlr e es 'Z
  where
    getHandler' _ (EndpointsCons h _) = h

instance ( index ~ Offset es e
         , GetHandler' hndlr e es index
         )
    => GetHandler' hndlr e (x ': es) ('S n)
  where
    getHandler' _ (EndpointsCons _ es) =
      let index = Index :: Index index
      in getHandler' index es

class GetHandler hndlr e es where
  getHandler :: Endpoints hndlr ms c es
             -> hndlr ms c e

instance ( index ~ Offset es e
         , GetHandler' hndlr e es index
         )
    => GetHandler hndlr e es
  where
    getHandler es =
      let index = Index :: Index index
      in getHandler' index es

class (Removed es e ~ es')
    => DeleteHandler hndlr e es es'
  where
    deleteHandler :: Proxy e
                  -> Endpoints hndlr ms c es
                  -> Endpoints hndlr ms c es'

instance (Removed (e ': es) e ~ es)
    => DeleteHandler hndlr e (e ': es) es
  where
  deleteHandler _ (EndpointsCons _ hs) = hs

instance ( DeleteHandler hndlr e es es''
         , Removed (x ': es) e ~ es'
         , es' ~ (x ': es'')
         ) => DeleteHandler hndlr e (x ': es) es' where
  deleteHandler p (EndpointsCons mh mhs) = EndpointsCons mh (deleteHandler p mhs)

data ActiveEndpoints ms c es
  where
    ActiveEndpointsNull
      :: ActiveEndpoints ms c '[]

    ActiveEndpointsCons
      :: Proxy e
      -> Endpoint ms c a
      -> ActiveEndpoints ms c es
      -> ActiveEndpoints ms c (e ': es)

class EnactEndpoints api_ty hndlr ms c es es' where
  -- we take api_ty's es to be our fixed basis
  enactEndpoints :: api_ty es
                 -> Endpoints hndlr ms c es'
                 -> Code ms c (ActiveEndpoints ms c es)

instance (Monad c, Functor (Messages ms)) => EnactEndpoints api_ty hndlr ms c '[] '[] where
  enactEndpoints _ _ = return ActiveEndpointsNull

data ActiveAPI ms c messages requests =
  ActiveAPI (ActiveEndpoints ms c messages) (ActiveEndpoints ms c requests)

type family Equal a b :: Bool
  where

    Equal a a = 'True

    Equal a b = 'False
