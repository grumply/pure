{-# language UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Pure.WebSocket.API.Interface
  ( module Pure.WebSocket.API.Interface
  , module Pure.WebSocket.API.ProxyList
  , module Ef.Type.List
  ) where

import Data.Proxy
import Ef.Type.List

import Pure.WebSocket.Message
import Pure.WebSocket.Request

import Pure.WebSocket.API.ProxyList

import GHC.Exts

data API (f :: k -> Constraint) (es :: [k])
  where
    APINull
      :: API f '[]

    APICons
      :: f e
      => Proxy e
      -> API f es
      -> API f (e ': es)

class (Removed es e ~ es')
    => DeleteEndpoint f e es es'
  where
    deleteEndpoint :: Proxy e -> API f es -> API f es'

instance (Removed (e ': es) e ~ es)
    => DeleteEndpoint f e (e ': es) es
  where
    deleteEndpoint _ (APICons _ mapi) = mapi

instance ( Removed es e ~ es'
         , DeleteEndpoint f e es es'
         , Removed (x ': es) e ~ es''
         , es'' ~ (x ': es')
         ) => DeleteEndpoint f e (x ': es) es'' where
  deleteEndpoint p (APICons mh mapi) =
    APICons mh (deleteEndpoint p mapi)

instance ( Appended (x ': xs) (y ': ys) ~ zs
         , Appended (x ': xs) (y ': ys) ~ (xy ': xys)
         , TListAppend (API f) xs (y ': ys) xys
         )
    => TListAppend (API f) (x ': xs) (y ': ys) zs
  where
    (<++>) (APICons x xs) ys = APICons x (xs <++> ys)

type MessageAPI = API Message
type RequestAPI = API Request

class ToAPI f es where
  toAPI :: PList es -> API f es

instance ToAPI Request '[] where
  toAPI _ = APINull

instance ToAPI Message '[] where
  toAPI _ = APINull

instance (Message x, ToAPI Message xs) => ToAPI Message (x ': xs) where
  toAPI (PCons p ps) = APICons p (toAPI ps)

instance (Request x, ToAPI Request xs) => ToAPI Request (x ': xs) where
  toAPI (PCons p ps) = APICons p (toAPI ps)

class FromAPI f es where
  fromAPI :: API f es -> PList es

instance FromAPI Request '[] where
  fromAPI _ = PNull

instance FromAPI Message '[] where
  fromAPI _ = PNull

instance (Message x, FromAPI Message xs) => FromAPI Message (x ': xs) where
  fromAPI (APICons p ps) = PCons p (fromAPI ps)

instance (Request x, FromAPI Request xs) => FromAPI Request (x ': xs) where
  fromAPI (APICons p ps) = PCons p (fromAPI ps)

data FullAPI messages requests
  = API { messageAPI :: MessageAPI messages
        , requestAPI :: RequestAPI requests
        }

class DeriveMessageAPI msgs where
  deriveMessageAPI :: PList msgs
instance DeriveMessageAPI '[] where
  deriveMessageAPI = PNull
instance (Message x, DeriveMessageAPI xs) => DeriveMessageAPI (x ': xs) where
  deriveMessageAPI = PCons (Proxy :: Proxy x) deriveMessageAPI

class DeriveRequestAPI reqs where
  deriveRequestAPI :: PList reqs
instance DeriveRequestAPI '[] where
  deriveRequestAPI = PNull
instance (Request x, DeriveRequestAPI xs) => DeriveRequestAPI (x ': xs) where
  deriveRequestAPI = PCons (Proxy :: Proxy x) deriveRequestAPI

deriveAPI :: (ToAPI Message msgs, ToAPI Request reqs, DeriveMessageAPI msgs, DeriveRequestAPI reqs) => FullAPI msgs reqs
deriveAPI = api deriveMessageAPI deriveRequestAPI

api :: (ToAPI Message ms, ToAPI Request rs) => PList ms -> PList rs -> FullAPI ms rs
api msgs reqs = API (toAPI msgs) (toAPI reqs)

(<:+:>) :: ( FromAPI Message ms
           , FromAPI Message ms'
           , FromAPI Request rs
           , FromAPI Request rs'
           , TListAppend PList ms ms' ms''
           , TListAppend PList rs rs' rs''
           , ToAPI Message ms''
           , ToAPI Request rs''
           )
        => FullAPI ms rs -> FullAPI ms' rs' -> FullAPI (Appended ms ms') (Appended rs rs')
(<:+:>) (API msl rsl) (API msr rsr) =
  api (fromAPI msl <++> fromAPI msr) (fromAPI rsl <++> fromAPI rsr)
