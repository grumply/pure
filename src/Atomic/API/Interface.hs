{-# language UndecidableInstances #-}
module Atomic.API.Interface
  ( module Atomic.API.Interface
  , module Atomic.API.ProxyList
  , module Ef.Type.List
  ) where

import Data.Proxy
import Ef.Type.List

import Unsafe.Coerce

import Atomic.Message
import Atomic.Request

import Atomic.API.ProxyList

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

-- instance ToAPI f '[] where
--   toAPI _ = APINull

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

api msgs reqs = API (toAPI msgs) (toAPI reqs)

(<:+:>) (API msl rsl) (API msr rsr) =
  api (fromAPI msl <++> fromAPI msr) (fromAPI rsl <++> fromAPI rsr)

{- Example API

data Manager_ a
manager :: Proxy Manager_
manager = Proxy

data Server_ a
server :: Proxy Server_
server = Proxy

data Put_ a
put :: Proxy Put_
put = Proxy

data Get_ a
get :: Proxy Get_
get = Proxy

data Post_ a
post :: Proxy Post_
post = Proxy

int :: Proxy Int
int = Proxy

double :: Proxy Double
double = Proxy

-- type MAPI
--   = '[ Manager_ (Post_ Int)
--      , Manager_ (Put_ Int)
--      , Manager_ (Put_ Double)
--      , Server_ (Post_ Double)
--      , Server_ (Put_ Double)
--      ]

-- type NAPI
--   = '[ Manager_ (Get_ Int)
--      , Manager_ (Get_ Double)
--      , Server_ (Get_ Double)
--      ]

-- theAPI :: FullAPI MAPI NAPI

theAPI = api mesages nuclear
  where

    messages =
           manager |>
                  post <&> put <| int
             <++> only put <| double
      <||> server |>
            post <&> put <| double

    nuclear =
           manager |> get |>
             int <&> double
      <||> server |> get |>
             only double

-}
