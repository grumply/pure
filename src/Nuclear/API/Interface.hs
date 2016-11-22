{-# language UndecidableInstances #-}
module Nuclear.API.Interface
  ( module Nuclear.API.Interface
  , module Nuclear.API.ProxyList
  , module Nuclear.API.TypeList
  ) where

import Data.Proxy

import Unsafe.Coerce

import Nuclear.Message
import Nuclear.Request

import Nuclear.API.ProxyList
import Nuclear.API.TypeList

import GHC.Exts

data API (f :: k -> Constraint) (endpoints :: [k])
  where
    APINull
      :: API f '[]

    APICons
      :: f endpoint
      => Proxy endpoint
      -> API f endpoints
      -> API f (endpoint ': endpoints)

class (Removed endpoint endpoints ~ endpoints')
    => DeleteEndpoint f endpoint endpoints endpoints'
  where
    deleteEndpoint :: Proxy endpoint -> API f endpoints -> API f endpoints'

instance (Removed endpoint (endpoint ': endpoints) ~ endpoints)
    => DeleteEndpoint f endpoint (endpoint ': endpoints) endpoints
  where
    deleteEndpoint _ (APICons _ mapi) = mapi

instance ( Removed endpoint endpoints ~ endpoints'
         , DeleteEndpoint f endpoint endpoints endpoints'
         , Removed endpoint (x ': endpoints) ~ endpoints''
         , endpoints'' ~ (x ': endpoints')
         ) => DeleteEndpoint f endpoint (x ': endpoints) endpoints'' where
  deleteEndpoint p (APICons mh mapi) =
    APICons mh (deleteEndpoint p mapi)

type MessageAPI = API Message
type RequestAPI = API Request

class ToAPI f endpoints where
  toAPI :: PList endpoints -> API f endpoints

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

data FullAPI messages requests
  = API (MessageAPI messages) (RequestAPI requests)

api msgs reqs = API (toAPI msgs) (toAPI reqs)

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
