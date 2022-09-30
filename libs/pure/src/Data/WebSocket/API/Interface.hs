{-# language UndecidableInstances #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Data.Websocket.API.Interface
  ( module Data.Websocket.API.Interface
  , module Data.Websocket.API.ProxyList
  ) where

import Data.Proxy
import Data.Websocket.Message
import Data.Websocket.Request
import Data.Websocket.API.ProxyList
import GHC.Exts

data Interface (f :: k -> Constraint) (es :: [k])
  where
    InterfaceNull
      :: Interface f '[]

    InterfaceCons
      :: f e
      => Proxy e
      -> Interface f es
      -> Interface f (e ': es)

class (Removed es e ~ es')
    => DeleteEndpoint f e es es'
  where
    deleteEndpoint :: Proxy e -> Interface f es -> Interface f es'

instance (Removed (e ': es) e ~ es)
    => DeleteEndpoint f e (e ': es) es
  where
    deleteEndpoint _ (InterfaceCons _ mapi) = mapi

instance ( Removed es e ~ es'
         , DeleteEndpoint f e es es'
         , Removed (x ': es) e ~ es''
         , es'' ~ (x ': es')
         ) => DeleteEndpoint f e (x ': es) es'' where
  deleteEndpoint p (InterfaceCons mh mapi) =
    InterfaceCons mh (deleteEndpoint p mapi)

class RemoveInterface ws api_ty xs where
  removeInterface :: ws -> api_ty xs -> IO ()

instance RemoveInterface ws api_ty '[] where
  removeInterface _ _ = pure ()

instance ( Appended (x ': xs) (y ': ys) ~ zs
         , Appended (x ': xs) (y ': ys) ~ (xy ': xys)
         , TListAppend (Interface f) xs (y ': ys) xys
         )
    => TListAppend (Interface f) (x ': xs) (y ': ys) zs
  where
    (<+++>) (InterfaceCons x xs) ys = InterfaceCons x (xs <+++> ys)

class ToInterface f es where
  toInterface :: PList es -> Interface f es

instance ToInterface Request '[] where
  toInterface _ = InterfaceNull

instance ToInterface Message '[] where
  toInterface _ = InterfaceNull

instance (Message x, ToInterface Message xs) => ToInterface Message (x ': xs) where
  toInterface (PCons p ps) = InterfaceCons p (toInterface ps)

instance (Request x, ToInterface Request xs) => ToInterface Request (x ': xs) where
  toInterface (PCons p ps) = InterfaceCons p (toInterface ps)

class FromInterface f es where
  fromInterface :: Interface f es -> PList es

instance FromInterface Request '[] where
  fromInterface _ = PNull

instance FromInterface Message '[] where
  fromInterface _ = PNull

instance (Message x, FromInterface Message xs) => FromInterface Message (x ': xs) where
  fromInterface (InterfaceCons p ps) = PCons p (fromInterface ps)

instance (Request x, FromInterface Request xs) => FromInterface Request (x ': xs) where
  fromInterface (InterfaceCons p ps) = PCons p (fromInterface ps)

data API messages requests
  = API { messages :: Interface Message messages
        , requests :: Interface Request requests
        }

class DeriveMessageInterface msgs where
  deriveMessageInterface :: PList msgs
instance DeriveMessageInterface '[] where
  deriveMessageInterface = PNull
instance (Message x, DeriveMessageInterface xs) => DeriveMessageInterface (x ': xs) where
  deriveMessageInterface = PCons (Proxy :: Proxy x) deriveMessageInterface

class DeriveRequestInterface reqs where
  deriveRequestInterface :: PList reqs
instance DeriveRequestInterface '[] where
  deriveRequestInterface = PNull
instance (Request x, DeriveRequestInterface xs) => DeriveRequestInterface (x ': xs) where
  deriveRequestInterface = PCons (Proxy :: Proxy x) deriveRequestInterface

deriveAPI :: (ToInterface Message msgs, ToInterface Request reqs, DeriveMessageInterface msgs, DeriveRequestInterface reqs) => API msgs reqs
deriveAPI = api deriveMessageInterface deriveRequestInterface

api :: (ToInterface Message ms, ToInterface Request rs) => PList ms -> PList rs -> API ms rs
api msgs reqs = API (toInterface msgs) (toInterface reqs)

(<:+:>) :: ( FromInterface Message ms
           , FromInterface Message ms'
           , FromInterface Request rs
           , FromInterface Request rs'
           , TListAppend PList ms ms' ms''
           , TListAppend PList rs rs' rs''
           , ToInterface Message ms''
           , ToInterface Request rs''
           )
        => API ms rs -> API ms' rs' -> API ms'' rs''
(<:+:>) (API msl rsl) (API msr rsr) =
  api (fromInterface msl <+++> fromInterface msr) (fromInterface rsl <+++> fromInterface rsr)
