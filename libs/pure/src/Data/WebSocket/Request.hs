{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Websocket.Request where

import Data.Monoid
import Data.Txt
import Data.Typeable
import Data.Websocket.Identify
import Data.Websocket.TypeRep

class (Typeable (requestType :: *)) => Request requestType where
  type Req requestType :: *
  type Rsp requestType :: *

  requestHeader :: Proxy requestType -> Txt
  {-# INLINE requestHeader #-}
  default requestHeader :: Proxy requestType -> Txt
  requestHeader = rep

  responseHeader :: (Req requestType ~ request) => Proxy requestType -> request -> Txt
  {-# INLINE responseHeader #-}
  default responseHeader :: ( Req requestType ~ request
                            , Identify request
                            , I request ~ requestIdentity
                            , ToTxt requestIdentity
                            )
                        => Proxy requestType -> request -> Txt
  responseHeader = rspHdr

rspHdr :: ( Typeable requestType
                , Request requestType
                , Req requestType ~ request
                , Identify request
                , I request ~ requestIdentity
                , ToTxt requestIdentity
                )
             => Proxy requestType -> request -> Txt
rspHdr rqty_proxy req = rep rqty_proxy <> " " <> toTxt (identify req)