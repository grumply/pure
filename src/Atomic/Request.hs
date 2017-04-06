{-# language OverloadedStrings #-}
module Atomic.Request where

import Atomic.TypeRep

import Data.Txt
import Data.Monoid
import Data.Typeable

import Atomic.ToTxt
import Atomic.Identify

class (Typeable (requestType :: *)) => Request requestType where
  type Req requestType :: *
  type Rsp requestType :: *

  requestHeader :: Proxy requestType -> Txt
  {-# INLINE requestHeader #-}
  default requestHeader :: Proxy requestType -> Txt
  requestHeader = qualReqHdr

  responseHeader :: (Req requestType ~ request) => Proxy requestType -> request -> Txt
  {-# INLINE responseHeader #-}
  default responseHeader :: ( Req requestType ~ request
                            , Identify request
                            , I request ~ requestIdentity
                            , ToTxt requestIdentity
                            )
                        => Proxy requestType -> request -> Txt
  responseHeader = qualRspHdr

simpleReqHdr :: forall (requestType :: *). Typeable requestType => Proxy requestType -> Txt
simpleReqHdr = rep

qualReqHdr :: forall (requestType :: *). Typeable requestType => Proxy requestType -> Txt
qualReqHdr = qualRep

fullReqHdr :: forall (requestType :: *). Typeable requestType => Proxy requestType -> Txt
fullReqHdr = fullRep

simpleRspHdr :: ( Typeable requestType
                , Request requestType
                , Req requestType ~ request
                , Identify request
                , I request ~ requestIdentity
                , ToTxt requestIdentity
                )
             => Proxy requestType -> request -> Txt
simpleRspHdr rqty_proxy req = rep rqty_proxy <> " " <> toTxt (identify req)

qualRspHdr :: ( Typeable requestType
              , Request requestType
              , Req requestType ~ request
              , Identify request
              , I request ~ requestIdentity
              , ToTxt requestIdentity
              )
           => Proxy requestType -> request -> Txt
qualRspHdr rqty_proxy req = qualRep rqty_proxy <> " " <> toTxt (identify req)

fullRspHdr :: ( Typeable requestType
              , Request requestType
              , Req requestType ~ request
              , Identify request
              , I request ~ requestIdentity
              , ToTxt requestIdentity
              )
           => Proxy requestType -> request -> Txt
fullRspHdr rqty_proxy req = fullRep rqty_proxy <> " " <> toTxt (identify req)

