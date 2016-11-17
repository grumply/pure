{-# language OverloadedStrings #-}
module Nuclear.Nuclear where

import Nuclear.TypeRep

import Data.Monoid
import Data.Text hiding (index)
import Data.Typeable

import Nuclear.ToText
import Nuclear.Indexed

class (Typeable (requestType :: *)) => Nuclear requestType where
  type Req requestType :: *
  type Rsp requestType :: *

  requestHeader :: Proxy requestType -> Text
  {-# INLINE requestHeader #-}
  default requestHeader :: Proxy requestType -> Text
  requestHeader = qualReqHdr

  responseHeader :: (Req requestType ~ request) => Proxy requestType -> request -> Text
  {-# INLINE responseHeader #-}
  default responseHeader :: ( Req requestType ~ request
                            , Indexed request
                            , I request ~ requestIndex
                            , ToText requestIndex
                            )
                        => Proxy requestType -> request -> Text
  responseHeader = qualRspHdr

simpleReqHdr :: forall (requestType :: *). Typeable requestType => Proxy requestType -> Text
simpleReqHdr = rep

qualReqHdr :: forall (requestType :: *). Typeable requestType => Proxy requestType -> Text
qualReqHdr = qualRep

fullReqHdr :: forall (requestType :: *). Typeable requestType => Proxy requestType -> Text
fullReqHdr = fullRep

simpleRspHdr :: ( Typeable requestType
                , Nuclear requestType
                , Req requestType ~ request
                , Indexed request
                , I request ~ requestIndex
                , ToText requestIndex
                )
             => Proxy requestType -> request -> Text
simpleRspHdr rqty_proxy req = rep rqty_proxy <> " " <> toText (index req)

qualRspHdr :: ( Typeable requestType
              , Nuclear requestType
              , Req requestType ~ request
              , Indexed request
              , I request ~ requestIndex
              , ToText requestIndex
              )
           => Proxy requestType -> request -> Text
qualRspHdr rqty_proxy req = qualRep rqty_proxy <> " " <> toText (index req)

fullRspHdr :: ( Typeable requestType
              , Nuclear requestType
              , Req requestType ~ request
              , Indexed request
              , I request ~ requestIndex
              , ToText requestIndex
              )
           => Proxy requestType -> request -> Text
fullRspHdr rqty_proxy req = fullRep rqty_proxy <> " " <> toText (index req)

