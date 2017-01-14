{-# language OverloadedStrings #-}
module Nuclear.Request where

import Nuclear.TypeRep

import Data.JSText
import Data.Monoid
import Data.Typeable

import Nuclear.ToText
import Nuclear.Indexed

class (Typeable (requestType :: *)) => Request requestType where
  type Req requestType :: *
  type Rsp requestType :: *

  requestHeader :: Proxy requestType -> JSText
  {-# INLINE requestHeader #-}
  default requestHeader :: Proxy requestType -> JSText
  requestHeader = qualReqHdr

  responseHeader :: (Req requestType ~ request) => Proxy requestType -> request -> JSText
  {-# INLINE responseHeader #-}
  default responseHeader :: ( Req requestType ~ request
                            , Indexed request
                            , I request ~ requestIndex
                            , ToText requestIndex
                            )
                        => Proxy requestType -> request -> JSText
  responseHeader = qualRspHdr

simpleReqHdr :: forall (requestType :: *). Typeable requestType => Proxy requestType -> JSText
simpleReqHdr = rep

qualReqHdr :: forall (requestType :: *). Typeable requestType => Proxy requestType -> JSText
qualReqHdr = qualRep

fullReqHdr :: forall (requestType :: *). Typeable requestType => Proxy requestType -> JSText
fullReqHdr = fullRep

simpleRspHdr :: ( Typeable requestType
                , Request requestType
                , Req requestType ~ request
                , Indexed request
                , I request ~ requestIndex
                , ToText requestIndex
                )
             => Proxy requestType -> request -> JSText
simpleRspHdr rqty_proxy req = rep rqty_proxy <> " " <> toText (index req)

qualRspHdr :: ( Typeable requestType
              , Request requestType
              , Req requestType ~ request
              , Indexed request
              , I request ~ requestIndex
              , ToText requestIndex
              )
           => Proxy requestType -> request -> JSText
qualRspHdr rqty_proxy req = qualRep rqty_proxy <> " " <> toText (index req)

fullRspHdr :: ( Typeable requestType
              , Request requestType
              , Req requestType ~ request
              , Indexed request
              , I request ~ requestIndex
              , ToText requestIndex
              )
           => Proxy requestType -> request -> JSText
fullRspHdr rqty_proxy req = fullRep rqty_proxy <> " " <> toText (index req)

