module Atomic.Endpoint where

import Ef.Base hiding (Endpoint)

import Atomic.Dispatch
import Atomic.Message
import Atomic.Request

import Data.Proxy
import Data.Txt
import Data.JSON

import System.Mem.Weak

data Endpoint a
  = Endpoint
    { wsEndpointHeader       :: Txt
    , wsEndpointSubscription :: forall c. Subscription c a
    , wsEndpointSyndicate    :: Syndicate a
    }
instance Eq (Endpoint a) where
  (==) (Endpoint h su sy) (Endpoint h' su' sy') =
    h == h' && su == su'
instance Ord (Endpoint a) where
  compare (Endpoint t0 _ _) (Endpoint t1 _ _) = compare t0 t1

sendEndpoint :: MonadIO c => Endpoint a -> a -> c ()
sendEndpoint (Endpoint _ sub _) = issue sub

sendEndpointSyndicate :: MonadIO c => Endpoint a -> a -> c ()
sendEndpointSyndicate (Endpoint _ _ syn) a = publish syn a

messageEndpoint :: (MonadIO c, Message mty, M mty ~ message, ToJSON message)
                => Proxy mty -> message -> Endpoint Dispatch -> c Bool
messageEndpoint mty_proxy message (Endpoint h sub _) =
  if h == messageHeader mty_proxy then do
    issue sub (encodeDispatch h message)
    return True
  else
    return False

messageEndpointSyndicate :: (MonadIO c, Message mty, M mty ~ message, ToJSON message)
                         => Proxy mty -> message -> Endpoint Dispatch -> c Bool
messageEndpointSyndicate mty_proxy message (Endpoint h _ syn) =
  if h == messageHeader mty_proxy then do
    publish syn (encodeDispatch h message)
    return True
  else
    return False

requestEndpoint :: ( MonadIO c
                   , Request rqty
                   , Req rqty ~ request
                   , ToJSON request
                   , FromJSON request
                   )
                => Proxy rqty -> request -> Endpoint Dispatch -> c Bool
requestEndpoint rqty_proxy req (Endpoint h sub _) =
  if h == requestHeader rqty_proxy then do
    issue sub (encodeDispatch h req)
    return True
  else
    return False

requestEndpointSyndicate :: ( MonadIO c
                            , Request rqty
                            , Req rqty ~ request
                            , ToJSON request
                            , FromJSON request
                            )
                          => Proxy rqty -> request -> Endpoint Dispatch -> c Bool
requestEndpointSyndicate rqty_proxy req (Endpoint h _ syn) =
  if h == requestHeader rqty_proxy then do
    publish syn (encodeDispatch h req)
    return True
  else
    return False

respondEndpoint :: ( MonadIO c
                   , Request rqty
                   , Req rqty ~ request
                   , Rsp rqty ~ response
                   , ToJSON request
                   , FromJSON request
                   , ToJSON response
                   , FromJSON response
                   )
                => Proxy rqty -> request -> response -> Endpoint Dispatch -> c Bool
respondEndpoint rqty_proxy req rsp (Endpoint h sub _) = do
  if h == responseHeader rqty_proxy req then do
    issue sub (encodeDispatch h rsp)
    return True
  else
    return False

respondEndpointSyndicate :: ( MonadIO c
                            , Request rqty
                            , Req rqty ~ request
                            , Rsp rqty ~ response
                            , ToJSON request
                            , FromJSON request
                            , ToJSON response
                            , FromJSON response
                            )
                          => Proxy rqty -> request -> response -> Endpoint Dispatch -> c Bool
respondEndpointSyndicate rqty_proxy req rsp (Endpoint h _ syn) =
  if h == responseHeader rqty_proxy req then do
    publish syn (encodeDispatch h rsp)
    return True
  else
    return False

