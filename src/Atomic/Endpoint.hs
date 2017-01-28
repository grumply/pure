module Atomic.Endpoint where

import Ef.Base hiding (Endpoint)

import Atomic.Dispatch
import Atomic.Message
import Atomic.Request

import Data.Proxy
import Data.Txt
import Data.JSON

data Endpoint ms c
  = Endpoint
    { wsEndpointHeader :: Txt
    , wsEndpointSubscription :: Subscription ms c Dispatch
    , wsEndpointPeriodical :: Periodical ms c Dispatch
    } deriving Eq
instance Ord (Endpoint ms c) where
  compare (Endpoint t0 _ _) (Endpoint t1 _ _) = compare t0 t1

messageEndpointSubscription :: (MonadIO c, Message mty, Functor (Messages ms), M mty ~ message, ToJSON message)
                            => Proxy mty -> message -> Endpoint ms c -> Code ms c Bool
messageEndpointSubscription mty_proxy message (Endpoint h s _) =
  if h == messageHeader mty_proxy then do
    trigger s (encodeDispatch h message)
    return True
  else
    return False

messageEndpoint :: (MonadIO c, Message mty, Functor (Messages ms), M mty ~ message, ToJSON message)
                => Proxy mty -> message -> Endpoint ms c -> Code ms c Bool
messageEndpoint mty_proxy message (Endpoint h _ p) =
  if h == messageHeader mty_proxy then do
    publish p (encodeDispatch h message)
    return True
  else
    return False

-- NOTE: The dual end of the WS probably isn't awaiting the response! You should be looking for the
--       message instance of this method.
requestEndpointSubscription :: ( MonadIO c
                               , Functor (Messages ms)
                               , Request rqty
                               , Req rqty ~ request
                               , ToJSON request
                               , FromJSON request
                               )
                            => Proxy rqty -> request -> Endpoint ms c -> Code ms c Bool
requestEndpointSubscription rqty_proxy req (Endpoint h s _) =
  if h == requestHeader rqty_proxy then do
    trigger s (encodeDispatch h req)
    return True
  else
    return False

-- NOTE: The dual end of the WS probably isn't awaiting the response! You should be looking for the
--       message instance of this method.
requestEndpoint :: ( MonadIO c
                   , Functor (Messages ms)
                   , Request rqty
                   , Req rqty ~ request
                   , ToJSON request
                   , FromJSON request
                   )
                => Proxy rqty -> request -> Endpoint ms c -> Code ms c Bool
requestEndpoint rqty_proxy req (Endpoint h _ p) =
  if h == requestHeader rqty_proxy then do
    publish p (encodeDispatch h req)
    return True
  else
    return False

respondEndpointSubscription :: ( MonadIO c
                               , Functor (Messages ms)
                               , Request rqty
                               , Req rqty ~ request
                               , Rsp rqty ~ response
                               , ToJSON request
                               , FromJSON request
                               , ToJSON response
                               , FromJSON response
                               )
                            => Proxy rqty -> request -> response -> Endpoint ms c -> Code ms c Bool
respondEndpointSubscription rqty_proxy req rsp (Endpoint h s _) =
  if h == responseHeader rqty_proxy req then do
    trigger s (encodeDispatch h rsp)
    return True
  else
    return False

respondEndpoint :: ( MonadIO c
                   , Functor (Messages ms)
                   , Request rqty
                   , Req rqty ~ request
                   , Rsp rqty ~ response
                   , ToJSON request
                   , FromJSON request
                   , ToJSON response
                   , FromJSON response
                   )
                => Proxy rqty -> request -> response -> Endpoint ms c -> Code ms c Bool
respondEndpoint rqty_proxy req rsp (Endpoint h _ p) =
  if h == responseHeader rqty_proxy req then do
    publish p (encodeDispatch h rsp)
    return True
  else
    return False

