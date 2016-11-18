module Nuclear.Endpoint where

import Ef.Base hiding (Endpoint)

import Nuclear.Msg
import Nuclear.Message
import Nuclear.Request

import Data.Proxy
import Data.Aeson
import Data.Text

data Endpoint self super
  = Endpoint
    { wsEndpointHeader :: Text
    , wsEndpointSubscription :: Subscription self super Msg
    , wsEndpointPeriodical :: Periodical self super Msg
    } deriving Eq
instance Ord (Endpoint self super) where
  compare (Endpoint t0 _ _) (Endpoint t1 _ _) = compare t0 t1

messageEndpointSubscription :: (Monad super, MonadIO super, Message mty, M mty ~ message, ToJSON message)
                           => Proxy mty -> message -> Endpoint self super -> Narrative self super Bool
messageEndpointSubscription mty_proxy message (Endpoint h s _) =
  if h == messageHeader mty_proxy then do
    trigger s (encodeMsg h message)
    return True
  else
    return False

messageEndpoint :: (Monad super, MonadIO super, Message mty, M mty ~ message, ToJSON message)
               => Proxy mty -> message -> Endpoint self super -> Narrative self super Bool
messageEndpoint mty_proxy message (Endpoint h _ p) =
  if h == messageHeader mty_proxy then do
    publish p (encodeMsg h message)
    return True
  else
    return False

-- NOTE: The dual end of the WS probably isn't awaiting the response! You should be looking for the
--       message instance of this method.
requestEndpointSubscription :: ( Monad super, MonadIO super
                              , Request rqty
                              , Req rqty ~ request
                              , ToJSON request
                              , FromJSON request
                              )
                           => Proxy rqty -> request -> Endpoint self super -> Narrative self super Bool
requestEndpointSubscription rqty_proxy req (Endpoint h s _) =
  if h == requestHeader rqty_proxy then do
    trigger s (encodeMsg h req)
    return True
  else
    return False

-- NOTE: The dual end of the WS probably isn't awaiting the response! You should be looking for the
--       message instance of this method.
requestEndpoint :: ( Monad super, MonadIO super
                  , Request rqty
                  , Req rqty ~ request
                  , ToJSON request
                  , FromJSON request
                  )
                => Proxy rqty -> request -> Endpoint self super -> Narrative self super Bool
requestEndpoint rqty_proxy req (Endpoint h _ p) =
  if h == requestHeader rqty_proxy then do
    publish p (encodeMsg h req)
    return True
  else
    return False

respondEndpointSubscription :: ( Monad super, MonadIO super
                              , Request rqty
                              , Req rqty ~ request
                              , Rsp rqty ~ response
                              , ToJSON request
                              , FromJSON request
                              , ToJSON response
                              , FromJSON response
                              )
                           => Proxy rqty -> request -> response -> Endpoint self super -> Narrative self super Bool
respondEndpointSubscription rqty_proxy req rsp (Endpoint h s _) =
  if h == responseHeader rqty_proxy req then do
    trigger s (encodeMsg h rsp)
    return True
  else
    return False

respondEndpoint :: ( Monad super, MonadIO super
                  , Request rqty
                  , Req rqty ~ request
                  , Rsp rqty ~ response
                  , ToJSON request
                  , FromJSON request
                  , ToJSON response
                  , FromJSON response
                  )
                => Proxy rqty -> request -> response -> Endpoint self super -> Narrative self super Bool
respondEndpoint rqty_proxy req rsp (Endpoint h _ p) =
  if h == responseHeader rqty_proxy req then do
    publish p (encodeMsg h rsp)
    return True
  else
    return False

