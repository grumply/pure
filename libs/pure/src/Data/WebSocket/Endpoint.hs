{-# LANGUAGE TypeFamilies, CPP #-}
module Data.Websocket.Endpoint where

import Data.IORef
import Data.JSON
import Data.Proxy
import Data.Txt (Txt)
import Data.Websocket.Callbacks
import Data.Websocket.Dispatch
import Data.Websocket.Message
import Data.Websocket.Request

data Endpoint a
  = Endpoint
    { epEndpointHeader   :: Txt
    , epDispatchCallback :: DispatchCallback
    } deriving (Eq)
instance Ord (Endpoint a) where
  compare (Endpoint t0 _) (Endpoint t1 _) = compare t0 t1

sendEndpoint :: (ToJSON a) => Endpoint a -> a -> IO ()
sendEndpoint (Endpoint eph (DispatchCallback cbRef _)) a = do
  cb <- readIORef cbRef
  cb (encodeDispatch eph a)

messageEndpoint :: (Message mty, M mty ~ message, ToJSON message)
                => Proxy mty -> message -> Endpoint Dispatch -> IO Bool
messageEndpoint mty_proxy message (Endpoint h (DispatchCallback cbRef _)) = do
  cb <- readIORef cbRef
  if h == messageHeader mty_proxy
    then cb (encodeDispatch h message) >> return True
    else return False

requestEndpoint :: ( Request rqty
                   , Req rqty ~ request
                   , ToJSON request
                   , FromJSON request
                   )
                => Proxy rqty -> request -> Endpoint Dispatch -> IO Bool
requestEndpoint rqty_proxy req (Endpoint h (DispatchCallback cbRef _)) = do
  cb <- readIORef cbRef
  if h == requestHeader rqty_proxy
    then cb (encodeDispatch h req) >> return True
    else return False

respondEndpoint :: ( Request rqty
                   , Req rqty ~ request
                   , Rsp rqty ~ response
                   , ToJSON request
                   , FromJSON request
                   , ToJSON response
                   , FromJSON response
                   )
                => Proxy rqty -> request -> response -> Endpoint Dispatch -> IO Bool
respondEndpoint rqty_proxy req rsp (Endpoint h (DispatchCallback cbRef _)) = do
  cb <- readIORef cbRef
  if h == responseHeader rqty_proxy req
    then cb (encodeDispatch h rsp) >> return True
    else return False
