{-# LANGUAGE CPP, RecordWildCards, OverloadedStrings, LambdaCase, BangPatterns, PatternSynonyms, ScopedTypeVariables, FlexibleContexts, BlockArguments, DerivingStrategies, NamedFieldPuns, DeriveGeneric, DeriveAnyClass, TypeApplications, RankNTypes, AllowAmbiguousTypes #-}
module Server 
  (serve,lambda,Endpoint,Fingerprint()
  ,Server.post,post_
  ,Server.get,get_
  ,sseWith,sse
  -- ,ws,wssend,wsmessage,wserror
  ,channel
  ,middleware
  ,cache,logging
  ,Request(..)
  ,Fetch.XHRError,Fetch.err,Fetch.response
  ) where

#ifdef __GHCJS__
import Control.Exception
import Data.JSON as JSON
#else
import Data.Aeson as JSON
#endif

import Control.Concurrent
import Control.Producer (Producer,yield)
import Control.Retry
import Control.State
import qualified Control.Component as Component
import qualified Control.Log as Log
import Data.DOM
import Data.Foldable
import Data.Function ((&))
import Control.Monad
import Data.Default
import Data.Exists
import Data.IORef
import qualified Data.List as List
import Data.Maybe
import Data.Time (pattern Seconds,pattern Second,Time)
import Data.Typeable
import Data.Txt as Txt
import Data.URI
import Data.View
import qualified Data.Fetch as Fetch
import qualified Effect.Fetch as Fetch (response,err)
import GHC.Generics
import GHC.Fingerprint
import System.IO.Unsafe

import Data.String
import Data.ByteString.Lazy as BSL
#ifndef __GHCJS__
import Data.Binary.Builder (fromLazyByteString)
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Network.Wai.Handler.Warp as Warp
#endif
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai as Wai
-- import Network.Wai.Middleware.Cors as Cors

instance ToJSON Fingerprint where
  toJSON (Fingerprint w1 w2) = toJSON (w1,w2)

instance FromJSON Fingerprint where
  parseJSON o = do
    (w1,w2) <- parseJSON o
    pure (Fingerprint w1 w2)

#ifdef __GHCJS__
serve :: Int -> [Endpoint] -> View
serve port _ = Null
#else
serve :: Warp.Port -> [Endpoint] -> View
serve port = Component $ \self -> 
  def
    { onConstruct = do
        ref <- ask self >>= newIORef 
        tid <- forkIO do
          Warp.runSettings (Warp.setServerName "pure" (Warp.setPort port Warp.defaultSettings)) $ \request respond -> do
            eps <- readIORef ref
            let p = Txt.intercalate "/" (pathInfo request)
            case List.find (\Endpoint { methods, path } -> p == path && requestMethod request `Prelude.elem` methods) eps of
              Nothing -> respond (responseLBS status404 [] mempty)
              Just (Endpoint f _ _) -> f request respond
        pure (ref,tid)
    , onReceive = \eps (ref,tid) -> writeIORef ref eps >> pure (ref,tid)
    , onUnmounted = Data.View.get self >>= \(_,tid) -> killThread tid
    , render = \_ _ -> Data.View.Null
    }
#endif

data Endpoint = Endpoint { endpoint :: Application, methods :: [Method], path :: Txt }

channel :: forall a b. (Typeable a, FromJSON a, ToJSON b) => Txt -> (a -> IO [b]) -> Endpoint
channel path l = Endpoint {..}
  where
    methods = [methodPost,methodGet,methodOptions]

    endpoint :: Application
#ifdef __GHCJS__
    endpoint request respond = error "Not Implemented."
#else
    endpoint request respond
      | requestMethod request == methodOptions =
        respond (responseLBS status200 [(hAllow,"POST, GET, OPTIONS")] def)

      | otherwise = do
        let
          payload
            | requestMethod request == methodGet
            , Just b64 <- join (List.lookup "payload" (queryString request))
            , Right bs <- B64.decode (BSL.fromStrict b64) 
            = pure bs

            | otherwise = consumeRequestBodyLazy request

        pl <- payload
        case eitherDecode pl of
          Left e -> respond (responseLBS status400 [] (encode e))
          Right (a :: a) -> do
            let 
              responder write flush = 
                let
                  push b = do
                    write ("data: " <> fromLazyByteString (encode b) <> "\n\n")
                    flush
                in
                  l a >>= mapM_ push

            respond (responseStream status200 
              [(hContentType,"text/event-stream")
              ,(hCacheControl,"no-cache")
              ,(hConnection,"keep-alive")
              ] responder
              )
#endif


lambda :: forall a b. (Typeable a, FromJSON a, ToJSON b) => Txt -> (a -> IO b) -> Endpoint
lambda path l = Endpoint {..}
  where
    methods = [methodPost,methodGet,methodOptions]

    endpoint :: Application
#ifdef __GHCJS__
    endpoint request respond = error "Not Implemented."
#else
    endpoint request respond
      | requestMethod request == methodOptions =
        respond (responseLBS status200 [(hAllow,"POST, GET, OPTIONS")] def)

      | otherwise = do
        let
          payload
            | requestMethod request == methodGet
            , Just b64 <- join (List.lookup "payload" (queryString request))
            , Right bs <- B64.decode (BSL.fromStrict b64) 
            = pure bs

            | otherwise = consumeRequestBodyLazy request

        pl <- payload
        case eitherDecode pl of
          Left e -> respond (responseLBS status400 [] (encode e))
          Right (a :: a) -> respond . responseLBS status200 [(hContentType,"application/json")] . encode =<< l a

#endif

cache :: Time -> Middleware
cache (Seconds duration _) app request respond = 
  app request 
    (respond . mapResponseHeaders (++ [(hCacheControl,"public, max-age=" <> fromTxt (toTxt @Int (round duration)))]))

middleware :: Middleware -> Endpoint -> Endpoint
middleware mw Endpoint {..} = 
  Endpoint
    { endpoint = mw endpoint
    , ..
    }

logging :: Log.Logging => Log.Level -> Middleware
logging level app request respond = do
  Log.log level (show request)
  app request respond

post :: forall rsp req. (Typeable req, Typeable rsp, ToJSON req, FromJSON rsp) => Txt -> Txt -> req -> IO (Either Fetch.XHRError rsp)
post host ep = Fetch.postWith @req @rsp [] (host <> "/" <> ep) 

post_ :: forall req. (Typeable req, ToJSON req) => Txt -> Txt -> req -> IO (Either Fetch.XHRError ())
post_ = post @() 

get :: forall rsp req. (Typeable req, Typeable rsp, ToJSON req, FromJSON rsp) => Txt -> Txt -> req -> IO (Either Fetch.XHRError rsp)
get host ep req = do
#ifdef __GHCJS__
  Fetch.getWith @rsp [] (host <> "/" <> ep <> "?payload=" <> encodeURIComponent (btoa_js (encode req)))
#else
  Fetch.getWith @rsp [] (host <> "/" <> ep <> "?payload=" <> encodeURIComponent (toTxt (B64.encode (encode req))))
#endif

get_ :: forall rsp. (Typeable rsp, FromJSON rsp) => Txt -> Txt -> IO (Either Fetch.XHRError rsp)
get_ host ep = Server.get @rsp host ep ()

#ifdef __GHCJS__
foreign import javascript unsafe
  "btoa($1)" btoa_js :: Txt -> Txt
#endif

sse :: forall e a. (Producer e, FromJSON e, Typeable a, ToJSON a) => Txt -> Txt -> a -> View
sse = sseWith @e (jittered Second & limitDelay (Seconds 30 0))

sseWith :: forall e a. (Producer e, FromJSON e, Typeable a, ToJSON a) => Policy -> Txt -> Txt -> a -> View
sseWith policy host ep = 
#ifdef __GHCJS__
  Component \self -> def
    { onConstruct = do
        rs <- newIORef def
        let stop = join (readIORef rs)
        forkIO do
          handle (\ThreadKilled -> stop) do
            void do
              retrying policy do
                mv <- newEmptyMVar
                pl <- ask self
                es <- new_event_source_js (host <> "/" <> ep <> "?payload=" <> encodeURIComponent (btoa_js (encode pl)))

                msgs <- onRaw es "message" def \_ msg ->
                  case msg .# "data" of
                    Just d | Just e <- decode d -> Control.Producer.yield @e e
                    _ -> putMVar mv stop

                errs <- onRaw es "error" def \_ _ -> putMVar mv (stop >> retry)

                writeIORef rs (msgs >> errs >> close_es_js es)

                join (takeMVar mv)
                
    , onUnmounted = Data.View.get self >>= killThread
    }

newtype EventSource = EventSource JSV

foreign import javascript unsafe
  "$r = new EventSource($1)" new_event_source_js :: Txt -> IO EventSource

foreign import javascript unsafe
  "$1.close()" close_es_js :: EventSource -> IO ()
#else
  -- TODO?
  const Data.View.Null
#endif

--------------------------------------------------------------------------------
-- nascent; not exported

#ifdef __GHCJS__
foreign import javascript unsafe
  "$r = new WebSocket($1)" new_websocket_js :: Txt -> IO Websocket

foreign import javascript unsafe
  "$1.close()" close_ws_js :: Websocket -> IO ()

foreign import javascript unsafe
  "$1.send($2)" send_ws_js :: Websocket -> Txt -> IO ()
#endif

newtype Websocket = Websocket JSV

ws :: Policy -> Txt -> (Exists Websocket => View) -> View
ws policy url =
#ifdef __GHCJS__
  let 
    open = do
      s <- newIORef def
      let stop = join (readIORef s)
      retrying policy do
        ws <- new_websocket_js url 
        mv <- newEmptyMVar
        fail <- onRaw ws "error" def (\_ _ -> putMVar mv (stop >> retry))
        open <- onRaw ws "open" def (\stop _ -> putMVar mv stop)
        writeIORef s (open >> fail >> close_ws_js ws)
        join (takeMVar mv)
        pure (ws,close_ws_js)
  in
    stateWith (\_ -> pure) (fromJust <$> open)
#else
  state (Websocket ())
#endif

wssend :: (Exists Websocket, ToJSON a) => a -> IO ()
wssend msg =
#ifdef __GHCJS__
  send_ws_js it (encode msg)
#else
  pure ()
#endif

wsmessage :: forall a. (FromJSON a, Exists Websocket) => View -> (Producer a => View)
#ifdef __GHCJS__
wsmessage = OnMounted (\_ -> onRaw (it :: Websocket) "message" def go)
  where
    go _ msg
      | Just m <- msg .# "data"
      , Just a <- decode m
      = Control.Producer.yield @a a
        
      | otherwise 
      = pure ()
#else
wsmessage = id
#endif

newtype WebsocketError = WebsocketError JSV

wserror :: Exists Websocket => View -> (Producer WebsocketError => View)
#ifdef __GHCJS__
wserror = OnMounted (\_ -> onRaw (it :: Websocket) "error" def (\_ -> Control.Producer.yield . WebsocketError))
#else
wserror = id
#endif
