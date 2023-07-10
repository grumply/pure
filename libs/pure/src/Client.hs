{-# LANGUAGE CPP, RecordWildCards, OverloadedStrings, LambdaCase, BangPatterns, PatternSynonyms, ScopedTypeVariables, FlexibleContexts, BlockArguments, DerivingStrategies, NamedFieldPuns, DeriveGeneric, DeriveAnyClass, TypeApplications, RankNTypes, AllowAmbiguousTypes #-}
module Client 
  ( Client.post, post_
  , Client.get, get_
  -- ,ws,wssend,wsmessage,wserror
  , Fetch.XHRError, Fetch.err, Fetch.response
  , sseWith, sse
  ) where

#ifdef __GHCJS__
import Data.JSON as JSON
#else
import Data.Aeson as JSON
#endif

import Control.Concurrent
import Control.Component
import Control.Exception
import Control.Monad
import Control.Producer
import Control.Retry
import Control.State
import Data.Default
import Data.DOM
import Data.Exists
import Data.Function ((&))
import Data.IORef
import Data.Maybe
import Data.Time
import Data.Typeable
import Data.Txt
import Data.URI
import Data.View
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.Fetch as Fetch
import qualified Effect.Fetch as Fetch (response,err)

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
get_ host ep = Client.get @rsp host ep ()

#ifdef __GHCJS__
foreign import javascript unsafe
  "btoa($1)" btoa_js :: Txt -> Txt
#endif

sse :: forall e a. (Producer e, FromJSON e, Typeable a, ToJSON a) => Txt -> Txt -> a -> View
#ifndef __GHCJS__
sse _ _ _ = Data.View.Null
#else
sse = sseWith @e (jittered Second & limitDelay (Seconds 30 0))
#endif

sseWith :: forall e a. (Producer e, FromJSON e, Typeable a, ToJSON a) => Policy -> Txt -> Txt -> a -> View
sseWith policy host ep = 
#ifndef __GHCJS__
  const Data.View.Null
#else
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

--------------------------------------------------------------------------------
-- nascent; not exported

foreign import javascript unsafe
  "$r = new WebSocket($1)" new_websocket_js :: Txt -> IO Websocket

foreign import javascript unsafe
  "$1.close()" close_ws_js :: Websocket -> IO ()

foreign import javascript unsafe
  "$1.send($2)" send_ws_js :: Websocket -> Txt -> IO ()

newtype Websocket = Websocket JSV

ws :: Policy -> Txt -> (Exists Websocket => View) -> View
ws policy url =
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

wssend :: (Exists Websocket, ToJSON a) => a -> IO ()
wssend msg =
  send_ws_js it (encode msg)

wsmessage :: forall a. (FromJSON a, Exists Websocket) => View -> (Producer a => View)
wsmessage = OnMounted (\_ -> onRaw (it :: Websocket) "message" def go)
  where
    go _ msg
      | Just m <- msg .# "data"
      , Just a <- decode m
      = Control.Producer.yield @a a
        
      | otherwise 
      = pure ()

newtype WebsocketError = WebsocketError JSV

wserror :: Exists Websocket => View -> (Producer WebsocketError => View)
wserror = OnMounted (\_ -> onRaw (it :: Websocket) "error" def (\_ -> Control.Producer.yield . WebsocketError))
#endif
