{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables, FlexibleContexts, BlockArguments, DerivingStrategies, TypeApplications, RankNTypes, AllowAmbiguousTypes, FlexibleInstances, InstanceSigs, ScopedTypeVariables #-}
module Client 
  ( Client.post
  , Client.get
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
import Endpoint

class Post req where
  post :: Txt -> Endpoint req -> req

instance (Typeable a, Typeable r, ToJSON a, FromJSON r) => Post (a -> IO r) where
  post host ep a = Fetch.postWith @a @r [] (host <> toTxt ep) a >>= either throw pure
 
instance (Typeable a, Typeable b, Typeable r, ToJSON a, ToJSON b, FromJSON r) => Post (a -> b -> IO r) where
  post host ep a b = post host (fromTxt (toTxt ep)) (a,b)

instance (Typeable a, Typeable b, Typeable c, Typeable r, ToJSON a, ToJSON b, ToJSON c, FromJSON r) => Post (a -> b -> c -> IO r) where
  post host ep a b c = post host (fromTxt (toTxt ep)) (a,b,c)

instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, FromJSON r) => Post (a -> b -> c -> d -> IO r) where
  post host ep a b c d = post host (fromTxt (toTxt ep)) (a,b,c,d)

instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, FromJSON r) => Post (a -> b -> c -> d -> e -> IO r) where
  post host ep a b c d e = post host (fromTxt (toTxt ep)) (a,b,c,d,e)

instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, FromJSON r) => Post (a -> b -> c -> d -> e -> f -> IO r) where
  post host ep a b c d e f = post host (fromTxt (toTxt ep)) (a,b,c,d,e,f)

instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, FromJSON r) => Post (a -> b -> c -> d -> e -> f -> g -> IO r) where
  post host ep a b c d e f g = post host (fromTxt (toTxt ep)) (a,b,c,d,e,f,g)

class Get req where
  get :: Txt -> Endpoint req -> req

instance (Typeable r, FromJSON r) => Get (IO r) where
  get host ep = Fetch.getWith @r [] (host <> toTxt ep) >>= either throw pure

instance (Typeable a, Typeable r, ToJSON a, FromJSON r) => Get (a -> IO r) where
  get host ep a = 
#ifdef __GHCJS__
    Fetch.getWith @r [] (host <> toTxt ep <> "?payload=" <> encodeURIComponent (btoa_js (encode a)))
      >>= either throw pure
#else
    Fetch.getWith @r [] (host <> toTxt ep <> "?payload=" <> encodeURIComponent (toTxt (B64.encode (encode a))))
      >>= either throw pure
#endif

instance (Typeable a, Typeable b, Typeable r, ToJSON a, ToJSON b, FromJSON r) => Get (a -> b -> IO r) where
  get host ep a b = Client.get host (fromTxt (toTxt ep)) (a,b)

instance (Typeable a, Typeable b, Typeable c, Typeable r, ToJSON a, ToJSON b, ToJSON c, FromJSON r) => Get (a -> b -> c -> IO r) where
  get host ep a b c = Client.get host (fromTxt (toTxt ep)) (a,b,c)

instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, FromJSON r) => Get (a -> b -> c -> d -> IO r) where
  get host ep a b c d = Client.get host (fromTxt (toTxt ep)) (a,b,c,d)

instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, FromJSON r) => Get (a -> b -> c -> d -> e -> IO r) where
  get host ep a b c d e = Client.get host (fromTxt (toTxt ep)) (a,b,c,d,e)

instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, FromJSON r) => Get (a -> b -> c -> d -> e -> f -> IO r) where
  get host ep a b c d e f = Client.get host (fromTxt (toTxt ep)) (a,b,c,d,e,f)

instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, FromJSON r) => Get (a -> b -> c -> d -> e -> f -> g -> IO r) where
  get host ep a b c d e f g = Client.get host (fromTxt (toTxt ep)) (a,b,c,d,e,f,g)

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

sseWith :: forall e a. (FromJSON e, Typeable a, ToJSON a) => Policy -> Txt -> Txt -> a -> (Producer e => View)
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
                es <- new_event_source_js (host <> ep <> "?payload=" <> encodeURIComponent (btoa_js (encode pl)))

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
