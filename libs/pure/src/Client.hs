{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables, FlexibleContexts, BlockArguments, DerivingStrategies, TypeApplications, RankNTypes, AllowAmbiguousTypes, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeOperators, DefaultSignatures, ViewPatterns #-}
module Client 
  ( Get(..), Got(..), Post(..), Patch(..)
  , Client.query, Client.update, Client.create
  -- ,ws,wssend,wsmessage,wserror
  , sseWith, sse
  , base, API(..)
  , Query, Update, Create
  , module Export
  ) where

import Pure as Export hiding (Event,liftIO,throw,index,read,get,Result)
import qualified Pure as Export (throw)

import Data.JSON as JSON hiding (Key,Result)
#ifndef __GHCJS__
import Data.Aeson as Aeson hiding (Key,Result)
#endif

import Control.Concurrent hiding (yield)
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Data.Default
import Data.DOM
import Data.Function ((&))
import Data.IORef
import Data.Kind
import Data.Maybe
import Data.Retry
import Data.Time
import Data.Typeable
import Data.Txt
import Data.URI
import Data.View hiding (Event,liftIO,throw)
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.Fetch as Fetch
import Effect.Router
import Endpoint
import System.IO.Unsafe
import Prelude hiding (or)

class Post api req where
  post :: POST req -> req

instance (API api, Typeable r, FromJSON r) => Post api (IO r) where
  post ep = do
    let url = api @api <> toTxt ep
    r <- Fetch.post Fetch.json url def
    case r of
      Fetch.Response (Fetch.Good _) (decodeEither -> Right r) -> pure r
      _ -> throw r

instance (API api, Typeable a, Typeable r, ToJSON a, FromJSON r) => Post api (a -> IO r) where
  post ep a = do
    let url = api @api <> toTxt ep
    r <- Fetch.post Fetch.json url (JSON.encode a) 
    case r of
      Fetch.Response (Fetch.Good _) (decodeEither -> Right r) -> pure r
      _ -> throw r
 
instance (API api, Typeable a, Typeable b, Typeable r, ToJSON a, ToJSON b, FromJSON r) => Post api (a -> b -> IO r) where
  post ep a b = post @api (fromTxt (toTxt ep)) (a,b)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable r, ToJSON a, ToJSON b, ToJSON c, FromJSON r) => Post api (a -> b -> c -> IO r) where
  post ep a b c = post @api (fromTxt (toTxt ep)) (a,b,c)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, FromJSON r) => Post api (a -> b -> c -> d -> IO r) where
  post ep a b c d = post @api (fromTxt (toTxt ep)) (a,b,c,d)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, FromJSON r) => Post api (a -> b -> c -> d -> e -> IO r) where
  post ep a b c d e = post @api (fromTxt (toTxt ep)) (a,b,c,d,e)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, FromJSON r) => Post api (a -> b -> c -> d -> e -> f -> IO r) where
  post ep a b c d e f = post @api (fromTxt (toTxt ep)) (a,b,c,d,e,f)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, FromJSON r) => Post api (a -> b -> c -> d -> e -> f -> g -> IO r) where
  post ep a b c d e f g = post @api (fromTxt (toTxt ep)) (a,b,c,d,e,f,g)

class Patch api req where
  patch :: PATCH req -> req

instance (API api, Typeable r, FromJSON r) => Patch api (IO r) where
  patch ep = do
    let url = api @api <> toTxt ep
    r <- Fetch.patch Fetch.json url def
    case r of
      Fetch.Response (Fetch.Good _) (decodeEither -> Right r) -> pure r
      _ -> throw r

instance (API api, Typeable a, Typeable r, ToJSON a, FromJSON r) => Patch api (a -> IO r) where
  patch ep a = do
    let url = api @api <> toTxt ep
    r <- Fetch.patch Fetch.json url (JSON.encode a) 
    case r of
      Fetch.Response (Fetch.Good _) (decodeEither -> Right r) -> pure r
      _ -> throw r
 
instance (API api, Typeable a, Typeable b, Typeable r, ToJSON a, ToJSON b, FromJSON r) => Patch api (a -> b -> IO r) where
  patch ep a b = patch @api (fromTxt (toTxt ep)) (a,b)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable r, ToJSON a, ToJSON b, ToJSON c, FromJSON r) => Patch api (a -> b -> c -> IO r) where
  patch ep a b c = patch @api (fromTxt (toTxt ep)) (a,b,c)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, FromJSON r) => Patch api (a -> b -> c -> d -> IO r) where
  patch ep a b c d = patch @api (fromTxt (toTxt ep)) (a,b,c,d)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, FromJSON r) => Patch api (a -> b -> c -> d -> e -> IO r) where
  patch ep a b c d e = patch @api (fromTxt (toTxt ep)) (a,b,c,d,e)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, FromJSON r) => Patch api (a -> b -> c -> d -> e -> f -> IO r) where
  patch ep a b c d e f = patch @api (fromTxt (toTxt ep)) (a,b,c,d,e,f)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, FromJSON r) => Patch api (a -> b -> c -> d -> e -> f -> g -> IO r) where
  patch ep a b c d e f g = patch @api (fromTxt (toTxt ep)) (a,b,c,d,e,f,g)

class Get api req where
  get :: GET req -> req

instance (API api, Typeable r, FromJSON r) => Get api (IO r) where
  get ep = do
    let url = api @api <> toTxt ep
    r <- Fetch.get Fetch.json url 
    case r of
      Fetch.Response (Fetch.Good _) (decodeEither -> Right r) -> pure r
      _ -> throw r

instance (API api, Typeable a, Typeable r, ToJSON a, FromJSON r) => Get api (a -> IO r) where
  get ep a = do
#ifdef __GHCJS__
    let url = api @api <> toTxt ep <> "?payload=" <> encodeURIComponent (btoa_js (encode a))
    r <- Fetch.get Fetch.json url
    case r of
      Fetch.Response (Fetch.Good _) (decodeEither -> Right r) -> pure r
      _ -> throw r
#else
    let url = api @api <> toTxt ep <> "?payload=" <> encodeURIComponent (toTxt (B64.encode (Aeson.encode a)))
    r <- Fetch.get Fetch.json url
    case r of
      Fetch.Response n (decodeEither -> Right r) -> pure r
      _ -> throw r
#endif

instance (API api, Typeable a, Typeable b, Typeable r, ToJSON a, ToJSON b, FromJSON r) => Get api (a -> b -> IO r) where
  get ep a b = Client.get @api (fromTxt (toTxt ep)) (a,b)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable r, ToJSON a, ToJSON b, ToJSON c, FromJSON r) => Get api (a -> b -> c -> IO r) where
  get ep a b c = Client.get @api (fromTxt (toTxt ep)) (a,b,c)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, FromJSON r) => Get api (a -> b -> c -> d -> IO r) where
  get ep a b c d = Client.get @api (fromTxt (toTxt ep)) (a,b,c,d)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, FromJSON r) => Get api (a -> b -> c -> d -> e -> IO r) where
  get ep a b c d e = Client.get @api (fromTxt (toTxt ep)) (a,b,c,d,e)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, FromJSON r) => Get api (a -> b -> c -> d -> e -> f -> IO r) where
  get ep a b c d e f = Client.get @api (fromTxt (toTxt ep)) (a,b,c,d,e,f)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, FromJSON r) => Get api (a -> b -> c -> d -> e -> f -> g -> IO r) where
  get ep a b c d e f g = Client.get @api (fromTxt (toTxt ep)) (a,b,c,d,e,f,g)

class Got api req where
  type Unsafe req :: *
  got :: GET req -> Unsafe req

instance (API api, Typeable r, FromJSON r) => Got api (IO r) where
  type Unsafe (IO r) = r
  got ep = unsafePerformIO (Client.get @api ep)

instance (API api, Typeable r, FromJSON r, Typeable a, ToJSON a) => Got api (a -> IO r) where
  type Unsafe (a -> IO r) = a -> r
  got ep a = unsafePerformIO (Client.get @api ep a)

instance (API api, Typeable r, FromJSON r, Typeable a, ToJSON a, Typeable b, ToJSON b) => Got api (a -> b -> IO r) where
  type Unsafe (a -> b -> IO r) = a -> b -> r
  got ep a b = unsafePerformIO (Client.get @api ep a b)

instance (API api, Typeable r, FromJSON r, Typeable a, ToJSON a, Typeable b, ToJSON b, Typeable c, ToJSON c) => Got api (a -> b -> c -> IO r) where
  type Unsafe (a -> b -> c -> IO r) = a -> b -> c -> r
  got ep a b c = unsafePerformIO (Client.get @api ep a b c)

instance (API api, Typeable r, FromJSON r, Typeable a, ToJSON a, Typeable b, ToJSON b, Typeable c, ToJSON c, Typeable d, ToJSON d) => Got api (a -> b -> c -> d -> IO r) where
  type Unsafe (a -> b -> c -> d -> IO r) = a -> b -> c -> d -> r
  got ep a b c d = unsafePerformIO (Client.get @api ep a b c d)

instance (API api, Typeable r, FromJSON r, Typeable a, ToJSON a, Typeable b, ToJSON b, Typeable c, ToJSON c, Typeable d, ToJSON d, Typeable e, ToJSON e) => Got api (a -> b -> c -> d -> e -> IO r) where
  type Unsafe (a -> b -> c -> d -> e -> IO r) = a -> b -> c -> d -> e -> r
  got ep a b c d e = unsafePerformIO (Client.get @api ep a b c d e)

instance (API api, Typeable r, FromJSON r, Typeable a, ToJSON a, Typeable b, ToJSON b, Typeable c, ToJSON c, Typeable d, ToJSON d, Typeable e, ToJSON e, Typeable f, ToJSON f) => Got api (a -> b -> c -> d -> e -> f -> IO r) where
  type Unsafe (a -> b -> c -> d -> e -> f -> IO r) = a -> b -> c -> d -> e -> f -> r
  got ep a b c d e f = unsafePerformIO (Client.get @api ep a b c d e f)

instance (API api, Typeable r, FromJSON r, Typeable a, ToJSON a, Typeable b, ToJSON b, Typeable c, ToJSON c, Typeable d, ToJSON d, Typeable e, ToJSON e, Typeable f, ToJSON f, Typeable g, ToJSON g) => Got api (a -> b -> c -> d -> e -> f -> g -> IO r) where
  type Unsafe (a -> b -> c -> d -> e -> f -> g -> IO r) = a -> b -> c -> d -> e -> f -> g -> r
  got ep a b c d e f g = unsafePerformIO (Client.get @api ep a b c d e f g)

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
                pl <- askref self
                es <- new_event_source_js (host <> ep <> "?payload=" <> encodeURIComponent (btoa_js (encode pl)))

                msgs <- onRaw es "message" def \_ msg ->
                  case msg .# "data" of
                    Just d | Just e <- decode d -> yield @e e
                    _ -> putMVar mv stop

                errs <- onRaw es "error" def \_ _ -> putMVar mv (stop >> retry)

                writeIORef rs (msgs >> errs >> close_es_js es)

                join (takeMVar mv)
                
    , onUnmounted = getref self >>= killThread
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
      = yield @a a
        
      | otherwise 
      = pure ()

newtype WebsocketError = WebsocketError JSV

wserror :: Exists Websocket => View -> (Producer WebsocketError => View)
wserror = OnMounted (\_ -> onRaw (it :: Websocket) "error" def (\_ -> yield . WebsocketError))
#endif

query :: forall api r. (Resource r, Got api (Query r)) => Unsafe (Query r)
query = Client.got @api (Endpoint.query @r)

update :: forall api r. (Resource r, Patch api (Update r)) => Update r
update = patch @api (Endpoint.update @r)

create :: forall api r.  (Resource r, Post api (Create r)) => Create r
create = post @api (Endpoint.create @r)

