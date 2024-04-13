{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables, FlexibleContexts, BlockArguments, DerivingStrategies, TypeApplications, RankNTypes, AllowAmbiguousTypes, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeOperators, DefaultSignatures, ViewPatterns #-}
module Client 
  ( Get_(..), Got_(..), Post_(..), Patch_(..), Delete_(..), Put_(..)
  , Client.query, Client.update, Client.create, Client.replace, Client.delete, Client.query'
  -- ,ws,wssend,wsmessage,wserror
  , sseWith, sse
  , endpoint, API(..)
  , Query, Update, Create
  , module Export
  ) where

import Pure as Export hiding (Event,liftIO,throw,index,read,get,Result,replace)
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

class Post_ (api :: *) req where
  post_ :: POST req -> req

instance (API api, Typeable r, FromJSON r) => Post_ api (IO r) where
  post_ ep = do
    let url = api @api <> toTxt ep
    r <- Fetch.post Fetch.json url def
    case r of
      Fetch.Response (Fetch.Good _) (decodeEither -> Right r) -> pure r
      _ -> throw r

instance (API api, Typeable a, Typeable r, ToJSON a, FromJSON r) => Post_ api (a -> IO r) where
  post_ ep a = do
    let url = api @api <> toTxt ep
    r <- Fetch.post Fetch.json url (JSON.encode a) 
    case r of
      Fetch.Response (Fetch.Good _) (decodeEither -> Right r) -> pure r
      _ -> throw r
 
instance (API api, Typeable a, Typeable b, Typeable r, ToJSON a, ToJSON b, FromJSON r) => Post_ api (a -> b -> IO r) where
  post_ ep a b = post_ @api (fromTxt (toTxt ep)) (a,b)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable r, ToJSON a, ToJSON b, ToJSON c, FromJSON r) => Post_ api (a -> b -> c -> IO r) where
  post_ ep a b c = post_ @api (fromTxt (toTxt ep)) (a,b,c)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, FromJSON r) => Post_ api (a -> b -> c -> d -> IO r) where
  post_ ep a b c d = post_ @api (fromTxt (toTxt ep)) (a,b,c,d)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, FromJSON r) => Post_ api (a -> b -> c -> d -> e -> IO r) where
  post_ ep a b c d e = post_ @api (fromTxt (toTxt ep)) (a,b,c,d,e)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, FromJSON r) => Post_ api (a -> b -> c -> d -> e -> f -> IO r) where
  post_ ep a b c d e f = post_ @api (fromTxt (toTxt ep)) (a,b,c,d,e,f)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, FromJSON r) => Post_ api (a -> b -> c -> d -> e -> f -> g -> IO r) where
  post_ ep a b c d e f g = post_ @api (fromTxt (toTxt ep)) (a,b,c,d,e,f,g)

class Patch_ (api :: *) req where
  patch_ :: PATCH req -> req

instance (API api, Typeable r, FromJSON r) => Patch_ api (IO r) where
  patch_ ep = do
    let url = api @api <> toTxt ep
    r <- Fetch.patch Fetch.json url def
    case r of
      Fetch.Response (Fetch.Good _) (decodeEither -> Right r) -> pure r
      _ -> throw r

instance (API api, Typeable a, Typeable r, ToJSON a, FromJSON r) => Patch_ api (a -> IO r) where
  patch_ ep a = do
    let url = api @api <> toTxt ep
    r <- Fetch.patch Fetch.json url (JSON.encode a) 
    case r of
      Fetch.Response (Fetch.Good _) (decodeEither -> Right r) -> pure r
      _ -> throw r
 
instance (API api, Typeable a, Typeable b, Typeable r, ToJSON a, ToJSON b, FromJSON r) => Patch_ api (a -> b -> IO r) where
  patch_ ep a b = patch_ @api (fromTxt (toTxt ep)) (a,b)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable r, ToJSON a, ToJSON b, ToJSON c, FromJSON r) => Patch_ api (a -> b -> c -> IO r) where
  patch_ ep a b c = patch_ @api (fromTxt (toTxt ep)) (a,b,c)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, FromJSON r) => Patch_ api (a -> b -> c -> d -> IO r) where
  patch_ ep a b c d = patch_ @api (fromTxt (toTxt ep)) (a,b,c,d)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, FromJSON r) => Patch_ api (a -> b -> c -> d -> e -> IO r) where
  patch_ ep a b c d e = patch_ @api (fromTxt (toTxt ep)) (a,b,c,d,e)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, FromJSON r) => Patch_ api (a -> b -> c -> d -> e -> f -> IO r) where
  patch_ ep a b c d e f = patch_ @api (fromTxt (toTxt ep)) (a,b,c,d,e,f)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, FromJSON r) => Patch_ api (a -> b -> c -> d -> e -> f -> g -> IO r) where
  patch_ ep a b c d e f g = patch_ @api (fromTxt (toTxt ep)) (a,b,c,d,e,f,g)

class Get_ (api :: *) req where
  get_ :: GET req -> req

instance (API api, Typeable r, FromJSON r) => Get_ api (IO r) where
  get_ ep = do
    let url = api @api <> toTxt ep
    r <- Fetch.get Fetch.json url 
    case r of
      Fetch.Response (Fetch.Good _) (decodeEither -> Right r) -> pure r
      _ -> throw r

instance (API api, Typeable a, Typeable r, ToJSON a, FromJSON r) => Get_ api (a -> IO r) where
  get_ ep a = do
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

instance (API api, Typeable a, Typeable b, Typeable r, ToJSON a, ToJSON b, FromJSON r) => Get_ api (a -> b -> IO r) where
  get_ ep a b = Client.get_ @api (fromTxt (toTxt ep)) (a,b)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable r, ToJSON a, ToJSON b, ToJSON c, FromJSON r) => Get_ api (a -> b -> c -> IO r) where
  get_ ep a b c = Client.get_ @api (fromTxt (toTxt ep)) (a,b,c)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, FromJSON r) => Get_ api (a -> b -> c -> d -> IO r) where
  get_ ep a b c d = Client.get_ @api (fromTxt (toTxt ep)) (a,b,c,d)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, FromJSON r) => Get_ api (a -> b -> c -> d -> e -> IO r) where
  get_ ep a b c d e = Client.get_ @api (fromTxt (toTxt ep)) (a,b,c,d,e)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, FromJSON r) => Get_ api (a -> b -> c -> d -> e -> f -> IO r) where
  get_ ep a b c d e f = Client.get_ @api (fromTxt (toTxt ep)) (a,b,c,d,e,f)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, FromJSON r) => Get_ api (a -> b -> c -> d -> e -> f -> g -> IO r) where
  get_ ep a b c d e f g = Client.get_ @api (fromTxt (toTxt ep)) (a,b,c,d,e,f,g)

class Got_ (api :: *) req where
  type Unsafe req :: *
  got_ :: GET req -> Unsafe req

instance (API api, Typeable r, FromJSON r) => Got_ api (IO r) where
  type Unsafe (IO r) = r
  got_ ep = unsafePerformIO (Client.get_ @api ep)

instance (API api, Typeable r, FromJSON r, Typeable a, ToJSON a) => Got_ api (a -> IO r) where
  type Unsafe (a -> IO r) = a -> r
  got_ ep a = unsafePerformIO (Client.get_ @api ep a)

instance (API api, Typeable r, FromJSON r, Typeable a, ToJSON a, Typeable b, ToJSON b) => Got_ api (a -> b -> IO r) where
  type Unsafe (a -> b -> IO r) = a -> b -> r
  got_ ep a b = unsafePerformIO (Client.get_ @api ep a b)

instance (API api, Typeable r, FromJSON r, Typeable a, ToJSON a, Typeable b, ToJSON b, Typeable c, ToJSON c) => Got_ api (a -> b -> c -> IO r) where
  type Unsafe (a -> b -> c -> IO r) = a -> b -> c -> r
  got_ ep a b c = unsafePerformIO (Client.get_ @api ep a b c)

instance (API api, Typeable r, FromJSON r, Typeable a, ToJSON a, Typeable b, ToJSON b, Typeable c, ToJSON c, Typeable d, ToJSON d) => Got_ api (a -> b -> c -> d -> IO r) where
  type Unsafe (a -> b -> c -> d -> IO r) = a -> b -> c -> d -> r
  got_ ep a b c d = unsafePerformIO (Client.get_ @api ep a b c d)

instance (API api, Typeable r, FromJSON r, Typeable a, ToJSON a, Typeable b, ToJSON b, Typeable c, ToJSON c, Typeable d, ToJSON d, Typeable e, ToJSON e) => Got_ api (a -> b -> c -> d -> e -> IO r) where
  type Unsafe (a -> b -> c -> d -> e -> IO r) = a -> b -> c -> d -> e -> r
  got_ ep a b c d e = unsafePerformIO (Client.get_ @api ep a b c d e)

instance (API api, Typeable r, FromJSON r, Typeable a, ToJSON a, Typeable b, ToJSON b, Typeable c, ToJSON c, Typeable d, ToJSON d, Typeable e, ToJSON e, Typeable f, ToJSON f) => Got_ api (a -> b -> c -> d -> e -> f -> IO r) where
  type Unsafe (a -> b -> c -> d -> e -> f -> IO r) = a -> b -> c -> d -> e -> f -> r
  got_ ep a b c d e f = unsafePerformIO (Client.get_ @api ep a b c d e f)

instance (API api, Typeable r, FromJSON r, Typeable a, ToJSON a, Typeable b, ToJSON b, Typeable c, ToJSON c, Typeable d, ToJSON d, Typeable e, ToJSON e, Typeable f, ToJSON f, Typeable g, ToJSON g) => Got_ api (a -> b -> c -> d -> e -> f -> g -> IO r) where
  type Unsafe (a -> b -> c -> d -> e -> f -> g -> IO r) = a -> b -> c -> d -> e -> f -> g -> r
  got_ ep a b c d e f g = unsafePerformIO (Client.get_ @api ep a b c d e f g)

class Delete_ (api :: *) req where
  delete_ :: DELETE req -> req

instance (API api, Typeable r, FromJSON r) => Delete_ api (IO r) where
  delete_ ep = do
    let url = api @api <> toTxt ep
    r <- Fetch.delete Fetch.json url 
    case r of
      Fetch.Response (Fetch.Good _) (decodeEither -> Right r) -> pure r
      _ -> throw r

instance (API api, Typeable a, Typeable r, ToJSON a, FromJSON r) => Delete_ api (a -> IO r) where
  delete_ ep a = do
#ifdef __GHCJS__
    let url = api @api <> toTxt ep <> "?payload=" <> encodeURIComponent (btoa_js (encode a))
    r <- Fetch.delete Fetch.json url
    case r of
      Fetch.Response (Fetch.Good _) (decodeEither -> Right r) -> pure r
      _ -> throw r
#else
    let url = api @api <> toTxt ep <> "?payload=" <> encodeURIComponent (toTxt (B64.encode (Aeson.encode a)))
    r <- Fetch.delete Fetch.json url
    case r of
      Fetch.Response n (decodeEither -> Right r) -> pure r
      _ -> throw r
#endif

instance (API api, Typeable a, Typeable b, Typeable r, ToJSON a, ToJSON b, FromJSON r) => Delete_ api (a -> b -> IO r) where
  delete_ ep a b = Client.delete_ @api (fromTxt (toTxt ep)) (a,b)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable r, ToJSON a, ToJSON b, ToJSON c, FromJSON r) => Delete_ api (a -> b -> c -> IO r) where
  delete_ ep a b c = Client.delete_ @api (fromTxt (toTxt ep)) (a,b,c)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, FromJSON r) => Delete_ api (a -> b -> c -> d -> IO r) where
  delete_ ep a b c d = Client.delete_ @api (fromTxt (toTxt ep)) (a,b,c,d)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, FromJSON r) => Delete_ api (a -> b -> c -> d -> e -> IO r) where
  delete_ ep a b c d e = Client.delete_ @api (fromTxt (toTxt ep)) (a,b,c,d,e)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, FromJSON r) => Delete_ api (a -> b -> c -> d -> e -> f -> IO r) where
  delete_ ep a b c d e f = Client.delete_ @api (fromTxt (toTxt ep)) (a,b,c,d,e,f)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, FromJSON r) => Delete_ api (a -> b -> c -> d -> e -> f -> g -> IO r) where
  delete_ ep a b c d e f g = Client.delete_ @api (fromTxt (toTxt ep)) (a,b,c,d,e,f,g)

class Put_ (api :: *) req where
  put_ :: PUT req -> req

instance (API api, Typeable r, FromJSON r) => Put_ api (IO r) where
  put_ ep = do
    let url = api @api <> toTxt ep
    r <- Fetch.put Fetch.json url def
    case r of
      Fetch.Response (Fetch.Good _) (decodeEither -> Right r) -> pure r
      _ -> throw r

instance (API api, Typeable a, Typeable r, ToJSON a, FromJSON r) => Put_ api (a -> IO r) where
  put_ ep a = do
    let url = api @api <> toTxt ep
    r <- Fetch.put Fetch.json url (JSON.encode a) 
    case r of
      Fetch.Response (Fetch.Good _) (decodeEither -> Right r) -> pure r
      _ -> throw r
 
instance (API api, Typeable a, Typeable b, Typeable r, ToJSON a, ToJSON b, FromJSON r) => Put_ api (a -> b -> IO r) where
  put_ ep a b = Client.put_ @api (fromTxt (toTxt ep)) (a,b)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable r, ToJSON a, ToJSON b, ToJSON c, FromJSON r) => Put_ api (a -> b -> c -> IO r) where
  put_ ep a b c = Client.put_ @api (fromTxt (toTxt ep)) (a,b,c)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, FromJSON r) => Put_ api (a -> b -> c -> d -> IO r) where
  put_ ep a b c d = Client.put_ @api (fromTxt (toTxt ep)) (a,b,c,d)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, FromJSON r) => Put_ api (a -> b -> c -> d -> e -> IO r) where
  put_ ep a b c d e = Client.put_ @api (fromTxt (toTxt ep)) (a,b,c,d,e)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, FromJSON r) => Put_ api (a -> b -> c -> d -> e -> f -> IO r) where
  put_ ep a b c d e f = Client.put_ @api (fromTxt (toTxt ep)) (a,b,c,d,e,f)

instance (API api, Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, Typeable r, ToJSON a, ToJSON b, ToJSON c, ToJSON d, ToJSON e, ToJSON f, ToJSON g, FromJSON r) => Put_ api (a -> b -> c -> d -> e -> f -> g -> IO r) where
  put_ ep a b c d e f g = Client.put_ @api (fromTxt (toTxt ep)) (a,b,c,d,e,f,g)

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
    { construct = do
        rs <- newIORef def
        let stop = join (readIORef rs)
        forkIO do
          handle (\ThreadKilled -> stop) do
            void do
              retryingIO policy do
                mv <- newEmptyMVar
                pl <- askref self
                es <- new_event_source_js (host <> ep <> "?payload=" <> encodeURIComponent (btoa_js (encode pl)))

                msgs <- onRaw es "message" def \_ msg ->
                  case msg .# "data" of
                    Just d | Just e <- decode @Txt d -> yield @e e
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
      retryingIO policy do
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
      , Just a <- decode @Txt m
      = yield @a a
        
      | otherwise 
      = pure ()

newtype WebsocketError = WebsocketError JSV

wserror :: Exists Websocket => View -> (Producer WebsocketError => View)
wserror = OnMounted (\_ -> onRaw (it :: Websocket) "error" def (\_ -> yield . WebsocketError))
#endif

-- | Send a `GET` request to the given endpoint using `unsafePerformIO`
-- following the `Query` specification for `r`.
--
-- > instance Methods SomeR where
-- >   type Query SomeR = ParamA -> ParamB -> IO ResponseR
-- >
-- > let 
-- >   r :: ResponseR
-- >   r = query @SomeR a b
--
--
-- Notes: 
--   1. Use with or within `or` or `caught` is strongly suggested.
--   2. All parameters will be base64-encoded in a `payload` query parameter.
--      For large payloads, this could be an issue.
-- 
query :: forall api r. (Methods r, Got_ api (Query r)) => Unsafe (Query r)
query = Client.got_ @api (Endpoint.query @r)

-- | Send a `GET` request to the given endpoint following the `Query`
-- specification for `r`.
--
-- > instance Methods SomeR where
-- >   type Query SomeR = ParamA -> ParamB -> IO ResponseR
-- >
-- > r :: ResponseR <- query @SomeR a b
--
-- Note: All parameters will be base64-encoded in a `payload` query parameter.
--       For large payloads, this could be an issue.
-- 
query' :: forall api r. (Methods r, Get_ api (Query r)) => Query r
query' = Client.get_ @api (Endpoint.query @r)

-- | Send a `PATCH` request to the given endpoint following the `Update`
-- specification for `r`.
--
-- > instance Methods SomeR where
-- >   type Update SomeR = ParamA -> ParamB -> IO ResponseR
-- >
-- > r :: ResponseR <- update @SomeR a b
--
update :: forall api r. (Methods r, Patch_ api (Update r)) => Update r
update = Client.patch_ @api (Endpoint.update @r)

-- | Send a `POST` request to the given endpoint following the `Create`
-- specification for `r`.
--
-- > instance Methods SomeR where
-- >   type Create SomeR = ParamA -> ParamB -> IO ResponseR
-- >
-- > r :: ResponseR <- create @SomeR a b
--
create :: forall api r. (Methods r, Post_ api (Create r)) => Create r
create = Client.post_ @api (Endpoint.create @r)

-- | Send a `DELETE` request to the given endpoint following the `Delete`
-- specification for `r`.
--
-- > instance Methods SomeR where
-- >   type Delete SomeR = ParamA -> ParamB -> IO ResponseR
-- >
-- > r :: ResponseR <- delete @SomeR a b
--
delete :: forall api r. (Methods r, Delete_ api (Delete r)) => Delete r
delete = Client.delete_ @api (Endpoint.delete @r)

-- | Send a `PUT` request to the given endpoint following the `Replace`
-- specification for `r`.
--
-- > instance Methods SomeR where
-- >   type Replace SomeR = ParamA -> ParamB -> IO ResponseR
-- >
-- > r :: ResponseR <- replace @SomeR a b
--
replace :: forall api r. (Methods r, Put_ api (Replace r)) => Replace r
replace = Client.put_ @api (Endpoint.replace @r)
