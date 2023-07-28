{-# LANGUAGE CPP, OverloadedStrings, ScopedTypeVariables, FlexibleContexts, BlockArguments, DerivingStrategies, TypeApplications, RankNTypes, AllowAmbiguousTypes, FlexibleInstances, MultiParamTypeClasses, TypeFamilies, TypeOperators, DefaultSignatures #-}
module Client 
  ( Client.post
  , Client.get
  , Client.got
  , Get(), Got(Unsafe), Post()
  , Client.catch, Client.or, Client.within
  -- ,ws,wssend,wsmessage,wserror
  , Fetch.XHRError, Fetch.err, Fetch.response
  , sseWith, sse
  , base, API(..)
  , Index, Auth, Name, Event, Product, Preview
  , Client(..)
  , module Export
  ) where

import Pure as Export hiding (read,list,get,index,Read,or,catch,within)

#ifdef __GHCJS__
import Data.JSON as JSON hiding (Key)
#else
import Data.Aeson as JSON hiding (Key)
#endif

import Control.Concurrent
import Control.Component hiding (Update)
import Control.Dynamic
import Control.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Producer
import Control.Retry
import Control.State
import Data.Default
import Data.DOM
import Data.Exists
import Data.Function ((&))
import Data.IORef
import Data.Kind
import Data.Maybe
import Data.Time
import Data.Typeable
import Data.Txt
import Data.URI
import Data.View
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Data.Fetch as Fetch
import qualified Effect.Fetch as Fetch (response,err)
import Effect.Router
import Endpoint
import System.IO.Unsafe
import Prelude hiding (or)

class Post api req where
  post :: Endpoint req -> req

instance (API api, Typeable r, FromJSON r) => Post api (IO r) where
  post ep = Fetch.postWith @() @r [] (api @api <> toTxt ep) () >>= either Control.Exception.throw pure

instance (API api, Typeable a, Typeable r, ToJSON a, FromJSON r) => Post api (a -> IO r) where
  post ep a = Fetch.postWith @a @r [] (api @api <> toTxt ep) a >>= either Control.Exception.throw pure
 
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

class Get api req where
  get :: Endpoint req -> req

instance (API api, Typeable r, FromJSON r) => Get api (IO r) where
  get ep = Fetch.getWith @r [] (api @api <> toTxt ep) >>= either Control.Exception.throw pure

instance (API api, Typeable a, Typeable r, ToJSON a, FromJSON r) => Get api (a -> IO r) where
  get ep a = 
#ifdef __GHCJS__
    Fetch.getWith @r [] (api @api <> toTxt ep <> "?payload=" <> encodeURIComponent (btoa_js (encode a)))
      >>= either Control.Exception.throw pure
#else
    Fetch.getWith @r [] (api @api <> toTxt ep <> "?payload=" <> encodeURIComponent (toTxt (B64.encode (JSON.encode a))))
      >>= either Control.Exception.throw pure
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
  got :: Endpoint req -> Unsafe req

within :: Time -> a -> a
within t a = unsafePerformIO (fromJust <$> timeout t (evaluate a))

catch :: Exception e => a -> (e -> a) -> a
catch a f = unsafePerformIO (Control.Exception.catch (evaluate a) (pure . f))

or :: a -> a -> a
or l r = Client.catch @SomeException l (const r)

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
                pl <- Data.View.ask self
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

class Client r where 

  type Context r :: Constraint 
  type Context r = ()

  routes :: forall x. Context r => Maybe (Auth r) -> Routing (x :=> View) ()
  default routes 
    :: forall x.
       ( Context r
       , Resource r
       , API r
       , Typeable r
       , Typeable (Context r)
       , Typeable (Index r)
       , Typeable (Preview r)
       , Typeable (Event r)
       , Typeable (Product r)
       , Typeable (Auth r), ToJSON (Auth r)
       , Typeable (Name r), FromTxt (Name r), ToJSON (Name r)
       , ToJSON r, FromJSON r
       , ToJSON (Event r)
       , FromJSON (Preview r)
       , FromJSON (Product r)
       , FromJSON (Index r)
       ) => Maybe (Auth r) -> Routing (x :=> View) ()
  routes (Just a) = void do

    Effect.Router.match (fromTxt (toTxt (base @r))) do
      rs <- liftIO (Client.get @r (Endpoint.index @r))
      route (with rs (Client.index @r))

    path (fromTxt (toTxt (base @r))) do

      path "/new" do
        route do
          cont @(Context r) do
            stream (post @r (Endpoint.create @r) a) do 
              dynamic (Client.create @r)
        
      path "/:res" do
        k <- "res" 
        path "/edit" do
          mr <- liftIO (post @r (Endpoint.raw @r) a k)
          case mr of
            Nothing -> 
              route do
                cont @(Context r) do
                  stream (post @r (Endpoint.create @r) a) do 
                    dynamic (Client.create @r)
            Just r -> 
              stream (post @r (Endpoint.update @r) a k) do
                route (with r (with k (Client.update @r)))
        mr <- liftIO (Client.get @r (Endpoint.read @r) k)
        case mr of
          Nothing -> do
            rs <- liftIO (Client.get @r (Endpoint.index @r))
            route (with rs (Client.index @r))
          Just r ->
            route (with k (with r (Client.read @r)))

  routes Nothing = void do

    Effect.Router.match (fromTxt (toTxt (base @r))) do
      rs <- liftIO (Client.get @r (Endpoint.index @r))
      route (with rs (Client.index @r))
    
    path (fromTxt (toTxt (base @r)) <> "/:res") do
      k <- "res" 
      mr <- liftIO (Client.get @r (Endpoint.read @r) k)
      case mr of
        Nothing -> do
          rs <- liftIO (Client.get @r (Endpoint.index @r))
          route (with rs (Client.index @r))
        Just r ->
          route (with k (with r (Client.read @r)))

  create :: (Context r, Producer r) => View
  create = Data.View.Null

  read :: (Context r, Exists (Name r), Exists (Product r)) => View
  read = Data.View.Null

  update :: (Context r, Exists r, Producer (Event r)) => View
  update = Data.View.Null

  index :: (Context r, Exists (Index r)) => View
  index = Data.View.Null
