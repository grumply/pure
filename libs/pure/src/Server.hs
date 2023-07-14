{-# LANGUAGE CPP, RecordWildCards, OverloadedStrings, PatternSynonyms, ScopedTypeVariables, FlexibleContexts, BlockArguments, NamedFieldPuns, TypeApplications, RankNTypes, AllowAmbiguousTypes, FlexibleInstances, DerivingVia #-}
module Server 
  ( Handler
  , serve
  , lambda, channel
  , middleware, cache, logging, withIdentity
  , unauthorized
  , Host(..), Agent(..)
  , Request(..)
  , WarpTLS.tlsSettings
  ) where

import Control.Concurrent
import Control.Exception (Exception,throw,handle,catch)
import Control.Monad
import Control.Producer (Producer,yield)
import Control.Retry
import Control.State
import qualified Control.Component as Component
import qualified Control.Log as Log
import Data.DOM
import Data.Foldable
import Data.Function ((&))
import Data.Aeson as JSON hiding (Key)
import Data.Default
import Data.Exists
import Data.IORef
import qualified Data.List as List
import Data.Maybe
import Data.Time (pattern Seconds,pattern Second,Time)
import Data.Typeable
import Data.Txt as Txt
import Data.View
import GHC.Generics
import System.IO.Unsafe
import System.Directory
import System.Posix.Files

import Crypto.Hash
import qualified Crypto.Random.Types as CRT
import Data.String
import Data.ByteString.Lazy as BSL
import Data.Binary.Builder (fromLazyByteString)
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai
import Effect.Async
import Endpoint

serve :: Warp.Port -> Maybe WarpTLS.TLSSettings -> [Handler] -> View
serve port mtlss = Component $ \self -> 
  def
    { onConstruct = do
        ref <- ask self >>= newIORef 
        tid <- forkIO do
          maybe Warp.runSettings WarpTLS.runTLS mtlss (Warp.setServerName "pure" (Warp.setPort port Warp.defaultSettings)) $ \request respond -> do
            eps <- readIORef ref
            let p = toTxt (rawPathInfo request)
            case List.find (\Handler { methods, path } -> p == path && requestMethod request `Prelude.elem` methods) eps of
              Nothing -> respond (responseLBS status404 [] mempty)
              Just (Handler f _ _) -> f request respond
        pure (ref,tid)
    , onReceive = \eps (ref,tid) -> writeIORef ref eps >> pure (ref,tid)
    , onUnmounted = Data.View.get self >>= \(_,tid) -> killThread tid
    , render = \_ _ -> Data.View.Null
    }

data Handler = Handler { endpoint :: Application, methods :: [Method], path :: Txt }

class Channel a where
  channel :: Endpoint a -> a -> Handler

instance {-# OVERLAPPING #-} ToJSON r => Channel (IO [r]) where
  channel (Endpoint (path,_)) l = Handler {..}
    where
      methods = [methodPost,methodGet,methodOptions]

      endpoint _ respond = do
        let 
          responder write flush = 
            let
              push b = do
                write ("data: " <> fromLazyByteString (encode b) <> "\n\n")
                flush
            in
              handle (\Unauthorized -> pure ())
                (l >>= mapM_ push)

        respond (responseStream status200 
          [(hContentType,"text/event-stream")
          ,(hCacheControl,"no-cache")
          ,(hConnection,"keep-alive")
          ] responder
          )

instance (Typeable a, FromJSON a, ToJSON r) => Channel (a -> IO [r]) where 
  channel (Endpoint (path,_)) l = Handler {..}
    where
      methods = [methodPost,methodGet,methodOptions]

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
                    handle (\Unauthorized -> pure ())
                      (l a >>= mapM_ push)

              respond (responseStream status200 
                [(hContentType,"text/event-stream")
                ,(hCacheControl,"no-cache")
                ,(hConnection,"keep-alive")
                ] responder
                )

instance (Typeable a, Typeable b, FromJSON a, FromJSON b, ToJSON r) => Channel (a -> b -> IO [r]) where
  channel path l = channel (fromTxt (toTxt path)) (uncurry l)

instance (Typeable a, Typeable b, Typeable c, FromJSON a, FromJSON b, FromJSON c, ToJSON r) => Channel (a -> b -> c -> IO [r]) where
  channel path l = channel (fromTxt (toTxt path)) (\(a,b,c) -> l a b c)

instance (Typeable a, Typeable b, Typeable c, Typeable d, FromJSON a, FromJSON b, FromJSON c, FromJSON d, ToJSON r) => Channel (a -> b -> c -> d -> IO [r]) where
  channel path l = channel (fromTxt (toTxt path)) (\(a,b,c,d) -> l a b c d)

instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, ToJSON r) => Channel (a -> b -> c -> d -> e -> IO [r]) where
  channel path l = channel (fromTxt (toTxt path)) (\(a,b,c,d,e) -> l a b c d e)

instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, ToJSON r) => Channel (a -> b -> c -> d -> e -> f -> IO [r]) where
  channel path l = channel (fromTxt (toTxt path)) (\(a,b,c,d,e,f) -> l a b c d e f)

instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, ToJSON r) => Channel (a -> b -> c -> d -> e -> f -> g -> IO [r]) where
  channel path l = channel (fromTxt (toTxt path)) (\(a,b,c,d,e,f,g) -> l a b c d e f g)

class Lambda a where
  lambda :: Endpoint a -> a -> Handler

instance {-# OVERLAPPING #-} ToJSON r => Lambda (IO r) where
  lambda (Endpoint (path,_)) l = Handler {..}
    where
      methods = [methodPost,methodGet,methodOptions]

      endpoint request respond
        | requestMethod request == methodOptions =
          respond (responseLBS status200 [(hAllow,"POST, GET, OPTIONS")] def)

        | otherwise = do
          r <- handle (\Unauthorized -> pure (responseLBS unauthorized401 [] def)) do
            responseLBS status200 [(hContentType,"application/json")] . encode <$> l
          respond r

instance (Typeable a, FromJSON a, ToJSON r) => Lambda (a -> IO r) where 
  lambda (Endpoint (path,_)) l = Handler {..}
    where
      methods = [methodPost,methodGet,methodOptions]

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
          respond =<< case eitherDecode pl of
            Left e -> pure (responseLBS status400 [] (encode e))
            Right (a :: a) ->
              handle (\Unauthorized -> pure (responseLBS unauthorized401 [] def)) do
                responseLBS status200 [(hContentType,"application/json")] . encode <$> l a

instance (Typeable a, Typeable b, FromJSON a, FromJSON b, ToJSON r) => Lambda (a -> b -> IO r) where
  lambda path l = lambda (fromTxt (toTxt path)) (uncurry l)

instance (Typeable a, Typeable b, Typeable c, FromJSON a, FromJSON b, FromJSON c, ToJSON r) => Lambda (a -> b -> c -> IO r) where
  lambda path l = lambda (fromTxt (toTxt path)) (\(a,b,c) -> l a b c)

instance (Typeable a, Typeable b, Typeable c, Typeable d, FromJSON a, FromJSON b, FromJSON c, FromJSON d, ToJSON r) => Lambda (a -> b -> c -> d -> IO r) where
  lambda path l = lambda (fromTxt (toTxt path)) (\(a,b,c,d) -> l a b c d)

instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, ToJSON r) => Lambda (a -> b -> c -> d -> e -> IO r) where
  lambda path l = lambda (fromTxt (toTxt path)) (\(a,b,c,d,e) -> l a b c d e)

instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, ToJSON r) => Lambda (a -> b -> c -> d -> e -> f -> IO r) where
  lambda path l = lambda (fromTxt (toTxt path)) (\(a,b,c,d,e,f) -> l a b c d e f)

instance (Typeable a, Typeable b, Typeable c, Typeable d, Typeable e, Typeable f, Typeable g, FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, ToJSON r) => Lambda (a -> b -> c -> d -> e -> f -> g -> IO r) where
  lambda path l = lambda (fromTxt (toTxt path)) (\(a,b,c,d,e,f,g) -> l a b c d e f g)

cache :: Time -> Middleware
cache (Seconds duration _) app request respond = 
  app request 
    (respond . mapResponseHeaders (++ [(hCacheControl,"public, max-age=" <> fromTxt (toTxt @Int (round duration)))]))

middleware :: Middleware -> Handler -> Handler
middleware mw Handler {..} = 
  Handler
    { endpoint = mw endpoint
    , ..
    }

logging :: Log.Logging => Log.Level -> Middleware
logging level app request respond = do
  Log.log level (show request)
  app request respond
 
withIdentity :: Lambda r => Endpoint r -> (Host -> Agent -> r) -> Handler
withIdentity ep f = Handler (go :: Application) [methodPost,methodGet,methodOptions] (toTxt ep)
  where
    go request respond =
      let 
        h = fromTxt (Txt.init (Txt.dropWhileEnd (/= ':') (toTxt (show (remoteHost request)))))
        ua = fromTxt (maybe def toTxt (requestHeaderUserAgent request))

      in
        Server.endpoint (lambda ep (f h ua)) request respond

-- Note that Unauthorized is not exported; only lambda and channel can catch Unauthorized.
-- lambda: sends unauthorized401
-- channel: stops SSE connection
data Unauthorized = Unauthorized deriving Show
instance Exception Unauthorized

unauthorized :: a
unauthorized = throw Unauthorized
