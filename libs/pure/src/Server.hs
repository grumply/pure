{-# LANGUAGE CPP, RecordWildCards, OverloadedStrings, LambdaCase, BangPatterns, PatternSynonyms, ScopedTypeVariables, FlexibleContexts, BlockArguments, DerivingStrategies, NamedFieldPuns, DeriveGeneric, DeriveAnyClass, TypeApplications, RankNTypes, AllowAmbiguousTypes, FlexibleInstances #-}
module Server 
  ( Handler
  , serve
  , lambda, channel
  , middleware, cache, logging
  , Request(..)
  , withSecret, newSecret, secretFile
  , sign
  , authorized
  , Unauthorized
  , WarpTLS.tlsSettings
  ) where

import Auth

import Control.Concurrent
import Control.Exception (Exception,throw,handle)
import Control.Monad
import Control.Producer (Producer,yield)
import Control.Retry
import Control.State
import qualified Control.Component as Component
import qualified Control.Log as Log
import Data.DOM
import Data.Foldable
import Data.Function ((&))
import Data.Aeson as JSON
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
            let p = Txt.intercalate "/" (pathInfo request)
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
  channel path l = channel (fromTxt (toTxt path)) (\(a,b) -> l a b)

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

-- lambda (endpoint "get" :: Endpoint (IO r))

class Lambda a where
  lambda :: Endpoint a -> a -> Handler

instance {-# OVERLAPPING #-} ToJSON r => Lambda (IO r) where
  lambda (Endpoint (path,_)) l = Handler {..}
    where
      methods = [methodPost,methodGet,methodOptions]

      endpoint request respond
        | requestMethod request == methodOptions =
          respond (responseLBS status200 [(hAllow,"POST, GET, OPTIONS")] def)

        | otherwise =
          handle (\Unauthorized -> respond $ responseLBS status401 [] def) $
            respond . responseLBS status200 [(hContentType,"application/json")] . encode =<< l

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
          case eitherDecode pl of
            Left e -> respond (responseLBS status400 [] (encode e))
            Right (a :: a) ->
              handle (\Unauthorized -> respond $ responseLBS status401 [] def) $
                respond . responseLBS status200 [(hContentType,"application/json")] . encode =<< l a

instance (Typeable a, Typeable b, FromJSON a, FromJSON b, ToJSON r) => Lambda (a -> b -> IO r) where
  lambda path l = lambda (fromTxt (toTxt path)) (\(a,b) -> l a b)

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

newtype Secret = Secret ByteString
  deriving Show

instance ToJSON Secret where
  toJSON (Secret sec) = toJSON (show sec)

instance FromJSON Secret where
  parseJSON v = Secret . read <$> parseJSON v

newSecret :: IO Secret
newSecret = Secret . fromStrict <$> CRT.getRandomBytes 100

withSecret :: IO Secret -> (Exists Secret => View) -> View
withSecret = async

secretFile :: FilePath -> IO Secret
secretFile fp = do
  fe <- doesFileExist fp
  if fe then do
    ms <- decode <$> BSL.readFile fp
    case ms of
      Nothing -> do
        sec <- newSecret
        encodeFile fp sec
        pure sec
      Just s -> 
        pure s
  else do
    sec <- newSecret
    encodeFile fp sec
    pure sec

sign :: Exists Secret => [Txt] -> Token
sign claims = Token {..}
  where
    Secret s = it
    proof = fromString (show (hashWith SHA256 (toStrict (s <> encode claims))))

data Unauthorized = Unauthorized
  deriving Show
instance Exception Unauthorized

authorized :: Exists Secret => Txt -> r -> Token -> r
authorized prove r t 
  | Secret s <- it
  , prove `Data.Foldable.elem` claims t
  , h <- fromString (show (hashWith SHA256 (toStrict (s <> encode (claims t)))))
  , h == proof t
  = r

  | otherwise 
  = throw Unauthorized
