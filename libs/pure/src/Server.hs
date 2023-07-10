{-# LANGUAGE CPP, RecordWildCards, OverloadedStrings, LambdaCase, BangPatterns, PatternSynonyms, ScopedTypeVariables, FlexibleContexts, BlockArguments, DerivingStrategies, NamedFieldPuns, DeriveGeneric, DeriveAnyClass, TypeApplications, RankNTypes, AllowAmbiguousTypes #-}
module Server 
  ( Endpoint
  , serve
  , lambda, channel
  , middleware, cache, logging
  , Request(..)
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
import Data.View
import GHC.Generics
import System.IO.Unsafe

import Data.String
import Data.ByteString.Lazy as BSL
import Data.Binary.Builder (fromLazyByteString)
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Network.Wai.Handler.Warp as Warp
import Network.HTTP.Types.Header
import Network.HTTP.Types.Method
import Network.HTTP.Types.Status
import Network.Wai
-- import Network.Wai.Middleware.Cors as Cors

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

data Endpoint = Endpoint { endpoint :: Application, methods :: [Method], path :: Txt }

channel :: forall a b. (Typeable a, FromJSON a, ToJSON b) => Txt -> (a -> IO [b]) -> Endpoint
channel path l = Endpoint {..}
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
                  l a >>= mapM_ push

            respond (responseStream status200 
              [(hContentType,"text/event-stream")
              ,(hCacheControl,"no-cache")
              ,(hConnection,"keep-alive")
              ] responder
              )

lambda :: forall a b. (Typeable a, FromJSON a, ToJSON b) => Txt -> (a -> IO b) -> Endpoint
lambda path l = Endpoint {..}
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
          Right (a :: a) -> respond . responseLBS status200 [(hContentType,"application/json")] . encode =<< l a

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


