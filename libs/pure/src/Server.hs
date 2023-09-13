{-# LANGUAGE CPP, RecordWildCards, OverloadedStrings, PatternSynonyms, ScopedTypeVariables, FlexibleContexts, BlockArguments, NamedFieldPuns, TypeApplications, RankNTypes, AllowAmbiguousTypes, FlexibleInstances, DerivingVia, TypeFamilies, ConstrainedClassMethods, DefaultSignatures #-}
module Server 
  ( Handler
  , serve
  , lambda, channel
  , middleware, cache, logging, withIdentity
  , unauthorized
  , Host(..), Agent(..)
  , Request(..)
  , WarpTLS.tlsSettings
  , Server(..)
  , Name,Auth,Event,Product,Preview,Query,Result
  , respond, respondWith
  , module Export
  ) where

import qualified Pure as Export hiding (Handler,read,Read)

import Control.Concurrent
import Control.Exception (SomeException,Exception(..),throw,handle,catch)
import Control.Monad
import qualified Data.Log as Log
import Data.Aeson as JSON hiding (Name,Result)
import Data.Default
import Data.DOM
import Data.Foldable
import Data.Function ((&))
import Data.IORef
import Data.Map as Map
import Data.Kind
import qualified Data.List as List
import Data.Maybe
import Data.Time (pattern Seconds,pattern Second,Time)
import Data.Traversable
import Data.Typeable
import Data.Txt as Txt
import Data.View hiding (Event,Handler,channel)
import GHC.Generics
import System.IO.Unsafe
import System.Directory
import System.Posix.Files

import Crypto.Hash hiding (Context)
import qualified Crypto.Random.Types as CRT
import Data.String
import Data.ByteString.Lazy as BSL
import Data.Binary.Builder (fromLazyByteString)
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import Network.HTTP.Types.Header as Export
import Network.HTTP.Types.Method 
import Network.HTTP.Types.Status as Export
import Network.Wai
import Endpoint

serve :: Warp.Port -> Maybe WarpTLS.TLSSettings -> [Handler] -> View
serve port mtlss = Component $ \self -> 
  def
    { onConstruct = do
        ref <- askref self >>= newIORef 
        tid <- forkIO do
          maybe Warp.runSettings WarpTLS.runTLS mtlss (Warp.setOnException exception (Warp.setServerName "pure" (Warp.setOnExceptionResponse onExceptionResponse (Warp.setPort port Warp.defaultSettings)))) $ \request respond -> do
            eps <- readIORef ref
            let p = toTxt (rawPathInfo request)
            case List.find (\Handler { methods, path } -> p == path && requestMethod request `Prelude.elem` methods) eps of
              Nothing -> respond (responseLBS status404 [] mempty)
              Just (Handler f _ _) -> f request respond
        pure (ref,tid)
    , onReceive = \eps (ref,tid) -> writeIORef ref eps >> pure (ref,tid)
    , onUnmounted = getref self >>= \(_,tid) -> killThread tid
    , render = \_ _ -> Data.View.Null
    }
  where
    exception :: Maybe Request -> SomeException -> IO ()
    exception r se
      | Just Respond {} <- fromException se
      = pure ()

      | otherwise
      = Warp.defaultOnException r se
      
    onExceptionResponse :: SomeException -> Response
    onExceptionResponse e
      | Just (Respond s hs t) <- fromException e 
      = responseLBS s hs (fromTxt t)

      | otherwise 
      = Warp.defaultOnExceptionResponse e

-- I think it would be beneficial to parameterize Handler with a reified constraint context;
-- if `Handler App` => { endpoint :: Props App |- Application, methods :: [Method], path :: Txt }
-- then we can store the entailment-based endpoint statically in a hashmap and do O(1) dispatch.
-- Then the server would be responsible for satisfying the entailment proof at execution.
-- This would maintain the dynamics of each endpoint, while reducing the update cost - the
-- server could avoid being parameterized on the `[Handler App]`. This would prevent dynamism in
-- the set of handlers, but not dynamism in the handlers themselves. This might be a worthy
-- trade-off. It does, however, introduce the possibility of accidental constraint satisfaction;
-- if the `endpoint :: Props App |- Application` captures an existential, it would be annoying to
-- debug/catch. It would be nice to be able to have a constraint that can /only/ be satisfied in
-- the entailment monad, but I don't yet see how to approach that. Another issue is the cost of
-- constraint satisfaction; I know GHC is exceptional at eliminating unused variables, but the
-- server will still have to satisfy all of the constraints in `Props App` manually, regardless
-- of which ones the endpoint will actually use - the endpoint itself can drop whatever it 
-- doesn't need, but I think the server will still have to pass them, which could be costly, 
-- though I'm not sure how costly the current approach is and how the two might compare.
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

logging :: Log.Logging Request => Log.Level -> Middleware
logging level app request respond = do
  Log.log level request
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

-- Note here that the middlewares are context-dependent, but do not have access to
-- authentication, etc.... 
--
-- Some thoughts:
--  1. This doesn't seem to render the middleware safe. That is, third-party middlewares
--     can still see the content of the request.
--  2. This doesn't allow for authentication-dependent middlewares, like caching based
--     on user role, etc....
--
class Resource r => Server r where

  type Context r :: Constraint
  type Context r = ()

  handlers :: Context r => [Server.Handler]
  default handlers 
    :: ( Context r
       , ToJSON r, FromJSON r
       , Typeable (Auth r), FromJSON (Auth r)
       , Typeable (Name r), FromJSON (Name r)
       , ToJSON (Product r), FromJSON (Product r)
       , Typeable (Event r), ToJSON (Event r), FromJSON (Event r)
       , ToJSON (Preview r)
       , ToJSON (Result r)
       , Typeable (Query r), FromJSON (Query r)
       , ToJSON (Name r)
       ) => [Server.Handler]
  handlers = 
    [ middleware (createMiddleware @r) (lambda (Endpoint.create @r) (Server.create @r))
    , middleware (readMiddleware @r) (lambda (Endpoint.read @r) (Server.read @r))
    , middleware (rawMiddleware @r) (lambda (Endpoint.raw @r) (Server.raw @r))
    , middleware (updateMiddleware @r) (lambda (Endpoint.update @r) (Server.update @r))
    , middleware (queryMiddleware @r) (lambda (Endpoint.query @r) (Server.query @r))
    ]

  create :: Context r => Auth r -> r -> IO (Maybe (Name r))
  create = unauthorized

  createMiddleware :: Context r => Middleware
  createMiddleware = id

  raw :: Context r => Auth r -> Name r -> IO (Maybe r)
  raw = unauthorized

  rawMiddleware :: Context r => Middleware
  rawMiddleware = id

  read :: Context r => Maybe (Auth r) -> Name r -> IO (Maybe (Product r))
  read = unauthorized
  
  readMiddleware :: Context r => Middleware
  readMiddleware = id

  update :: Context r => Auth r -> Name r -> Event r -> IO ()
  update = unauthorized
  
  updateMiddleware :: Context r => Middleware
  updateMiddleware = id

  query :: Context r => Maybe (Auth r) -> Maybe (Query r) -> IO (Result r)
  query = unauthorized
  
  queryMiddleware :: Context r => Middleware
  queryMiddleware = id

data Respond = Respond Status [Header] Txt deriving Show
instance Exception Respond

respond :: Int -> Txt -> a
respond c = respondWith (toEnum c) []

respondWith :: Status -> [Header] -> Txt -> a
respondWith s hs = Control.Exception.throw . Respond s hs
