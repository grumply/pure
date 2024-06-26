{-# LANGUAGE CPP, RecordWildCards, OverloadedStrings, PatternSynonyms, ScopedTypeVariables, FlexibleContexts, BlockArguments, NamedFieldPuns, TypeApplications, RankNTypes, AllowAmbiguousTypes, FlexibleInstances, DerivingVia, TypeFamilies, ConstrainedClassMethods, DefaultSignatures, MultiWayIf, DataKinds, MultiParamTypeClasses #-}
{-# OPTIONS_GHC -O2 #-}
module Server 
  ( Handler(..)
  , serve
  , Lambda(..), Channel(..), ToMethod(..)
  , SomeEventId(),EventId(..),lastEventId
  , middleware, cache, logging
  , path, method, agent, host, match
  , unauthorized
  , Host(..), Agent(..)
  , Request(..)
  , WarpTLS.tlsSettings
  , Server(..)
  , Methods,Create,Update,Query
  , respond, respondWith
  , respondFile, respondFileWith
  , respondFilePart, respondFilePartWith
  , corsHeaders
  , Respond
  , module Export
  ) where

import qualified Pure as Export hiding (Handler,read,Read,Key)

import Control.Concurrent
import Control.Exception as E (SomeException,Exception(..),throw,handle,catch,evaluate)
import Control.DeepSeq (force)
import Control.Monad
import Data.Aeson as JSON hiding (Name,Result)
import Data.Coerce
import Data.Default
import Data.DOM
import Data.Foldable
import Data.Function ((&))
import Data.IORef
import qualified Data.Log as Log
import Data.Map as Map
import Data.Kind
import qualified Data.List as List
import Data.Maybe
import Data.Marker (Marker(..))
import Data.Key (Key(..))
import Data.Time (pattern Seconds,pattern Second,Time)
import Data.Traversable
import Data.Typeable
import Data.Txt as Txt
import Data.Exists hiding (Handler)
import Data.View hiding (Event,Handler,channel,force)
import Data.Void
import GHC.Generics
import Text.Read (readMaybe)

import qualified Data.ByteString as BS
import Data.ByteString.Lazy as BSL
import Data.Binary.Builder (fromLazyByteString)
import qualified Data.ByteString.Base64.Lazy as B64
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai.Handler.WarpTLS as WarpTLS
import Network.HTTP.Types.Header as Export
import Network.HTTP.Types.Method as Method
import Network.HTTP.Types.Status as Export
import Network.Wai
import Endpoint

{- |

A web server for use in @pure@ applications (with GHC, not GHCJS). 

# Handler

A server is, essentially, '[Handler]'.

  > newtype Handler = Handler { handler :: Exists Request => Maybe Application }

'Handler' is, therefore, equivalent to @'Request' -> Maybe 'Application'@.

The server chooses the first matching 'Handler', something like:

  > listToMaybe (mapMaybe (($ currentRequest) . handler) handlers)

The server component is a 'View'. Thus, embedding is implicit.

  > serve :: Port -> Maybe TLSSettings -> [Handler] -> View

Reactivity is available for the '[Handler]' (but not the 'Port' or 
'Maybe TLSSettings'); changes to a 'Handler' are propagated to the live
server implicitly through the server component's 'onReceive'. This is achieved
by:

  1. When the server 'View' component receives a new '[Handler]' properties, 
     it writes them to an @IORef@.

  2. Every request received by the @warp@ server uses the most-recent handlers
     by reading the @IORef '[Handler]'@. 
  
This is safe in the majority of cases, but could lead to issues depending on
what you allow, reactively, into the server. A server may be wrapped with
'static' to prevent reactivity, if desired for: safety, performance, isolation,
or correctness. Other approaches to controlling reactivity, like 'reactive'
and 'weak', are also available.

# Lambda

There is a convenience method for constructing low-level handlers when the
arguments and responses are JSON.

  > lambda :: Endpoint method a -> Bool -> Bool -> [Header] -> (Exists Request => a) -> Handler

It handles up-to arity 7 for the handler, @a@. The booleans are development flags
for replying, if true, with internal JSON-decoding errors and exceptions, respectively.

As an example, here is a simple server to roll an arbitrary-sided die:

  > roll :: GET (Int -> IO Int)
  > roll = "/roll"
  >
  > server :: View
  > server = 
  >   state (initialSeed 42) do
  >     serve 8081 Nothing 
  >       [ lambda roll False False [] rollImpl
  >       ]
  >
  >   where
  >     rollImpl :: State Seed => Int -> IO Int
  >     rollImpl n = 
  >       let (seed,result) = generate (uniformR 0 n) it
  >       in put seed >> pure result

Note that in the above example, it is possible to have two calls that, arriving
sufficiently close together, produce the same value (use the same random seed).
To prevent this, you could protect the seed with an 'MVar' and use 'modifyMVar'
in the @lambda@. 

# Channel

For 'SSE', or coment-style handlers, there is: 

  > channel :: Endpoint 'Endpoint.GET a -> Bool -> (Exists Request => a) -> Handler

Channels are written much like lambdas, but have access to 'lastEventId', for
resumption. Like 'Lambda', 'Channel' has instances for up to 7 arguments, and
works for any 'GET' returning a @ToJSON a => IO [a]@.

# Server

'Server' is a convenience class that uses 'Methods' and 'Lambda'.

One possible equivalent definition of the demonstrated @roll@ lambda is:

  - shared

    > data Roll = Roll Int
    >
    > instance Methods Roll where
    >   type Query _ = Int -> IO Roll

  - backend

    > instance Server Dice where
    >   Type Env Dice = State Seed
    >   query n = 
    >     let (seed,result) = generate (uniformR 0 n) it 
    >     in put seed >> pure (Roll result)

Note that the 'query' method needs access to the random seed state, so an
'Env' is defined for all handlers of @Server Dice@.

'Server' also has instance methods for CORS headers to send with all responses
(that defaults to @[]@), flags for sending exceptions and parse errors,
per-method middlewares, and default @501 Not Implemented@ responses for all methods:

  > class Methods x => Server x where
  >   showParseErrors :: Bool
  >   showParseErrors = False
  >
  >   showExceptions :: Bool
  >   showExceptions = False
  >
  >   cors :: [Header]
  >   cors = []
  >
  >   createMiddleware :: (Exists Request, Env r) => Middleware
  >   createMiddleware = id
  >
  >   updateMiddleware :: (Exists Request, Env r) => Middleware
  >   updateMiddleware = id
  >    
  >   queryMiddleware :: (Exists Request, Env r) => Middleware
  >   queryMiddleware = id
  >
  >   replaceMiddleware :: (Exists Request, Env r) => Middleware
  >   replaceMiddleware = id
  >
  >   deleteMiddleware :: (Exists Request, Env r) => Middleware
  >   deleteMiddleware = id
  >
  >   create :: (Exists Request, Env x) => Create x
  >   create = respond 501 mempty
  >
  >   update :: (Exists Request, Env x) => Update x
  >   update = respond 501 mempty
  >   
  >   query :: (Exists Request, Env x) => Query x
  >   query = respond 501 mempty
  >
  >   replace :: (Exists Request, Env x) => Replace x
  >   replace = respond 501 mempty
  >
  >   delete :: (Exists Request, Env x) => Delete x
  >   delete = respond 501 mempty
  >

Note that `Methods` has defaults, as well:

  > class Methods (r :: *) where
  > 
  >   type Create r :: *
  >   type Create r = Void
  >
  >   type Update r :: *
  >   type Update r = Void
  >
  >   type Query r :: *
  >   type Query r = Void
  >
  >   type Replace r :: *
  >   type Replace r = Void
  >
  >   type Delete r :: *
  >   type Delete r = Void
  >
  >   endpoint :: Endpoint method x
  >   default endpoint :: Typeable r => Endpoint method x
  >   endpoint = defaultBase @r
   
Server sends a @501 Not Implemented@ for all unimplemented and all 'Void' 
methods.

# Communication

Use 'query', 'query\'', 'update', 'create', 'delete', or 'replace' to make
requests to a server implementing 'Server'.

Use 'post_', 'patch_', 'delete_', 'put_', 'get_', or 'got_' with a custom 
'Endpoint' to make stand-alone requests to 'lambda's or 'channel's.

Request payloads are expected to be base64-encoded json, which is handled
automatically here in 'Server' and in 'Client'. 'query' and 'delete' encode the
payload in the url, but other methods use the body. 'Server' supports both for
'GET' and 'DELETE', but only body payloads for 'POST', 'PATCH', and 'PUT'.

-}

serve :: Warp.Port -> Maybe WarpTLS.TLSSettings -> [Handler] -> View
serve port mtlss = Component $ \self -> 
  def
    { construct = do
        ref <- askref self >>= newIORef 
        tid <- forkIO do
          maybe Warp.runSettings WarpTLS.runTLS mtlss (Warp.setOnException exception (Warp.setServerName "pure" (Warp.setOnExceptionResponse onExceptionResponse (Warp.setPort port Warp.defaultSettings)))) $ \request respond ->
            with request do
              eps <- readIORef ref
              case listToMaybe (Data.Maybe.mapMaybe handler eps) of
                Nothing -> respond (responseLBS notFound404 [] mempty)
                Just f  -> f request respond
        pure (ref,tid)
    , onReceive = \eps (ref,tid) -> writeIORef ref eps >> pure (ref,tid)
    , onUnmounted = getref self >>= \(_,tid) -> killThread tid
    , render = \_ _ -> Data.View.Null
    }
  where
    exception :: Maybe Request -> SomeException -> IO ()
    exception r se
      | Just (_ :: Respond) <- fromException se = pure ()
      | Just Unauthorized <- fromException se = pure ()
      | otherwise = Warp.defaultOnException r se
      
    onExceptionResponse :: SomeException -> Response
    onExceptionResponse e
      | Just r <- fromException e 
      = case r of
          Respond s hs t -> responseLBS s hs (fromTxt t)
          RespondFile s hs fp -> responseFile s hs fp Nothing
          RespondFilePart s hs fp p -> responseFile s hs fp (Just p)

      | Just Unauthorized <- fromException e
      = responseLBS unauthorized401 [] def

      | otherwise 
      = Warp.defaultOnExceptionResponse e

newtype Handler = Handler { handler :: Exists Request => Maybe Application }

method :: Exists Request => Method.Method
method = requestMethod it

path :: Exists Request => Txt
path = toTxt (rawPathInfo it)

host :: Exists Request => Host
host = fromTxt (Txt.init (Txt.dropWhileEnd (/= ':') (toTxt (show (remoteHost it)))))

agent :: Exists Request => Agent
agent = fromTxt (maybe def toTxt (requestHeaderUserAgent it))
 
class ToMethod (method :: Endpoint.Method) where
  toMethod :: Method.Method

instance ToMethod 'Endpoint.GET where toMethod = methodGet
instance ToMethod 'Endpoint.HEAD where toMethod = methodHead
instance ToMethod 'Endpoint.POST where toMethod = methodPost
instance ToMethod 'Endpoint.PATCH where toMethod = methodPatch
instance ToMethod 'Endpoint.PUT where toMethod = methodPut
instance ToMethod 'Endpoint.DELETE where toMethod = methodDelete
instance ToMethod 'Endpoint.OPTIONS where toMethod = methodOptions
instance ToMethod 'Endpoint.CONNECT where toMethod = methodConnect

match :: forall method x. ToMethod method => Endpoint method x -> (Exists Request => Bool)
match (Endpoint _ p) = method == toMethod @method && path == p

newtype SomeEventId = SomeEventId Txt
class EventId x where
  toEventId :: x -> SomeEventId
  default toEventId :: Show x => x -> SomeEventId
  toEventId = SomeEventId . toTxt . show
  fromEventId :: SomeEventId -> Maybe x
  default fromEventId :: Read x => SomeEventId -> Maybe x
  fromEventId (SomeEventId x) = readMaybe (fromTxt x)
instance EventId Txt where
  toEventId = SomeEventId
  fromEventId (SomeEventId eid) = Just eid
instance EventId Int
instance EventId Integer
instance EventId (Marker a) where
  toEventId = SomeEventId . toTxt
  fromEventId (SomeEventId e) = Just (fromTxt e)
instance EventId Data.Key.Key where
  toEventId = SomeEventId . toTxt
  fromEventId (SomeEventId e) = Just (fromTxt e)

lastEventId :: (Exists Request, EventId x) => Maybe x
lastEventId = do
  eid <- Prelude.lookup "last-event-id" (requestHeaders it)
  fromEventId (SomeEventId (toTxt eid))

class Channel a where
  channel :: Endpoint 'Endpoint.GET a -> Bool -> (Exists Request => a) -> Handler

instance Channel Void where
  channel ep _ _ = Handler (if match ep then Just endpoint else Nothing)
    where
      endpoint :: Exists Request => Application
      endpoint request respond = respond (responseLBS status501 [] def) 

instance {-# OVERLAPPING #-} ToJSON r => Channel (IO [(SomeEventId,r)]) where
  channel ep _ l = Handler (if match ep then Just endpoint else Nothing)
    where
      endpoint :: Exists Request => Application
      endpoint _ respond = do
        let 
          responder write flush = 
            let
              push (SomeEventId eid,b) = do
                write ("id: " <> fromLazyByteString (fromTxt eid) <> "\ndata: " <> fromLazyByteString (encode b) <> "\n\n")
                flush

              handler e 
                -- short-circuits simply end the stream without raising an error
                -- while unknown exceptions get re-thrown.
                | Just Unauthorized   <- fromException e = pure ()
                | Just (_ :: Respond) <- fromException e = pure () 
                | otherwise                              = E.throw e

            in 
              handle handler (l >>= mapM_ push)

        respond (responseStream status200 
          [(hContentType,"text/event-stream")
          ,(hCacheControl,"no-cache")
          ,(hConnection,"keep-alive")
          ] responder
          )

instance {-# OVERLAPPABLE #-} ToJSON r => Channel (IO [r]) where
  channel ep _ l = Handler (if match ep then Just endpoint else Nothing)
    where
      endpoint :: Exists Request => Application
      endpoint _ respond = do
        let 
          responder write flush = 
            let
              push b = do
                write ("data: " <> fromLazyByteString (encode b) <> "\n\n")
                flush

              handler e 
                -- short-circuits simply end the stream without raising an error
                -- while unknown exceptions get re-thrown.
                | Just Unauthorized   <- fromException e = pure ()
                | Just (_ :: Respond) <- fromException e = pure () 
                | otherwise                              = E.throw e

            in 
              handle handler (l >>= mapM_ push)

        respond (responseStream status200 
          [(hContentType,"text/event-stream")
          ,(hCacheControl,"no-cache")
          ,(hConnection,"keep-alive")
          ] responder
          )

instance {-# OVERLAPPING #-} (FromJSON a, ToJSON r) => Channel (a -> IO [(SomeEventId,r)]) where 
  channel ep showParseErrors l = Handler (if match ep then Just endpoint else Nothing)
    where
      endpoint :: Exists Request => Application
      endpoint request respond = do
        let
          payload
            | method == methodGet || method == methodDelete
            , Just b64 <- join (List.lookup "payload" (queryString request))
            , Right bs <- B64.decode (BSL.fromStrict b64) 
            = pure bs

            | otherwise = consumeRequestBodyLazy request

        pl <- payload
        case eitherDecode pl of
          Left e -> respond (responseLBS status400 [] (if showParseErrors then encode e else def))
          Right (a :: a) -> do
            let 
              responder write flush = 
                let
                  push (SomeEventId eid,b) = do
                    write ("id: " <> fromLazyByteString (fromTxt eid) <> "\ndata: " <> fromLazyByteString (encode b) <> "\n\n")
                    flush

                  handler e
                    -- short-circuits simply end the stream without raising an error
                    -- while unknown exceptions get re-thrown.
                    | Just Unauthorized   <- fromException e = pure ()
                    | Just (_ :: Respond) <- fromException e = pure ()
                    | otherwise                              = E.throw e

                in
                  handle handler (l a >>= mapM_ push)

            respond (responseStream status200 
              [(hContentType,"text/event-stream")
              ,(hCacheControl,"no-cache")
              ,(hConnection,"keep-alive")
              ] responder
              )


instance {-# OVERLAPPABLE #-} (FromJSON a, ToJSON r) => Channel (a -> IO [r]) where 
  channel ep showParseErrors l = Handler (if match ep then Just endpoint else Nothing)
    where
      endpoint :: Exists Request => Application
      endpoint request respond = do
        let
          payload
            | method == methodGet || method == methodDelete
            , Just b64 <- join (List.lookup "payload" (queryString request))
            , Right bs <- B64.decode (BSL.fromStrict b64) 
            = pure bs

            | otherwise = consumeRequestBodyLazy request

        pl <- payload
        case eitherDecode pl of
          Left e -> respond (responseLBS status400 [] (if showParseErrors then encode e else def))
          Right (a :: a) -> do
            let 
              responder write flush = 
                let
                  push b = do
                    write ("data: " <> fromLazyByteString (encode b) <> "\n\n")
                    flush

                  handler e
                    -- short-circuits simply end the stream without raising an error
                    -- while unknown exceptions get re-thrown.
                    | Just Unauthorized   <- fromException e = pure ()
                    | Just (_ :: Respond) <- fromException e = pure ()
                    | otherwise                              = E.throw e

                in
                  handle handler (l a >>= mapM_ push)

            respond (responseStream status200 
              [(hContentType,"text/event-stream")
              ,(hCacheControl,"no-cache")
              ,(hConnection,"keep-alive")
              ] responder
              )

reshape :: Endpoint method a -> Endpoint method b
reshape = fromTxt . toTxt

instance (FromJSON a, FromJSON b, ToJSON r) => Channel (a -> b -> IO [r]) where
  channel path showParseErrors l = channel (reshape path) showParseErrors (uncurry l)

instance (FromJSON a, FromJSON b, FromJSON c, ToJSON r) => Channel (a -> b -> c -> IO [r]) where
  channel path showParseErrors l = channel (reshape path) showParseErrors (\(a,b,c) -> l a b c)

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, ToJSON r) => Channel (a -> b -> c -> d -> IO [r]) where
  channel path showParseErrors l = channel (reshape path) showParseErrors (\(a,b,c,d) -> l a b c d)

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, ToJSON r) => Channel (a -> b -> c -> d -> e -> IO [r]) where
  channel path showParseErrors l = channel (reshape path) showParseErrors (\(a,b,c,d,e) -> l a b c d e)

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, ToJSON r) => Channel (a -> b -> c -> d -> e -> f -> IO [r]) where
  channel path showParseErrors l = channel (reshape path) showParseErrors (\(a,b,c,d,e,f) -> l a b c d e f)

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, ToJSON r) => Channel (a -> b -> c -> d -> e -> f -> g -> IO [r]) where
  channel path showParseErrors l = channel (reshape path) showParseErrors (\(a,b,c,d,e,f,g) -> l a b c d e f g)

class Lambda a where
  lambda :: ToMethod method => Endpoint method a -> Bool -> Bool -> [Header] -> (Exists Request => a) -> Handler

instance Lambda Void where
  lambda ep _ _ _ _ = Handler (if match ep then Just endpoint else Nothing)
    where
      endpoint :: Exists Request => Application
      endpoint request respond = respond (responseLBS status501 [] def) 

instance ToJSON r => Lambda (IO r) where
  lambda ep showParseErrors showExceptions headers l = Handler (if match ep then Just endpoint else Nothing)
    where
      endpoint :: Exists Request => Application
      endpoint request respond = do
        let 
          passthrough e 
            | Just (_ :: Respond) <- fromException e = E.throw e
            | Just Unauthorized   <- fromException e = E.throw e
            | otherwise                              = pure (Left (show e))

        er <- handle passthrough (Right <$> (l >>= evaluate . force . encode))
        respond do
          case er of
            Left e  -> responseLBS status500 [(hContentType,"application/json")] (if showExceptions then encode e else def)
            Right r -> responseLBS status200 ((hContentType,"application/json"):headers) r

instance (FromJSON a, ToJSON r) => Lambda (a -> IO r) where 
  lambda ep showParseErrors showExceptions headers l = Handler (if match ep then Just endpoint else Nothing)
    where
      endpoint :: Exists Request => Application
      endpoint request respond = do
        let
          payload
            | method == methodGet || method == methodDelete
            , Just b64 <- join (List.lookup "payload" (queryString request))
            , Right bs <- B64.decode (BSL.fromStrict b64) 
            = pure bs

            | otherwise = consumeRequestBodyLazy request

        pl <- payload
        case eitherDecode pl of
          Left e -> respond (responseLBS status400 [(hContentType,"application/json")] (if showParseErrors then encode e else def))
          Right (a :: a) -> do
            let 
              passthrough e 
                | Just (_ :: Respond) <- fromException e = E.throw e
                | Just Unauthorized   <- fromException e = E.throw e
                | otherwise                              = pure (Left (show e))

            er <- handle passthrough (Right <$> (l a >>= evaluate . force . encode))
            respond do
              case er of
                Left e  -> responseLBS status500 [(hContentType,"application/json")] (if showExceptions then encode e else def)
                Right r -> responseLBS status200 ((hContentType,"application/json"):headers) r

instance (FromJSON a, FromJSON b, ToJSON r) => Lambda (a -> b -> IO r) where
  lambda path showParseErrors showExceptions headers l = lambda (reshape path) showParseErrors showExceptions headers (uncurry l)

instance (FromJSON a, FromJSON b, FromJSON c, ToJSON r) => Lambda (a -> b -> c -> IO r) where
  lambda path showParseErrors showException headers l = lambda (reshape path) showParseErrors showException headers (\(a,b,c) -> l a b c)

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, ToJSON r) => Lambda (a -> b -> c -> d -> IO r) where
  lambda path showParseErrors showException headers l = lambda (reshape path) showParseErrors showException headers (\(a,b,c,d) -> l a b c d)

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, ToJSON r) => Lambda (a -> b -> c -> d -> e -> IO r) where
  lambda path showParseErrors showException headers l = lambda (reshape path) showParseErrors showException headers (\(a,b,c,d,e) -> l a b c d e)

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, ToJSON r) => Lambda (a -> b -> c -> d -> e -> f -> IO r) where
  lambda path showParseErrors showException headers l = lambda (reshape path) showParseErrors showException headers (\(a,b,c,d,e,f) -> l a b c d e f)

instance (FromJSON a, FromJSON b, FromJSON c, FromJSON d, FromJSON e, FromJSON f, FromJSON g, ToJSON r) => Lambda (a -> b -> c -> d -> e -> f -> g -> IO r) where
  lambda path showParseErrors showException headers l = lambda (reshape path) showParseErrors showException headers (\(a,b,c,d,e,f,g) -> l a b c d e f g)

cache :: Time -> Middleware
cache (Seconds duration _) app request respond = 
  app request 
    (respond . mapResponseHeaders (++ [(hCacheControl,"public, max-age=" <> fromTxt (toTxt @Int (round duration)))]))

middleware :: Middleware -> Handler -> Handler
middleware f (Handler h) = Handler (fmap f h)

logging :: Log.Logging Request => Log.Level -> Middleware
logging level app request respond = do
  Log.log level request
  app request respond

class (Methods r) => Server r where

  cors :: [Header]
  cors = []
              
  methods :: BS.ByteString             
  methods = "GET, PATCH, POST, PUT, DELETE, OPTIONS"

  showParseErrors :: Bool
  showParseErrors = False

  showExceptions :: Bool
  showExceptions = False

  type Env r :: Constraint
  type Env r = ()

  handlers :: Env r => Server.Handler
  default handlers 
    :: ( Env r
       , Lambda (Create r)
       , Lambda (Update r)
       , Lambda (Query r)
       , Lambda (Replace r)
       , Lambda (Delete r)
       ) => Server.Handler
  handlers = Handler (if path == toTxt (endpoint @r) then go else Nothing)
    where
      go :: Exists Request => Maybe Application
      go | method == methodGet     = fmap (queryMiddleware  @r) (handler (lambda (Endpoint.query  @r) (showParseErrors @r) (showExceptions @r) (cors @r) (Server.query  @r)))
         | method == methodPatch   = fmap (updateMiddleware @r) (handler (lambda (Endpoint.update @r) (showParseErrors @r) (showExceptions @r) (cors @r) (Server.update @r)))
         | method == methodPost    = fmap (createMiddleware @r) (handler (lambda (Endpoint.create @r) (showParseErrors @r) (showExceptions @r) (cors @r) (Server.create @r)))
         | method == methodPut     = fmap (replaceMiddleware @r)  (handler (lambda (Endpoint.replace @r) (showParseErrors @r) (showExceptions @r) (cors @r) (Server.replace @r)))
         | method == methodDelete  = fmap (deleteMiddleware @r) (handler (lambda (Endpoint.delete @r) (showParseErrors @r) (showExceptions @r) (cors @r) (Server.delete @r)))
         | method == methodOptions = Just \_ respond -> respond (responseLBS status200 ((hAllow,methods @r):cors @r) def)
         | otherwise = Just \_ respond -> respond (responseLBS notImplemented501 [] def)

  create :: (Exists Request, Env r) => Create r
  create = respond 501 mempty

  createMiddleware :: (Exists Request, Env r) => Middleware
  createMiddleware = id

  update :: (Exists Request, Env r) => Update r
  update = respond 501 mempty
  
  updateMiddleware :: (Exists Request, Env r) => Middleware
  updateMiddleware = id

  query :: (Exists Request, Env r) => Query r
  query = respond 501 mempty
  
  queryMiddleware :: (Exists Request, Env r) => Middleware
  queryMiddleware = id

  replace :: (Exists Request, Env r) => Replace r
  replace = respond 501 mempty

  replaceMiddleware :: (Exists Request, Env r) => Middleware
  replaceMiddleware = id

  delete :: (Exists Request, Env r) => Delete r
  delete = respond 501 mempty

  deleteMiddleware :: (Exists Request, Env r) => Middleware
  deleteMiddleware = id

data Respond 
  = Respond Status [Header] Txt 
  | RespondFile Status [Header] FilePath 
  | RespondFilePart Status [Header] FilePath FilePart
  deriving Show
instance Exception Respond

-- We assume a default of JSON for all of the responses, so
-- the methods `respond`, `respondFile`, and `respondFilePart`
-- all add n text/json MIME content type. The methods
-- `respondWith`, `respondFileWith`, and `respondFilePartWith`
-- allow for custom mime types and additional headers.

respond :: Int -> Txt -> a
respond c = respondWith (toEnum c) [Server.json]

respondWith :: Int -> [Header] -> Txt -> a
respondWith s hs t = E.throw (Respond (toEnum s) hs t)

respondFile :: Int -> FilePath -> a
respondFile c = respondFileWith (toEnum c) [Server.json]

respondFileWith :: Int -> [Header] -> FilePath -> a
respondFileWith s hs fp = E.throw (RespondFile (toEnum s) hs fp)

respondFilePart :: Int -> FilePath -> FilePart -> a
respondFilePart c = respondFilePartWith (toEnum c) [Server.json]

respondFilePartWith :: Int -> [Header] -> FilePath -> FilePart -> a
respondFilePartWith s hs fp p = E.throw (RespondFilePart (toEnum s) hs fp p)

corsHeaders :: forall r. Server r => [Header]
corsHeaders =
  [ ("Access-Control-Allow-Origin","*")
  , ("Access-Control-Allow-Methods",methods @r)
  , ("Access-Control-Max-Age","86400")
  ]

json :: Header
json = (hContentType,"text/json")