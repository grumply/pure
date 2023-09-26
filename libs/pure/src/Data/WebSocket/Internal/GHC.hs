{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Websocket.Internal.GHC where

-- internal
import Data.Retry as Retry hiding (Status)
import qualified Data.Retry as Retry
import Data.Log as Log
import Data.JSON as AE hiding (Error)
import Data.Time as Time
import Data.Txt (Txt,ToTxt(..),FromTxt(..))
import Data.Random
import Data.Websocket.API
import Data.Websocket.Callbacks
import Data.Websocket.Dispatch
import Data.Websocket.Endpoint
import Data.Websocket.Events
import Data.Websocket.Identify
import Data.Websocket.Message
import Data.Websocket.TypeRep
import Data.Websocket.Request

-- from base
import Control.Concurrent
import Control.Exception as E
import Control.Monad
import Data.Foldable (for_)
import Data.Function
import Data.IORef
import Data.Int
import Data.List as List
import Data.Maybe
import Data.Monoid
import Data.Proxy
import Data.Ratio
import GHC.Generics
import System.IO
import System.IO.Unsafe
import Text.Read hiding (get,lift)
import Unsafe.Coerce

import Control.Exception (handle)
import Control.Monad (when)
import Data.Binary.Get (Get, runGet, getWord8, getWord16be, getWord64be)
import Data.Bits (shiftR, xor, (.&.))
import qualified Data.ByteString as S
import Data.Word (Word8, Word16, Word64)

-- from websockets
import           Network.WebSockets.Stream     (Stream)
import qualified Network.WebSockets.Stream     as Stream
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WS

-- from bytestring
import qualified Data.ByteString as Strict
import qualified Data.ByteString        as Strict
import qualified Data.ByteString.Internal as Strict
import qualified Data.ByteString.Unsafe as Strict
import qualified Data.ByteString.Lazy.Char8 as Lazy
import qualified Data.ByteString.Lazy as Lazy
import qualified Data.ByteString.Lazy.Internal as Lazy
import qualified Data.ByteString.Lazy as Lazy hiding (putStrLn)

-- from text
import qualified Data.Text.Lazy.Encoding as TL

-- from unordered-containers
import qualified Data.HashMap.Strict as Map

import System.IO.Error as IO.Error
import Network.Connection as C
import Network.Socket as S ( Socket, getPeerName )

import GHC.Stack
import System.Timeout

import Prelude hiding (log)

import OpenSSL as SSL
import OpenSSL.Session as SSL
import System.IO.Streams.SSL as Streams
import System.IO.Streams as Streams

import qualified Data.IP as IPR (IP(..),fromSockAddr)

{-
This is all a bit hacked together because we want a unified 
implementation for both client and server. What we're lacking
to clean it up is an implementation of server connections from
Network.Connection.
-}

data Websocket_
  = Websocket
    { wsSocket            :: Maybe (C.Connection,WS.Connection,WS.Stream)
    , wsDispatchCallbacks :: !(Map.HashMap Txt [IORef (Dispatch -> IO ())])
    , wsStatus            :: Status
    , wsStatusCallbacks   :: ![IORef (Status -> IO ())]
    , wsReceiver          :: Maybe ThreadId
    }

type Websocket = IORef Websocket_

type RequestCallback request response = IO () -> Either Dispatch (Either Lazy.ByteString response -> IO (Either Status ()),request) -> IO ()

-- | Construct a status callback. This is a low-level method.
onStatus :: Websocket -> (Status -> IO ()) -> IO StatusCallback
onStatus ws_ f = do
  cb <- newIORef f
  modifyIORef' ws_ $ \ws -> ws
    { wsStatusCallbacks = wsStatusCallbacks ws ++ [cb]
    }
  return $ StatusCallback cb $
    modifyIORef' ws_ $ \ws -> ws
      { wsStatusCallbacks = List.filter (/= cb) (wsStatusCallbacks ws)
      }

-- | Set status and call status callbacks.
setStatus :: Websocket -> Status  -> IO ()
setStatus ws_ s = do
  cbs <- atomicModifyIORef' ws_ $ \ws -> (ws { wsStatus = s },wsStatusCallbacks ws)
  for_ cbs $ \cb_ -> do
    cb <- readIORef cb_
    cb s

-- | Construct a dispatch callback. This is a low-level method.
onDispatch :: Websocket -> Txt -> (Dispatch -> IO ()) -> IO DispatchCallback
onDispatch ws_ hdr f = do
  cb <- newIORef f
  atomicModifyIORef' ws_ $ \ws -> (ws { wsDispatchCallbacks = Map.insertWith (flip (++)) hdr [cb] (wsDispatchCallbacks ws) },())
  return $ DispatchCallback cb $
    atomicModifyIORef' ws_ $ \ws -> (ws { wsDispatchCallbacks = Map.adjust (List.filter (/= cb)) hdr (wsDispatchCallbacks ws) },())


-- | Initialize a websocket without connecting.
websocket :: IO Websocket
websocket = do
  newIORef Websocket
    { wsSocket            = Nothing
    , wsDispatchCallbacks = Map.empty
    , wsStatus            = Initialized
    , wsStatusCallbacks   = []
    , wsReceiver          = Nothing
    }

reader :: C.Connection -> IO (Maybe Strict.ByteString)
reader c =
  catchIOError 
    (Just <$> C.connectionGetChunk c)
    (\e -> pure Nothing)

writer :: C.Connection -> Maybe Lazy.ByteString -> IO ()
writer _ Nothing = pure ()
writer c (Just bytes) = C.connectionPut c (Lazy.toStrict bytes)

-- Construct a server websocket from an open socket with reader, writer, and websocket options.
server :: Logging Value => WS.ConnectionOptions -> S.Socket -> IO Websocket
server options sock = do
  ctx <- C.initConnectionContext
  sa <- S.getPeerName sock
  let (show -> h,fromIntegral -> p) = fromJust (IPR.fromSockAddr sa)
  ws_ <- websocket
  conn <- C.connectFromSocket ctx sock C.ConnectionParams
    { connectionHostname = h
    , connectionPort = p
    , connectionUseSecure = Nothing
    , connectionUseSocks = Nothing
    }
  wsStream <- WS.makeStream (reader conn) (writer conn)
  pc <- WS.makePendingConnectionFromStream wsStream options
  c <- WS.acceptRequest pc
  modifyIORef' ws_ $ \ws -> ws { wsSocket = Just (conn,c,wsStream) }
  onStatus ws_ $ \case
    status -> do
      now <- Time.time
      informational Info $ toJSON $ ConnectionEvent
        { host = h
        , port = fromIntegral p
        , secure = False
        , ..
        }
  setStatus ws_ Opened
  return ws_

{- This likely has the same issue with message buffers of the old insecure
   implementation.
-}
fromSecureStreams (i,o) = WS.makeStream reader writer
  where
    reader = Streams.read i
    writer = flip Streams.write o . fmap Lazy.toStrict

secureServer :: WS.ConnectionOptions -> S.Socket -> SSL -> IO Websocket
secureServer options sock ssl = SSL.withOpenSSL $ do
  ctx <- C.initConnectionContext
  sa <- S.getPeerName sock
  let (show -> h,fromIntegral -> p) = fromJust (IPR.fromSockAddr sa)
  ws_ <- websocket
  conn <- C.connectFromSocket ctx sock C.ConnectionParams
    { connectionHostname = h
    , connectionPort = fromIntegral p
    , connectionUseSecure = Nothing
    , connectionUseSocks = Nothing
    }
  streams <- Streams.sslToStreams ssl
  wsStream <- fromSecureStreams streams
  pc <- WS.makePendingConnectionFromStream wsStream options
  c <- WS.acceptRequest pc
  modifyIORef' ws_ $ \ws -> ws
    { wsSocket = Just (conn,c,wsStream)
    , wsStatus = Opened
    }
  return ws_

activate :: Logging Value => Websocket -> IO ()
activate ws_ = do
  ws <- readIORef ws_
  case wsReceiver ws of
    Just _ -> pure ()
    Nothing -> 
      case wsSocket ws of
        Nothing -> pure ()
        Just (conn,c,_) -> do
          let (host,port) = C.connectionID conn
          rt <- forkIO $ receiveLoop host (fromIntegral port) ws_ c
          modifyIORef' ws_ $ \ws -> ws { wsReceiver = Just rt }

receiveLoop :: Logging Value => String -> Int -> Websocket -> WS.Connection -> IO ()
receiveLoop host port ws_ c = go
  where
    go = do
      eem <- E.handle (\(_ :: WS.ConnectionException) -> return (Left Disconnect)) (Right <$> WS.receiveData c)
      case eem of
        Left cr -> close ws_ cr
        Right t 
          | Right m <- eitherDecode' t -> do
            ws <- readIORef ws_
            case Map.lookup (ep m) (wsDispatchCallbacks ws) of
              Nothing -> do
                -- Unknown endpoint; close the connection.
                now <- Time.time
                critical Error $ toJSON $ UnknownMessage
                  { content = toTxt t
                  , ..
                  }
                close ws_ InvalidMessage
              Just cbs -> do
                for_ cbs (readIORef >=> ($ m))
                now <- Time.time
                informational Debug $ toJSON $ DispatchedMessage
                  { endpoint = ep m
                  , payload = pl m
                  , .. 
                  }
                go
          | otherwise -> do
            -- unparseable messages mean the client does not
            -- understand the messaging protocol; close the
            -- connection.
            now <- Time.time
            critical Error $ toJSON $ UnknownMessage
              { content = toTxt t
              , .. 
              }
            close ws_ InvalidMessage

close :: Websocket -> CloseReason -> IO ()
close ws_ cr = do
  ws <- readIORef ws_
  for_ (wsSocket ws) $ \(conn,_,s) -> do
    WS.close s
    C.connectionClose conn
    writeIORef ws_ ws { wsSocket = Nothing, wsReceiver = Nothing }
  for_ (wsReceiver ws) killThread
  setStatus ws_ (Closed cr)

-- Connect via the given ConnectionParams. The default retry policy is jittered
-- with a minimum delay of 1 second and a maximum delay of 30 seconds. Retries
-- and failures are logged.
client :: Logging Value => C.ConnectionParams -> WS.ConnectionOptions -> IO Websocket
client ps = clientWith "/" policy ps
  where
    policy = 
      jittered Second 
        & limitDelay (Seconds 30 0) 
        & logRetry (Critical Warn) retrying
        & logFailure (Critical Error) failed

    retrying Retry.Status { retries, current, start } _ = do
      let 
        host = C.connectionHostname ps
        port = fromIntegral (C.connectionPort ps)
        secure = isJust (C.connectionUseSecure ps)
        status = Connecting

      toJSON ConnectionEvent {..} 

    failed Retry.Status { retries, current, start  } = do
      let
        host = C.connectionHostname ps
        port = fromIntegral (C.connectionPort ps)
        secure = isJust (C.connectionUseSecure ps)
        status = Closed Disconnect 

      toJSON ConnectionEvent {..}

clientWith :: Logging Value => String -> Retry.Policy -> C.ConnectionParams -> WS.ConnectionOptions -> IO Websocket
clientWith root policy params options = do
  ws_ <- websocket
  let
    cce now status = ConnectionEvent
      { host = C.connectionHostname params
      , port = fromIntegral (C.connectionPort params)
      , secure = isJust (C.connectionUseSecure params)
      , ..
      }
  now <- Time.time
  informational Info (toJSON $ cce now Initialized)
  onStatus ws_ $ \ev -> do
    now <- Time.time
    informational Info (toJSON $ cce now ev)
  forkIO $ do 
    fix $ \restart -> do
      ctx <- C.initConnectionContext
      mconn <- retryingIO policy (C.connectTo ctx params)
      for_ mconn $ \conn -> do
        start <- Time.time
        stream <- WS.makeStream (reader conn) (writer conn)
        c <- WS.newClientConnection stream (C.connectionHostname params) root options []
        modifyIORef' ws_ $ \ws -> ws { wsSocket = Just (conn,c,stream) }
        setStatus ws_ Opened
        onStatus ws_ $ \case
          Closed _ -> void (forkIO restart)
          _ -> pure ()
  return ws_

--------------------------------------------------------------------------------
-- Custom file reading utilities

readFile8k :: FilePath -> IO Lazy.ByteString
readFile8k = readFileN 8192

readFile16k :: FilePath -> IO Lazy.ByteString
readFile16k = readFileN 16384

readFileN :: Int -> FilePath -> IO Lazy.ByteString
readFileN chk f = openBinaryFile f ReadMode >>= hGetContentsN chk

hGetContentsN :: Int -> Handle -> IO Lazy.ByteString
hGetContentsN chk h = streamRead
  where
    streamRead = unsafeInterleaveIO loop

    loop = do
        c <- S.hGetSome h chk
        if S.null c
          then hClose h >> return Lazy.empty
          else Lazy.chunk c <$> streamRead

--------------------------------------------------------------------------------
-- Raw byte-level websocket access

sendRaw :: Logging Value => Websocket -> Lazy.ByteString -> IO (Either Status SendStatus)
sendRaw ws_ b = do
  Websocket {..} <- readIORef ws_
  case wsSocket of
    Just (conn,c,_) -> do
      eum <- E.handle (\(e :: IOException) -> return (Left ()))
                 (Right <$> WS.sendTextData c (TL.decodeUtf8 b))
      let (h,p) = C.connectionID conn
      now <- Time.time
      informational Trace $ toJSON $ SentMessage
        { host = h
        , port = fromIntegral p
        , payload = toJSON (toTxt b)
        , ..
        }
      case eum of
        Left _ -> do
          close ws_ Disconnect
          return (Left (Closed Disconnect))
        _ -> do
          return (Right Sent)
    Nothing -> do
      cb <- newIORef undefined
      st <- onStatus ws_ $ \case
        Opened -> void $ do
          readIORef cb >>= scCleanup
          sendRaw ws_ b
        _ -> pure ()
      writeIORef cb st
      return (Right Buffered)

data SendStatus = Buffered | Sent

--------------------------------------------------------------------------------
-- Streaming Dispatch interface to websockets

request :: ( Request rqTy
           , Req rqTy ~ request
           , ToJSON request
           , Identify request
           , I request ~ rqI
           , Rsp rqTy ~ rsp
           , FromJSON rsp
           , Logging Value
           )
         => Websocket
         -> Proxy rqTy
         -> request
         -> (IO () -> Either Dispatch rsp -> IO ())
         -> IO DispatchCallback
request ws_ rqty_proxy req f = do
  s_ <- newIORef undefined
  let header = responseHeader rqty_proxy req
      bhvr m = f (readIORef s_ >>= dcCleanup) (maybe (Left m) Right (decodeDispatch m))
  dpc <- onDispatch ws_ header bhvr
  writeIORef s_ dpc
  let rqHeader = requestHeader rqty_proxy
  sendRaw ws_ $ encodeBS $ encodeDispatch rqHeader req
  return dpc

apiRequest :: ( Request rqTy
              , Req rqTy ~ request
              , ToJSON request
              , Identify request
              , I request ~ rqI
              , Rsp rqTy ~ response
              , FromJSON response
              , (rqTy ∈ rqs) ~ 'True
              , Logging Value
              )
           => API msgs rqs
           -> Websocket
           -> Proxy rqTy
           -> request
           -> (IO () -> Either Dispatch response -> IO ())
           -> IO DispatchCallback
apiRequest _ = request

respond :: ( Request rqTy
           , Req rqTy ~ request
           , Identify request
           , I request ~ rqI
           , FromJSON request
           , Rsp rqTy ~ response
           , ToJSON response
           , Logging Value
           )
        => Websocket
        -> Proxy rqTy
        -> (IO () -> Either Dispatch (Either Lazy.ByteString response -> IO (Either Status ()),request) -> IO ())
        -> IO DispatchCallback
respond ws_ rqty_proxy f = do
  s_ <- newIORef undefined
  let header = requestHeader rqty_proxy
      bhvr m = f (readIORef s_ >>= dcCleanup)
                 $ maybe (Left m) (\rq -> Right
                    (fmap (fmap (const ())) . sendRaw ws_ . either (buildEncodedDispatchByteString (responseHeader rqty_proxy rq)) (encodeBS . encodeDispatch (responseHeader rqty_proxy rq))
                    , rq
                    )
                 ) (decodeDispatch m)
  dcb <- onDispatch ws_ header bhvr
  writeIORef s_ dcb
  return dcb

message :: ( Message mTy , M mTy ~ msg , ToJSON msg, Logging Value )
        => Websocket
        -> Proxy mTy
        -> msg
        -> IO (Either Status SendStatus)
message ws_ mty_proxy m =
  sendRaw ws_ $ encodeBS $ encodeDispatch (messageHeader mty_proxy) m

apiMessage :: ( Message mTy , M mTy ~ msg , ToJSON msg , (mTy ∈ msgs) ~ 'True, Logging Value )
           => API msgs rqs
           -> Websocket
           -> Proxy mTy
           -> msg
           -> IO (Either Status SendStatus)
apiMessage _ = message

onMessage :: ( Message mTy
             , M mTy ~ msg
             , FromJSON msg
             )
          => Websocket
          -> Proxy mTy
          -> (IO () -> Either Dispatch msg -> IO ())
          -> IO DispatchCallback
onMessage ws_ mty_proxy f = do
  s_ <- newIORef undefined
  let header = messageHeader mty_proxy
      bhvr m = f (readIORef s_ >>= dcCleanup) (maybe (Left m) Right (decodeDispatch m))
  dpc <- onDispatch ws_ header bhvr
  writeIORef s_ dpc
  return dpc

instance ( Request request
         , RemoveInterface Websocket (Interface Request) rqs
         )
  => RemoveInterface Websocket (Interface Request) (request ': rqs) where
  removeInterface ws_ (InterfaceCons pm rest) = do
    atomicModifyIORef' ws_ $ \ws -> 
      let header = requestHeader (Proxy :: Proxy request) 
      in (ws { wsDispatchCallbacks = Map.delete header (wsDispatchCallbacks ws) },())
    removeInterface ws_ rest

instance ( Message message
         , RemoveInterface Websocket (Interface Message) msgs
         )
  => RemoveInterface Websocket (Interface Message) (message ': msgs) where
  removeInterface ws_ (InterfaceCons pm rest) = do
    atomicModifyIORef' ws_ $ \ws -> 
      let header = messageHeader (Proxy :: Proxy message) 
      in (ws { wsDispatchCallbacks = Map.delete header (wsDispatchCallbacks ws) },())
    removeInterface ws_ rest

remove :: 
    ( RemoveInterface Websocket (Interface Request) reqs
    , RemoveInterface Websocket (Interface Message) msgs
    ) => Websocket -> API msgs reqs -> IO ()
remove ws_ (API mapi rapi) = do
  removeInterface ws_ mapi
  removeInterface ws_ rapi
