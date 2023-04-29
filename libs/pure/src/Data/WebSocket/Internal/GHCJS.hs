{-# LANGUAGE CPP #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
module Data.Websocket.Internal.GHCJS where

-- internal
import Control.Log as Log
import Control.Retry as Retry hiding (Status)
import qualified Control.Retry as Retry
import Data.Function
import Data.DOM
import Data.JSON as JSON hiding (Error)
import qualified Data.JSON as JSON
import Data.Random
import Data.Time as Time
import Data.Txt (Txt,ToTxt(..),FromTxt(..))
import Data.Websocket.API
import Data.Websocket.Callbacks
import Data.Websocket.Dispatch
import Data.Websocket.Endpoint
import Data.Websocket.Events
import Data.Websocket.Identify
import Data.Websocket.Message
import Data.Websocket.TypeRep
import Data.Websocket.Request

-- from bytestring
import qualified Data.ByteString.Lazy as BSL

-- from ghcjs-base
import qualified GHCJS.Foreign.Callback as CB
import qualified GHCJS.Buffer as GB
import qualified GHCJS.Marshal as M
import qualified GHCJS.Marshal.Pure as M
import qualified JavaScript.Object.Internal as OI
import qualified GHCJS.Types as T
import qualified Data.JSString as JS (uncons)
import qualified JavaScript.TypedArray.ArrayBuffer as TAB
import qualified JavaScript.Web.MessageEvent as WME

-- from base
import Control.Concurrent
import Control.Monad
import Data.Data
import Data.Int
import Data.IORef
import Data.Foldable (for_)
import Data.List as List
import Data.Maybe
import Data.Monoid
import GHC.Generics
import Text.Read hiding (lift,get)
import Unsafe.Coerce
import Prelude hiding (log)

-- from unordered-containers
import qualified Data.HashMap.Strict as Map

type LazyByteString = BSL.ByteString

#ifdef __GHCJS__
foreign import javascript unsafe
  "$r = JSON.parse($1);" jsonParse :: Txt -> IO T.JSVal

foreign import javascript unsafe
  "$r = JSON.stringify($1);" jsonEncode :: T.JSVal -> IO Txt
#endif

type CB = CB.Callback (JSV -> IO ())

data Websocket_
  = Websocket
    { wsSocket            :: Maybe (JSV,CB,CB,CB,CB,StatusCallback)
    , wsDispatchCallbacks :: !(Map.HashMap Txt [IORef (Dispatch -> IO ())])
    , wsStatus            :: Status
    , wsStatusCallbacks   :: ![IORef (Status -> IO ())]
    }

type Websocket = IORef Websocket_

type RequestCallback request response = IO () -> Either Dispatch (Either Txt response -> IO (Either Status SendStatus),request) -> IO ()

-- | Construct a status callback. This is a low-level method.
onStatus :: Websocket -> (Status -> IO ()) -> IO StatusCallback
onStatus ws_ f = do
  cb <- newIORef f
  atomicModifyIORef' ws_ $ \ws -> (ws { wsStatusCallbacks = wsStatusCallbacks ws ++ [cb] },())
  return $ StatusCallback cb $
    atomicModifyIORef' ws_ $ \ws -> (ws { wsStatusCallbacks = List.filter (/= cb) (wsStatusCallbacks ws) },())

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
    }

newWS :: Logging => String -> Int -> Bool -> IO Websocket
newWS host port secure = do
  -- TODO: make the delay method configurable.
  ws <- websocket
  activate ws host port secure
  pure ws

activateGHCJS :: Logging => Websocket -> String -> Int -> Bool -> IO ()
activateGHCJS = activate

activate :: Logging => Websocket -> String -> Int -> Bool -> IO ()
activate ws host port secure = activateWith policy ws host port secure
  where
    context = location 
    
    policy = 
      jittered Second 
        & limitDelay (Seconds 30 0) 
        & logRetry Warn retrying
        & logFailure Error failed

    retrying Retry.Status { retries, current, start } _ = do
      let 
        status = Connecting

      ConnectionEvent {..} 

    failed Retry.Status { retries, current, start  } = do
      let
        status = Closed Disconnect 

      ConnectionEvent {..}

activateWith :: Logging => Retry.Policy -> Websocket -> String -> Int -> Bool -> IO ()
activateWith policy ws_ host port secure = do
  s <- newSeed
  void $ do
    forkIO $ do
      msock <- retrying policy $
        tryNewWebsocket (toTxt $ (if secure then "wss://" else "ws://") ++ host ++ ':': show port) 
          >>= maybe retry pure
      for_ msock $ \sock -> do
        openCallback    <- CB.syncCallback1 CB.ContinueAsync $ \_ -> setStatus ws_ Opened
        closeCallback   <- CB.syncCallback1 CB.ContinueAsync $ \_ -> setStatus ws_ (Closed Disconnect)
        errorCallback   <- CB.syncCallback1 CB.ContinueAsync $ \err -> setStatus ws_ (Closed Disconnect)
        messageCallback <- CB.syncCallback1 CB.ContinueAsync $ \ev -> do
          case WME.getData $ unsafeCoerce ev of
            WME.StringData sd -> do
              case fromJSON (js_JSON_parse sd) of
                JSON.Error e -> do
                  now <- Time.time
                  log Log.Error UnknownMessage
                    { content = sd
                    , ..
                    }
                Success m -> do
                  ws <- readIORef ws_
                  case Map.lookup (ep m) (wsDispatchCallbacks ws) of
                    Nothing -> do
                      now <- Time.time
                      log Log.Error UnknownMessage
                        { content = sd
                        , ..
                        }
                    Just dcbs -> do
                      now <- Time.time
                      log Trace DispatchedMessage
                        { endpoint = ep m
                        , payload = pl m
                        , .. 
                        }
                      for_ dcbs (readIORef >=> ($ m))

            _ -> return ()

        addEventListener sock "open" openCallback False
        addEventListener sock "close" closeCallback False
        addEventListener sock "error" errorCallback False
        addEventListener sock "message" messageCallback False

        scb <- onStatus ws_ $ \case
          Closed _ -> do
            ws <- readIORef ws_
            let Just (_,ocb,ccb,ecb,mcb,scb) = wsSocket ws
            CB.releaseCallback ocb
            CB.releaseCallback ccb
            CB.releaseCallback ecb
            CB.releaseCallback mcb
            scCleanup scb
            modifyIORef' ws_ $ \ws -> ws { wsSocket = Nothing }
            void (forkIO retry)

          _ -> return ()

        modifyIORef' ws_ $ \ws -> ws
          { wsSocket = Just (sock,openCallback,closeCallback,errorCallback,messageCallback,scb) }

clientWS :: Logging => String -> Int -> IO Websocket
clientWS h p = newWS h p False

clientWSS :: Logging => String -> Int -> IO Websocket
clientWSS h p = newWS h p True

foreign import javascript unsafe
  "try { $r = new window[\"WebSocket\"]($1) } catch (e) { $r = null}"
    js_tryNewWebsocket :: Txt -> IO JSV

tryNewWebsocket :: Txt -> IO (Maybe JSV)
tryNewWebsocket url = do
  ws <- js_tryNewWebsocket url
  if isNull ws
    -- I've never seen this return Nothing. Can it?
    -- In fact, it caused a bug when I expected a
    -- failed connection to return Nothing.
    then return Nothing
    else return (Just ws)

foreign import javascript unsafe
  "$1.close()" ws_close_js :: JSV -> Int -> Txt -> IO ()

close :: Websocket -> IO ()
close ws_ = do
  ws <- readIORef ws_
  forM_ (wsSocket ws) $ \(sock,o,c,e,m,scb) -> do
    scCleanup scb
    ws_close_js sock 1000 "closed"
    CB.releaseCallback o
    CB.releaseCallback c
    CB.releaseCallback e
    CB.releaseCallback m
    writeIORef ws_ ws { wsSocket = Nothing }
  setStatus ws_ (Closed Disconnect)

send' :: Websocket -> Either Txt Dispatch -> IO (Either Status SendStatus)
send' ws_ m = go True
  where
    go b = do
      ws <- readIORef ws_
      case wsStatus ws of
        Opened ->
          case wsSocket ws of
            Just (ws,_,_,_,_,_) -> do
              send_js ws (either id (encode . toJSON) m)
              return (Right Sent)
        _ -> do
          cb <- newIORef undefined
          st <- onStatus ws_ $ \case
            Opened -> do
              send' ws_ m
              readIORef cb >>= scCleanup
            _ -> return ()
          writeIORef cb st
          return (Right Buffered)

data SendStatus = Buffered | Sent

foreign import javascript unsafe
  "$1.send($2);" send_js :: JSV -> Txt -> IO ()

send :: (ToJSON a) => Websocket -> Txt -> a -> IO (Either Status SendStatus)
send ws_ h = send' ws_ . Right . Dispatch h . toJSON

request :: ( Request rqTy
           , Req rqTy ~ request
           , ToJSON request
           , Identify request
           , I request ~ rqI
           , Rsp rqTy ~ rsp
           , FromJSON rsp
           )
         => Websocket
         -> Proxy rqTy
         -> request
         -> (IO () -> Either Dispatch rsp -> IO ())
         -> IO DispatchCallback
request ws_ rqty_proxy req f = do
  s_ <- newIORef undefined
  let rspHdr = responseHeader rqty_proxy req
      reqHdr = requestHeader rqty_proxy
      bhvr m = f (readIORef s_ >>= dcCleanup) (maybe (Left m) Right (decodeDispatch m))
  dpc <- onDispatch ws_ rspHdr bhvr
  writeIORef s_ dpc
  send ws_ reqHdr req
  return dpc

apiRequest :: ( Request rqTy
              , Req rqTy ~ request
              , ToJSON request
              , Identify request
              , I request ~ rqI
              , Rsp rqTy ~ rsp
              , FromJSON rsp
              , (rqTy ∈ rqs) ~ 'True
              )
          => API msgs rqs
          -> Websocket
          -> Proxy rqTy
          -> request
          -> (IO () -> Either Dispatch rsp -> IO ())
          -> IO DispatchCallback
apiRequest _ = request

respond :: ( Request rqTy
           , Req rqTy ~ request
           , Identify request
           , I request ~ rqI
           , FromJSON request
           , Rsp rqTy ~ rsp
           , ToJSON rsp
           )
        => Websocket
        -> Proxy rqTy
        -> (IO () -> Either Dispatch (Either Txt rsp -> IO (Either Status SendStatus),request) -> IO ())
        -> IO DispatchCallback
respond ws_ rqty_proxy f = do
  s_ <- newIORef undefined
  let header = requestHeader rqty_proxy
      bhvr m = f (readIORef s_ >>= dcCleanup)
                 $ maybe (Left m) (\rq -> Right
                    (send' ws_ . either (Left . buildEncodedDispatchTxt (responseHeader rqty_proxy rq)) (Right . encodeDispatch (responseHeader rqty_proxy rq))
                    , rq
                    )
                 ) (decodeDispatch m)
  dcb <- onDispatch ws_ header bhvr
  writeIORef s_ dcb
  return dcb

message :: ( Message mTy , M mTy ~ msg , ToJSON msg)
        => Websocket
        -> Proxy mTy
        -> msg
        -> IO (Either Status SendStatus)
message ws_ mty_proxy = send ws_ (messageHeader mty_proxy)

apiMessage :: ( Message mTy , M mTy ~ msg , ToJSON msg , (mTy ∈ msgs) ~ 'True)
            => API msgs rqs
            -> Websocket
            -> Proxy mTy
            -> msg
            -> IO (Either Status SendStatus)
apiMessage _ = message

onMessage :: (Message mTy, M mTy ~ msg, FromJSON msg)
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
  