{-# LANGUAGE CPP #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UnboxedTuples #-}
{-# LANGUAGE UnliftedFFITypes #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Data.Websocket.Internal.GHCJS where

-- internal
import Data.DOM
import Data.JSON as AE
import Data.Random
import Data.Txt (Txt,ToTxt(..),FromTxt(..))
import Data.Websocket.API
import Data.Websocket.Callbacks
import Data.Websocket.Dispatch
import Data.Websocket.Endpoint
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
    , wsStatus            = Unopened
    , wsStatusCallbacks   = []
    }

newWS :: String -> Int -> Bool -> IO Websocket
newWS host port secure = do
  -- TODO: make the delay method configurable.
  ws <- websocket
  activate ws host port secure
  pure ws

activateGHCJS :: Websocket -> String -> Int -> Bool -> IO ()
activateGHCJS = activate


type FailedAttempts = Int
type Milliseconds = Int
type Backoff = Seed -> FailedAttempts -> IO Seed

defaultExponentialBackoffWithJitter :: Milliseconds -> Backoff
defaultExponentialBackoffWithJitter maximum seed failed = do
  let (seed',jitter) = generate (uniformR 1 1000) seed
      potential = 2 ^ (min 20 failed) * 1000 -- `min 20 failed` prevents overflow, but allows a reasonably high `maximum`
      delay = (min maximum potential + jitter) * 1000
  threadDelay delay
  pure seed'

activate :: Websocket -> String -> Int -> Bool -> IO ()
activate = activateWith (defaultExponentialBackoffWithJitter 32000)

activateWith :: Backoff -> Websocket -> String -> Int -> Bool -> IO ()
activateWith backoff ws host port secure = do
  s <- newSeed
  void $ do
    forkIO $ do
      connectWith ws 0 s
  where
    connectWith ws_ n s = do
      let retry = connectWith ws_ (n + 1) =<< backoff s (n + 1)

      msock <- tryNewWebsocket (toTxt $ (if secure then "wss://" else "ws://") ++ host ++ ':': show port)

      case msock of
        Nothing -> retry

        Just sock -> do

          openCallback    <- CB.syncCallback1 CB.ContinueAsync $ \_ -> setStatus ws_ Opened
          closeCallback   <- CB.syncCallback1 CB.ContinueAsync $ \_ -> setStatus ws_ (Closed UnexpectedClosure)
          errorCallback   <- CB.syncCallback1 CB.ContinueAsync $ \err -> setStatus ws_ (Errored err)
          messageCallback <- CB.syncCallback1 CB.ContinueAsync $ \ev -> do
            case WME.getData $ unsafeCoerce ev of
              WME.StringData sd -> do
                case fromJSON (js_JSON_parse sd) of
                  Error e -> putStrLn $ "fromJSON failed with: " ++ e
                  Success m -> do
                    ws <- readIORef ws_
                    case Map.lookup (ep m) (wsDispatchCallbacks ws) of
                      Nothing   -> putStrLn $ "No handler found: " ++ show (ep m)
                      Just dcbs -> for_ dcbs (readIORef >=> ($ m))

              _ -> return ()

          addEventListener sock "open" openCallback False
          addEventListener sock "close" closeCallback False
          addEventListener sock "error" errorCallback False
          addEventListener sock "message" messageCallback False

          scb <- onStatus ws_ $ \case
            Closed UnexpectedClosure -> do
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

clientWS :: String -> Int -> IO Websocket
clientWS h p = newWS h p False

clientWSS :: String -> Int -> IO Websocket
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

close :: Websocket -> CloseReason -> IO ()
close ws_ cr = do
  ws <- readIORef ws_
  forM_ (wsSocket ws) $ \(sock,o,c,e,m,scb) -> do
    scCleanup scb
    ws_close_js sock 1000 "closed"
    CB.releaseCallback o
    CB.releaseCallback c
    CB.releaseCallback e
    CB.releaseCallback m
    writeIORef ws_ ws { wsSocket = Nothing }
  setStatus ws_ (Closed cr)

send' :: Websocket -> Either Txt Dispatch -> IO (Either Status SendStatus)
send' ws_ m = go True
  where
    go b = do
      ws <- readIORef ws_
      case wsStatus ws of
        Opened ->
          case wsSocket ws of
            Just (ws,_,_,_,_,_) -> do
#if defined(DEBUGWS) || defined(DEVEL)
              putStrLn $ "send' sending: " ++ show (fmap pretty v)
#endif
              send_js ws (either id (encode . toJSON) m)
              return (Right Sent)
        _ -> do
          cb <- newIORef undefined
          st <- onStatus ws_ $ \case
            Opened -> do
#if defined(DEBUGWS) || defined(DEVEL)
              liftIO $ putStrLn $ "send' sending after websocket state changed: " ++ show (fmap pretty m)
#endif
              send' ws_ m
              readIORef cb >>= scCleanup
            _ -> return ()
          writeIORef cb st
          return (Right Buffered)

data SendStatus = Buffered | Sent

foreign import javascript unsafe
  "$1.send($2);" send_js :: JSV -> Txt -> IO ()

foreign import javascript unsafe
  "Math.floor((Math.random() * $1) + 1)" random :: Int -> IO Int

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
  