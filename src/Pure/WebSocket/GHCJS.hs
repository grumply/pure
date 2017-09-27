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
module Pure.WebSocket.GHCJS where

import Ef.Base

import Pure.Data.JSON as AE

import Pure.Data
import Pure.Types
import Pure.Lifted

import Pure.WebSocket.API
import Pure.WebSocket.Dispatch
import Pure.WebSocket.Endpoint
import Pure.WebSocket.Message
import Pure.WebSocket.TypeRep
import Pure.WebSocket.Request

import qualified GHCJS.Foreign.Callback as CB
import qualified GHCJS.Buffer as GB
import qualified GHCJS.Marshal as M
import qualified GHCJS.Marshal.Pure as M
import qualified JavaScript.Object.Internal as OI
import qualified GHCJS.Types as T
import qualified Data.JSString as JS (uncons)
import qualified JavaScript.TypedArray.ArrayBuffer as TAB
import qualified JavaScript.Web.MessageEvent as WME

import qualified Data.HashMap.Strict as Map
import Control.Monad
import Data.Data
import Data.Int
import Data.IORef
import Data.Maybe
import Data.Monoid
import GHC.Generics
import Unsafe.Coerce

import Text.Read hiding (lift,get)

import GHC.Prim

import Data.ByteString.Lazy as BSL hiding (putStrLn)

-- import Control.Lens as L

type LazyByteString = BSL.ByteString

#ifdef __GHCJS__
foreign import javascript unsafe
  "$r = JSON.parse($1);" jsonParse :: Txt -> IO T.JSVal

foreign import javascript unsafe
  "$r = JSON.stringify($1);" jsonEncode :: T.JSVal -> IO Txt
#endif

data WSCloseReason
  = MessageLengthLimitExceeded Int64
  | MessageThroughputLimitExceeded
     { msgsPerSecondSeen :: Int
     , msgsPerSecondAllowed :: Int
     , msgsPerMinuteSeen :: Int
     , msgsPerMinuteAllowed :: Int
     }
  | BadMessageReceived Txt
  | ClientClosedConnection
  | ServerClosedConnection
  deriving (Show,Eq,Generic,ToJSON,FromJSON)

data WSStatus = WSUninitialized | WSOpened | WSClosed WSCloseReason deriving (Eq,Show)

data WebSocket k
  = WebSocket
    { webSocket       :: (Maybe JSV,k)
    , webSocketSetter :: Maybe JSV -> k

    , webSocketReconnecting       :: (Bool,k)
    , webSocketReconnectingSetter :: Bool -> k

    , webSocketState       :: (WSStatus,k)
    , webSocketStateSetter :: WSStatus -> k

    , webSocketStates         :: (Syndicate WSStatus,k)
    , webSocketStatesSetter   :: Syndicate WSStatus -> k

    , webSocketMsgHandlers :: (IORef (Map.HashMap Txt (Syndicate Dispatch)),k)
    , webSocketMsgHandlersSetter :: IORef (Map.HashMap Txt (Syndicate Dispatch)) -> k

    , webSocketInitializer :: k

    , webSocketConnecter :: EvQueue -> k

    }
  | GetWebSocket (Maybe JSV -> k)
  | SetWebSocket (Maybe JSV) k

  | GetWebSocketReconnecting (Bool -> k)
  | SetWebSocketReconnecting Bool k

  | GetWSStatus (WSStatus -> k)
  | SetWSStatus WSStatus k

  | GetWSStatuss (Syndicate WSStatus -> k)
  | SetWSStatuss (Syndicate WSStatus) k

  | GetWebSocketMsgHandlers (IORef (Map.HashMap Txt (Syndicate Dispatch)) -> k)
  | SetWebSocketMsgHandlers (IORef (Map.HashMap Txt (Syndicate Dispatch))) k

  | InitializeWebSocket k

  | ConnectWebSocket EvQueue k
  deriving Functor

type WS = WebSocket

instance Delta WebSocket WebSocket where
  delta eval WebSocket{..} (GetWebSocket wsk)           = delta eval webSocket wsk
  delta eval WebSocket{..} (SetWebSocket ws k)          = delta eval webSocketSetter (ws,k)
  delta eval WebSocket{..} (GetWebSocketReconnecting tk)= delta eval webSocketReconnecting tk
  delta eval WebSocket{..} (SetWebSocketReconnecting t k) = delta eval webSocketReconnectingSetter (t,k)
  delta eval WebSocket{..} (GetWSStatus wssk)     = delta eval webSocketState wssk
  delta eval WebSocket{..} (SetWSStatus wss k)    = delta eval webSocketStateSetter (wss,k)
  delta eval WebSocket{..} (GetWSStatuss wssk)    = delta eval webSocketStates wssk
  delta eval WebSocket{..} (SetWSStatuss wss k)   = delta eval webSocketStatesSetter (wss,k)
  delta eval WebSocket{..} (GetWebSocketMsgHandlers wsmhk) = delta eval webSocketMsgHandlers wsmhk
  delta eval WebSocket{..} (SetWebSocketMsgHandlers wsmh k) = delta eval webSocketMsgHandlersSetter (wsmh,k)
  delta eval WebSocket{..} (InitializeWebSocket k)      = eval webSocketInitializer k
  delta eval WebSocket{..} (ConnectWebSocket s k)       = delta eval webSocketConnecter (s,k)

data RequestHandler ms c rqTy
  where
    RequestHandler
      :: ( MonadIO c
         , ms <: '[Evented,WebSocket]

         , Request rqTy

         , Req rqTy ~ request
         , Identify request
         , I request ~ rqI
         , FromJSON request

         , Rsp rqTy ~ rsp
         , ToJSON rsp
         )
      => Proxy rqTy
      -> (Ef ms c () -> Either Dispatch (Either LazyByteString rsp -> Ef ms c (),request) -> Ef '[Event Dispatch] (Ef ms c) ())
      -> RequestHandler ms c rqTy

responds :: ( MonadIO c
            , ms <: '[Evented,WebSocket]

            , Request rqTy

            , Req rqTy ~ request
            , Identify request
            , I request ~ rqI
            , FromJSON request

            , Rsp rqTy ~ rsp
            , ToJSON rsp
            )
          => Proxy rqTy
          -> (Ef ms c () -> Either Dispatch (Either LazyByteString rsp -> Ef ms c (),request) -> Ef '[Event Dispatch] (Ef ms c) ())
          -> RequestHandler ms c rqTy
responds = RequestHandler

type ReqHandlers ms c rqs = Endpoints RequestHandler ms c rqs

data MessageHandler ms c mTy
  where
    MessageHandler
      :: ( MonadIO c
         , ms <: '[Evented,WebSocket]
         , Message mTy
         , M mTy ~ message
         , ToJSON message
         )
      => Proxy mTy
      -> (Ef ms c () -> Either Dispatch message -> Ef '[Event Dispatch] (Ef ms c) ())
      -> MessageHandler ms c mTy

accepts :: ( MonadIO c
           , ms <: '[Evented,WebSocket]
           , Message mTy
           , M mTy ~ message
           , ToJSON message
           )
        => Proxy mTy
        -> (Ef ms c () -> Either Dispatch message -> Ef '[Event Dispatch] (Ef ms c) ())
        -> MessageHandler ms c mTy
accepts = MessageHandler

type MsgHandlers ms c msgs = Endpoints MessageHandler ms c msgs

instance ( GetHandler MessageHandler message msgs'
         , DeleteHandler MessageHandler message msgs' msgs''
         , Removed msgs' message ~ msgs''
         , EnactEndpoints MessageAPI MessageHandler ms c msgs msgs''
         , Message message
         , M message ~ msg
         , ToJSON msg
         , FromJSON msg
         )
  => EnactEndpoints MessageAPI MessageHandler ms c (message ': msgs) msgs' where
  enactEndpoints (APICons pm ms) mhs =
    case getHandler mhs :: MessageHandler ms c message of
      MessageHandler _ f -> do
        let p = Proxy :: Proxy message
            mhs' = deleteHandler p mhs :: MsgHandlers ms c msgs''
        amh <- onMessage p ((unsafeCoerce f) :: Ef ms c () -> Either Dispatch msg -> Ef '[Event Dispatch] (Ef ms c) ())
        ams <- enactEndpoints ms mhs'
        return $ ActiveEndpointsCons pm amh ams

instance ( GetHandler RequestHandler request rqs'
         , DeleteHandler RequestHandler request rqs' rqs''
         , Removed rqs' request ~ rqs''
         , EnactEndpoints RequestAPI RequestHandler ms c rqs rqs''
         , Request request
         , Req request ~ req
         , Rsp request ~ rsp
         , ToJSON req
         , FromJSON rsp
         )
  => EnactEndpoints RequestAPI RequestHandler ms c (request ': rqs) rqs' where
  enactEndpoints (APICons pm ms) mhs =
    case getHandler mhs :: RequestHandler ms c request of
      RequestHandler _ f -> do
        let p = Proxy :: Proxy request
            mhs' = deleteHandler p mhs :: ReqHandlers ms c rqs''
        amh <- respond p ((unsafeCoerce f) :: Ef ms c () -> Either Dispatch (Either LazyByteString rsp -> Ef ms c (),req) -> Ef '[Event Dispatch] (Ef ms c) ())
        ams <- enactEndpoints ms mhs'
        return $ ActiveEndpointsCons pm amh ams

data Implementation ms c msgs rqs msgs' rqs'
  where
    Impl
      :: ( EnactEndpoints MessageAPI MessageHandler ms c msgs msgs'
         , EnactEndpoints RequestAPI RequestHandler ms c rqs rqs'
         , MonadIO c
         )
      => FullAPI msgs rqs
      -> Endpoints MessageHandler ms c msgs'
      -> Endpoints RequestHandler ms c rqs'
      -> Implementation ms c msgs rqs msgs' rqs'

-- well that's an obnoxious type signature
(<+++>) :: (TListAppend (Endpoints RequestHandler ms c) rqsl' rqsr' rqs'
           ,TListAppend (Endpoints MessageHandler ms c) msgsl' msgsr' msgs'
           ,TListAppend (API Request) rqsl rqsr rqs
           ,TListAppend (API Message) msgsl msgsr msgs
           ,EnactEndpoints RequestAPI RequestHandler ms c rqs rqs'
           ,EnactEndpoints MessageAPI MessageHandler ms c msgs msgs'
           )
        => Implementation ms c msgsl rqsl msgsl' rqsl'
        -> Implementation ms c msgsr rqsr msgsr' rqsr'
        -> Implementation ms c msgs rqs msgs' rqs'
(Impl (API ml rl) eml erl) <+++> (Impl (API mr rr) emr err) =
  Impl (API (ml <++> mr) (rl <++> rr)) (eml <++> emr) (erl <++> err)

enact :: Functor (Messages ms)
      => Implementation ms c msgs rqs msgs' rqs'
      -> Ef ms c (ActiveAPI ms c msgs rqs)
enact (Impl local mhs rhs) = do
  let API mapi rapi = local
  amapi <- enactEndpoints mapi mhs
  arapi <- enactEndpoints rapi rhs
  let active = ActiveAPI amapi arapi
  return active

cleanupEndpoints :: (MonadIO c, ms <: '[WebSocket])
                 => Ef ms c ()
cleanupEndpoints = do
  mh_ <- getWSMsgHandlers
  mh <- liftIO $ readIORef mh_
  let mhl = Map.toList mh
  mmhl' <- forM mhl $ \(h,n) -> do
             nn <- nullSyndicate n
             if nn then
               return Nothing
             else
               return $ Just (h,n)
  let mhl' = catMaybes mmhl'
      mh' = Map.fromList mhl'
  liftIO $ writeIORef mh_ mh'

ws :: forall ms c ts.
      ( MonadIO c
      , ts <. '[WebSocket]
      , ms <: '[WebSocket]
      , ts <=> ms
      )
   => String -> Int -> WebSocket (Action ts c)
ws hn p = ws_ hn p False

wss :: forall ms c ts.
       ( MonadIO c
       , ts <. '[WebSocket]
       , ms <: '[WebSocket]
       , ts <=> ms
       )
    => String -> Int -> WebSocket (Action ts c)
wss hn p = ws_ hn p True

ws_ :: forall ms c ts.
      (MonadIO c
      , ts <. '[WebSocket]
      , ms <: '[WebSocket]
      , ts <=> ms
      )
   => String -> Int -> Bool -> WebSocket (Action ts c)
ws_ hn p secure = WebSocket
    { webSocket = (Nothing,return)
    , webSocketSetter = \newws o ->
        let Module ws _ = o
            (_,wsGetter) = webSocket ws
        in return $ Module ws { webSocket = (newws,wsGetter) } o

    , webSocketReconnecting = (False,return)
    , webSocketReconnectingSetter = \mr o ->
        let Module ws _ = o
            (_,wsrGetter) = webSocketReconnecting ws
        in return $ Module ws { webSocketReconnecting = (mr,wsrGetter) } o

    , webSocketState = (WSUninitialized,return)
    , webSocketStateSetter = \wss o ->
        let Module ws _ = o
            (_,wssGetter) = webSocketState ws
        in return $ Module ws { webSocketState = (wss,wssGetter) } o

    , webSocketStates = (undefined,return)
    , webSocketStatesSetter = \wss o ->
        let Module ws _ = o
            (_,wssGetter) = webSocketStates ws
        in return $ Module ws { webSocketStates = (unsafeCoerce wss,wssGetter) } o

    , webSocketMsgHandlers = (undefined,return)
    , webSocketMsgHandlersSetter = \wsmh o ->
        let Module ws _ = o
            (_,wsmhGetter) = webSocketMsgHandlers ws
        in return $ Module ws { webSocketMsgHandlers = (unsafeCoerce wsmh,wsmhGetter) } o

    , webSocketInitializer = \o ->
        let Module ws _ = o
            (_,wsssGetter) = webSocketStates ws
            (_,wsmhsGetter) = webSocketMsgHandlers ws
        in do statesSyndicate :: Syndicate WSStatus <- syndicate
              msgHandlers <- liftIO $ newIORef (Map.empty :: Map.HashMap Txt (Syndicate Dispatch))
              return $ Module ws
                { webSocketStates = (statesSyndicate,wsssGetter)
                , webSocketMsgHandlers = (msgHandlers,wsmhsGetter)
                } o

    , webSocketConnecter = \gb o ->
        let Module ws _ = o
            (cws,wsGetter) = webSocket ws
            (statesSyndicate,wsssGetter) = webSocketStates ws
            (curState,wssGetter) = webSocketState ws
            (mhs_,_) = webSocketMsgHandlers ws
            port = show p
        in do msock <- case cws of
                Just sock -> return cws
                Nothing -> liftIO $ do
                  msock <- tryNewWebSocket (toTxt $ (if secure then "wss://" else "ws://") ++ hn ++ ':':port)
                  (rnr,_) :: (Signal ms c (Ef ms c ()),Behavior ms c (Ef ms c ())) <- runner
                  cBuf <- liftIO $ newIORef mempty
                  case msock of
                    Nothing -> return Nothing
                    Just sock -> do
                      openCallback <- CB.syncCallback1 CB.ContinueAsync $ \_ ->
                        publish statesSyndicate WSOpened
                      addEventListener sock "open" openCallback False

                      closeCallback <- CB.syncCallback1 CB.ContinueAsync $ \_ ->
                        publish statesSyndicate $ WSClosed ServerClosedConnection
                      addEventListener sock "close" closeCallback False

                      errorCallback <- CB.syncCallback1 CB.ContinueAsync $ \_ ->
                        publish statesSyndicate $ WSClosed ServerClosedConnection
                      addEventListener sock "error" errorCallback False

                      messageCallback <- CB.syncCallback1 CB.ContinueAsync $ \ev -> do
                        case WME.getData $ unsafeCoerce ev of
                          WME.StringData sd -> liftIO $ do
                            -- printAny sd
#if defined(DEBUGWS) || defined(DEVEL)
                            putStrLn $ "Received message: " ++ show sd
#endif
                            case JS.uncons sd of

                              Just ('C',rst) -> do
                                modifyIORef cBuf (<> rst)

                              Just ('F',rst) -> do
                                b <- readIORef cBuf
                                writeIORef cBuf mempty
                                let msg = b <> rst
                                    val = js_JSON_parse msg
                                case fromJSON val of
                                  Error e ->
                                    putStrLn $ "(multi-part):fromJSON message failed with: " ++ e
                                  Success m -> do
                                    mhs <- liftIO $ readIORef mhs_
                                    case Map.lookup (ep m) mhs of
                                      Nothing -> putStrLn $ "(multi-part):No handler found: " ++ show (ep m)
                                      Just h  -> do
#if defined(DEBUGWS) || defined(DEVEL)
                                        putStrLn $ "Handled message at endpoint: " ++ show (ep m)
#endif
                                        buffer gb rnr $ publish h m

                              Just ('{',_) -> do
                                let val = js_JSON_parse sd
                                case fromJSON val of
                                  Error e -> putStrLn $ "fromJSON failed with: " ++ e
                                  Success m -> do
                                    mhs <- liftIO $ readIORef mhs_
                                    case Map.lookup (ep m) mhs of
                                      Nothing -> putStrLn $ "No handler found: " ++ show (ep m)
                                      Just h  -> do
#if defined(DEBUGWS) || defined(DEVEL)
                                        putStrLn $ "Handled message at endpoint: " ++ show (ep m)
#endif
                                        buffer gb rnr $ publish h m

                              _ ->
                              -- Any message not beginning with 'C', 'F', or '{' is guaranteed to be invalid.
                               putStrLn $ "Invalid message: " ++ show sd

                          _ -> return ()

                      addEventListener sock "message" messageCallback False
                      return $ Just sock
              return $ Module ws { webSocket = (msock,wsGetter)} o
    }

foreign import javascript unsafe
  "try { $r = new window[\"WebSocket\"]($1) } catch (e) { $r = null}"
    js_tryNewWebSocket :: Txt -> IO JSV

tryNewWebSocket :: Txt -> IO (Maybe JSV)
tryNewWebSocket url = do
  ws <- js_tryNewWebSocket url
  if isNull ws
    then return Nothing
    else return (Just ws)

isWSReconnecting :: forall ms c.
                    (Monad c, ms <: '[WebSocket])
                 => Ef ms c Bool
isWSReconnecting = Send (GetWebSocketReconnecting Return)

setWSReconnecting :: forall ms c.
                     (Monad c, ms <: '[WebSocket])
                  => Bool -> Ef ms c ()
setWSReconnecting b = Send (SetWebSocketReconnecting b (Return ()))

getWS :: forall ms c.
         (Monad c, ms <: '[WebSocket])
      => Ef ms c (Maybe JSV)
getWS = Send (GetWebSocket Return)

setWS :: forall ms c.
         (Monad c, ms <: '[WebSocket])
      => Maybe JSV -> Ef ms c ()
setWS mws = Send (SetWebSocket mws (Return ()))

getWSState :: forall ms c.
              (Monad c, ms <: '[WebSocket])
           => Ef ms c WSStatus
getWSState = Send (GetWSStatus Return)

setWSState :: forall ms c.
              (Monad c, ms <: '[WebSocket])
           => WSStatus -> Ef ms c ()
setWSState wss = Send (SetWSStatus wss (Return ()))

getWSMsgHandlers :: forall ms c.
                    (Monad c, ms <: '[WebSocket])
                 => Ef ms c (IORef (Map.HashMap Txt (Syndicate Dispatch)))
getWSMsgHandlers = Send (GetWebSocketMsgHandlers Return)

putWSMsgHandlers :: forall ms c.
                    (Monad c, ms <: '[WebSocket])
                => IORef (Map.HashMap Txt (Syndicate Dispatch))
                -> Ef ms c ()
putWSMsgHandlers hm = Send (SetWebSocketMsgHandlers hm (Return ()))

wsSetup :: forall ms c.
           (MonadIO c, ms <: '[Evented,WebSocket])
        => Ef ms c ()
wsSetup = Send (InitializeWebSocket (Return ()))

wsInitialize :: forall ms c.
                (MonadIO c, ms <: '[Evented,WebSocket])
             => Ef ms c WSStatus
wsInitialize = do
  wsSetup
  wssn <- Send (GetWSStatuss Return)
  connect wssn $ \wss -> lift $ do
    setWSState wss
    case wss of
      WSClosed _ -> void $ do
        setWS Nothing
        reconnectOnInterval 500000
      _ -> return ()
  wsConnect

wsConnect :: forall ms c.
             (MonadIO c, ms <: '[Evented,WebSocket])
          => Ef ms c WSStatus
wsConnect = do
  sig <- get
  Send (ConnectWebSocket sig (Return ()))
  getWSState

foreign import javascript unsafe
  "$1.close()" ws_close_js :: JSV -> Int -> Txt -> IO ()

wsDisconnect :: forall ms c.
                (MonadIO c, ms <: '[WebSocket])
             => Ef ms c ()
wsDisconnect = do
  mws <- getWS
  case mws of
    Just ws -> liftIO (ws_close_js ws 1000 "wsDisconnect called.")
    Nothing -> return ()


send' :: forall ms c.
        (MonadIO c, ms <: '[Evented,WebSocket])
     => Either LazyByteString Dispatch -> Ef ms c (Either WSException SendStatus)
send' m = go True
  where
    go b = do
      wss <- getWSState
      case wss of
        WSOpened -> do
          mws <- getWS
          case mws of
            -- If WSOpened, getWS => (Just ws)
            Nothing -> return (Left InvalidSocketState)
            Just ws -> do
              let bs = either id toBS m
                  (sbi,_,_) = GB.fromByteString $ BSL.toStrict bs
                  sabi = GB.getArrayBuffer sbi
#if defined(DEBUGWS) || defined(DEVEL)
              liftIO $ putStrLn $ "send' sending: " ++ show bs
#endif
              liftIO $ send_js ws sabi
              return (Right Sent)
        _ -> do
          -- buffer the message for when the socket opens back up
          -- note that this could lead to excessive memory usage!
          onWSStatus $ \wss ->
            case wss of
              WSOpened -> do
                mws <- lift getWS
                case mws of
                  Nothing -> return () -- huh?
                  Just ws -> do
                    let bs = either id toBS m
                        (sbi,_,_) = GB.fromByteString $ BSL.toStrict bs
                        sabi = GB.getArrayBuffer sbi
#if defined(DEBUGWS) || defined(DEVEL)
                    liftIO $ putStrLn $ "send' sending after websocket state changed: " ++ show bs
#endif
                    liftIO $ send_js ws sabi
                -- stop when successful
                end
              -- If not opened, just wait for the next event.
              _ ->
                return ()
          return (Right BufferedSend)

data SendStatus = BufferedSend | Sent

foreign import javascript unsafe
  "$1.send($2)" send_js :: JSV -> TAB.ArrayBuffer -> IO ()

trySend' :: forall ms c.
           (MonadIO c, ms <: '[WebSocket])
        => Either LazyByteString Dispatch -> Ef ms c (Either WSStatus ())
trySend' m = do
  wss <- getWSState
  case wss of
    WSOpened -> do
      mws <- getWS
      case mws of
        Nothing -> return (Left WSUninitialized) -- not correct....
        Just ws -> do
          let (sbi,_,_) = GB.fromByteString $ BSL.toStrict $ either id toBS m
              sabi = GB.getArrayBuffer sbi
#if defined(DEBUGWS) || defined(DEVEL)
          liftIO $ putStrLn $ "trySend' sending: " ++ show (fmap (encode . toJSON) m)
#endif
          liftIO (Right <$> send_js ws sabi)
    _ -> do
#if defined(DEBUGWS) || defined(DEVEL)
      liftIO $ putStrLn $ "trySend' couldn't send: " ++ show (fmap (encode . toJSON) m)
#endif
      return $ Left wss

data WSException = InvalidSocketState deriving Show
instance Exception WSException

unsubscribe :: forall ms c.
               (MonadIO c, Functor (Messages ms))
            => Behavior ms c WME.MessageEvent -> Ef ms c ()
unsubscribe = stop

onWSStatus :: forall ms c.
           (MonadIO c, ms <: '[Evented,WebSocket])
        => (WSStatus -> Ef '[Event WSStatus] (Ef ms c) ())
        -> Ef ms c (IO ())
onWSStatus bhvr = do
  wsn <- Send (GetWSStatuss Return)
  connect wsn bhvr

onWSClose :: forall ms c.
            (MonadIO c, ms <: '[Evented,WebSocket])
        => (WSCloseReason -> Ef '[Event WSStatus] (Ef ms c) ())
        -> Ef ms c (IO ())
onWSClose f = onWSStatus (\wss -> case wss of { WSClosed wscr -> f wscr; _ -> return () } )

foreign import javascript unsafe
  "Math.floor((Math.random() * $1) + 1)" random :: Int -> Int

-- exponential backoff based on a minimum interval
reconnectOnInterval :: forall ms c port.
                       (MonadIO c, ms <: '[Evented,WebSocket])
                    => Int -> Ef ms c ()
reconnectOnInterval interval = go
  where
    go = do
      isR <- isWSReconnecting
      unless isR $ do
        setWSReconnecting True
        reconnecter 0

    reconnecter n = do
      wss <- wsConnect
      case wss of
        WSOpened ->
          setWSReconnecting False
        _ -> do
          let r = random (2 ^ n - 1)
              i = interval * r
          void $ delay i $ reconnecter (min (n + 1) 12)

send :: ( ToJSON a
        , MonadIO c
        , MonadIO c'
        , ms' <: '[Evented,WebSocket]
        , With w (Ef ms' c') IO
        , Functor (Messages ms)
        )
     => w
     -> Txt
     -> a
     -> Ef ms c (Promise (Either WSException SendStatus))
send s h a = with s $ send' $ Right $ Dispatch h $ toJSON a

trySend :: ( ToJSON a
           , MonadIO c
           , MonadIO c'
           , ms' <: '[WebSocket]
           , With w (Ef ms' c') IO
           , Functor (Messages ms)
           )
        => w
        -> Txt
        -> a
        -> Ef ms c (Promise (Either WSStatus ()))
trySend s h a = with s $ trySend' $ Right $ Dispatch h $ toJSON a


-- use Endpoint
sendSelfMessage :: ( MonadIO c
                   , MonadIO c'

                   , With w (Ef ms' c') IO

                   , ms' <: '[WebSocket]

                   , ms <: '[Evented]

                   , Message mTy
                   , M mTy ~ msg
                   , ToJSON msg
                   )
                => w
                -> Proxy mTy
                -> msg
                -> Ef ms c (Promise ())
sendSelfMessage s mty_proxy m =
  with s $ do
    mhs_ <- getWSMsgHandlers
    mhs <- liftIO $ readIORef mhs_
    let header = messageHeader mty_proxy
    case Map.lookup header mhs of
      Nothing -> return ()
      Just n -> publish n (Dispatch header $ toJSON m)

request :: forall c ms rqTy request rqI rsp.
            ( MonadIO c

            , ms <: '[Evented,WebSocket]

            , Request rqTy

            , Req rqTy ~ request
            , ToJSON request
            , Identify request
            , I request ~ rqI

            , Rsp rqTy ~ rsp
            , FromJSON rsp
            )
         => Proxy rqTy
         -> request
         -> (Ef ms c () -> Either Dispatch rsp -> Ef '[Event Dispatch] (Ef ms c) ())
         -> Ef ms c (Endpoint Dispatch)
request rqty_proxy req f = do
  s_ <- liftIO $ newIORef undefined
  let header = responseHeader rqty_proxy req
      bhvr m =
        let done = do
              (syn,stopper) <- liftIO $ readIORef s_
              liftIO stopper
              (me,subs) <- takeSyndicate syn
              if Prelude.null subs then do
                mhs_ <- getWSMsgHandlers
                liftIO $ modifyIORef mhs_ $ \mhs ->
                  let !mhs' = Map.delete header mhs
                  in mhs'
                putSyndicate syn (me,subs)
              else
                putSyndicate syn (me,subs)
        in f done (maybe (Left m) Right (decodeDispatch m))
  mhs_ <- getWSMsgHandlers
  newn <- syndicate
  n <- liftIO $ atomicModifyIORef' mhs_ $ \mhs ->
          case Map.lookup header mhs of
            Nothing -> (Map.insert header newn mhs,newn)
            Just n -> (mhs,n)

  sub :: Subscription (Ef ms c) Dispatch <- subscribe n get
  bhv <- listen sub bhvr
  let stopper = forkStop bhv >> leaveSyndicate n sub

  liftIO $ writeIORef s_ (n,stopper)
  send' $ Right (Dispatch (requestHeader rqty_proxy) (toJSON req))
  return (Endpoint header (unsafeCoerce sub) n)

apiRequest :: ( MonadIO c

              , ms <: '[Evented,WebSocket]

              , Request rqTy

              , Req rqTy ~ request
              , ToJSON request
              , Identify request
              , I request ~ rqI

              , Rsp rqTy ~ rsp
              , FromJSON rsp

              , (rqTy ∈ rqs) ~ 'True
              )
          => FullAPI msgs rqs
          -> Proxy rqTy
          -> request
          -> (Ef ms c () -> Either Dispatch rsp -> Ef '[Event Dispatch] (Ef ms c) ())
          -> Ef ms c (Endpoint Dispatch)
apiRequest _ = request

requestWith :: forall c c' ms ms' w rqTy request rqI rsp.
               ( MonadIO c
               , MonadIO c'

               , With w (Ef ms' c') IO

               , ms' <: '[Evented,WebSocket]

               , ms <: '[Evented]

               , Request rqTy

               , Req rqTy ~ request
               , ToJSON request
               , Identify request
               , I request ~ rqI

               , Rsp rqTy ~ rsp
               , FromJSON rsp
               )
            => w
            -> Proxy rqTy
            -> request
            -> (Ef ms c () -> Either Dispatch rsp -> Ef '[Event Dispatch] (Ef ms c) ())
            -> Ef ms c (Promise (Endpoint Dispatch))
requestWith s rqty_proxy req f = do
  pr <- promise
  buf <- get
  s_ <- liftIO $ newIORef undefined

  let header = responseHeader rqty_proxy req

      bhvr m =
        let done = do
              (syn,stopper) <- liftIO $ readIORef s_
              liftIO stopper
              (me,subs) <- takeSyndicate syn
              if Prelude.null subs then
                void $ with s $ do
                  mhs_ <- getWSMsgHandlers
                  liftIO $ modifyIORef mhs_ $ \mhs ->
                    let !mhs' = Map.delete header mhs
                    in mhs'
                  putSyndicate syn (me,subs)
              else
                putSyndicate syn (me,subs)

        in f done (maybe (Left m) Right (decodeDispatch m))

  newn <- syndicate
  with s $ do
    mhs_ <- getWSMsgHandlers
    n <- liftIO $ atomicModifyIORef' mhs_ $ \mhs ->
           case Map.lookup header mhs of
              Nothing -> (Map.insert header newn mhs,newn)
              Just n -> (mhs,n)

    sub :: Subscription (Ef ms c) Dispatch <- subscribe n (return buf)
    bhv <- listen sub bhvr
    let stopper = forkStop bhv >> leaveSyndicate n sub

    liftIO $ writeIORef s_ (n,stopper)
    send' $ Right (Dispatch (requestHeader rqty_proxy) (toJSON req))
    fulfill pr (Endpoint header (unsafeCoerce sub) n)

  return pr

apiRequestWith :: ( MonadIO c
                  , MonadIO c'

                  , With w (Ef ms' c') IO

                  , ms' <: '[Evented,WebSocket]

                  , ms <: '[Evented]

                  , Request rqTy

                  , Req rqTy ~ request
                  , ToJSON request
                  , Identify request
                  , I request ~ rqI

                  , Rsp rqTy ~ rsp
                  , FromJSON rsp

                  , (rqTy ∈ rqs) ~ 'True
                  )
               => FullAPI msgs rqs
               -> w
               -> Proxy rqTy
               -> request
               -> (Ef ms c () -> Either Dispatch rsp -> Ef '[Event Dispatch] (Ef ms c) ())
               -> Ef ms c (Promise (Endpoint Dispatch))
apiRequestWith _ = requestWith

respondWith :: forall c c' w ms ms' rqTy request rqI rsp.
               ( MonadIO c
               , MonadIO c'

               , With w (Ef ms' c') IO

               , ms <: '[Evented]

               , ms' <: '[Evented,WebSocket]

               , Request rqTy

               , Req rqTy ~ request
               , Identify request
               , I request ~ rqI
               , FromJSON request

               , Rsp rqTy ~ rsp
               , ToJSON rsp
               )
            => w
            -> Proxy rqTy
            -> (Ef ms c () -> Either Dispatch (Either LazyByteString rsp -> Ef ms c (Promise ()),request) -> Ef '[Event Dispatch] (Ef ms c) ())
            -> Ef ms c (Promise (Endpoint Dispatch))
respondWith s rqty_proxy rr = do
  pr <- promise
  buf <- get
  s_ <- liftIO $ newIORef undefined

  let header = requestHeader rqty_proxy

      bhvr m =
        let done = do
              (syn,stopper) <- liftIO $ readIORef s_
              liftIO stopper
              (me,subs) <- takeSyndicate syn
              if Prelude.null subs then
                void $ with s $ do
                  mhs_ <- getWSMsgHandlers
                  liftIO $ modifyIORef mhs_ $ \mhs ->
                    let !mhs' = Map.delete header mhs
                    in mhs'
                  putSyndicate syn (me,subs)
              else
                putSyndicate syn (me,subs)

        in rr done (maybe (Left m) (\rq -> Right (with s . void . send' . either Left (Right . Dispatch (responseHeader rqty_proxy rq) . toJSON),rq)) (decodeDispatch m))

  newn <- syndicate
  with s $ do
    mhs_ <- getWSMsgHandlers
    n <- liftIO $ atomicModifyIORef' mhs_ $ \mhs ->
           case Map.lookup header mhs of
              Nothing -> (Map.insert header newn mhs,newn)
              Just n -> (mhs,n)

    sub :: Subscription (Ef ms c) Dispatch <- subscribe n (return buf)
    bhv <- listen sub bhvr
    let stopper = forkStop bhv >> leaveSyndicate n sub

    liftIO $ writeIORef s_ (n,stopper)
    fulfill pr (Endpoint header (unsafeCoerce sub) n)

  return pr

respond :: forall c ms rqTy request rqI rsp.
           ( MonadIO c

           , ms <: '[Evented,WebSocket]

           , Request rqTy

           , Req rqTy ~ request
           , Identify request
           , I request ~ rqI
           , FromJSON request

           , Rsp rqTy ~ rsp
           , ToJSON rsp
           )
        => Proxy rqTy
        -> (Ef ms c () -> Either Dispatch (Either LazyByteString rsp -> Ef ms c (),request) -> Ef '[Event Dispatch] (Ef ms c) ())
        -> Ef ms c (Endpoint Dispatch)
respond rqty_proxy rr = do
  s_ <- liftIO $ newIORef undefined
  let header = requestHeader rqty_proxy

      bhvr m =
        let done = do
              (syn,stopper) <- liftIO $ readIORef s_
              liftIO stopper
              (me,subs) <- takeSyndicate syn
              if Prelude.null subs then do
                mhs_ <- getWSMsgHandlers
                liftIO $ modifyIORef mhs_ $ \mhs ->
                  let !mhs' = Map.delete header mhs
                  in mhs'
                putSyndicate syn (me,subs)
              else
                putSyndicate syn (me,subs)

        in rr done (maybe (Left m) (\rq -> Right (void . send' . either Left (Right . Dispatch (responseHeader rqty_proxy rq) . toJSON),rq)) (decodeDispatch m))

  newn <- syndicate
  mhs_ <- getWSMsgHandlers
  n <- liftIO $ atomicModifyIORef' mhs_ $ \mhs ->
          case Map.lookup header mhs of
            Nothing -> (Map.insert header newn mhs,newn)
            Just n -> (mhs,n)

  sub :: Subscription (Ef ms c) Dispatch <- subscribe n get
  bhv <- listen sub bhvr
  let stopper = forkStop bhv >> leaveSyndicate n sub

  liftIO $ writeIORef s_ (n,stopper)
  return (Endpoint header (unsafeCoerce sub) n)

messageWith :: ( MonadIO c
               , MonadIO c'
               , ms' <: '[Evented,WebSocket]
               , With w (Ef ms' c') IO
               , Message mTy
               , M mTy ~ msg
               , ToJSON msg
               , Functor (Messages ms)
               )
            => w
            -> Proxy mTy
            -> msg
            -> Ef ms c (Promise (Either WSException SendStatus))
messageWith s mty_proxy m = Pure.WebSocket.GHCJS.send s (messageHeader mty_proxy) m

apiMessageWith :: ( MonadIO c
                  , MonadIO c'
                  , ms' <: '[Evented,WebSocket]
                  , With w (Ef ms' c') IO
                  , Message mTy
                  , M mTy ~ msg
                  , ToJSON msg
                  , (mTy ∈ msgs) ~ 'True
                  , Functor (Messages ms)
                  )
                => FullAPI msgs rqs
                -> w
                -> Proxy mTy
                -> msg
                -> Ef ms c (Promise (Either WSException SendStatus))
apiMessageWith _ = messageWith

message :: ( MonadIO c
           , ms <: '[Evented,WebSocket]
           , Message mTy
           , M mTy ~ msg
           , ToJSON msg
           )
        => Proxy mTy
        -> msg
        -> Ef ms c (Either WSException SendStatus)
message mty_proxy m = send' $ Right (Dispatch (messageHeader mty_proxy) (toJSON m))

apiMessage :: ( MonadIO c
              , ms <: '[Evented,WebSocket]
              , Message mTy
              , M mTy ~ msg
              , ToJSON msg
              , (mTy ∈ msgs) ~ 'True
              )
            => FullAPI msgs rqs
            -> Proxy mTy
            -> msg
            -> Ef ms c (Either WSException SendStatus)
apiMessage _ = message

onMessageWith :: forall c c' ms ms' w mTy msg.
                 ( MonadIO c
                 , MonadIO c'
                 , ms' <: '[WebSocket]
                 , ms <: '[Evented]
                 , With w (Ef ms' c') IO
                 , Message mTy
                 , M mTy ~ msg
                 , FromJSON msg
                 )
               => w
               -> Proxy mTy
               -> (Ef ms c () -> Either Dispatch msg -> Ef '[Event Dispatch] (Ef ms c) ())
               -> Ef ms c (Promise (Endpoint Dispatch))
onMessageWith s mty_proxy f = do
  pr <- promise
  buf <- get
  s_ <- liftIO $ newIORef undefined

  let header = messageHeader mty_proxy

      bhvr m =
        let done = do
              (syn,stopper) <- liftIO $ readIORef s_
              liftIO stopper
              (me,subs) <- takeSyndicate syn
              if Prelude.null subs then
                void $ with s $ do
                  mhs_ <- getWSMsgHandlers
                  liftIO $ modifyIORef mhs_ $ \mhs ->
                    let !mhs' = Map.delete header mhs
                    in mhs'
                  putSyndicate syn (me,subs)
              else
                putSyndicate syn (me,subs)

        in f done (maybe (Left m) Right (decodeDispatch m))

  newn <- syndicate
  with s $ do
    mhs_ <- getWSMsgHandlers
    n <- liftIO $ atomicModifyIORef' mhs_ $ \mhs ->
            case Map.lookup header mhs of
              Nothing -> (Map.insert header newn mhs,newn)
              Just n -> (mhs,n)

    sub :: Subscription (Ef ms c) Dispatch <- subscribe n (return buf)
    bhv <- listen sub bhvr
    let stopper = forkStop bhv >> leaveSyndicate n sub

    liftIO $ writeIORef s_ (n,stopper)
    fulfill pr (Endpoint header (unsafeCoerce sub) n)
  return pr

onMessage :: forall c ms mTy msg.
             ( MonadIO c
             , ms <: '[Evented,WebSocket]
             , Message mTy
             , M mTy ~ msg
             , FromJSON msg
             )
          => Proxy mTy
          -> (Ef ms c () -> Either Dispatch msg -> Ef '[Event Dispatch] (Ef ms c) ())
          -> Ef ms c (Endpoint Dispatch)
onMessage mty_proxy f = do
  s_ <- liftIO $ newIORef undefined

  let header = messageHeader mty_proxy

      bhvr m =
        let done = do
              (syn,stopper) <- liftIO $ readIORef s_
              liftIO stopper
              (me,subs) <- takeSyndicate syn
              if Prelude.null subs then do
                mhs_ <- getWSMsgHandlers
                liftIO $ modifyIORef mhs_ $ \mhs ->
                  let !mhs' = Map.delete header mhs
                  in mhs'
                putSyndicate syn (me,subs)
              else
                putSyndicate syn (me,subs)

        in f done (maybe (Left m) Right (decodeDispatch m))

  newn <- syndicate
  mhs_ <- getWSMsgHandlers
  n <- liftIO $ atomicModifyIORef' mhs_ $ \mhs ->
          case Map.lookup header mhs of
            Nothing -> (Map.insert header newn mhs,newn)
            Just n -> (mhs,n)

  sub :: Subscription (Ef ms c) Dispatch <- subscribe n get
  bhv <- listen sub bhvr
  let stopper = forkStop bhv >> leaveSyndicate n sub

  liftIO $ writeIORef s_ (n,stopper)
  return (Endpoint header (unsafeCoerce sub) n)
