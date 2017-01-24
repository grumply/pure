{-# language MagicHash #-}
{-# language UnboxedTuples #-}
{-# language GHCForeignImportPrim #-}
{-# language UnliftedFFITypes #-}
{-# language UndecidableInstances #-}
{-# language InstanceSigs #-}
{-# language ScopedTypeVariables #-}
{-# language KindSignatures #-}
{-# language TypeFamilies #-}
{-# language GADTs #-}
{-# language CPP #-}
module Fusion.WebSocket (module Fusion.WebSocket) where

import Ef.Base

import Fusion.JS

import Data.JSText as AE
import Nuclear.API
import Nuclear.Nuclear
import Nuclear.Endpoint
import Nuclear.Indexed
import Nuclear.Message
import Nuclear.TypeRep
import Nuclear.Request
import Nuclear.Revent
import Nuclear.With
import Nuclear.ToBS
import Nuclear.ToText
import Nuclear.FromBS
import Nuclear.FromText
import Nuclear.Strict

import qualified GHCJS.Buffer as GB
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.EventM as Ev
import qualified GHCJS.DOM.Location as L
import qualified GHCJS.DOM.Types as DT
import qualified GHCJS.DOM.WebSocket as WS
import qualified GHCJS.DOM.Window as W
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
import Data.IORef
import Data.Maybe
import Data.Monoid
import Unsafe.Coerce

import Text.Read hiding (lift,get)

import GHC.Prim

data WebSocketState = WSUninitialized | WSOpened | WSError | WSClosed deriving Show

data WebSocket k
  = WebSocket
    { webSocket       :: (Maybe WS.WebSocket,k)
    , webSocketSetter :: Maybe WS.WebSocket -> k

    , webSocketReconnecting       :: (Bool,k)
    , webSocketReconnectingSetter :: Bool -> k

    , webSocketState       :: (WebSocketState,k)
    , webSocketStateSetter :: WebSocketState -> k

    , webSocketStates         :: (Network WebSocketState,k)
    , webSocketStatesSetter   :: Network WebSocketState -> k

    , webSocketMsgHandlers :: (IORef (Map.HashMap JSText (Network Nuclear)),k)
    , webSocketMsgHandlersSetter :: IORef (Map.HashMap JSText (Network Nuclear)) -> k

    , webSocketInitializer :: k

    , webSocketConnecter :: Signaled -> k

    }
  | GetWebSocket (Maybe WS.WebSocket -> k)
  | SetWebSocket (Maybe WS.WebSocket) k

  | GetWebSocketReconnecting (Bool -> k)
  | SetWebSocketReconnecting Bool k

  | GetWebSocketState (WebSocketState -> k)
  | SetWebSocketState WebSocketState k

  | GetWebSocketStates (Network WebSocketState -> k)
  | SetWebSocketStates (Network WebSocketState) k

  | GetWebSocketMsgHandlers (IORef (Map.HashMap JSText (Network Nuclear)) -> k)
  | SetWebSocketMsgHandlers (IORef (Map.HashMap JSText (Network Nuclear))) k

  | InitializeWebSocket k

  | ConnectWebSocket Signaled k
  deriving Functor

instance Delta WebSocket WebSocket where
  delta eval WebSocket{..} (GetWebSocket wsk)           = delta eval webSocket wsk
  delta eval WebSocket{..} (SetWebSocket ws k)          = delta eval webSocketSetter (ws,k)
  delta eval WebSocket{..} (GetWebSocketReconnecting tk)= delta eval webSocketReconnecting tk
  delta eval WebSocket{..} (SetWebSocketReconnecting t k) = delta eval webSocketReconnectingSetter (t,k)
  delta eval WebSocket{..} (GetWebSocketState wssk)     = delta eval webSocketState wssk
  delta eval WebSocket{..} (SetWebSocketState wss k)    = delta eval webSocketStateSetter (wss,k)
  delta eval WebSocket{..} (GetWebSocketStates wssk)    = delta eval webSocketStates wssk
  delta eval WebSocket{..} (SetWebSocketStates wss k)   = delta eval webSocketStatesSetter (wss,k)
  delta eval WebSocket{..} (GetWebSocketMsgHandlers wsmhk) = delta eval webSocketMsgHandlers wsmhk
  delta eval WebSocket{..} (SetWebSocketMsgHandlers wsmh k) = delta eval webSocketMsgHandlersSetter (wsmh,k)
  delta eval WebSocket{..} (InitializeWebSocket k)      = eval webSocketInitializer k
  delta eval WebSocket{..} (ConnectWebSocket s k)       = delta eval webSocketConnecter (s,k)

data RequestHandler ms c rqTy
  where
    RequestHandler
      :: ( MonadIO c
         , '[Revent,WebSocket] <: ms

         , Request rqTy

         , Req rqTy ~ request
         , Indexed request
         , I request ~ rqI
         , FromJSON request
         , ToText rqI

         , Rsp rqTy ~ rsp
         , ToJSON rsp
         )
      => Proxy rqTy
      -> (Code ms c () -> Either Nuclear (rsp -> Code ms c (),request) -> Code '[Event Nuclear] (Code ms c) ())
      -> RequestHandler ms c rqTy

responds :: ( MonadIO c
            , '[Revent,WebSocket] <: ms

            , Request rqTy

            , Req rqTy ~ request
            , Indexed request
            , I request ~ rqI
            , FromJSON request
            , ToText rqI

            , Rsp rqTy ~ rsp
            , ToJSON rsp
            )
          => Proxy rqTy
          -> (Code ms c () -> Either Nuclear (rsp -> Code ms c (),request) -> Code '[Event Nuclear] (Code ms c) ())
          -> RequestHandler ms c rqTy
responds = RequestHandler

type ReqHandlers ms c rqs = Endpoints RequestHandler ms c rqs

data MessageHandler ms c mTy
  where
    MessageHandler
      :: ( MonadIO c
         , '[Revent,WebSocket] <: ms
         , Message mTy
         , M mTy ~ message
         , ToJSON message
         )
      => Proxy mTy
      -> (Code ms c () -> Either Nuclear message -> Code '[Event Nuclear] (Code ms c) ())
      -> MessageHandler ms c mTy

accepts :: ( MonadIO c
           , '[Revent,WebSocket] <: ms
           , Message mTy
           , M mTy ~ message
           , ToJSON message
           )
        => Proxy mTy
        -> (Code ms c () -> Either Nuclear message -> Code '[Event Nuclear] (Code ms c) ())
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
        amh <- onMessage p ((unsafeCoerce f) :: Code ms c () -> Either Nuclear msg -> Code '[Event Nuclear] (Code ms c) ())
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
        amh <- respond p ((unsafeCoerce f) :: Code ms c () -> Either Nuclear (rsp -> Code ms c (),req) -> Code '[Event Nuclear] (Code ms c) ())
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
(<+++>) :: (NuclearAppend (Endpoints RequestHandler ms c) rqsl' rqsr' rqs'
           ,NuclearAppend (Endpoints MessageHandler ms c) msgsl' msgsr' msgs'
           ,NuclearAppend (API Request) rqsl rqsr rqs
           ,NuclearAppend (API Message) msgsl msgsr msgs
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
      -> Code ms c (ActiveAPI ms c msgs rqs)
enact (Impl local mhs rhs) = do
  let API mapi rapi = local
  amapi <- enactEndpoints mapi mhs
  arapi <- enactEndpoints rapi rhs
  let active = ActiveAPI amapi arapi
  return active

cleanupEndpoints :: (MonadIO c, '[WebSocket] <: ms)
                 => Code ms c ()
cleanupEndpoints = do
  mh_ <- getWSMsgHandlers
  mh <- liftIO $ readIORef mh_
  let mhl = Map.toList mh
  mmhl' <- forM mhl $ \(h,n) -> do
             nn <- nullNetwork n
             if nn then
               return Nothing
             else
               return $ Just (h,n)
  let mhl' = catMaybes mmhl'
      mh' = Map.fromList mhl'
  liftIO $ writeIORef mh_ mh'

ws :: forall ms c ts.
      ( MonadIO c
      , '[WebSocket] <. ts
      , '[WebSocket] <: ms
      , Delta (Modules ts) (Messages ms)
      )
   => String -> Int -> WebSocket (Action ts c)
ws hn p = ws_ hn p False

wss :: forall ms c ts.
       ( MonadIO c
       , '[WebSocket] <. ts
       , '[WebSocket] <: ms
       , Delta (Modules ts) (Messages ms)
       )
    => String -> Int -> WebSocket (Action ts c)
wss hn p = ws_ hn p True

ws_ :: forall ms c ts.
      (MonadIO c
      , '[WebSocket] <. ts
      , '[WebSocket] <: ms
      , Delta (Modules ts) (Messages ms)
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
        in do statesNetwork :: Network WebSocketState <- network
              msgHandlers <- liftIO $ newIORef (Map.empty :: Map.HashMap JSText (Network Nuclear))
              return $ Module ws
                { webSocketStates = (statesNetwork,wsssGetter)
                , webSocketMsgHandlers = (msgHandlers,wsmhsGetter)
                } o

    , webSocketConnecter = \gb o ->
        let Module ws _ = o
            (cws,wsGetter) = webSocket ws
            (statesNetwork,wsssGetter) = webSocketStates ws
            (curState,wssGetter) = webSocketState ws
            (mhs_,_) = webSocketMsgHandlers ws
            port = show p
        in do msock <- case cws of
                Just sock -> return cws
                Nothing -> liftIO $ do
                  msock <- tryNewWebSocket ((if secure then "wss://" else "ws://") ++ hn ++ ':':port) (Just [] :: Maybe [String])
                  rnr :: Signal ms c (Code ms c ()) <- runner
                  cBuf <- liftIO $ newIORef mempty
                  case msock of
                    Nothing -> return Nothing
                    Just sock -> do
                      Ev.on sock WS.open $ lift $ syndicate statesNetwork WSOpened
                      Ev.on sock WS.closeEvent $ lift $ syndicate statesNetwork WSClosed
                      Ev.on sock WS.error $ lift $ syndicate statesNetwork WSError
                      Ev.on sock WS.message $ do
                        ev <- Ev.event
                        -- printAny ev
                        case WME.getData $ unsafeCoerce ev of
                          WME.StringData sd -> liftIO $ do
                          --  printAny sd
#ifdef DEBUGWS
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
#ifdef DEBUGWS
                                        putStrLn $ "Handled message at endpoint: " ++ show (ep m)
#endif
                                        buffer gb rnr $ syndicate h m

                              Just ('{',_) -> do
                                let val = js_JSON_parse sd
                                case fromJSON val of
                                  Error e -> putStrLn $ "fromJSON failed with: " ++ e
                                  Success m -> do
                                    mhs <- liftIO $ readIORef mhs_
                                    case Map.lookup (ep m) mhs of
                                      Nothing -> putStrLn $ "No handler found: " ++ show (ep m)
                                      Just h  -> do
#ifdef DEBUGWS
                                        putStrLn $ "Handled message at endpoint: " ++ show (ep m)
#endif
                                        buffer gb rnr $ syndicate h m

                              _ ->
                              -- Any message not beginning with 'C', 'F', or '{' is guaranteed to be invalid.
                               putStrLn $ "Invalid message: " ++ show sd

                          _ -> return ()

                      return $ Just sock
              return $ Module ws { webSocket = (msock,wsGetter)} o
    }

foreign import javascript unsafe
  "try { $r = new window[\"WebSocket\"]($1, $2) } catch (e) { $r = null}"
    js_tryNewWebSocket :: JSText -> T.JSVal -> IO (DT.Nullable DT.WebSocket)

tryNewWebSocket :: (DT.ToJSString url, DT.ToJSString protocols)
                => url -> Maybe [protocols] -> IO (Maybe DT.WebSocket)
tryNewWebSocket url protocols = do
  ps <- M.toJSVal protocols
  mws <- js_tryNewWebSocket (DT.toJSString url) ps
  return $ DT.nullableToMaybe mws

initializeWS :: forall ms c.
                (Monad c, '[WebSocket] <: ms)
             => Code ms c ()
initializeWS = Send (InitializeWebSocket (Return ()))

initializeWSMsgHandlers :: (MonadIO c, '[WebSocket] <: ms)
                        => Code ms c ()
initializeWSMsgHandlers = do
  mhs <- liftIO $ newIORef Map.empty
  putWSMsgHandlers mhs

isWSReconnecting :: forall ms c.
                    (Monad c, '[WebSocket] <: ms)
                 => Code ms c Bool
isWSReconnecting = Send (GetWebSocketReconnecting Return)

setWSReconnecting :: forall ms c.
                     (Monad c, '[WebSocket] <: ms)
                  => Bool -> Code ms c ()
setWSReconnecting b = Send (SetWebSocketReconnecting b (Return ()))

getWS :: forall ms c.
         (Monad c, '[WebSocket] <: ms)
      => Code ms c (Maybe WS.WebSocket)
getWS = Send (GetWebSocket Return)

setWS :: forall ms c.
         (Monad c, '[WebSocket] <: ms)
      => Maybe DT.WebSocket -> Code ms c ()
setWS mws = Send (SetWebSocket mws (Return ()))

getWSState :: forall ms c.
              (Monad c, '[WebSocket] <: ms)
           => Code ms c WebSocketState
getWSState = Send (GetWebSocketState Return)

setWSState :: forall ms c.
              (Monad c, '[WebSocket] <: ms)
           => WebSocketState -> Code ms c ()
setWSState wss = Send (SetWebSocketState wss (Return ()))

getWSMsgHandlers :: forall ms c.
                    (Monad c, '[WebSocket] <: ms)
                 => Code ms c (IORef (Map.HashMap JSText (Network Nuclear)))
getWSMsgHandlers = Send (GetWebSocketMsgHandlers Return)

putWSMsgHandlers :: forall ms c.
                    (Monad c, '[WebSocket] <: ms)
                => IORef (Map.HashMap JSText (Network Nuclear))
                -> Code ms c ()
putWSMsgHandlers hm = Send (SetWebSocketMsgHandlers hm (Return ()))

wsSetup :: forall ms c.
           (MonadIO c, '[Revent,WebSocket] <: ms)
        => Code ms c ()
wsSetup = Send (InitializeWebSocket (Return ()))

wsInitialize :: forall ms c.
                (MonadIO c, '[Revent,WebSocket] <: ms)
             => Code ms c WebSocketState
wsInitialize = do
  wsSetup
  buf <- getReventBuffer
  wssn <- Send (GetWebSocketStates Return)
  p :: Periodical ms c WebSocketState <- periodical
  joinNetwork wssn p buf
  subscribe p $ \wss -> lift $ do
    setWSState wss
    case wss of
      WSClosed -> void $ do
        setWS Nothing
        reconnectOnInterval 500000
      _ -> return ()
  wsConnect

wsConnect :: forall ms c.
             (MonadIO c, '[Revent,WebSocket] <: ms)
          => Code ms c WebSocketState
wsConnect = do
  sig <- getReventBuffer
  Send (ConnectWebSocket sig (Return ()))
  getWSState

wsDisconnect :: forall ms c.
                (MonadIO c, '[WebSocket] <: ms)
             => Code ms c ()
wsDisconnect = do
  mws <- getWS
  case mws of
    Just ws -> liftIO (WS.close ws 1000 ("wsDisconnect called." :: String))
    Nothing -> return ()

send' :: forall ms c.
        (MonadIO c, '[Revent,WebSocket] <: ms)
     => Nuclear -> Code ms c (Either WSException SendStatus)
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
              let bs = toBS m
                  (sbi,_,_) = GB.fromByteString $ strictify bs
                  sabi = GB.getArrayBuffer sbi
#ifdef DEBUGWS
              liftIO $ putStrLn $ "send' sending: " ++ show bs
#endif
              liftIO $ WS.send ws $ Just (M.pFromJSVal (T.jsval sabi) :: DT.ArrayBuffer)
              return (Right Sent)
        _ -> do
          -- buffer the message for when the socket opens back up
          -- note that this could lead to excessive memory usage!
          onState $ \wss ->
            case wss of
              WSOpened -> do
                mws <- lift getWS
                case mws of
                  Nothing -> return () -- huh?
                  Just ws -> do
                    let bs = toBS m
                        (sbi,_,_) = GB.fromByteString $ strictify bs
                        sabi = GB.getArrayBuffer sbi
#ifdef DEBUGWS
                    liftIO $ putStrLn $ "send' sending after websocket state changed: " ++ show bs
#endif
                    liftIO $ WS.send ws $ Just (M.pFromJSVal (T.jsval sabi) :: DT.ArrayBuffer)
                -- stop when successful
                end
              -- If not opened, just wait for the next event.
              _ ->
                return ()
          return (Right BufferedSend)

data SendStatus = BufferedSend | Sent

trySend' :: forall ms c.
           (MonadIO c, '[WebSocket] <: ms)
        => Nuclear -> Code ms c (Either WebSocketState ())
trySend' m = do
  wss <- getWSState
  case wss of
    WSOpened -> do
      mws <- getWS
      case mws of
        Nothing -> return (Left WSClosed) -- not correct....
        Just ws -> do
          let (sbi,_,_) = GB.fromByteString $ strictify $ toBS m
              sabi = GB.getArrayBuffer sbi
#ifdef DEBUGWS
          liftIO $ putStrLn $ "trySend' sending: " ++ show (toText m)
#endif
          liftIO (Right <$> WS.send ws (Just (M.pFromJSVal (T.jsval sabi) :: DT.ArrayBuffer)))
    _ -> do
#ifdef DEBUGWS
      liftIO $ putStrLn $ "trySend' couldn't send: " ++ show (toText m)
#endif
      return $ Left wss

data WSException = InvalidSocketState deriving Show
instance Exception WSException

unsubscribe :: forall ms c.
               (MonadIO c, Functor (Messages ms))
            => Behavior ms c WME.MessageEvent -> Code ms c ()
unsubscribe = stop

onState :: forall ms c.
           (MonadIO c, '[Revent,WebSocket] <: ms)
        => (WebSocketState -> Code '[Event WebSocketState] (Code ms c) ())
        -> Code ms c ()
onState bhvr = do
  buf <- getReventBuffer
  p :: Periodical ms c WebSocketState <- periodical
  s <- subscribe p bhvr
  wsn <- Send (GetWebSocketStates Return)
  joinNetwork wsn p buf

onClosed :: forall ms c.
            (MonadIO c, '[Revent,WebSocket] <: ms)
         => (Code '[Event WebSocketState] (Code ms c) ()) -> Code ms c ()
onClosed bhvr =
  onState $ \wss ->
    case wss of
      WSClosed -> bhvr
      _ -> return ()

foreign import javascript unsafe
  "Math.floor((Math.random() * $1) + 1)" random :: Int -> Int

-- exponential backoff based on a minimum interval
reconnectOnInterval :: forall ms c port.
                       (MonadIO c, '[Revent,WebSocket] <: ms)
                    => Int -> Code ms c ()
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
        , '[Revent,WebSocket] <: ms'
        , With w (Code ms' c') IO
        , Functor (Messages ms)
        )
     => w
     -> JSText
     -> a
     -> Code ms c (Promise (Either WSException SendStatus))
send s h a = with s $ send' $ Nuclear h $ toJSON a

trySend :: ( ToJSON a
           , MonadIO c
           , MonadIO c'
           , '[WebSocket] <: ms'
           , With w (Code ms' c') IO
           , Functor (Messages ms)
           )
        => w
        -> JSText
        -> a
        -> Code ms c (Promise (Either WebSocketState ()))
trySend s h a = with s $ trySend' $ Nuclear h $ toJSON a


-- use Endpoint
sendSelfMessage :: ( MonadIO c
                   , MonadIO c'

                   , With w (Code ms' c') IO

                   , '[WebSocket] <: ms'

                   , '[Revent] <: ms

                   , Message mTy
                   , M mTy ~ msg
                   , ToJSON msg
                   )
                => w
                -> Proxy mTy
                -> msg
                -> Code ms c (Promise ())
sendSelfMessage s mty_proxy m =
  with s $ do
    mhs_ <- getWSMsgHandlers
    mhs <- liftIO $ readIORef mhs_
    let header = messageHeader mty_proxy
    case Map.lookup header mhs of
      Nothing -> return ()
      Just n -> syndicate n (Nuclear header $ toJSON m)

request :: ( MonadIO c

            , '[Revent,WebSocket] <: ms

            , Request rqTy

            , Req rqTy ~ request
            , ToJSON request
            , Indexed request
            , I request ~ rqI
            , ToText rqI

            , Rsp rqTy ~ rsp
            , FromJSON rsp
            )
         => Proxy rqTy
         -> request
         -> (Code ms c () -> Either Nuclear rsp -> Code '[Event Nuclear] (Code ms c) ())
         -> Code ms c (Endpoint ms c) -- (Subscription ms c Msg,Periodical ms c Msg)
request rqty_proxy req f = do
  buf <- getReventBuffer
  p   <- periodical
  s_ <- liftIO $ newIORef undefined
  newn <- network
  let header = responseHeader rqty_proxy req
      bhvr m =
        let done = do
              (su,n) <- liftIO $ readIORef s_
              stop su
              leaveNetwork n p
              nn <- nullNetwork n
              when nn $ do
                mhs_ <- getWSMsgHandlers
                liftIO $ modifyIORef mhs_ (Map.delete header)
        in f done (maybe (Left m) Right (decodeNuclear m))
  Just sb <- subscribe p bhvr
  mhs_ <- getWSMsgHandlers
  n <- liftIO $ atomicModifyIORef' mhs_ $ \mhs ->
          case Map.lookup header mhs of
            Nothing -> (Map.insert header newn mhs,newn)
            Just n -> (mhs,n)
  joinNetwork n p buf
  liftIO $ writeIORef s_ (sb,n)
  send' (Nuclear (requestHeader rqty_proxy) (toJSON req))
  return $ Endpoint header sb p

apiRequest :: ( MonadIO c

              , '[Revent,WebSocket] <: ms

              , Request rqTy

              , Req rqTy ~ request
              , ToJSON request
              , Indexed request
              , I request ~ rqI
              , ToText rqI

              , Rsp rqTy ~ rsp
              , FromJSON rsp

              , (rqTy ∈ rqs) ~ 'True
              )
          => FullAPI msgs rqs
          -> Proxy rqTy
          -> request
          -> (Code ms c () -> Either Nuclear rsp -> Code '[Event Nuclear] (Code ms c) ())
          -> Code ms c (Endpoint ms c) -- (Subscription ms c Msg,Periodical ms c Msg)
apiRequest _ rqty_proxy req f = do
  buf <- getReventBuffer
  p   <- periodical
  s_ <- liftIO $ newIORef undefined
  newn <- network
  let header = responseHeader rqty_proxy req
      bhvr m =
        let done = do
              (su,n) <- liftIO $ readIORef s_
              stop su
              leaveNetwork n p
              nn <- nullNetwork n
              when nn $ do
                mhs_ <- getWSMsgHandlers
                liftIO $ modifyIORef mhs_ (Map.delete header)
        in f done (maybe (Left m) Right (decodeNuclear m))
  Just sb <- subscribe p bhvr
  mhs_ <- getWSMsgHandlers
  n <- liftIO $ atomicModifyIORef' mhs_ $ \mhs ->
          case Map.lookup header mhs of
            Nothing -> (Map.insert header newn mhs,newn)
            Just n -> (mhs,n)
  joinNetwork n p buf
  liftIO $ writeIORef s_ (sb,n)
  send' (Nuclear (requestHeader rqty_proxy) (toJSON req))
  return $ Endpoint header sb p

requestWith :: ( MonadIO c
               , MonadIO c'

               , With w (Code ms' c') IO

               , '[Revent,WebSocket] <: ms'

               , '[Revent] <: ms

               , Request rqTy

               , Req rqTy ~ request
               , ToJSON request
               , Indexed request
               , I request ~ rqI
               , ToText rqI

               , Rsp rqTy ~ rsp
               , FromJSON rsp
               )
            => w
            -> Proxy rqTy
            -> request
            -> (Code ms c () -> Either Nuclear rsp -> Code '[Event Nuclear] (Code ms c) ())
            -> Code ms c (Endpoint ms c) -- (Subscription ms c Msg,Periodical ms c Msg)
requestWith s rqty_proxy req f = do
  buf <- getReventBuffer
  p   <- periodical
  s_ <- liftIO $ newIORef undefined
  newn <- network
  let header = responseHeader rqty_proxy req
      bhvr m =
        let done = do
              (su,n) <- liftIO $ readIORef s_
              stop su
              leaveNetwork n p
              nn <- nullNetwork n
              when nn $
                void $ with s $ do
                  nn <- nullNetwork n -- might have changed
                  when nn $ do
                    mhs_ <- getWSMsgHandlers
                    liftIO $ atomicModifyIORef' mhs_ $ \mhs -> (Map.delete header mhs,())
        in f done (maybe (Left m) Right (decodeNuclear m))
  Just sb <- subscribe p bhvr
  with s $ do
    mhs_ <- getWSMsgHandlers
    n <- liftIO $ atomicModifyIORef' mhs_ $ \mhs ->
           case Map.lookup header mhs of
              Nothing -> (Map.insert header newn mhs,newn)
              Just n -> (mhs,n)
    joinNetwork n p buf
    liftIO $ writeIORef s_ (sb,n)
    send' (Nuclear (requestHeader rqty_proxy) (toJSON req))
  return $ Endpoint header sb p

apiRequestWith :: ( MonadIO c
                  , MonadIO c'

                  , With w (Code ms' c') IO

                  , '[Revent,WebSocket] <: ms'

                  , '[Revent] <: ms

                  , Request rqTy

                  , Req rqTy ~ request
                  , ToJSON request
                  , Indexed request
                  , I request ~ rqI
                  , ToText rqI

                  , Rsp rqTy ~ rsp
                  , FromJSON rsp

                  , (rqTy ∈ rqs) ~ 'True
                  )
               => FullAPI msgs rqs
               -> w
               -> Proxy rqTy
               -> request
               -> (Code ms c () -> Either Nuclear rsp -> Code '[Event Nuclear] (Code ms c) ())
               -> Code ms c (Endpoint ms c) -- (Subscription ms c Msg,Periodical ms c Msg)
apiRequestWith _ s rqty_proxy req f = do
  buf <- getReventBuffer
  p   <- periodical
  s_ <- liftIO $ newIORef undefined
  newn <- network
  let header = responseHeader rqty_proxy req
      bhvr m =
        let done = do
              (su,n) <- liftIO $ readIORef s_
              stop su
              leaveNetwork n p
              nn <- nullNetwork n
              when nn $
                void $ with s $ do
                  nn <- nullNetwork n -- might have changed
                  when nn $ do
                    mhs_ <- getWSMsgHandlers
                    liftIO $ atomicModifyIORef' mhs_ $ \mhs -> (Map.delete header mhs,())
        in f done (maybe (Left m) Right (decodeNuclear m))
  Just sb <- subscribe p bhvr
  with s $ do
    mhs_ <- getWSMsgHandlers
    n <- liftIO $ atomicModifyIORef' mhs_ $ \mhs ->
           case Map.lookup header mhs of
              Nothing -> (Map.insert header newn mhs,newn)
              Just n -> (mhs,n)
    joinNetwork n p buf
    liftIO $ writeIORef s_ (sb,n)
    send' (Nuclear (requestHeader rqty_proxy) (toJSON req))
  return $ Endpoint header sb p

respondWith :: ( MonadIO c
               , MonadIO c'

               , With w (Code ms' c') IO

               , '[Revent] <: ms

               , '[Revent,WebSocket] <: ms'

               , Request rqTy

               , Req rqTy ~ request
               , Indexed request
               , I request ~ rqI
               , FromJSON request
               , ToText rqI

               , Rsp rqTy ~ rsp
               , ToJSON rsp
               )
            => w
            -> Proxy rqTy
            -> (Code ms c () -> Either Nuclear (rsp -> Code ms c (Promise ()),request) -> Code '[Event Nuclear] (Code ms c) ())
            -> Code ms c (Endpoint ms c)
respondWith s rqty_proxy rr = do
  buf <- getReventBuffer
  p <- periodical
  newn <- network
  s_ <- liftIO $ newIORef undefined
  let header = requestHeader rqty_proxy
  let bhvr m =
        let done = do
              (n,su) <- liftIO $ readIORef s_
              stop su
              leaveNetwork n p
              nn <- nullNetwork n
              when nn $
                void $ with s $ do
                  mhs_ <- getWSMsgHandlers
                  liftIO $ atomicModifyIORef' mhs_ $ \old_mhs ->
                    let !new_mhs = Map.delete header old_mhs
                    in (new_mhs,())
        in rr done (maybe (Left m) (\rq -> Right (with s . void . send' . Nuclear (responseHeader rqty_proxy rq) . toJSON,rq)) (decodeNuclear m))
  Just sb <- subscribe p bhvr
  with s $ do
    mhs_ <- getWSMsgHandlers
    n <- liftIO $ atomicModifyIORef' mhs_ $ \mhs ->
           case Map.lookup header mhs of
              Nothing -> (Map.insert header newn mhs,newn)
              Just n -> (mhs,n)
    joinNetwork n p buf
    liftIO $ writeIORef s_ (n,sb)
  return $ Endpoint header sb p

respond :: ( MonadIO c

           , '[Revent,WebSocket] <: ms

           , Request rqTy

           , Req rqTy ~ request
           , Indexed request
           , I request ~ rqI
           , FromJSON request
           , ToText rqI
 
           , Rsp rqTy ~ rsp
           , ToJSON rsp
           )
        => Proxy rqTy
        -> (Code ms c () -> Either Nuclear (rsp -> Code ms c (),request) -> Code '[Event Nuclear] (Code ms c) ())
        -> Code ms c (Endpoint ms c)
respond rqty_proxy rr = do
  buf <- getReventBuffer
  p <- periodical
  newn <- network
  s_ <- liftIO $ newIORef undefined
  let header = requestHeader rqty_proxy
  let bhvr m =
        let done = do
              (n,su) <- liftIO $ readIORef s_
              stop su
              leaveNetwork n p
              nn <- nullNetwork n
              when nn $ do
                mhs_ <- getWSMsgHandlers
                liftIO $ atomicModifyIORef' mhs_ $ \old_mhs ->
                  let !new_mhs = Map.delete header old_mhs
                  in (new_mhs,())
        in rr done (maybe (Left m) (\rq -> Right (void . send' . Nuclear (responseHeader rqty_proxy rq) . toJSON,rq)) (decodeNuclear m))
  Just sb <- subscribe p bhvr
  mhs_ <- getWSMsgHandlers
  n <- liftIO $ atomicModifyIORef' mhs_ $ \mhs ->
          case Map.lookup header mhs of
            Nothing -> (Map.insert header newn mhs,newn)
            Just n -> (mhs,n)
  liftIO $ writeIORef s_ (n,sb)
  joinNetwork n p buf
  return $ Endpoint header sb p

apiMessageWith :: ( MonadIO c
                  , MonadIO c'
                  , '[Revent,WebSocket] <: ms'
                  , With w (Code ms' c') IO
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
                -> Code ms c (Promise (Either WSException SendStatus))
apiMessageWith _ s mty_proxy m = messageWith s mty_proxy m

messageWith :: ( MonadIO c
               , MonadIO c'
               , '[Revent,WebSocket] <: ms'
               , With w (Code ms' c') IO
               , Message mTy
               , M mTy ~ msg
               , ToJSON msg
               , Functor (Messages ms)
               )
            => w
            -> Proxy mTy
            -> msg
            -> Code ms c (Promise (Either WSException SendStatus))
messageWith s mty_proxy m = send s (messageHeader mty_proxy) m

apiMessage :: ( MonadIO c
              , '[Revent,WebSocket] <: ms
              , Message mTy
              , M mTy ~ msg
              , ToJSON msg
              , (mTy ∈ msgs) ~ 'True
              )
            => FullAPI msgs rqs
            -> Proxy mTy
            -> msg
            -> Code ms c (Either WSException SendStatus)
apiMessage _ mty_proxy m = send' (Nuclear (messageHeader mty_proxy) (toJSON m))

message :: ( MonadIO c
           , '[Revent,WebSocket] <: ms
           , Message mTy
           , M mTy ~ msg
           , ToJSON msg
           )
        => Proxy mTy
        -> msg
        -> Code ms c (Either WSException SendStatus)
message mty_proxy m = send' (Nuclear (messageHeader mty_proxy) (toJSON m))

onMessageWith :: ( MonadIO c
                 , MonadIO c'
                 , '[WebSocket] <: ms'
                 , '[Revent] <: ms
                 , With w (Code ms' c') IO
                 , Message mTy
                 , M mTy ~ msg
                 , FromJSON msg
                 )
               => w
               -> Proxy mTy
               -> (Code ms c () -> Either Nuclear msg -> Code '[Event Nuclear] (Code ms c) ())
               -> Code ms c (Endpoint ms c) -- (Subscription ms c Msg,Periodical ms c Msg)
onMessageWith s mty_proxy f = do
  buf <- getReventBuffer
  s_ <- liftIO $ newIORef undefined
  p <- periodical
  let header = messageHeader mty_proxy
  let bhvr m =
        let done = do
              (n,su) <- liftIO $ readIORef s_
              stop su
              leaveNetwork n p
              nn <- nullNetwork n
              when nn $
                void $ with s $ do
                  mhs_ <- getWSMsgHandlers
                  liftIO $ atomicModifyIORef' mhs_ $ \old_mhs ->
                    let !new_mhs = Map.delete header old_mhs
                    in (new_mhs,())
        in f done (maybe (Left m) Right (decodeNuclear m))
  newn <- network
  Just sb <- subscribe p bhvr
  with s $ do
    mhs_ <- getWSMsgHandlers
    n <- liftIO $ atomicModifyIORef' mhs_ $ \mhs ->
            case Map.lookup header mhs of
              Nothing -> (Map.insert header newn mhs,newn)
              Just n -> (mhs,n)
    joinNetwork n p buf
    liftIO $ writeIORef s_ (n,sb)
  return $ Endpoint header sb p

onMessage :: ( MonadIO c
             , '[Revent,WebSocket] <: ms
             , Message mTy
             , M mTy ~ msg
             , FromJSON msg
             )
          => Proxy mTy
          -> (Code ms c () -> Either Nuclear msg -> Code '[Event Nuclear] (Code ms c) ())
          -> Code ms c (Endpoint ms c) -- (Subscription ms c Msg,Periodical ms c Msg)
onMessage mty_proxy f = do
  buf <- getReventBuffer
  s_ <- liftIO $ newIORef undefined
  p <- periodical
  let header = messageHeader mty_proxy
  let bhvr m =
        let done = do
              (n,su) <- liftIO $ readIORef s_
              stop su
              leaveNetwork n p
              nn <- nullNetwork n
              when nn $ do
                mhs_ <- getWSMsgHandlers
                liftIO $ atomicModifyIORef' mhs_ $ \old_mhs ->
                  let !new_mhs = Map.delete header old_mhs
                  in (new_mhs,())
        in f done (maybe (Left m) Right (decodeNuclear m))
  newn <- network
  Just sb <- subscribe p bhvr
  mhs_ <- getWSMsgHandlers
  n <- liftIO $ atomicModifyIORef' mhs_ $ \mhs ->
          case Map.lookup header mhs of
            Nothing -> (Map.insert header newn mhs,newn)
            Just n -> (mhs,n)
  liftIO $ writeIORef s_ (n,sb)
  joinNetwork n p buf
  return $ Endpoint header sb p
