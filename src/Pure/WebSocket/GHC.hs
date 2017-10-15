{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveAnyClass #-}
module Pure.WebSocket.GHC (module Pure.WebSocket.GHC, S.SockAddr) where

import Ef.Base
import Data.Queue

import Pure.Data
import Pure.Data.JSON as AE

import Pure.WebSocket.API
import Pure.WebSocket.Dispatch
import Pure.WebSocket.Endpoint
import Pure.WebSocket.Message
import Pure.WebSocket.TypeRep
import Pure.WebSocket.Request

import GHC.Generics

import Control.Concurrent
import Control.Exception as E
import Data.Monoid
import Text.Read hiding (get,lift)

import qualified Network.Socket as S

import           Network.WebSockets.Stream     (Stream)
import qualified Network.WebSockets.Stream     as Stream
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WS

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as BLC
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Internal as BL

#ifdef SECURE
import OpenSSL as SSL
import OpenSSL.Session as SSL
import qualified System.IO.Streams.SSL as Streams
#endif
import qualified System.IO.Streams as Streams
import qualified System.IO.Streams.Internal as Streams

import Data.IORef
import Data.Int
import Data.Maybe
import Data.Ratio
import System.IO
import System.Random

import qualified Data.ByteString        as S
import qualified Data.ByteString.Internal as S
import qualified Data.ByteString.Unsafe as S

import qualified Data.HashMap.Strict as Map

import System.IO.Unsafe
import Unsafe.Coerce

import Data.ByteString.Lazy as BSL hiding (putStrLn)

-- import Control.Lens as L

type LazyByteString = BSL.ByteString

-- This code is hideous, but there is reason in the chaos:
--   1. there is a split between client and server to support fission-based desktop clients as well as servers.
--   2. there is a split between secure (SSL) and insecure websockets.
--   3. there is a split between streaming and non-streaming send because the current websocket library does not support streaming.
-- This interface is very generic and thus the type signatures are somewhat daunting. If you understand what Dispatch implements,
-- what With implements, what Eventer implements, and what Evented implements (and thus how Services, Components, Clients and Servers
-- work), you'll see how this fits together.

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

data WSStatus
  = WSUnopened
  | WSClosed WSCloseReason
  | WSOpened
  | WSConnecting
  deriving (Eq,Show)

data WebSocket
  = WebSocket
    { wsSocket           :: !(Maybe (S.SockAddr,S.Socket,WS.Connection,WS.Stream))
    , wsReceiveThread    :: !(Maybe ThreadId)
    , wsMessageHandlers  :: !(IORef (Map.HashMap Txt (Syndicate Dispatch)))
    , wsStatus           :: !WSStatus
    , wsStatusSyndicate    :: !(Syndicate WSStatus)
    , wsThroughputLimits :: !(Throughput,ThroughputLimits)
    , wsBytesReadRef     :: !(IORef (Int64,Int64))
    }

type WS = State () WebSocket

getWSMsgHandlers :: (ms <: '[State () WebSocket], Monad c)
                 => Ef ms c (IORef (Map.HashMap Txt (Syndicate Dispatch)))
getWSMsgHandlers = do
  WebSocket {..} <- get
  return wsMessageHandlers

data ThroughputLimits
  = ThroughputLimits
    -- second limits burst
    -- minute limits throughput
    { msgsPerSecond :: IORef Int
    , msgsPerMinute :: IORef Int
    }

data Throughput
  = Throughput
    { messagesPerSecond :: Int
    , messagesPerMinute :: Int
    }

throughputToThroughputLimits :: (MonadIO c) => Throughput -> c ThroughputLimits
throughputToThroughputLimits Throughput {..} = liftIO $
  ThroughputLimits <$> newIORef messagesPerSecond <*> newIORef messagesPerMinute

unlimited :: Throughput
unlimited = Throughput 100000000 1000000000

-- defaultServerThroughput :: Throughput
-- defaultServerThroughput = Throughput 11 101

-- defaultClientThroughput :: Throughput
-- defaultClientThroughput = Throughput 101 1001

data RequestHandler ms c rqTy
  where
    RequestHandler
      :: ( MonadIO c
         , ms <: '[Evented,State () WebSocket]

         , Request rqTy

         , Req rqTy ~ request
         , Identify request
         , I request ~ rqI
         , FromJSON request

         , Rsp rqTy ~ response
         , ToJSON response
         )
      => Proxy rqTy
      -> (Ef ms c ()
           -> Either Dispatch (Either LazyByteString response -> Ef ms c (Either WSStatus ()),request)
           -> Ef '[Event Dispatch] (Ef ms c) ()
         )
      -> RequestHandler ms c rqTy

responds :: ( MonadIO c
            , ms <: '[Evented,State () WebSocket]

            , Request rqTy

            , Req rqTy ~ request
            , Identify request
            , I request ~ rqI
            , FromJSON request

            , Rsp rqTy ~ response
            , ToJSON response
            )
         => Proxy rqTy
         -> (Ef ms c ()
              -> Either Dispatch (Either LazyByteString response -> Ef ms c (Either WSStatus ()),request)
              -> Ef '[Event Dispatch] (Ef ms c) ()
            )
         -> RequestHandler ms c rqTy
responds = RequestHandler

type ReqHandlers ms c rqs = Endpoints RequestHandler ms c rqs

data MessageHandler ms c mTy
  where
    MessageHandler
      :: ( MonadIO c
         , ms <: '[Evented,State () WebSocket]
         , Message mTy
         , M mTy ~ msg
         , ToJSON msg
         )
      => Proxy mTy
      -> (Ef ms c () -> Either Dispatch msg -> Ef '[Event Dispatch] (Ef ms c) ())
      -> MessageHandler ms c mTy

accepts :: ( MonadIO c
           , ms <: '[Evented,State () WebSocket]
           , Message mTy
           , M mTy ~ msg
           , ToJSON msg
           )
        => Proxy mTy
        -> (Ef ms c () -> Either Dispatch msg -> Ef '[Event Dispatch] (Ef ms c) ())
        -> MessageHandler ms c mTy
accepts = MessageHandler

type MsgHandlers ms c msgs = Endpoints MessageHandler ms c msgs

instance ( GetHandler MessageHandler message msgs'
         , Removed msgs' message ~ msgs''
         , DeleteHandler MessageHandler message msgs' msgs''
         , EnactEndpoints MessageAPI MessageHandler ms c msgs msgs''
         , Message message
         , M message ~ msg
         , ToJSON msg
         , FromJSON msg
         , MonadIO c
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
         , Removed rqs' request ~ rqs''
         , DeleteHandler RequestHandler request rqs' rqs''
         , EnactEndpoints RequestAPI RequestHandler ms c rqs rqs''
         , Request request
         , Req request ~ req
         , Rsp request ~ response
         , ToJSON req
         , FromJSON response
         )
  => EnactEndpoints RequestAPI RequestHandler ms c (request ': rqs) rqs' where
  enactEndpoints (APICons pm ms) mhs =
    case getHandler mhs :: RequestHandler ms c request of
      RequestHandler _ f -> do
        let p = Proxy :: Proxy request
            mhs' = deleteHandler p mhs :: ReqHandlers ms c rqs''
        amh <- respond p ((unsafeCoerce f) :: Ef ms c () -> Either Dispatch (Either LazyByteString response -> Ef ms c (Either WSStatus ()),req) -> Ef '[Event Dispatch] (Ef ms c) ())
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

enact :: (MonadIO c, Functor (Messages ms))
      => Implementation ms c msgs rqs msgs' rqs'
      -> Ef ms c (ActiveAPI ms c msgs rqs)
enact (Impl local mhs rhs) = do
  let API mapi rapi = local
  amapi <- enactEndpoints mapi mhs
  arapi <- enactEndpoints rapi rhs
  let active = ActiveAPI amapi arapi
  return active

cleanupEndpoints :: (MonadIO c, ms <: '[State () WebSocket])
                 => Ef ms c ()
cleanupEndpoints = do
  WebSocket {..} <- get
  mh <- liftIO $ readIORef wsMessageHandlers
  let mhl = Map.toList mh
  mmhl' <- forM mhl $ \(h,n) -> do
             nn <- nullSyndicate n
             if nn then
               return Nothing
             else
               return $ Just (h,n)
  let mhl' = catMaybes mmhl'
      mh' = Map.fromList mhl'
  liftIO $ writeIORef wsMessageHandlers mh'

-- takes effect after the first message has been received after the current sixty
-- second window has elapsed.
setMsgsPerSecond n = do
  ws@WebSocket {..} <- get
  liftIO $ writeIORef (msgsPerSecond $ snd wsThroughputLimits) n
  put ws { wsThroughputLimits = ((fst wsThroughputLimits) { messagesPerSecond = n},snd wsThroughputLimits) }

-- takes effect after the first message has been received after the current one
-- second window has elapsed. Note that a value of
-- > msgsPerMinute > 60 * msgsPerSecond
-- will have no effect.
setMsgsPerMinute n = do
  ws@WebSocket {..} <- get
  liftIO $ writeIORef (msgsPerMinute $ snd wsThroughputLimits) n
  put ws { wsThroughputLimits = ((fst wsThroughputLimits) { messagesPerMinute = n},snd wsThroughputLimits) }

onWSStatus :: (ms <: '[State () WebSocket, Evented],MonadIO c)
        => (WSStatus -> Ef '[Event WSStatus] (Ef ms c) ())
        -> Ef ms c (IO ())
onWSStatus f = do
  WebSocket {..} <- get
  Ef.Base.connect wsStatusSyndicate f

onWSClose :: (ms <: '[State () WebSocket, Evented],MonadIO c)
        => (WSCloseReason -> Ef '[Event WSStatus] (Ef ms c) ())
        -> Ef ms c (IO ())
onWSClose f = onWSStatus (\wss -> case wss of { WSClosed wscr -> f wscr; _ -> return () } )

getWSStatus :: (ms <: '[State () WebSocket], Monad c)
            => Ef ms c WSStatus
getWSStatus = do
  WebSocket {..} <- get
  return wsStatus

setWSStatus :: (ms <: '[State () WebSocket], MonadIO c)
            => WSStatus -> Ef ms c ()
setWSStatus wss = do
  ws <- get
  put ws { wsStatus = wss }
  publish (wsStatusSyndicate ws) wss

getWSInfo :: (ms <: '[State () WebSocket], MonadIO c)
          => Ef ms c (Maybe (S.SockAddr,S.Socket))
getWSInfo = do
  ws <- get
  case wsSocket ws of
    Just (sa,s,_,_) -> return (Just (sa,s))
    _ -> return Nothing

onWSCloseSimplyShutdown :: forall ms c.
                         (ms <: '[State () WebSocket, Evented, State () Shutdown], MonadIO c)
                      => Ef ms c (IO ())
onWSCloseSimplyShutdown = do
  onWSClose $ \closeReason -> lift $ do
    buf <- get
    Shutdown sdn <- get
    publish sdn ()
    (rnr :: Signal ms c (Ef ms c ()),_) <- runner
    buffer buf rnr $ liftIO $ do
      killBuffer buf
      tid <- myThreadId
      killThread tid

-- create a websocket without initializing it; this is used by clients and
-- services for optional and auxiliary connections.
websocket :: forall c. (MonadIO c) => Throughput -> c WebSocket
websocket tp = liftIO $ do
  tpl <- throughputToThroughputLimits tp
  mhs <- newIORef (Map.empty :: Map.HashMap Txt (Syndicate Dispatch))
  wssn <- syndicate
  brr <- newIORef (2 * 1024 * 1024,2 * 1024 * 1024) -- 2MiB
  return $ WebSocket
    { wsSocket           = Nothing
    , wsReceiveThread    = Nothing
    , wsMessageHandlers  = unsafeCoerce mhs
    , wsStatus           = WSUnopened
    , wsStatusSyndicate    = wssn
    , wsThroughputLimits = (tp,tpl)
    , wsBytesReadRef     = brr
    }

-- create and initialize a non-secured websocket from the given connection; this
-- should be the go-to for servers. When secure websockets are needed, I suggest
-- terminating SSL ahead of the server; AWS ELB can be configured for this
-- purpose, for instance.
serverWS :: forall ts ms c c'.
            ( MonadIO c'
            , MonadIO c
            , ts <. '[State () WebSocket]
            , ms <: '[State () WebSocket]
            , ts <=> ms
            )
         => EvQueue
         -> S.Socket
         -> Throughput
         -> c' (State () WebSocket (Action ts c))
serverWS q sock tp = liftIO $ do
  sa <- liftIO $ S.getSocketName sock
  tpl <- throughputToThroughputLimits tp
  mhs <- newIORef (Map.empty :: Map.HashMap Txt (Syndicate Dispatch))
  wssn <- syndicate
  brr <- newIORef (2 * 1024 * 1024,2 * 1024 * 1024)

  streams <- Streams.socketToStreams sock

  wsStream <- liftIO $ makeExhaustible brr wssn sock streams

  pc <- WS.makePendingConnectionFromStream
          wsStream
          WS.defaultConnectionOptions

  c <- WS.acceptRequest pc

  (rnr :: Signal ms c (Ef ms c ()),_) <- runner

  rt <- forkIO $ receiveLoop sock tpl mhs brr q c rnr

  return $ state $ WebSocket
    { wsSocket = Just (sa,sock,c,wsStream)
    , wsReceiveThread = Just rt
    , wsMessageHandlers = unsafeCoerce mhs
    , wsStatus = WSOpened
    , wsStatusSyndicate = wssn
    , wsThroughputLimits = (tp,tpl)
    , wsBytesReadRef = brr
    }

-- initialize an unconnected websocket created with 'websocket' in a narrative
-- context. This is used for client websockets or service websockets that may be
-- conditionally connected or connected on some delay.
initializeClientWS :: forall ms c ts.
                      ( MonadIO c
                      , ms <: '[Evented,State () WebSocket]
                      )
                   => String
                   -> Int
                   -> String
                   -> Ef ms c ()
initializeClientWS host port path = do

  wsstatus <- getWSStatus
  case wsstatus of
    WSConnecting -> return ()
    WSOpened     -> return ()
    _            -> do
      setWSStatus WSConnecting
      connectWithExponentialBackoff 0

  where
    connectWithExponentialBackoff n = do

      -- liftIO $ Prelude.putStrLn "Attempting connect."

      let interval = 50000 -- 10 times fater than fusion; 1/20th second min. interval

      msock <- liftIO $ newClientSocket host port
      case msock of
        Nothing -> do
          gen <- liftIO newStdGen
          let (r,_) = randomR (1,2 ^ n - 1) gen
              i = interval * r
          void $ delay i $ connectWithExponentialBackoff (min (n + 1) 12) -- ~ 200 second max interval; average max interval 100 seconds
        Just sock -> do
          sa <- liftIO $ S.getPeerName sock
          q <- get
          ws@WebSocket {..} <- get

          streams <- liftIO $ Streams.socketToStreams sock

          wsStream <- liftIO $ makeExhaustible wsBytesReadRef wsStatusSyndicate sock streams

          c <- liftIO $
            WS.runClientWithStream
              wsStream
              host
              path
              WS.defaultConnectionOptions
              []
              return

          (rnr :: Signal ms c (Ef ms c ()),_) <- runner

          rt <- liftIO $ forkIO $ receiveLoop sock (snd wsThroughputLimits) wsMessageHandlers wsBytesReadRef q c rnr

          put WebSocket
                { wsSocket = Just (sa,sock,c,wsStream)
                , wsReceiveThread = Just rt
                , .. }
          setWSStatus WSOpened

#ifdef SECURE
serverWSS :: forall ts ms c c'.
             ( MonadIO c'
             , MonadIO c
             , ts <. '[State () WebSocket]
             , ms <: '[State () WebSocket]
             , ts <=> ms
             )
          => EvQueue
          -> S.Socket
          -> SSL
          -> Throughput
          -> c' (State () WebSocket (Action ts c))
serverWSS q sock ssl tp = liftIO $ do
  sa <- liftIO $ S.getSocketName sock
  tpl <- throughputToThroughputLimits tp
  mhs <- newIORef (Map.empty :: Map.HashMap Txt (Syndicate Dispatch))
  wssn <- syndicate
  brr <- newIORef (2 * 1024 * 1024,2 * 1024 * 1024)

  streams <- Streams.sslToStreams ssl

  wsStream <- makeExhaustible brr wssn sock streams

  pc <- WS.makePendingConnectionFromStream
          wsStream
          WS.defaultConnectionOptions

  c <- WS.acceptRequest pc

  rnr :: Signal ms c (Ef ms c ()) <- runner

  rt <- forkIO $ receiveLoop sock tpl mhs brr q c rnr

  return $ state $ WebSocket
    { wsSocket = Just (sa,sock,c,wsStream)
    , wsReceiveThread = Just rt
    , wsMessageHandlers = unsafeCoerce mhs
    , wsStatus          = WSOpened
    , wsStatusSyndicate = wssn
    , wsBytesReadRef = brr
    , wsThroughputLimits = (tp,tpl)
    }

initializeClientWSS :: forall ms c ts.
                       ( MonadIO c
                       , ms <: '[Evented,State () WebSocket]
                       )
                    => String
                    -> Int
                    -> String
                    -> Ef ms c ()
initializeClientWSS host port path = do

  wsstatus <- getWSStatus
  case wsstatus of
    WSConnecting -> return ()
    WSOpened     -> return ()
    _            -> do
      setWSStatus WSConnecting
      connectWithExponentialBackoff 0

  where
    connectWithExponentialBackoff n = do

      let interval = 50000 -- 10 times fater than fusion; 1/20th second min. interval

      msock <- liftIO $ newClientSocket host port
      case msock of
        Nothing -> do
          gen <- liftIO newStdGen
          let (r,_) = randomR (1,2 ^ n - 1) gen
              i = interval * r
          void $ delay i $ connectWithExponentialBackoff (min (n + 1) 12) -- ~ 200 second max interval; average max interval 100 seconds
        Just sock -> do
          sa <- liftIO $ S.getPeerName sock
          q <- get
          ws@WebSocket {..} <- get

          -- following two lines differ from non-ssl
          ssl <- liftIO $ sslConnect sock

          streams <- liftIO $ Streams.sslToStreams ssl

          wsStream <- liftIO $ makeExhaustible wsBytesReadRef wsStatusSyndicate sock streams

          c <- liftIO $
            WS.runClientWithStream
              wsStream
              host
              path
              WS.defaultConnectionOptions
              []
              return

          rnr :: Signal ms c (Ef ms c ()) <- runner

          rt <- liftIO $ forkIO $ receiveLoop sock (snd wsThroughputLimits) wsMessageHandlers wsBytesReadRef q c rnr

          put WebSocket
                { wsSocket = Just (sa,sock,c,wsStream)
                , wsReceiveThread = Just rt
                , .. }
          setWSStatus WSOpened
#endif

wsClose :: forall ms c.
           (MonadIO c, ms <: '[State () WebSocket])
        => WSCloseReason -> Ef ms c ()
wsClose wscr = do
  -- liftIO $ putStrLn $ "Sending wsClose because: " ++ show wscr
  ws@WebSocket {..} <- get
  forM_ wsSocket $ \(sa,sock,c,_) -> do
    liftIO $ E.handle (\(e :: SomeException) -> return ()) $
      WS.sendClose c (BLC.pack "closed")
    liftIO $ E.handle (\(e :: SomeException) -> return ()) $
      S.sClose sock
    put ws { wsSocket = Nothing }
  setWSStatus (WSClosed wscr)

data WSException
  = Closed
  | BadMessage String
  | CouldNotEstablishClientWS
  | NoMsgsignal
  deriving (Show)
instance Exception WSException

-- timeInMicros :: (MonadIO c)
--              => c Integer
-- timeInMicros =
--   micros <$> (liftIO getPOSIXTime)
--   where
--     micros = numerator
--            . toRational
--            . (* 1000000)

receiveLoop sock ThroughputLimits {..} mhs_ brr_ q c rnr = do
  buffer <- newIORef mempty
  now <- timeInMicros
  mps <- readIORef msgsPerSecond
  mpm <- readIORef msgsPerMinute
  go now now mps mpm buffer
  where
    go sec minu mps mpm buf = do
      eem <- receiveIO c
#if defined(DEBUGWS) || defined(DEVEL)
      Prelude.putStrLn $ "Received websocket message: " ++ show eem
#endif
      now <- timeInMicros
      let mps' = pred mps

          mpm' = pred mpm

          continue =
            if now > sec then do
              new_mps <- readIORef msgsPerSecond
              if now > minu then do
                new_mpm <- readIORef msgsPerMinute
                go (now + 1000000) (now + 60000000) new_mps new_mpm buf
              else
                go (now + 1000000) minu new_mps mpm' buf
            else
              go sec minu mps' mpm' buf

          checkThroughput bad good =
            if mps' < 1 || mpm' < 1 then do
              bad
              mps_allowed <- readIORef msgsPerSecond
              mpm_allowed <- readIORef msgsPerMinute
              -- liftIO $ putStrLn "Buffering wsClose because mps' < 1 || mpm' < 1"
              buffer q rnr (wsClose (MessageThroughputLimitExceeded mps' mps_allowed mpm' mpm_allowed))
            else do
              good
              continue

          simpleCheckThroughput =
            checkThroughput (return ()) (return ())

          resetSyndicateAndCheckThroughput m@(Dispatch h _) = do
            resetBytesReadRef brr_
            mhs <- liftIO $ readIORef mhs_
            case Map.lookup h mhs of
              Nothing -> do
#if defined(DEBUGWS) || defined(DEVEL)
                Prelude.putStrLn $ "Unhandled message: " ++ show (encode m)
#endif
                return ()
              Just mnw -> do
#if defined(DEBUGWS) || defined(DEVEL)
                Prelude.putStrLn $ "Dispatching message: " ++ show (encode m)
#endif
                publish mnw m
            simpleCheckThroughput

      case eem of

        Right str -> do
          case BLC.uncons str of

            Just ('C',rest) ->
              checkThroughput
                (writeIORef buf mempty)
                (modifyIORef buf (<> rest))

            Just ('F',rest) -> do
              beg <- readIORef buf
              let msg = beg <> rest
              writeIORef buf mempty
              case fromBS msg of

                Left _ -> do
                  buffer q rnr (wsClose (BadMessageReceived $ toTxt str))

                Right m ->
                  resetSyndicateAndCheckThroughput m

            Just ('{',_) -> do
              writeIORef buf mempty -- just in case
              case fromBS str of

                Left _ ->
                  -- Simple generic protection against malicious msgs.
                  -- How often will this kill a valid connection?
                  buffer q rnr (wsClose (BadMessageReceived $ toTxt str))

                Right !(m :: Dispatch) ->
                  resetSyndicateAndCheckThroughput m

            _ ->
              buffer q rnr (wsClose (BadMessageReceived $ toTxt str))

        Left Closed -> do
#if defined(DEBUGWS) || defined(DEVEL)
          Prelude.putStrLn "Websocket is closed; receiveloop failed."
#endif
          buffer q rnr (wsClose ClientClosedConnection)

        x -> do
#if defined(DEBUGWS) || defined(DEVEL)
          Prelude.putStrLn $ "receiveloop websocket exception: " ++ show x
#endif
          writeIORef buf mempty -- just in case
          continue

resetBytesReadRef brr_ = atomicModifyIORef' brr_ $ \(_,tot) -> ((tot,tot),())

receiveIO c = do
  eem <- E.handle (\(e :: SomeException) -> do
                      -- liftIO (print e)
                      return (Left Closed)
                  ) $
           Right <$> WS.receiveDataMessage c
  case eem of
    Left e -> return (Left Closed)

    -- Right (WS.ControlMessage (WS.Close _ _)) -> return (Left Closed)

    -- is this right? All data is expected utf-8 encoded, so....
    -- I don't have the time to test with third-party libraries, but
    -- it seems to work with fusion and fission....
    -- I believe fusion is receiving text msgs but sending
    -- binary msgs for performance reasons.
    Right (WS.Binary b) -> return $ Right b
    Right (WS.Text t) -> return $ Right t

newListenSocket :: (MonadIO c)
                => String -> Int -> c S.Socket
newListenSocket hn p = liftIO $ WS.makeListenSocket hn p

makeExhaustible :: IORef (Int64,Int64)
                -> Syndicate WSStatus
                -> S.Socket
                -> (Streams.InputStream B.ByteString,Streams.OutputStream B.ByteString)
                -> IO WS.Stream
makeExhaustible readCount wssn sock (i,o) = do
  i' <- limit i
  stream <- WS.makeStream
    (Streams.read i')
    (\b -> Streams.write (BL.toStrict <$> b) o)
  return stream
  where

    {-# INLINE limit #-}
    limit i = return $! Streams.InputStream prod pb
      where

        {-# INLINE prod #-}
        prod = (>>=) (Streams.read i) $ maybe (return Nothing) $ \x -> do
          (count,kill) <- atomicModifyIORef' readCount $ \(remaining,total) ->
            let !remaining' = remaining - fromIntegral (B.length x)
                !res = if remaining' < 0 then ((0,total),(total,True)) else ((remaining',total),(total,False))
            in res
          when kill $ do
            S.sClose sock
            publish wssn (WSClosed (MessageLengthLimitExceeded count))
          return (Just x)

        {-# INLINE pb #-}
        pb s = do
          atomicModifyIORef' readCount $ \(remaining,total) ->
            let !remaining' = remaining + fromIntegral (B.length s)
            in ((remaining',total),())
          Streams.unRead s i

#ifdef SECURE
sslSetupServer keyFile certFile mayChainFile = do
  ctx <- SSL.context
  SSL.contextSetPrivateKeyFile ctx keyFile
  SSL.contextSetCertificateFile ctx certFile
  forM_ mayChainFile (SSL.contextSetCertificateChainFile ctx)
  SSL.contextSetCiphers ctx "HIGH"
  SSL.contextSetVerificationMode ctx VerifyNone -- VerifyPeer?
  return ctx

sslAccept conn = liftIO $ do
  ctx <- SSL.context
  ssl <- SSL.connection ctx conn
  SSL.accept ssl
  return ssl

sslConnect conn = liftIO $ do
  ctx <- SSL.context
  ssl <- SSL.connection ctx conn
  SSL.connect ssl
  return ssl
#endif

newClientSocket host port = E.handle (\(_ :: IOException) -> return Nothing) $ do
  let hints = S.defaultHints
                  { S.addrFlags = [S.AI_ADDRCONFIG, S.AI_NUMERICSERV]
                  , S.addrFamily = S.AF_INET
                  , S.addrSocketType = S.Stream
                  }
      fullHost = host ++ ":" ++ show port
  (addrInfo:_) <- S.getAddrInfo (Just hints) (Just host) (Just $ show port)
  let family = S.addrFamily addrInfo
      socketType = S.addrSocketType addrInfo
      protocol = S.addrProtocol addrInfo
      address = S.addrAddress addrInfo
  sock <- S.socket family socketType protocol
  -- S.setSocketOption sock S.NoDelay 1
  S.connect sock address
  return $ Just sock

-- Setting exhaustLimit to maxBound is practically equivalent to removing the
-- limit.
setExhaustLimit lim = do
  WebSocket {..} <- get
  liftIO $ writeIORef wsBytesReadRef (lim,lim)

--------------------------------------------------------------------------------
-- Custom file reading utilities

readFile8k :: FilePath -> IO LazyByteString
readFile8k = readFileN 8192

readFile16k :: FilePath -> IO LazyByteString
readFile16k = readFileN 16384

readFileN :: Int -> FilePath -> IO LazyByteString
readFileN chk f = openBinaryFile f ReadMode >>= hGetContentsN chk

hGetContentsN :: Int -> Handle -> IO LazyByteString
hGetContentsN chk h = streamRead
  where
    streamRead = unsafeInterleaveIO loop

    loop = do
        c <- S.hGetSome h chk
        if S.null c
          then hClose h >> return BL.Empty
          else do cs <- streamRead
                  return (BL.Chunk c cs)

--------------------------------------------------------------------------------
-- Raw byte-level websocket access in streaming and non-streaming variations

-- enable caching of pre-encoded msgs like an initial connection payload
sendRawWith :: ( ms' <: '[State () WebSocket]
               , With w (Ef ms' c') IO
               , MonadIO c'
               , MonadIO c
               )
            => w -> LazyByteString -> c (Promise (Either WSStatus ()))
sendRawWith s = with s . sendRaw

-- enable caching of pre-encoded msgs like an initial connection payload
sendRaw :: ( ms <: '[State () WebSocket]
           , MonadIO c
           )
        => LazyByteString -> Ef ms c (Either WSStatus ())
sendRaw b = do
  WebSocket {..} <- get
  case wsSocket of
    Just (_,_,c,_) -> do
#if defined(DEBUGWS) || defined(DEVEL)
      liftIO $ Prelude.putStrLn $ "sending: " ++ show b
#endif
      ewssu <- liftIO $ E.handle (\(e :: IOException) -> do
                                    -- liftIO $ putStrLn "Got an exception in sendRaw"
                                    return $ Left (WSClosed ClientClosedConnection)
                                 )
                      $ Right <$> WS.sendTextData c b
      either setWSStatus (const $ return ()) ewssu
      return ewssu
    Nothing -> return (Left wsStatus)

sendRawStreamWith :: ( MonadIO c
               , MonadIO c'
               , ms' <: '[State () WebSocket]
               , With w (Ef ms' c') IO
               )
            => w
            -> Txt
            -> LazyByteString
            -> c (Promise (Either WSStatus ()))
sendRawStreamWith s h bl = with s $ sendRawStream h bl

sendRawStream :: ( MonadIO c
                   , ms <: '[State () WebSocket]
                   )
                => Txt
                -> LazyByteString
                -> Ef ms c (Either WSStatus ())
sendRawStream h bl = do
  let chunks = BL.toChunks bl
  WebSocket {..} <- get
  case wsSocket of
    Just (_,_,c,_) -> do
#if defined(DEBUGWS) || defined(DEVEL)
      liftIO $ Prelude.putStrLn $ "sending: " ++ show bl
#endif
      ewssu <- liftIO $ E.handle (\(e :: IOException) -> do
                                     -- liftIO $ putStrLn "Got an exception in sendRawStream"
                                     return (Left $ WSClosed ClientClosedConnection)
                                 )
                                 (Right <$> go c chunks)
      either setWSStatus (const $ return ()) ewssu
      return ewssu
    _ -> return (Left wsStatus)
  where
    -- Optimized to limit msgs to c where c is the number of chunks in the
    -- stream bytestring, but makes a trade-off; keeps 2 chunks live at any one
    -- time to reduce calls to sendDataMessage. For most in-memory data, this
    -- will result in a single call to sendDataMessage; chunks are, at most, 32kB
    -- for locally encoded data. If the data is stored on disk, sendFile will
    -- read the file in, at most, 8kB chunks.
    go c [ch] =
      WS.sendTextData c $
        "{\"ep\":"
          <> encode h
          <> ",\"pl\":"
          <> BL.fromStrict ch
          <> "}"
    go c (a:as) = do
      WS.sendTextData c $
        "C{\"ep\":"
          <> encode h
          <> ",\"pl\":"
          <> BL.fromStrict a
      go' as
      where
        go' [a] = do
          WS.sendTextData c $
            "F" <> BL.fromStrict a <> "}"
        go' (a:as) = do
          WS.sendTextData c $
            "C" <> BL.fromStrict a
          go' as

--------------------------------------------------------------------------------
-- Send contents of files with JSON-encoded data with or without streaming

-- | Read a JSON encoded file from disk and wrap each chunk as an individual
-- message to be decoded by Fusion. All chunks except the last will be prefixed
-- with a 'C'. The last chunk will be prefixed with an 'F'. These chunks, when
-- concatenated, complete a full message that can be decoded as valid JSON.
--
-- Why: This is a workaround for not having streaming websockets.
--
-- Note: sendFileWith sacrifices compatability with external libraries for
-- streaming performance; the msgs sent with sendFileWith cannot be consumed
-- as standard JSON.
sendFileWith :: ( ms' <: '[State () WebSocket]
                , With w (Ef ms' c') IO
                , MonadIO c'
                , MonadIO c
                , Functor (Messages ms)
                )
             => w -> Txt -> FilePath -> Ef ms c (Promise (Either WSStatus ()))
sendFileWith s h fp = do
  bl <- liftIO $ readFile8k fp
  sendRawStreamWith s h bl

-- | Read a JSON encoded file from disk and wrap each chunk as an individual
-- message to be decoded by Fusion or Fission. All chunks except the last will
-- be prefixed with a 'C'. The last chunk will be prefixed with an 'F'. These
-- chunks, when concatenated, complete a full message that can be decoded as
-- valid JSON.
--
-- Why: This is a workaround for not having streaming websockets.
--
-- Note: sendFile sacrifices compatability with external libraries for
-- streaming performance; the msgs sent with sendFile cannot be consumed
-- as standard JSON.
sendFile :: ( ms <: '[State () WebSocket]
            , MonadIO c
            )
         => Txt -> FilePath -> Ef ms c (Either WSStatus ())
sendFile h fp = do
  bl <- liftIO $ readFile8k fp
  sendRawStream h bl

--------------------------------------------------------------------------------
-- Streaming Dispatch interface to websockets

requestWith :: ( MonadIO c
               , MonadIO c'

               , With w (Ef ms' c') IO

               , ms' <: '[State () WebSocket]

               , ms <: '[Evented]

               , Request rqTy

               , Req rqTy ~ request
               , ToJSON request
               , Identify request
               , I request ~ rqI

               , Rsp rqTy ~ response
               , FromJSON response
               )
            => w
            -> Proxy rqTy
            -> request
            -> (Ef ms c () -> Either Dispatch response -> Ef '[Event Dispatch] (Ef ms c) ())
            -> Ef ms c (Promise (Endpoint Dispatch))
requestWith srv rqty_proxy req f = do
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
                void $ with srv $ do
                  WebSocket {..} <- get
                  liftIO $ modifyIORef wsMessageHandlers $ \mhs ->
                    let !mhs' = Map.delete header mhs
                    in mhs'
                  putSyndicate syn (me,subs)
              else
                putSyndicate syn (me,subs)

        in f done (maybe (Left m) Right (decodeDispatch m))

  newn <- syndicate
  with srv $ do
    WebSocket {..} <- get
    n <- liftIO $ atomicModifyIORef' wsMessageHandlers $ \mhs ->
      case Map.lookup header mhs of
        Nothing -> (Map.insert header newn mhs,newn)
        Just n  -> (mhs,n)

    sub :: Subscription (Ef ms c) Dispatch <- subscribe n (return buf)
    bhv <- listen sub bhvr
    let stopper = forkStop bhv >> leaveSyndicate n sub

    liftIO $ writeIORef s_ (n,stopper)
    sendRawWith srv $ toBS $ encodeDispatch (requestHeader rqty_proxy) req
    fulfill pr (Endpoint header (unsafeCoerce sub) n)

  return pr

apiRequestWith :: forall w ms c ms' c' rqTy request rqI msgs rqs response.
                  ( MonadIO c
                  , MonadIO c'

                  , With w (Ef ms' c') IO

                  , ms' <: '[State () WebSocket]

                  , ms <: '[Evented]

                  , Request rqTy

                  , Req rqTy ~ request
                  , ToJSON request
                  , Identify request
                  , I request ~ rqI

                  , Rsp rqTy ~ response
                  , FromJSON response

                  , (rqTy ∈ rqs) ~ 'True
                  )
                => FullAPI msgs rqs
                -> w
                -> Proxy rqTy
                -> request
                -> (Ef ms c () -> Either Dispatch response -> Ef '[Event Dispatch] (Ef ms c) ())
                -> Ef ms c (Promise (Endpoint Dispatch))
apiRequestWith _ = requestWith

request :: forall c ms rqTy request rqI rsp.
            ( MonadIO c

            , ms <: '[Evented,State () WebSocket]

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
  sendRaw $ toBS $ encodeDispatch (requestHeader rqty_proxy) req
  return (Endpoint header (unsafeCoerce sub) n)

apiRequest :: forall c ms rqTy request rqI response rqs msgs.
              ( MonadIO c

              , ms <: '[State () WebSocket,Evented]

              , Request rqTy

              , Req rqTy ~ request
              , ToJSON request
              , Identify request
              , I request ~ rqI

              , Rsp rqTy ~ response
              , FromJSON response

              , (rqTy ∈ rqs) ~ 'True
              )
           => FullAPI msgs rqs
           -> Proxy rqTy
           -> request
           -> (Ef ms c () -> Either Dispatch response -> Ef '[Event Dispatch] (Ef ms c) ())
           -> Ef ms c (Endpoint Dispatch)
apiRequest _ = request

respondWith :: ( MonadIO c
               , MonadIO c'

               , With w (Ef ms' c') IO

               , ms <: '[Evented]

               , ms' <: '[State () WebSocket]

               , Request rqTy

               , Req rqTy ~ request
               , Identify request
               , I request ~ rqI
               , FromJSON request

               , Rsp rqTy ~ response
               , ToJSON response
               )
            => w
            -> Proxy rqTy
            -> (Ef ms c () -> Either Dispatch (Either LazyByteString response -> Ef ms c (Promise (Either WSStatus ())),request) -> Ef '[Event Dispatch] (Ef ms c) ()
               )
            -> Ef ms c (Promise (Endpoint Dispatch))
respondWith srv rqty_proxy rr = do
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
                void $ with srv $ do
                  mhs_ <- getWSMsgHandlers
                  liftIO $ modifyIORef mhs_ $ \mhs ->
                    let !mhs' = Map.delete header mhs
                    in mhs'
                  putSyndicate syn (me,subs)
              else
                putSyndicate syn (me,subs)

        in
            -- technically rr could kill the behavior and the syndicate might be left as garbage that never gets
            -- cleaned up, but it shouldn't matter too much since servers will have a limited set of msgs
            -- to which they're responding.
            void $ rr done $ maybe (Left m) (\rq -> Right
              (sendRawWith srv . either id (toBS . encodeDispatch (responseHeader rqty_proxy rq))
              ,rq
              )) (decodeDispatch m)
  newn <- syndicate
  with srv $ do
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

respond :: ( MonadIO c

           , ms <: '[Evented,State () WebSocket]

           , Request rqTy

           , Req rqTy ~ request
           , Identify request
           , I request ~ rqI
           , FromJSON request

           , Rsp rqTy ~ response
           , ToJSON response
           )
        => Proxy rqTy
        -> (Ef ms c () -> Either Dispatch (Either LazyByteString response -> Ef ms c (Either WSStatus ()),request) -> Ef '[Event Dispatch] (Ef ms c) ())
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

        in
            -- technically rr could kill the behavior and the syndicate might be left as garbage that never gets
            -- cleaned up, but it shouldn't matter too much since servers will have a limited set of msgs
            -- to which they're responding.
            void $ rr done $ maybe (Left m) (\rq -> Right
              (sendRaw . either id (toBS . encodeDispatch (responseHeader rqty_proxy rq))
              , rq
              )) (decodeDispatch m)

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
               , ms' <: '[State () WebSocket]
               , With w (Ef ms' c') IO
               , Message mTy
               , M mTy ~ message
               , ToJSON message
               , Functor (Messages ms)
               )
            => w
            -> Proxy mTy
            -> message
            -> Ef ms c (Promise (Either WSStatus ()))
messageWith s mty_proxy m =
  sendRawWith s $ toBS $ encodeDispatch (messageHeader mty_proxy) m

apiMessageWith :: ( MonadIO c
                  , MonadIO c'
                  , ms' <: '[State () WebSocket]
                  , With w (Ef ms' c') IO
                  , Message mTy
                  , M mTy ~ message
                  , ToJSON message
                  , (mTy ∈ msgs) ~ 'True
                  , Functor (Messages ms)
                  )
                => FullAPI msgs rqs
                -> w
                -> Proxy mTy
                -> message
                -> Ef ms c (Promise (Either WSStatus ()))
apiMessageWith _ = messageWith

message :: ( MonadIO c
           , ms <: '[State () WebSocket]
           , Message mTy
           , M mTy ~ msg
           , ToJSON msg
           )
        => Proxy mTy
        -> msg
        -> Ef ms c (Either WSStatus ())
message mty_proxy m =
  sendRaw $ toBS $ encodeDispatch (messageHeader mty_proxy) m

apiMessage :: ( MonadIO c
              , ms <: '[State () WebSocket]
              , Message mTy
              , M mTy ~ msg
              , ToJSON msg
              , (mTy ∈ msgs) ~ 'True
              )
           => FullAPI msgs rqs
           -> Proxy mTy
           -> msg
           -> Ef ms c (Either WSStatus ())
apiMessage _ mty_proxy m =
  sendRaw $ toBS $ encodeDispatch (messageHeader mty_proxy) m

onMessageWith :: ( MonadIO c
                 , MonadIO c'
                 , ms' <: '[State () WebSocket]
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

onMessage :: ( MonadIO c
             , ms <: '[Evented,State () WebSocket]
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
