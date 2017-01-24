{-# language OverloadedStrings #-}
{-# language UndecidableInstances #-}
{-# language CPP #-}
module Fission.WebSocket
  ( S.Socket, S.SockAddr, S.accept, S.sClose
#ifdef SECURE
  , SSL.withOpenSSL
#endif
  , module Fission.WebSocket
  ) where

import Ef.Base

import Data.JSText as AE
import Data.MicroTime
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

import Data.Queue

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

import Data.Time.Clock.POSIX

import System.IO.Unsafe
import Unsafe.Coerce

import Data.ByteString.Lazy as BSL

type LazyByteString = BSL.ByteString

-- This code is hideous, but there is reason in the chaos:
--   1. there is a split between client and server to support fission-based desktop clients as well as servers.
--   2. there is a split between secure (SSL) and insecure websockets.
--   3. there is a split between streaming and non-streaming send because the current websocket library does not support streaming.
-- This interface is very generic and thus the type signatures are somewhat daunting. If you understand what Nuclear implements,
-- what With implements, what Eventer implements, and what Revent implements (and thus how Services, Components, Clients and Servers
-- work), you'll see how this fits together.

data WSCloseReason
  = MessageLengthLimitExceeded Int64
  | MessageThroughputLimitExceeded
     { msgsPerSecondSeen :: Int
     , msgsPerSecondAllowed :: Int
     , msgsPerMinuteSeen :: Int
     , msgsPerMinuteAllowed :: Int
     }
  | BadMessageReceived JSText
  | ClientClosedConnection
  | ServerClosedConnection
  deriving (Show,Eq,Generic,ToJSON,FromJSON)

data WSStatus
  = WSUnopened
  | WSClosed WSCloseReason
  | WSOpen S.SockAddr S.Socket
  | WSConnecting
  deriving (Eq,Show)

data WebSocket
  = WebSocket
    { wsSocket           :: !(Maybe (S.SockAddr,S.Socket,WS.Connection,WS.Stream))
    , wsReceiveThread    :: !(Maybe ThreadId)
    , wsMessageHandlers  :: !(IORef (Map.HashMap JSText (Network Nuclear)))
    , wsStatus           :: !WSStatus
    , wsStatusNetwork    :: !(Network WSStatus)
    , wsThroughputLimits :: !(Throughput,ThroughputLimits)
    , wsBytesReadRef     :: !(IORef (Int64,Int64))
    }

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
         , '[Revent,State () WebSocket] <: ms

         , Request rqTy

         , Req rqTy ~ request
         , Indexed request
         , I request ~ rqI
         , FromJSON request
         , ToText rqI

         , Rsp rqTy ~ response
         , ToJSON response
         )
      => Bool
      -> Proxy rqTy
      -> (Code ms c ()
           -> Either Nuclear (Either LazyByteString response -> Code ms c (Either WSStatus ()),request)
           -> Code '[Event Nuclear] (Code ms c) ()
         )
      -> RequestHandler ms c rqTy

responds :: ( MonadIO c
            , '[Revent,State () WebSocket] <: ms

            , Request rqTy

            , Req rqTy ~ request
            , Indexed request
            , I request ~ rqI
            , FromJSON request
            , ToText rqI

            , Rsp rqTy ~ response
            , ToJSON response
            )
         => Bool
         -> Proxy rqTy
         -> (Code ms c ()
              -> Either Nuclear (Either LazyByteString response -> Code ms c (Either WSStatus ()),request)
              -> Code '[Event Nuclear] (Code ms c) ()
            )
         -> RequestHandler ms c rqTy
responds = RequestHandler

type ReqHandlers ms c rqs = Endpoints RequestHandler ms c rqs

data MessageHandler ms c mTy
  where
    MessageHandler
      :: ( MonadIO c
         , '[Revent,State () WebSocket] <: ms
         , Message mTy
         , M mTy ~ msg
         , ToJSON msg
         )
      => Proxy mTy
      -> (Code ms c () -> Either Nuclear msg -> Code '[Event Nuclear] (Code ms c) ())
      -> MessageHandler ms c mTy

accepts :: ( MonadIO c
           , '[Revent,State () WebSocket] <: ms
           , Message mTy
           , M mTy ~ msg
           , ToJSON msg
           )
        => Proxy mTy
        -> (Code ms c () -> Either Nuclear msg -> Code '[Event Nuclear] (Code ms c) ())
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
        amh <- onMessage p ((unsafeCoerce f) :: Code ms c () -> Either Nuclear msg -> Code '[Event Nuclear] (Code ms c) ())
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
      RequestHandler stream _ f -> do
        let p = Proxy :: Proxy request
            mhs' = deleteHandler p mhs :: ReqHandlers ms c rqs''
        amh <- respond stream p ((unsafeCoerce f) :: Code ms c () -> Either Nuclear (Either LazyByteString response -> Code ms c (Either WSStatus ()),req) -> Code '[Event Nuclear] (Code ms c) ())
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

enact :: (MonadIO c, Functor (Messages ms))
      => Implementation ms c msgs rqs msgs' rqs'
      -> Code ms c (ActiveAPI ms c msgs rqs)
enact (Impl local mhs rhs) = do
  let API mapi rapi = local
  amapi <- enactEndpoints mapi mhs
  arapi <- enactEndpoints rapi rhs
  let active = ActiveAPI amapi arapi
  return active

cleanupEndpoints :: (MonadIO c, '[State () WebSocket] <: ms)
                 => Code ms c ()
cleanupEndpoints = do
  WebSocket {..} <- get
  mh <- liftIO $ readIORef wsMessageHandlers
  let mhl = Map.toList mh
  mmhl' <- forM mhl $ \(h,n) -> do
             nn <- nullNetwork n
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

onWSStatus :: ('[State () WebSocket, Revent] <: ms,MonadIO c)
        => (WSStatus -> Code '[Event WSStatus] (Code ms c) ())
        -> Code ms c (Subscription ms c WSStatus,Periodical ms c WSStatus)
onWSStatus f = do
  buf <- getReventBuffer
  WebSocket {..} <- get
  p <- periodical
  Just s <- subscribe p f
  joinNetwork wsStatusNetwork p buf
  return (s,p)

onWSClose :: ('[State () WebSocket, Revent] <: ms,MonadIO c)
        => (WSCloseReason -> Code '[Event WSStatus] (Code ms c) ())
        -> Code ms c (Subscription ms c WSStatus,Periodical ms c WSStatus)
onWSClose f = onWSStatus (\wss -> case wss of { WSClosed wscr -> f wscr; _ -> return () } )

getWSStatus :: ('[State () WebSocket] <: ms, Monad c)
            => Code ms c WSStatus
getWSStatus = do
  WebSocket {..} <- get
  return wsStatus

setWSStatus :: ('[State () WebSocket] <: ms, MonadIO c)
            => WSStatus -> Code ms c ()
setWSStatus wss = do
  ws <- get
  put ws { wsStatus = wss }
  syndicate (wsStatusNetwork ws) wss

getWSInfo :: ('[State () WebSocket] <: ms, MonadIO c)
          => Code ms c (Maybe (S.SockAddr,S.Socket))
getWSInfo = do
  ws <- get
  case wsSocket ws of
    Just (sa,s,_,_) -> return (Just (sa,s))
    _ -> return Nothing

onWSCloseSimplyShutdown :: forall ms c.
                         ('[State () WebSocket, Revent, State () Shutdown] <: ms, MonadIO c)
                      => Code ms c (Subscription ms c WSStatus,Periodical ms c WSStatus)
onWSCloseSimplyShutdown = do
  onWSClose $ \closeReason -> lift $ do
    buf <- getReventBuffer
    Shutdown sdn <- get
    syndicate sdn ()
    rnr <- (runner :: Code ms c (Signal ms c (Code ms c ())))
    buffer buf rnr $ liftIO $ do
      killBuffer buf
      tid <- myThreadId
      killThread tid

-- create a websocket without initializing it; this is used by clients and
-- services for optional and auxiliary connections.
websocket :: forall c. (MonadIO c) => Throughput -> c WebSocket
websocket tp = liftIO $ do
  tpl <- throughputToThroughputLimits tp
  mhs <- newIORef (Map.empty :: Map.HashMap JSText (Network Nuclear))
  wssn <- network
  brr <- newIORef (2 * 1024 * 1024,2 * 1024 * 1024) -- 2MiB
  return $ WebSocket
    { wsSocket           = Nothing
    , wsReceiveThread    = Nothing
    , wsMessageHandlers  = unsafeCoerce mhs
    , wsStatus           = WSUnopened
    , wsStatusNetwork    = wssn
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
            , '[State () WebSocket] <. ts
            , '[State () WebSocket] <: ms
            , Delta (Modules ts) (Messages ms)
            )
         => Signaled
         -> S.Socket
         -> Throughput
         -> c' (State () WebSocket (Action ts c))
serverWS q sock tp = liftIO $ do
  sa <- liftIO $ S.getSocketName sock
  tpl <- throughputToThroughputLimits tp
  mhs <- newIORef (Map.empty :: Map.HashMap JSText (Network Nuclear))
  wssn <- network
  brr <- newIORef (2 * 1024 * 1024,2 * 1024 * 1024)

  streams <- Streams.socketToStreams sock

  wsStream <- liftIO $ makeExhaustible brr wssn sock streams

  pc <- WS.makePendingConnectionFromStream
          wsStream
          WS.defaultConnectionOptions

  c <- WS.acceptRequest pc

  rnr :: Signal ms c (Code ms c ()) <- runner

  rt <- forkIO $ receiveLoop sock tpl mhs brr q c rnr

  return $ state $ WebSocket
    { wsSocket = Just (sa,sock,c,wsStream)
    , wsReceiveThread = Just rt
    , wsMessageHandlers = unsafeCoerce mhs
    , wsStatus = WSOpen sa sock
    , wsStatusNetwork = wssn
    , wsThroughputLimits = (tp,tpl)
    , wsBytesReadRef = brr
    }

-- initialize an unconnected websocket created with 'websocket' in a narrative
-- context. This is used for client websockets or service websockets that may be
-- conditionally connected or connected on some delay.
initializeClientWS :: forall ms c ts.
                      ( MonadIO c
                      , '[Revent,State () WebSocket] <: ms
                      )
                   => String
                   -> Int
                   -> String
                   -> Code ms c ()
initializeClientWS host port path = do

  wsstatus <- getWSStatus
  case wsstatus of
    WSConnecting -> return ()
    WSOpen _ _   -> return ()
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
          q <- getReventBuffer
          ws@WebSocket {..} <- get

          streams <- liftIO $ Streams.socketToStreams sock

          wsStream <- liftIO $ makeExhaustible wsBytesReadRef wsStatusNetwork sock streams

          c <- liftIO $
            WS.runClientWithStream
              wsStream
              host
              path
              WS.defaultConnectionOptions
              []
              return

          rnr :: Signal ms c (Code ms c ()) <- runner

          rt <- liftIO $ forkIO $ receiveLoop sock (snd wsThroughputLimits) wsMessageHandlers wsBytesReadRef q c rnr

          put WebSocket
                { wsSocket = Just (sa,sock,c,wsStream)
                , wsReceiveThread = Just rt
                , .. }
          setWSStatus (WSOpen sa sock)

#ifdef SECURE
serverWSS :: forall ts ms c c'.
             ( MonadIO c'
             , MonadIO c
             , '[State () WebSocket] <. ts
             , '[State () WebSocket] <: ms
             , Delta (Modules ts) (Messages ms)
             )
          => Signaled
          -> S.Socket
          -> SSL
          -> Throughput
          -> c' (State () WebSocket (Action ts c))
serverWSS q sock ssl tp = liftIO $ do
  sa <- liftIO $ S.getSocketName sock
  tpl <- throughputToThroughputLimits tp
  mhs <- newIORef (Map.empty :: Map.HashMap JSText (Network Nuclear))
  wssn <- network
  brr <- newIORef (2 * 1024 * 1024,2 * 1024 * 1024)

  streams <- Streams.sslToStreams ssl

  wsStream <- makeExhaustible brr wssn sock streams

  pc <- WS.makePendingConnectionFromStream
          wsStream
          WS.defaultConnectionOptions

  c <- WS.acceptRequest pc

  rnr :: Signal ms c (Code ms c ()) <- runner

  rt <- forkIO $ receiveLoop sock tpl mhs brr q c rnr

  return $ state $ WebSocket
    { wsSocket = Just (sa,sock,c,wsStream)
    , wsReceiveThread = Just rt
    , wsMessageHandlers = unsafeCoerce mhs
    , wsStatus          = WSOpen sa sock
    , wsStatusNetwork = wssn
    , wsBytesReadRef = brr
    , wsThroughputLimits = (tp,tpl)
    }

initializeClientWSS :: forall ms c ts.
                       ( MonadIO c
                       , '[Revent,State () WebSocket] <: ms
                       )
                    => String
                    -> Int
                    -> String
                    -> Code ms c ()
initializeClientWSS host port path = do

  wsstatus <- getWSStatus
  case wsstatus of
    WSConnecting -> return ()
    WSOpen _ _   -> return ()
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
          q <- getReventBuffer
          ws@WebSocket {..} <- get

          -- following two lines differ from non-ssl
          ssl <- liftIO $ sslConnect sock

          streams <- liftIO $ Streams.sslToStreams ssl

          wsStream <- liftIO $ makeExhaustible wsBytesReadRef wsStatusNetwork sock streams

          c <- liftIO $
            WS.runClientWithStream
              wsStream
              host
              path
              WS.defaultConnectionOptions
              []
              return

          rnr :: Signal ms c (Code ms c ()) <- runner

          rt <- liftIO $ forkIO $ receiveLoop sock (snd wsThroughputLimits) wsMessageHandlers wsBytesReadRef q c rnr

          put WebSocket
                { wsSocket = Just (sa,sock,c,wsStream)
                , wsReceiveThread = Just rt
                , .. }
          setWSStatus (WSOpen sa sock)
#endif

wsClose :: forall ms c.
           (MonadIO c, '[State () WebSocket] <: ms)
        => WSCloseReason -> Code ms c ()
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
#ifdef DEBUGWS
      putStrLn $ "Received websocket message: " ++ show eem
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

          resetSyndicateAndCheckThroughput m@(Nuclear h _) = do
            resetBytesReadRef brr_
            mhs <- liftIO $ readIORef mhs_
            case Map.lookup h mhs of
              Nothing -> do
#ifdef DEBUGWS
                putStrLn $ "Unhandled message: " ++ show (toText m)
#endif
                return ()
              Just mnw -> do
#ifdef DEBUGWS
                putStrLn $ "Dispatching message: " ++ show h
#endif
                syndicate mnw m
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
                  buffer q rnr (wsClose (BadMessageReceived $ toText str))

                Right m ->
                  resetSyndicateAndCheckThroughput m

            Just ('{',_) -> do
              writeIORef buf mempty -- just in case
              case fromBS str of

                Left _ ->
                  -- Simple generic protection against malicious msgs.
                  -- How often will this kill a valid connection?
                  buffer q rnr (wsClose (BadMessageReceived $ toText str))

                Right !(m :: Nuclear) ->
                  resetSyndicateAndCheckThroughput m

            _ ->
              buffer q rnr (wsClose (BadMessageReceived $ toText str))

        Left Closed -> do
#ifdef DEBUGWS
          putStrLn "Websocket is closed; receiveloop failed."
#endif
          buffer q rnr (wsClose ClientClosedConnection)

        x -> do
#ifdef DEBUGWS
          putStrLn $ "receiveloop websocket exception: " ++ show x
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
                -> Network WSStatus
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
            syndicate wssn (WSClosed (MessageLengthLimitExceeded count))
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

newClientSocket host port = E.handle (\(_ :: SomeException) -> return Nothing) $ do
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
sendRawWith :: ( '[State () WebSocket] <: ms'
               , With w (Code ms' c') IO
               , MonadIO c'
               , MonadIO c
               )
            => w -> LazyByteString -> c (Promise (Either WSStatus ()))
sendRawWith s = with s . sendRaw

-- enable caching of pre-encoded msgs like an initial connection payload
sendRaw :: ( '[State () WebSocket] <: ms
           , MonadIO c
           )
        => LazyByteString -> Code ms c (Either WSStatus ())
sendRaw b = do
  WebSocket {..} <- get
  case wsSocket of
    Just (_,_,c,_) -> do
      ewssu <- liftIO $ E.handle (\(e :: SomeException) -> do
                                    -- liftIO $ putStrLn "Got an exception in sendRaw"
                                    return $ Left (WSClosed ClientClosedConnection)
                                 )
                      $ Right <$> WS.sendTextData c b
      either setWSStatus (const $ return ()) ewssu
      return ewssu
    Nothing -> return (Left wsStatus)

sendRawStreamWith :: ( MonadIO c
               , MonadIO c'
               , '[State () WebSocket] <: ms'
               , With w (Code ms' c') IO
               )
            => w
            -> JSText
            -> LazyByteString
            -> c (Promise (Either WSStatus ()))
sendRawStreamWith s h bl = with s $ sendRawStream h bl

sendRawStream :: ( MonadIO c
                   , '[State () WebSocket] <: ms
                   )
                => JSText
                -> LazyByteString
                -> Code ms c (Either WSStatus ())
sendRawStream h bl = do
  let chunks = BL.toChunks bl
  WebSocket {..} <- get
  case wsSocket of
    Just (_,_,c,_) -> do
      ewssu <- liftIO $ E.handle (\(e :: SomeException) -> do
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
sendFileWith :: ( '[State () WebSocket] <: ms'
                , With w (Code ms' c') IO
                , MonadIO c'
                , MonadIO c
                , Functor (Messages ms)
                )
             => w -> JSText -> FilePath -> Code ms c (Promise (Either WSStatus ()))
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
sendFile :: ( '[State () WebSocket] <: ms
            , MonadIO c
            )
         => JSText -> FilePath -> Code ms c (Either WSStatus ())
sendFile h fp = do
  bl <- liftIO $ readFile8k fp
  sendRawStream h bl

--------------------------------------------------------------------------------
-- Streaming Nuclear interface to websockets

requestWith :: ( MonadIO c
               , MonadIO c'

               , With w (Code ms' c') IO

               , '[State () WebSocket] <: ms'

               , '[Revent] <: ms

               , Request rqTy

               , Req rqTy ~ request
               , ToJSON request
               , Indexed request
               , I request ~ rqI
               , ToText rqI

               , Rsp rqTy ~ response
               , FromJSON response
               )
            => w
            -> Bool
            -> Proxy rqTy
            -> request
            -> (Code ms c () -> Either Nuclear response -> Code '[Event Nuclear] (Code ms c) ())
            -> Code ms c (Endpoint ms c)
requestWith srv stream rqty_proxy req f = do
  buf <- getReventBuffer
  p <- periodical
  newn <- network
  s_ <- liftIO $ newIORef undefined
  let header = responseHeader rqty_proxy req
      bhvr m =
        let done = do
              (n,su) <- liftIO $ readIORef s_
              stop su
              leaveNetwork n p
              nn <- nullNetwork n
              when nn $ do
                void $ with srv $ do
                  nn <- nullNetwork n -- might have changed
                  when nn $ do
                    WebSocket {..} <- get
                    liftIO $ atomicModifyIORef' wsMessageHandlers $ \old_mhs ->
                      let !new_mhs = Map.delete header old_mhs
                      in (new_mhs,())
        in f done (maybe (Left m) Right (decodeNuclear m))
  -- liftIO $ Prelude.putStrLn $ "Adding response handler in requestWith: " ++ show header
  Just sb <- subscribe p bhvr
  with srv $ do
    WebSocket {..} <- get
    n <- liftIO $ atomicModifyIORef' wsMessageHandlers $ \mhs ->
      case Map.lookup header mhs of
        Nothing -> (Map.insert header newn mhs,newn)
        Just n  -> (mhs,n)
    joinNetwork n p buf
    liftIO $ writeIORef s_ (n,sb)
    if stream then
      sendRawStreamWith srv (requestHeader rqty_proxy) (encode req)
    else
      sendRawWith srv $ toBS $ encodeNuclear (requestHeader rqty_proxy) req
  return $ Endpoint header sb p


apiRequestWith :: ( MonadIO c
                  , MonadIO c'

                  , With w (Code ms' c') IO

                  , '[State () WebSocket] <: ms'

                  , '[Revent] <: ms

                  , Request rqTy

                  , Req rqTy ~ request
                  , ToJSON request
                  , Indexed request
                  , I request ~ rqI
                  , ToText rqI

                  , Rsp rqTy ~ response
                  , FromJSON response

                  , (rqTy ∈ rqs) ~ 'True
                  )
                => FullAPI msgs rqs
                -> w
                -> Bool
                -> Proxy rqTy
                -> request
                -> (Code ms c () -> Either Nuclear response -> Code '[Event Nuclear] (Code ms c) ())
                -> Code ms c (Endpoint ms c)
apiRequestWith _ srv stream rqty_proxy req f = do
  buf <- getReventBuffer
  p <- periodical
  newn <- network
  s_ <- liftIO $ newIORef undefined
  let header = responseHeader rqty_proxy req
      bhvr m =
        let done = do
              (n,su) <- liftIO $ readIORef s_
              stop su
              leaveNetwork n p
              nn <- nullNetwork n
              when nn $
                void $ with srv $ do
                  nn <- nullNetwork n -- might have changed
                  when nn $ do
                    WebSocket {..} <- get
                    liftIO $ atomicModifyIORef' wsMessageHandlers $ \old_mhs ->
                      let !new_mhs = Map.delete header old_mhs
                      in (new_mhs,())
        in f done (maybe (Left m) Right (decodeNuclear m))
  -- liftIO $ Prelude.putStrLn $ "Adding response handler in requestWith: " ++ show header
  Just sb <- subscribe p bhvr
  with srv $ do
    WebSocket {..} <- get
    n <- liftIO $ atomicModifyIORef' wsMessageHandlers $ \mhs ->
      case Map.lookup header mhs of
        Nothing -> (Map.insert header newn mhs,newn)
        Just n  -> (mhs,n)
    joinNetwork n p buf
    liftIO $ writeIORef s_ (n,sb)
    if stream then
      sendRawStreamWith srv (requestHeader rqty_proxy) (encode req)
    else
      sendRawWith srv $ toBS $ encodeNuclear (requestHeader rqty_proxy) req
  return $ Endpoint header sb p

request :: ( MonadIO c

           , '[State () WebSocket,Revent] <: ms

           , Request rqTy

           , Req rqTy ~ request
           , ToJSON request
           , Indexed request
           , I request ~ rqI
           , ToText rqI

           , Rsp rqTy ~ response
           , FromJSON response
           )
        => Bool
        -> Proxy rqTy
        -> request
        -> (Code ms c () -> Either Nuclear response -> Code '[Event Nuclear] (Code ms c) ())
        -> Code ms c (Endpoint ms c)
request stream rqty_proxy req f = do
  buf <- getReventBuffer
  p <- periodical
  newn <- network
  s_ <- liftIO $ newIORef undefined
  let header = responseHeader rqty_proxy req
      bhvr m =
        let done = do
              (su,n) <- liftIO $ readIORef s_
              stop su
              leaveNetwork n p
              nn <- nullNetwork n
              when nn $ do
                WebSocket {..} <- get
                liftIO $ atomicModifyIORef' wsMessageHandlers $ \old_mhs ->
                  let !new_mhs = Map.delete header old_mhs
                  in (new_mhs,())
        in f done (maybe (Left m) Right (decodeNuclear m))
  Just sb <- subscribe p bhvr
  WebSocket {..} <- get
  hm <- liftIO $ readIORef wsMessageHandlers
  n <- liftIO $ atomicModifyIORef' wsMessageHandlers $ \mhs ->
    case Map.lookup header mhs of
      Nothing -> (Map.insert header newn mhs,newn)
      Just n  -> (mhs,n)
  joinNetwork n p buf
  liftIO $ writeIORef s_ (sb,n)
  if stream then
    sendRawStream (requestHeader rqty_proxy) (encode req)
  else
    sendRaw $ toBS $ encodeNuclear (requestHeader rqty_proxy) req
  return $ Endpoint header sb p

apiRequest :: forall c ms rqTy request rqI response rqs msgs.
              ( MonadIO c

              , '[State () WebSocket,Revent] <: ms

              , Request rqTy

              , Req rqTy ~ request
              , ToJSON request
              , Indexed request
              , I request ~ rqI
              , ToText rqI

              , Rsp rqTy ~ response
              , FromJSON response

              , (rqTy ∈ rqs) ~ 'True
              )
           => FullAPI msgs rqs
           -> Bool
           -> Proxy rqTy
           -> request
           -> (Code ms c () -> Either Nuclear response -> Code '[Event Nuclear] (Code ms c) ())
           -> Code ms c (Endpoint ms c)
apiRequest _ stream rqty_proxy req f = do
  buf <- getReventBuffer
  p :: Periodical ms c Nuclear <- periodical
  newn :: Network Nuclear <- network
  s_ <- liftIO $ newIORef undefined
  let header = responseHeader rqty_proxy req
      bhvr m =
        let done = do
              (su,n) <- liftIO $ readIORef s_
              stop su
              leaveNetwork n p
              nn <- nullNetwork n
              when nn $ do
                WebSocket {..} <- get
                liftIO $ atomicModifyIORef' wsMessageHandlers $ \old_mhs ->
                  let !new_mhs = Map.delete header old_mhs
                  in (new_mhs,())
        in f done (maybe (Left m) Right (decodeNuclear m))
  Just (sb :: Subscription ms c Nuclear) <- subscribe p bhvr
  WebSocket {..} <- get
  hm <- liftIO $ readIORef wsMessageHandlers
  n <- liftIO $ atomicModifyIORef' wsMessageHandlers $ \mhs ->
    case Map.lookup header mhs of
      Nothing -> (Map.insert header newn mhs,newn)
      Just n  -> (mhs,n)
  joinNetwork n p buf
  liftIO $ writeIORef s_ (sb,n)
  if stream then
    sendRawStream (requestHeader rqty_proxy) (encode req)
  else
    sendRaw $ toBS $ encodeNuclear (requestHeader rqty_proxy) req
  return $ Endpoint header sb p


respondWith :: ( MonadIO c
               , MonadIO c'

               , With w (Code ms' c') IO

               , '[Revent] <: ms

               , '[State () WebSocket] <: ms'

               , Request rqTy

               , Req rqTy ~ request
               , Indexed request
               , I request ~ rqI
               , FromJSON request
               , ToText rqI

               , Rsp rqTy ~ response
               , ToJSON response
               )
            => w
            -> Bool
            -> Proxy rqTy
            -> (Code ms c () -> Either Nuclear (Either LazyByteString response -> Code ms c (Promise (Either WSStatus ())),request) -> Code '[Event Nuclear] (Code ms c) ()
               )
            -> Code ms c (Endpoint ms c)
respondWith srv stream rqty_proxy rr = do
  buf <- getReventBuffer
  p <- periodical
  s_ <- liftIO $ newIORef undefined
  newn <- network
  let header = requestHeader rqty_proxy
      bhvr m =
        let done = do
              (su,n) <- liftIO $ readIORef s_
              stop su
              leaveNetwork n p
              nn <- nullNetwork n
              when nn $
                void $ with srv $ do
                  nn <- nullNetwork n
                  when nn $ do
                    WebSocket {..} <- get
                    liftIO $ atomicModifyIORef' wsMessageHandlers $ \old_mhs ->
                      let !new_mhs = Map.delete header old_mhs
                      in (new_mhs,())
        in
            -- technically rr could kill the behavior and the network might be left as garbage that never gets
            -- cleaned up, but it shouldn't matter too much since servers will have a limited set of msgs
            -- to which they're responding.
            void $ rr done $ maybe (Left m) (\rq -> Right
              (if stream then
                 sendRawStreamWith srv (responseHeader rqty_proxy rq) . either id encode
               else
                 sendRawWith srv . either id (toBS . encodeNuclear (responseHeader rqty_proxy rq))
              ,rq
              )) (decodeNuclear m)
  Just sb <- subscribe p bhvr
  with srv $ do
    WebSocket {..} <- get
    n <- liftIO $ atomicModifyIORef' wsMessageHandlers $ \mhs ->
      case Map.lookup header mhs of
        Nothing -> (Map.insert header newn mhs,newn)
        Just n  -> (mhs,n)
    joinNetwork n p buf
    liftIO $ writeIORef s_ (sb,n)
  return $ Endpoint header sb p

respond :: ( MonadIO c

           , '[Revent,State () WebSocket] <: ms

           , Request rqTy

           , Req rqTy ~ request
           , Indexed request
           , I request ~ rqI
           , FromJSON request
           , ToText rqI

           , Rsp rqTy ~ response
           , ToJSON response
           )
        => Bool
        -> Proxy rqTy
        -> (Code ms c () -> Either Nuclear (Either LazyByteString response -> Code ms c (Either WSStatus ()),request) -> Code '[Event Nuclear] (Code ms c) ())
        -> Code ms c (Endpoint ms c)
respond stream rqty_proxy rr = do
  buf <- getReventBuffer
  p <- periodical
  s_ <- liftIO $ newIORef undefined
  newn <- network
  let header = requestHeader rqty_proxy
      bhvr m =
        let done = do
              (su,n) <- liftIO $ readIORef s_
              stop su
              leaveNetwork n p
              nn <- nullNetwork n
              when nn $ do
                WebSocket {..} <- get
                liftIO $ atomicModifyIORef' wsMessageHandlers $ \old_mhs ->
                  let !new_mhs = Map.delete header old_mhs
                  in (new_mhs,())
        in
            -- technically rr could kill the behavior and the network might be left as garbage that never gets
            -- cleaned up, but it shouldn't matter too much since servers will have a limited set of msgs
            -- to which they're responding.
            void $ rr done $ maybe (Left m) (\rq -> Right
              (if stream then
                 sendRawStream (responseHeader rqty_proxy rq) . either id encode 
               else
                 sendRaw . either id (toBS . encodeNuclear (responseHeader rqty_proxy rq))
              , rq
              )) (decodeNuclear m)
  Just sb <- subscribe p bhvr
  WebSocket {..} <- get
  n <- liftIO $ atomicModifyIORef' wsMessageHandlers $ \mhs ->
    case Map.lookup header mhs of
      Nothing -> (Map.insert header newn mhs,newn)
      Just n  -> (mhs,n)
  joinNetwork n p buf
  liftIO $ writeIORef s_ (sb,n)
  return $ Endpoint header sb p

messageWith :: ( MonadIO c
               , MonadIO c'
               , '[State () WebSocket] <: ms'
               , With w (Code ms' c') IO
               , Message mTy
               , M mTy ~ message
               , ToJSON message
               , Functor (Messages ms)
               )
            => w
            -> Bool
            -> Proxy mTy
            -> message
            -> Code ms c (Promise (Either WSStatus ()))
messageWith s stream mty_proxy m =
  if stream then
    sendRawStreamWith s (messageHeader mty_proxy) (encode m)
  else
    sendRawWith s $ toBS $ encodeNuclear (messageHeader mty_proxy) m

apiMessageWith :: ( MonadIO c
                  , MonadIO c'
                  , '[State () WebSocket] <: ms'
                  , With w (Code ms' c') IO
                  , Message mTy
                  , M mTy ~ message
                  , ToJSON message
                  , (mTy ∈ msgs) ~ 'True
                  , Functor (Messages ms)
                  )
                => FullAPI msgs rqs
                -> w
                -> Bool
                -> Proxy mTy
                -> message
                -> Code ms c (Promise (Either WSStatus ()))
apiMessageWith _ = messageWith

message :: ( MonadIO c
           , '[State () WebSocket] <: ms
           , Message mTy
           , M mTy ~ msg
           , ToJSON msg
           )
        => Bool
        -> Proxy mTy
        -> msg
        -> Code ms c (Either WSStatus ())
message stream mty_proxy m =
  if stream then
    sendRawStream (messageHeader mty_proxy) (encode m)
  else
    sendRaw $ toBS $ encodeNuclear (messageHeader mty_proxy) m

apiMessage :: ( MonadIO c
              , '[State () WebSocket] <: ms
              , Message mTy
              , M mTy ~ msg
              , ToJSON msg
              , (mTy ∈ msgs) ~ 'True
              )
           => FullAPI msgs rqs
           -> Bool
           -> Proxy mTy
           -> msg
           -> Code ms c (Either WSStatus ())
apiMessage _ stream mty_proxy m =
  if stream then
    sendRawStream (messageHeader mty_proxy) (encode m)
  else
    sendRaw $ toBS $ encodeNuclear (messageHeader mty_proxy) m

onMessageWith :: ( MonadIO c
                 , MonadIO c'
                 , '[State () WebSocket] <: ms'
                 , '[Revent] <: ms
                 , With w (Code ms' c') IO
                 , Message mTy
                 , M mTy ~ msg
                 , FromJSON msg
                 )
              => w
              -> Proxy mTy
              -> (Code ms c () -> Either Nuclear msg -> Code '[Event Nuclear] (Code ms c) ())
              -> Code ms c (Endpoint ms c)
onMessageWith s mty_proxy f = do
  buf <- getReventBuffer
  p <- periodical
  s_ <- liftIO $ newIORef undefined
  newn <- network
  let header = messageHeader mty_proxy
      bhvr m =
        let done = do
              (su,n) <- liftIO $ readIORef s_
              stop su
              leaveNetwork n p
              nn <- nullNetwork n
              when nn $
                void $ with s $ do
                  nn <- nullNetwork n
                  when nn $ do
                    WebSocket {..} <- get
                    liftIO $ atomicModifyIORef' wsMessageHandlers $ \old_mhs ->
                      let !new_mhs = Map.delete header old_mhs
                      in (new_mhs,())
        in f done (maybe (Left m) Right (decodeNuclear m))
  Just sb <- subscribe p bhvr
  with s $ do
    WebSocket {..} <- get
    n <- liftIO $ atomicModifyIORef' wsMessageHandlers $ \mhs ->
      case Map.lookup header mhs of
        Nothing -> (Map.insert header newn mhs,newn)
        Just n  -> (mhs,n)
    joinNetwork n p buf
    liftIO $ writeIORef s_ (sb,n)
  return $ Endpoint header sb p

onMessage :: ( MonadIO c
             , '[Revent,State () WebSocket] <: ms
             , Message mTy
             , M mTy ~ msg
             , FromJSON msg
             )
          => Proxy mTy
          -> (Code ms c () -> Either Nuclear msg -> Code '[Event Nuclear] (Code ms c) ())
          -> Code ms c (Endpoint ms c)
onMessage mty_proxy f = do
  buf <- getReventBuffer
  p <- periodical
  s_ <- liftIO $ newIORef undefined
  newn <- network
  let header = messageHeader mty_proxy
      bhvr m =
        let done = do
              (su,n) <- liftIO $ readIORef s_
              stop su
              leaveNetwork n p
              nn <- nullNetwork n
              when nn $ do
                WebSocket {..} <- get
                liftIO $ atomicModifyIORef' wsMessageHandlers $ \old_mhs ->
                  let !new_mhs = Map.delete header old_mhs
                  in (new_mhs,())
        in f done (maybe (Left m) Right (decodeNuclear m))
  Just sb <- subscribe p bhvr
  WebSocket {..} <- get
  n <- liftIO $ atomicModifyIORef' wsMessageHandlers $ \mhs ->
    case Map.lookup header mhs of
      Nothing -> (Map.insert header newn mhs,newn)
      Just n  -> (mhs,n)
  joinNetwork n p buf
  liftIO $ writeIORef s_ (sb,n)
  return $ Endpoint header sb p
