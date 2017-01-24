{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language CPP #-}
module Fission (module Fission, module Export) where

import Ef.Base as Export hiding (Server,watch,transform)

import Nuclear as Export hiding (accept,Object)

import Fission.WebSocket as Export hiding (LazyByteString)
import Fission.User

import Data.Promise
import Data.Queue

import Control.Concurrent
import Control.Exception as E
import Control.Monad
import Data.IORef
import Data.Int as Export (Int64)
import Data.Ratio
import Text.Read hiding (get,lift)

import Data.HashMap.Strict as Map hiding ((!))
import Data.Time.Clock.POSIX
import Network.Socket as Export (SockAddr(..))
import qualified Network.WebSockets as WS
import qualified Network.WebSockets.Stream as WS
#ifdef SECURE
import qualified OpenSSL as SSL
#endif
import qualified System.IO.Streams as Streams

import Unsafe.Coerce
import System.IO.Unsafe

data ServerNotStarted = ServerNotStarted deriving Show
instance Exception ServerNotStarted

instance ( IsServer ts ms c
         , IsUser uTs uMs
         , MonadIO c
         , MonadIO c'
         )
  => With (Server ts ms c uTs uMs)
          (Code ms c)
          c'
  where
    using_ s = do
      mi_ <- lookupServer (key s)
      case mi_ of
        Just as -> return (liftIO . runAs as)
        -- Nothing -> hmm?
    with_ s m = do
      run <- using_ s
      run m
    shutdown_ s = do
      with_ s $ do
        buf <- getReventBuffer
        Shutdown sdn <- get
        syndicate sdn ()
        liftIO $ do
          killBuffer buf
          myThreadId >>= killThread
      deleteServer (key s)

type IsServer ts ms c =
  ( Server_ <: ms
  , Server_ <. ts
  , Delta (Modules ts) (Messages ms)
  , MonadIO c
  )

newtype Connections = Connections (Network (SockAddr,Socket,Signaled))

type Server_ =
  '[Revent
   ,State () Connections
   ,State () Vault
   ,State () Shutdown
   ]
data Server ts ms c uTs uMs
  =
#ifdef SECURE
    SecureServer
    { key      :: !(Key (As (Code ms c) IO))
    , ip       :: !String
    , port     :: !Int
    , sslKey   :: !FilePath
    , sslCert  :: !FilePath
    , sslChain :: !(Maybe FilePath)
    , build    :: !(    Modules Server_ (Action ts c)
                     -> c (Modules ts (Action ts c))
                   )
    , prime    :: !(Code ms c ())
    , user     :: !(As (Code ms c) IO -> User uTs uMs)
    }
  |
#endif
    Server
    { key    :: !(Key (As (Code ms c) IO))
    , ip     :: !String
    , port   :: !Int
    , build  :: !(    Modules Server_ (Action ts c)
                    -> c (Modules ts (Action ts c))
                 )
    , prime  :: !(Code ms c ())
    , user   :: !(As (Code ms c) IO -> User uTs uMs)
    }

forkRun :: ( MonadIO c
           , IsUser uTs uMs
           , IsServer ts ms IO
           , Functor (Messages uMs) 
           )
        => Server ts ms IO uTs uMs
        -> c ThreadId
forkRun = liftIO . forkIO . run

{-# INLINE run #-}
run :: forall ts ms c uTs uMs.
       ( IsUser uTs uMs
       , IsServer ts ms c
       , Functor (Messages uMs) -- why? ghc-8.0.1 bug; if I remove State () Connection, it works fine;
                                -- something about the number of traits/messages since IsUser contains
                                -- `User_ m <: uMs` which terminates in Functor (Messages uMs)
       )
    => Server ts ms c uTs uMs
    -> c ()
#ifdef SECURE
run SecureServer {..} = void $ do

  nw :: Network (SockAddr,Socket,Signaled) <- network

  q <- newSignalBuffer

  ctx <- liftIO $ sslSetupServer sslKey sslCert sslChain
  sock <- newListenSocket ip port

  gs <- createVault

  connSignal
    :: Signal ms c (State () WebSocket (Action uTs IO),SockAddr,Signaled)
    <- construct

  acSignal
    :: Network (Network uE)
    <- network

  serverSig
    :: Signal ms c (Code ms c ())
    <- runner

  let asServer = constructAs q serverSig
      cli = user asServer

  addServer key asServer

  liftIO $ forkIO $ void $ handleSecureUsers (ctx,sock) nw q connSignal

  sdn <- network

  serverObj <- build (revent q
                      *:* state (Connections nw)
                      *:* state gs
                      *:* state (Shutdown sdn)
                      *:* Empty
                     )

  eventloop prime cli connSignal q (Object serverObj)
  where
    handleSecureUsers (ctx,sock) nw gb connSignal = go
      where
        go = do
          (conn,sockAddr) <- accept sock
          buf <- newSignalBuffer
          syndicate nw (sockAddr,conn,buf) -- if you set up a periodical to kill the connection and shut down to the user, the worst a
                                           -- malicious user could do is create a temporary thread and fill network buffers of something
                                           -- like 65k?
          forkIO $ void $
            E.handle (\(e :: E.SomeException) -> sClose conn) $ do
              ssl <- sslAccept conn
              ws <- serverWSS buf conn ssl unlimited
              buffer gb connSignal (ws,sockAddr,buf)
          go
#endif
run Server {..} = void $ do

  nw :: Network (SockAddr,Socket,Signaled) <- network

  q <- newSignalBuffer

  sock <- newListenSocket ip port

  gs <- createVault

  connSignal
    :: Signal ms c (State () WebSocket (Action uTs IO),SockAddr,Signaled)
    <- construct

  acSignal
    :: Network (Network uE)
    <- network

  serverSig
    :: Signal ms c (Code ms c ())
    <- runner

  let asServer = constructAs q serverSig
      cli = user asServer

  addServer key asServer

  tid <- liftIO $ forkIO $ void $ handleUsers sock q connSignal

  sdn <- network

  serverObj <- build (revent q
                      *:* state (Connections nw)
                      *:* state gs
                      *:* state (Shutdown sdn)
                      *:* Empty
                     )

  eventloop prime cli connSignal q (Object serverObj)
  where
    handleUsers sock gb connSignal = go
      where
        go = do
          (conn,sockAddr) <- accept sock
          buf <- newSignalBuffer
          forkIO $ void $
            E.handle (\(e :: E.SomeException) -> sClose conn) $ do
              ws <- serverWS buf conn unlimited
              buffer gb connSignal (ws,sockAddr,buf)
          go

eventloop :: forall ts ms c uTs uMs.
             ( IsServer ts ms c
             , IsUser uTs uMs
             , Functor (Messages uMs)
             )
          => Code ms c ()
          -> User uTs uMs
          -> Signal ms c (State () WebSocket (Action uTs IO),SockAddr,Signaled)
          -> Signaled
          -> Object ts c
          -> c ()
eventloop primeS User {..} connSignal q serverObj = do
  (obj,_) <- serverObj ! do
    primeS
    void $ behavior connSignal $ \(websock,sockAddr,buf) -> do
      sdn <- network
      tid <- liftIO $ forkIO $ void $ do
               lv <- createVault
               built <- build $ websock
                              *:* revent buf
                              *:* (state (Origin sockAddr) :: State () Origin (Action uTs IO))
                              *:* state lv
                              *:* state (Shutdown sdn)
                              *:* Empty
               (userObj,_) <- (Object built) ! prime
               E.handle (\(_ :: E.SomeException) -> return ()) $ driver buf userObj
      return ()
  driver q obj

{-# NOINLINE serverVault__ #-}
serverVault__ = Vault (unsafePerformIO (newMVar Map.empty))

lookupServer :: (MonadIO c)
              => Key phantom -> c (Maybe phantom)
lookupServer = vaultLookup serverVault__

addServer :: (MonadIO c)
           => Key phantom -> phantom -> c ()
addServer = vaultAdd serverVault__

deleteServer :: (MonadIO c)
              => Key phantom -> c ()
deleteServer = vaultDelete serverVault__

