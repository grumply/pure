{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language CPP #-}
module Server (module Server, module Export) where

import Ef.Base as Export hiding (As,Index,watch,transform,construct,uncons,distribute,embed,observe)
import qualified Ef.Event
import qualified Ef.Base
import Prelude as Export hiding (all,exponent,div,head,span,tan,lookup,reverse)

import Atomic as Export hiding (accept)

import Atomic.WebSocket as Export hiding (LazyByteString)
import Connection hiding (Base,accept)
import qualified Connection

import qualified Atomic.Service as Service

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

instance ( IsServer' ts ms c
         , IsConnection' uTs uMs
         , MonadIO c
         , MonadIO c'
         )
  => With (Server' ts ms c uTs uMs)
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

type IsServer' ts ms c = (Base <: ms, Base <. ts, Delta (Modules ts) (Messages ms), MonadIO c)
type IsServer ms = IsServer' ms ms IO

type ServerKey ms = Key (Ef.Base.As (Code (Appended ms Base) IO) IO)
type ServerBuilder ts = Modules Base (Action (Appended ts Base) IO) -> IO (Modules (Appended ts Base) (Action (Appended ts Base) IO))
type ServerPrimer ms = Code (Appended ms Base) IO ()
type ServerConnection pms ms = Ef.Base.As (Code (Appended ms Base) IO) IO -> Connection' (Appended pms Connection.Base) (Appended pms Connection.Base)

newtype Connections = Connections (Network (SockAddr,Socket,Signaled))

type Base = '[Revent,State () Connections,State () Vault,State () Shutdown]

data Server' ts ms c pts pms
  =
#ifdef SECURE
    SecureServer
    { key      :: !(Key (Ef.Base.As (Code ms c) IO))
    , ip       :: !(String)
    , port     :: !(Int)
    , sslKey   :: !(FilePath)
    , sslCert  :: !(FilePath)
    , sslChain :: !(Maybe FilePath)
    , build    :: !(Modules Base (Action ts c) -> c (Modules ts (Action ts c)))
    , prime    :: !(Code ms c ())
    , presence :: !(Ef.Base.As (Code ms c) IO -> Connection' pts pms)
    }
  |
#endif
    Server
    { key      :: !(Key (Ef.Base.As (Code ms c) IO))
    , ip       :: !(String)
    , port     :: !(Int)
    , build    :: !(Modules Base (Action ts c) -> c (Modules ts (Action ts c)))
    , prime    :: !(Code ms c ())
    , presence :: !(Ef.Base.As (Code ms c) IO -> Connection' pts pms)
    }
type Server ms pms = Server' (Appended ms Base) (Appended ms Base) IO (Appended pms Connection.Base) (Appended pms Connection.Base)

forkRun :: ( MonadIO c
           , IsConnection' uTs uMs
           , IsServer' ts ms IO
           , Functor (Messages uMs) 
           )
        => Server' ts ms IO uTs uMs
        -> c ThreadId
forkRun = liftIO . forkIO . run

{-# INLINE run #-}
run :: forall ts ms c uTs uMs.
       ( IsConnection' uTs uMs
       , IsServer' ts ms c
       , Functor (Messages uMs) -- why? ghc-8.0.1 bug; if I remove State () Connection, it works fine;
                                -- something about the number of traits/messages since IsConnection contains
                                -- `Connection_ m <: uMs` which terminates in Functor (Messages uMs)
       )
    => Server' ts ms c uTs uMs
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
    <- Ef.Event.construct

  acSignal
    :: Network (Network uE)
    <- network

  populationSig
    :: Signal ms c (Code ms c ())
    <- runner

  let asServer = constructAs q populationSig
      cli = presence asServer

  addServer key asServer

  liftIO $ forkIO $ void $ handleSecureConnections (ctx,sock) nw q connSignal

  sdn <- network

  populationObj <- build (revent q
                      *:* state (Connections nw)
                      *:* state gs
                      *:* state (Shutdown sdn)
                      *:* Empty
                     )

  eventloop prime cli connSignal q (Object populationObj)
  where
    handleSecureConnections (ctx,sock) nw gb connSignal = go
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
              ws <- populationWSS buf conn ssl unlimited
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
    <- Ef.Event.construct

  acSignal
    :: Network (Network uE)
    <- network

  populationSig
    :: Signal ms c (Code ms c ())
    <- runner

  let asServer = constructAs q populationSig
      cli = presence asServer

  addServer key asServer

  tid <- liftIO $ forkIO $ void $ handleConnections sock q connSignal

  sdn <- network

  populationObj <- build (revent q
                      *:* state (Connections nw)
                      *:* state gs
                      *:* state (Shutdown sdn)
                      *:* Empty
                     )

  eventloop prime cli connSignal q (Ef.Base.Object populationObj)
  where
    handleConnections sock gb connSignal = go
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
             ( IsServer' ts ms c
             , IsConnection' uTs uMs
             , Functor (Messages uMs)
             )
          => Code ms c ()
          -> Connection' uTs uMs
          -> Signal ms c (State () WebSocket (Action uTs IO),SockAddr,Signaled)
          -> Signaled
          -> Ef.Base.Object ts c
          -> c ()
eventloop primeS pres connSignal q populationObj = do
  (obj,_) <- populationObj ! do
    primeS
    void $ behavior connSignal $ \(websock,sockAddr,buf) -> do
      Service.startService buf Service.Service
        { key = fromTxt (toTxt (show sockAddr))
        , build = \base -> do
            Connection.build pres (websock *:* state (Origin sockAddr) *:* base)
        , prime = Connection.prime pres
        }
  driver q obj

{-# NOINLINE populationVault__ #-}
populationVault__ = Vault (unsafePerformIO (newMVar Map.empty))

lookupServer :: (MonadIO c)
              => Key phantom -> c (Maybe phantom)
lookupServer = vaultLookup populationVault__

addServer :: (MonadIO c)
           => Key phantom -> phantom -> c ()
addServer = vaultAdd populationVault__

deleteServer :: (MonadIO c)
              => Key phantom -> c ()
deleteServer = vaultDelete populationVault__

