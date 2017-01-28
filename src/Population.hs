{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language CPP #-}
module Population (module Population, module Export) where

import Ef.Base as Export hiding (watch,transform,construct)
import qualified Ef.Event
import qualified Ef.Base
import Prelude as Export hiding (all,exponent,div,head,span,tan,lookup,reverse)

import Atomic as Export hiding (accept)

import Atomic.WebSocket as Export hiding (LazyByteString)
import Presence hiding (accept)

import qualified Atomic.Mediator as Mediator

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

data PopulationNotStarted = PopulationNotStarted deriving Show
instance Exception PopulationNotStarted

instance ( IsPopulation' ts ms c
         , IsPresence' uTs uMs
         , MonadIO c
         , MonadIO c'
         )
  => With (Population' ts ms c uTs uMs)
          (Code ms c)
          c'
  where
    using_ s = do
      mi_ <- lookupPopulation (key s)
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
      deletePopulation (key s)

type IsPopulation' ts ms c =
  ( PopulationBase <: ms
  , PopulationBase <. ts
  , Delta (Modules ts) (Messages ms)
  , MonadIO c
  )
type IsPopulation ms = IsPopulation' ms ms IO

newtype Connections = Connections (Network (SockAddr,Socket,Signaled))

type PopulationBase =
  '[Revent
   ,State () Connections
   ,State () Vault
   ,State () Shutdown
   ]

type PopulationKey' ms c = Key (As (Code ms c) IO)
type PopulationKey ms = PopulationKey' (Appended ms PopulationBase) IO
type PopulationIP = String
type PopulationPort = Int
#ifdef SECURE
type PopulationSSLKey = FilePath
type PopulationSSLCert = FilePath
type PopulationSSLChain = Maybe FilePath
#endif
type PopulationBuilder' ts c = Modules PopulationBase (Action ts c) -> c (Modules ts (Action ts c))
type PopulationBuilder ts = PopulationBuilder' (Appended ts PopulationBase) IO
type PopulationPrimer' ms c = Code ms c ()
type PopulationPrimer ms = PopulationPrimer' (Appended ms PopulationBase) IO
type PopulationPresence' pts pms ms c = As (Code ms c) IO -> Presence' pts pms
type PopulationPresence pms ms = PopulationPresence' (Appended pms PresenceBase) (Appended pms PresenceBase) (Appended ms PopulationBase) IO

type Population ms pms = Population' (Appended ms PopulationBase) (Appended ms PopulationBase) IO (Appended pms PresenceBase) (Appended pms PresenceBase)

data Population' ts ms c pts pms
  =
#ifdef SECURE
    SecurePopulation
    { key      :: !(PopulationKey' ms c)
    , ip       :: !(PopulationIP)
    , port     :: !(PopulationPort)
    , sslKey   :: !(PopulationSSLKey)
    , sslCert  :: !(PopulationSSLCert)
    , sslChain :: !(PopulationSSLChain)
    , build    :: !(PopulationBuilder' ts c)
    , prime    :: !(PopulationPrimer' ms c)
    , presence :: !(PopulationPrsence' pts pms ms c)
    }
  |
#endif
    Population
    { key      :: !(PopulationKey' ms c)
    , ip       :: !(PopulationIP)
    , port     :: !(PopulationPort)
    , build    :: !(PopulationBuilder' ts c)
    , prime    :: !(PopulationPrimer' ms c)
    , presence :: !(PopulationPresence' pts pms ms c)
    }

forkRun :: ( MonadIO c
           , IsPresence' uTs uMs
           , IsPopulation' ts ms IO
           , Functor (Messages uMs) 
           )
        => Population' ts ms IO uTs uMs
        -> c ThreadId
forkRun = liftIO . forkIO . run

{-# INLINE run #-}
run :: forall ts ms c uTs uMs.
       ( IsPresence' uTs uMs
       , IsPopulation' ts ms c
       , Functor (Messages uMs) -- why? ghc-8.0.1 bug; if I remove State () Connection, it works fine;
                                -- something about the number of traits/messages since IsPresence contains
                                -- `Presence_ m <: uMs` which terminates in Functor (Messages uMs)
       )
    => Population' ts ms c uTs uMs
    -> c ()
#ifdef SECURE
run SecurePopulation {..} = void $ do

  nw :: Network (SockAddr,Socket,Signaled) <- network

  q <- newSignalBuffer

  ctx <- liftIO $ sslSetupPopulation sslKey sslCert sslChain
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

  let asPopulation = constructAs q populationSig
      cli = presence asPopulation

  addPopulation key asPopulation

  liftIO $ forkIO $ void $ handleSecurePresences (ctx,sock) nw q connSignal

  sdn <- network

  populationObj <- build (revent q
                      *:* state (Connections nw)
                      *:* state gs
                      *:* state (Shutdown sdn)
                      *:* Empty
                     )

  eventloop prime cli connSignal q (Object populationObj)
  where
    handleSecurePresences (ctx,sock) nw gb connSignal = go
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
run Population {..} = void $ do

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

  let asPopulation = constructAs q populationSig
      cli = presence asPopulation

  addPopulation key asPopulation

  tid <- liftIO $ forkIO $ void $ handlePresences sock q connSignal

  sdn <- network

  populationObj <- build (revent q
                      *:* state (Connections nw)
                      *:* state gs
                      *:* state (Shutdown sdn)
                      *:* Empty
                     )

  eventloop prime cli connSignal q (Ef.Base.Object populationObj)
  where
    handlePresences sock gb connSignal = go
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
             ( IsPopulation' ts ms c
             , IsPresence' uTs uMs
             , Functor (Messages uMs)
             )
          => Code ms c ()
          -> Presence' uTs uMs
          -> Signal ms c (State () WebSocket (Action uTs IO),SockAddr,Signaled)
          -> Signaled
          -> Ef.Base.Object ts c
          -> c ()
eventloop primeS Presence {..} connSignal q populationObj = do
  (obj,_) <- populationObj ! do
    primeS
    void $ behavior connSignal $ \(websock,sockAddr,buf) -> do
      Mediator.startMediator buf Mediator.Mediator
        { key = fromTxt (toTxt (show sockAddr))
        , build = \base -> do
            build (websock *:* state (Origin sockAddr) *:* base)
        , prime = prime
        }
  driver q obj

{-# NOINLINE populationVault__ #-}
populationVault__ = Vault (unsafePerformIO (newMVar Map.empty))

lookupPopulation :: (MonadIO c)
              => Key phantom -> c (Maybe phantom)
lookupPopulation = vaultLookup populationVault__

addPopulation :: (MonadIO c)
           => Key phantom -> phantom -> c ()
addPopulation = vaultAdd populationVault__

deletePopulation :: (MonadIO c)
              => Key phantom -> c ()
deletePopulation = vaultDelete populationVault__

