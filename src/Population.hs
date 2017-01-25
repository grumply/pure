{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language CPP #-}
module Population (module Population, module Export) where

import Ef.Base as Export hiding (Object,watch,transform)
import qualified Ef.Base

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

instance ( IsPopulation ts ms c
         , IsPresence uTs uMs
         , MonadIO c
         , MonadIO c'
         )
  => With (Population ts ms c uTs uMs)
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

type IsPopulation ts ms c =
  ( Population_ <: ms
  , Population_ <. ts
  , Delta (Modules ts) (Messages ms)
  , MonadIO c
  )

newtype Connections = Connections (Network (SockAddr,Socket,Signaled))

type Population_ =
  '[Revent
   ,State () Connections
   ,State () Vault
   ,State () Shutdown
   ]

data Population ts ms c pts pms
  =
#ifdef SECURE
    SecurePopulation
    { key      :: !(Key (As (Code ms c) IO))
    , ip       :: !String
    , port     :: !Int
    , sslKey   :: !FilePath
    , sslCert  :: !FilePath
    , sslChain :: !(Maybe FilePath)
    , build    :: !(    Modules Population_ (Action ts c)
                     -> c (Modules ts (Action ts c))
                   )
    , prime    :: !(Code ms c ())
    , presence :: !(As (Code ms c) IO -> Presence pts pms)
    }
  |
#endif
    Population
    { key    :: !(Key (As (Code ms c) IO))
    , ip     :: !String
    , port   :: !Int
    , build  :: !(    Modules Population_ (Action ts c)
                    -> c (Modules ts (Action ts c))
                 )
    , prime  :: !(Code ms c ())
    , presence :: !(As (Code ms c) IO -> Presence pts pms)
    }

forkRun :: ( MonadIO c
           , IsPresence uTs uMs
           , IsPopulation ts ms IO
           , Functor (Messages uMs) 
           )
        => Population ts ms IO uTs uMs
        -> c ThreadId
forkRun = liftIO . forkIO . run

{-# INLINE run #-}
run :: forall ts ms c uTs uMs.
       ( IsPresence uTs uMs
       , IsPopulation ts ms c
       , Functor (Messages uMs) -- why? ghc-8.0.1 bug; if I remove State () Connection, it works fine;
                                -- something about the number of traits/messages since IsPresence contains
                                -- `Presence_ m <: uMs` which terminates in Functor (Messages uMs)
       )
    => Population ts ms c uTs uMs
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
    <- construct

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
    <- construct

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
             ( IsPopulation ts ms c
             , IsPresence uTs uMs
             , Functor (Messages uMs)
             )
          => Code ms c ()
          -> Presence uTs uMs
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

