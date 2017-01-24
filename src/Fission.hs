{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language CPP #-}
module Fission (module Fission, module Export) where

import Ef.Base as Export hiding (Object,watch,transform)
import qualified Ef.Base

import Nuclear as Export hiding (accept)

import Nuclear.WebSocket as Export hiding (LazyByteString)
import User hiding (accept)

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

data FissionNotStarted = FissionNotStarted deriving Show
instance Exception FissionNotStarted

instance ( IsFission ts ms c
         , IsUser uTs uMs
         , MonadIO c
         , MonadIO c'
         )
  => With (Fission ts ms c uTs uMs)
          (Code ms c)
          c'
  where
    using_ s = do
      mi_ <- lookupFission (key s)
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
      deleteFission (key s)

type IsFission ts ms c =
  ( Fission_ <: ms
  , Fission_ <. ts
  , Delta (Modules ts) (Messages ms)
  , MonadIO c
  )

newtype Connections = Connections (Network (SockAddr,Socket,Signaled))

type Fission_ =
  '[Revent
   ,State () Connections
   ,State () Vault
   ,State () Shutdown
   ]
data Fission ts ms c uTs uMs
  =
#ifdef SECURE
    SecureFission
    { key      :: !(Key (As (Code ms c) IO))
    , ip       :: !String
    , port     :: !Int
    , sslKey   :: !FilePath
    , sslCert  :: !FilePath
    , sslChain :: !(Maybe FilePath)
    , build    :: !(    Modules Fission_ (Action ts c)
                     -> c (Modules ts (Action ts c))
                   )
    , prime    :: !(Code ms c ())
    , user     :: !(As (Code ms c) IO -> User uTs uMs)
    }
  |
#endif
    Fission
    { key    :: !(Key (As (Code ms c) IO))
    , ip     :: !String
    , port   :: !Int
    , build  :: !(    Modules Fission_ (Action ts c)
                    -> c (Modules ts (Action ts c))
                 )
    , prime  :: !(Code ms c ())
    , user   :: !(As (Code ms c) IO -> User uTs uMs)
    }

forkRun :: ( MonadIO c
           , IsUser uTs uMs
           , IsFission ts ms IO
           , Functor (Messages uMs) 
           )
        => Fission ts ms IO uTs uMs
        -> c ThreadId
forkRun = liftIO . forkIO . run

{-# INLINE run #-}
run :: forall ts ms c uTs uMs.
       ( IsUser uTs uMs
       , IsFission ts ms c
       , Functor (Messages uMs) -- why? ghc-8.0.1 bug; if I remove State () Connection, it works fine;
                                -- something about the number of traits/messages since IsUser contains
                                -- `User_ m <: uMs` which terminates in Functor (Messages uMs)
       )
    => Fission ts ms c uTs uMs
    -> c ()
#ifdef SECURE
run SecureFission {..} = void $ do

  nw :: Network (SockAddr,Socket,Signaled) <- network

  q <- newSignalBuffer

  ctx <- liftIO $ sslSetupFission sslKey sslCert sslChain
  sock <- newListenSocket ip port

  gs <- createVault

  connSignal
    :: Signal ms c (State () WebSocket (Action uTs IO),SockAddr,Signaled)
    <- construct

  acSignal
    :: Network (Network uE)
    <- network

  fissionSig
    :: Signal ms c (Code ms c ())
    <- runner

  let asFission = constructAs q fissionSig
      cli = user asFission

  addFission key asFission

  liftIO $ forkIO $ void $ handleSecureUsers (ctx,sock) nw q connSignal

  sdn <- network

  fissionObj <- build (revent q
                      *:* state (Connections nw)
                      *:* state gs
                      *:* state (Shutdown sdn)
                      *:* Empty
                     )

  eventloop prime cli connSignal q (Object fissionObj)
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
              ws <- fissionWSS buf conn ssl unlimited
              buffer gb connSignal (ws,sockAddr,buf)
          go
#endif
run Fission {..} = void $ do

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

  fissionSig
    :: Signal ms c (Code ms c ())
    <- runner

  let asFission = constructAs q fissionSig
      cli = user asFission

  addFission key asFission

  tid <- liftIO $ forkIO $ void $ handleUsers sock q connSignal

  sdn <- network

  fissionObj <- build (revent q
                      *:* state (Connections nw)
                      *:* state gs
                      *:* state (Shutdown sdn)
                      *:* Empty
                     )

  eventloop prime cli connSignal q (Ef.Base.Object fissionObj)
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
             ( IsFission ts ms c
             , IsUser uTs uMs
             , Functor (Messages uMs)
             )
          => Code ms c ()
          -> User uTs uMs
          -> Signal ms c (State () WebSocket (Action uTs IO),SockAddr,Signaled)
          -> Signaled
          -> Ef.Base.Object ts c
          -> c ()
eventloop primeS User {..} connSignal q fissionObj = do
  (obj,_) <- fissionObj ! do
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
               (userObj,_) <- (Ef.Base.Object built) ! prime
               E.handle (\(_ :: E.SomeException) -> return ()) $ driver buf userObj
      return ()
  driver q obj

{-# NOINLINE fissionVault__ #-}
fissionVault__ = Vault (unsafePerformIO (newMVar Map.empty))

lookupFission :: (MonadIO c)
              => Key phantom -> c (Maybe phantom)
lookupFission = vaultLookup fissionVault__

addFission :: (MonadIO c)
           => Key phantom -> phantom -> c ()
addFission = vaultAdd fissionVault__

deleteFission :: (MonadIO c)
              => Key phantom -> c ()
deleteFission = vaultDelete fissionVault__

