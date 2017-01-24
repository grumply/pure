{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
module Fission.Client (module Fission.Client, module Export) where

import Ef.Base as Export hiding (Object,Client,watch,transform)
import qualified Ef.Base
import Ef.Event as Ev

import Nuclear as Export
import Fission.WebSocket as Export hiding (LazyByteString,accept)

import Data.Promise
import Data.Queue

import Control.Concurrent
import Control.Exception as E

import Text.Read hiding (get,lift)

import Data.IORef

import Unsafe.Coerce
import System.IO.Unsafe

import Data.HashMap.Strict as Map hiding ((!))

data ClientNotStarted = ClientNotStarted deriving Show
instance Exception ClientNotStarted

instance ( IsClient ts ms c
         , MonadIO c
         , MonadIO c'
         )
  => With (Client ts ms c)
          (Code ms c)
          c'
  where
    using_ c = do
      mi_ <- lookupClient (key c)
      case mi_ of
        Just as -> return (liftIO . runAs as)
        -- Nothing -> throwM ClientNotStarted
    with_ c m = do
      run <- using_ c
      run m
    shutdown_ c = do
      with_ c $ do
        buf <- getReventBuffer
        Shutdown sdn <- get
        syndicate sdn ()
        delay 0 $
          liftIO $ do
            killBuffer buf
            myThreadId >>= killThread
      deleteClient (key c)

type IsClient ts ms c =
  ( Client_ <: ms
  , Client_ <. ts
  , Delta (Modules ts) (Messages ms)
  , MonadIO c
  )

type Client_ =
  '[State () WebSocket
   ,Revent
   ,State () Vault
   ,State () Shutdown
   ]

data Client ts ms c = Client
  { key    :: !(Key (As (Code ms c) IO))
  , build  :: !(Modules Client_ (Action ts c) -> c (Modules ts (Action ts c)))
  , prime  :: !(Code ms c ())
  }

{-# INLINE run #-}
run :: forall ts ms c.
       IsClient ts ms c
    => Client ts ms c
    -> c ()
run Client {..} = void $ do
  q <- newSignalBuffer
  sig :: Signal userMs c (Code userMs c ()) <- runner
  v <- createVault
  ws <- websocket unlimited
  sdn :: Network () <- network
  udn :: Network m <- network
  built <- build $ state ws
               *:* revent q
               *:* state v
               *:* state (Shutdown sdn)
               *:* Empty
  let asClient = constructAs q sig
  addClient key asClient
  (userObj,_) <- Ef.Base.Object built ! prime

  driver q userObj

runIO client = E.handle (\(e :: DriverStopped) -> return ()) $ run client

{-# NOINLINE clientVault__ #-}
clientVault__ = Vault (unsafePerformIO (newMVar Map.empty))

lookupClient :: (MonadIO c)
              => Key phantom -> c (Maybe phantom)
lookupClient = vaultLookup clientVault__

addClient :: (MonadIO c)
           => Key phantom -> phantom -> c ()
addClient = vaultAdd clientVault__

deleteClient :: (MonadIO c)
              => Key phantom -> c ()
deleteClient = vaultDelete clientVault__
