{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language MagicHash #-}
module Atomic.Mediator (module Atomic.Mediator) where

import Ef.Base hiding (Client,Server)

import Atomic.Key
import Atomic.Revent
import Atomic.Vault
import Atomic.With
import Atomic.Observable

import Control.Concurrent
import Data.IORef
import GHC.Prim

import Data.HashMap.Strict as Map hiding ((!))

import System.IO.Unsafe
import Unsafe.Coerce

instance (IsMediator' ts ms, MonadIO c) =>
          With
          (Mediator' ts ms)
          (Code ms IO)
          c
  where
    using_ s = do
      -- faster lookup followed by modify if necessary which will check to
      -- make sure the mediator was not added between the lookup and the modify.
      mas <- vaultLookup mediatorVault__ (key s)
      case mas of
        Nothing -> do
          let Key (_,i) = key s
          modifyVault mediatorVault__ $ \v ->
            case Map.lookup i v of
              Nothing -> do
                rb <- newSignalBuffer
                sig :: Signal ms IO (Code ms IO ()) <- runner
                startMediator rb s
                let asMediator :: Code ms IO `As` IO
                    asMediator = constructAs rb sig
                    new_v = Map.insert i (unsafeCoerce asMediator) v
                return (new_v,liftIO . runAs asMediator)
              Just as ->
                return (v,liftIO . runAs as)
        Just as ->
          return (liftIO . runAs as)
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
      deleteMediator (key s)


type IsMediator' ts ms =
  ( MediatorBase <: ms
  , MediatorBase <. ts
  , Delta (Modules ts) (Messages ms)
  )
type IsMediator ms = IsMediator' (Appended MediatorBase ms) (Appended MediatorBase ms)

type MediatorBase =
  '[Revent
   ,State () Vault
   ,State () Shutdown
   ]

type MediatorKey' ms = Key (Code ms IO `As` IO)
type MediatorKey ms = MediatorKey' (Appended ms MediatorBase)
type MediatorBuilder' ts = Modules MediatorBase (Action ts IO) -> IO (Modules ts (Action ts IO))
type MediatorBuilder ts = MediatorBuilder' (Appended ts MediatorBase)
type MediatorPrimer' ms = Code ms IO ()
type MediatorPrimer ms = MediatorPrimer' (Appended ms MediatorBase)
type Mediator ms = Mediator' (Appended ms MediatorBase) (Appended ms MediatorBase)

data Mediator' ts ms
  = Mediator
      { key      :: !(MediatorKey' ms)
      , build    :: !(MediatorBuilder' ts)
      , prime    :: !(MediatorPrimer' ms)
      }

instance Eq (Mediator' ts ms) where
  (==) (Mediator i _ _) (Mediator i' _ _) =
    let Key k1 = i
        Key k2 = i'
    in case reallyUnsafePtrEquality# i i' of
         1# -> True
         _  -> k1 == k2

startMediator :: forall ms ts c.
                ( MonadIO c
                , IsMediator' ts ms
                )
              => Signaled
              -> Mediator' ts ms
              -> c ()
startMediator rb Mediator {..} = do
  sdn :: Network () <- network
  lv <- createVault
  built <- liftIO $ build $ revent rb
                         *:* state lv
                         *:* state (Shutdown sdn)
                         *:* Empty
  void $ liftIO $ forkIO $ do
    (obj,_) <- Object built ! do
      connect mediatorShutdownNetwork $ const (Ef.Base.lift shutdownSelf)
      prime
    driverPrintExceptions
      ("Mediator "
          ++ show key
          ++ " blocked in eventloop; likely caused by cyclic with calls. The standard solution is a 'delay'ed call to 'demand'. "
      ) rb obj

{-# NOINLINE mediatorShutdownNetwork #-}
mediatorShutdownNetwork :: Network ()
mediatorShutdownNetwork = unsafePerformIO network

{-# NOINLINE mediatorVault__ #-}
mediatorVault__ = Vault (unsafePerformIO (newMVar Map.empty))

lookupMediator :: (Monad c, MonadIO c)
              => Key phantom -> c (Maybe phantom)
lookupMediator = vaultLookup mediatorVault__

deleteMediator :: (Monad c, MonadIO c)
              => Key phantom -> c ()
deleteMediator = vaultDelete mediatorVault__
