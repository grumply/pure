{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language MagicHash #-}
{-# language CPP #-}
module Atomic.Service (module Atomic.Service) where

import Ef.Base hiding (Client,Server)

import Atomic.Key
import Atomic.Vault
import Atomic.Observable

import Control.Concurrent
import Data.IORef
import GHC.Prim

import Data.HashMap.Strict as Map hiding ((!))

import System.IO.Unsafe
import Unsafe.Coerce

instance (IsService' ts ms, MonadIO c) =>
          With
          (Service' ts ms)
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
                buf <- newEvQueue
                startService buf s
                asService :: As (Code ms IO) <- unsafeConstructAs buf
                let new_v = Map.insert i (unsafeCoerce asService) v
                return (new_v,liftIO . runAs asService)
              Just as ->
                return (v,liftIO . runAs as)
        Just as ->
          return (liftIO . runAs as)
    with_ s m = do
      run <- using_ s
      run m
    shutdown_ s = do
      with_ s $ do
        buf <- get
        Shutdown sdn <- get
        publish sdn ()
        delay 0 $ do
          liftIO $ do
            killBuffer buf
            myThreadId >>= killThread
      deleteService (key s)


type IsService' ts ms = (Base <: ms, Base <. ts, Delta (Modules ts) (Messages ms))
type IsService ms = IsService' (Appended Base ms) (Appended Base ms)

type Base = '[Evented,State () Vault,State () Shutdown]

type ServiceKey ms = Key (As (Code (Appended ms Base) IO))
type ServiceBuilder ts = Modules Base (Action (Appended ts Base) IO) -> IO (Modules (Appended ts Base) (Action (Appended ts Base) IO))
type ServicePrimer ms = Code (Appended ms Base) IO ()

data Service' ts ms
  = Service
      { key      :: !(Key (As (Code ms IO)))
      , build    :: !(Modules Base (Action ts IO) -> IO (Modules ts (Action ts IO)))
      , prime    :: !(Code ms IO ())
      }
type Service ms = Service' (Appended ms Base) (Appended ms Base)

instance Eq (Service' ts ms) where
  (==) (Service i _ _) (Service i' _ _) =
    let Key k1 = i
        Key k2 = i'
    in case reallyUnsafePtrEquality# i i' of
         1# -> True
         _  -> k1 == k2

startService :: forall ms ts c.
                ( MonadIO c
                , IsService' ts ms
                )
              => EvQueue
              -> Service' ts ms
              -> c ()
startService rb Service {..} = do
  sdn :: Syndicate () <- syndicate
  lv <- createVault
  built <- liftIO $ build $  state rb
                         *:* state lv
                         *:* state (Shutdown sdn)
                         *:* Empty
  void $ liftIO $ forkIO $ do
    (obj,_) <- Object built ! do
      connect mediatorShutdownSyndicate $ const (Ef.Base.lift shutdownSelf)
      prime
#ifdef __GHCJS__
    driverPrintExceptions
      ("Service "
          ++ show key
          ++ " blocked in eventloop; likely caused by cyclic with calls. The standard solution is a 'delay'ed call to 'demand'. "
      )
#else
    driver
#endif
      rb obj

{-# NOINLINE mediatorShutdownSyndicate #-}
mediatorShutdownSyndicate :: Syndicate ()
mediatorShutdownSyndicate = unsafePerformIO syndicate

{-# NOINLINE mediatorVault__ #-}
mediatorVault__ = Vault (unsafePerformIO (newMVar Map.empty))

lookupService :: (Monad c, MonadIO c)
              => Key phantom -> c (Maybe phantom)
lookupService = vaultLookup mediatorVault__

deleteService :: (Monad c, MonadIO c)
              => Key phantom -> c ()
deleteService = vaultDelete mediatorVault__
