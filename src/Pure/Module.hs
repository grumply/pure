{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleContexts #-}
module Pure.Module (module Pure.Module, module Export) where

import Ef.Base hiding (Module)

import Pure.Data hiding ((!),Module)
import qualified Pure.Data as Export

import Control.Concurrent
import GHC.Prim

import Data.HashMap.Strict as Map hiding ((!))

import Unsafe.Coerce

type ModuleBase = '[Evented, State () Vault, State () Shutdown]

type IsModule' ts ms = (ms <: ModuleBase, ts <. ModuleBase, Delta (Modules ts) (Messages ms))
type IsModule ms = IsModule' ms ms

data Module' ts ms
  = Module
    { key        :: !(Key (As (Ef ms IO)))
    , build      :: !(Modules ModuleBase (Action ts IO) -> IO (Modules ts (Action ts IO)))
    , prime      :: !(Ef ms IO ())
    }
type Module ms = Module' ms ms

instance Eq (Module' ts ms) where
  (==) (Module i _ _) (Module i' _ _) =
    let Key k1 = i
        Key k2 = i'
    in case reallyUnsafePtrEquality# i i' of
         1# -> True
         _  -> k1 == k2

-- Unlike services, modules are evented-context-local.
instance (IsModule' ts ms, MonadIO c, ms' <: ModuleBase)
  => With (Module' ts ms) (Ef ms IO) (Ef ms' c)
  where
    using_ c = do
      lv  <- get
      mi_ <- liftIO $ vaultLookup lv (key c)
      case mi_ of
        Nothing -> do
          sdn <- get
          let Key (_,i) = key c
          liftIO $ modifyVault lv $ \v ->
            case Map.lookup i v of
              Just as ->
                return (v,liftIO . runAs as)
              Nothing -> do
                buf <- newEvQueue
                startModule sdn lv buf c
                asModule :: As (Ef ms IO) <- unsafeConstructAs buf
                let new_v = Map.insert i (unsafeCoerce asModule) v
                return (new_v,liftIO . runAs asModule)
        Just as ->
          return (liftIO . runAs as)
    with_ c m = do
      run <- using_ c
      run m
    shutdown_ c = do
      lv <- get
      with_ c $ do
        buf <- get
        Shutdown sdn <- get
        publish sdn ()
        delay 0 $
          liftIO $ do
            killBuffer buf
            myThreadId >>= killThread
      liftIO $ vaultDelete lv (key c)

startModule :: forall ms' c ms ts.
             ( MonadIO c
             , IsModule' ts ms
             )
          => Shutdown
          -> Vault
          -> EvQueue
          -> Module' ts ms
          -> c ()
startModule (Shutdown sdn) lv rb Module {..} = do
  sdn' <- syndicate
  built <- liftIO $ build (   state rb
                          *:* state lv
                          *:* state (Shutdown sdn')
                          *:* Empty
                          )

  void $ liftIO $ forkIO $ void $ do
    (obj,_) <- Object built ! do
                 connect sdn (const (lift shutdownSelf))
                 prime
#ifdef __GHCJS__
    driverPrintExceptions ("Module exception (" ++ show key ++ "): ")
#else
    driver
#endif
      rb obj
