{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language MagicHash #-}
{-# language CPP #-}
module Atomic.Module (module Atomic.Module) where

import Ef.Base hiding (Module,Client,Server)

import Atomic.Key
import Atomic.Observable
import Atomic.Revent
import Atomic.Vault
import Atomic.With

import Control.Concurrent
import Data.IORef
import GHC.Prim

import Data.HashMap.Strict as Map hiding ((!))

import Unsafe.Coerce

type ModuleBase = '[Revent, State () Vault, State () Shutdown]

type IsModule' ts ms = (ModuleBase <: ms, ModuleBase <. ts, Delta (Modules ts) (Messages ms))
type IsModule ms = IsModule' ms ms

data Module' ts ms
  = Module
    { key        :: !(Key (Code ms IO `As` IO))
    , build      :: !(Modules ModuleBase (Action ts IO) -> IO (Modules ts (Action ts IO)))
    , prime      :: !(Code ms IO ())
    }
type Module ms = Module' ms ms

instance Eq (Module' ts ms) where
  (==) (Module i _ _) (Module i' _ _) =
    let Key k1 = i
        Key k2 = i'
    in case reallyUnsafePtrEquality# i i' of
         1# -> True
         _  -> k1 == k2

instance (IsModule' ts ms, MonadIO c, '[Revent,State () Vault] <: ms')
  => With (Module' ts ms) (Code ms IO) (Code ms' c)
  where
    using_ c = do
      -- NOTE: this atomic mvar modification is unnecessary for local components, no?
      lv <- get
      mi_ <- vaultLookup lv (key c)
      case mi_ of
        Nothing -> do
          let Key (_,i) = key c
          modifyVault lv $ \v ->
            case Map.lookup i v of
              Just as ->
                return (v,liftIO . runAs as)
              Nothing -> do
                rb <- newSignalBuffer
                sig :: Signal ms IO (Code ms IO ()) <- runner
                startModule lv rb c
                let asModule :: Code ms IO `As` IO
                    asModule = constructAs rb sig
                    new_v = Map.insert i (unsafeCoerce asModule) v
                return (new_v,liftIO . runAs asModule)
        Just as ->
          return (liftIO . runAs as)
    with_ c m = do
      run <- using_ c
      run m
    shutdown_ c = do
      lv <- get
      with_ c $ do
        buf <- getReventBuffer
        Shutdown sdn <- get
        syndicate sdn ()
        delay 0 $
          liftIO $ do
            killBuffer buf
            myThreadId >>= killThread
      vaultDelete lv (key c)

startModule :: forall ms' c ms ts.
             ( MonadIO c
             , IsModule' ts ms
             )
          => Vault
          -> Signaled
          -> Module' ts ms
          -> c ()
startModule lv rb Module {..} = do
  sdn <- network
  udn :: Network m <- network
  built <- liftIO $ build (revent rb
                          *:* state lv
                          *:* state (Shutdown sdn)
                          *:* Empty
                          )

  void $ liftIO $ forkIO $ void $ do
    (obj,_) <- Object built ! prime
#ifdef __GHCJS__
    driverPrintExceptions
      ("Module "
      ++ show key
      ++ " blocked in eventloop; likely caused by cyclic with calls. The standard solution is a 'delay'ed call to 'demand'. "
      )
#else
    driver
#endif
      rb obj

