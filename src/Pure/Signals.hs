{-# LANGUAGE CPP #-}
{-# LANGUAGE ConstraintKinds, DataKinds, TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Pure.Signals where

import Ef.Base

import Pure.Data
import Pure.Data.JSV
import Pure.Data.Txt (Txt,ToTxt,FromTxt)
import Pure.Types
import Pure.Lifted

#ifdef __GHCJS__
import GHCJS.Foreign.Callback as CB
import GHCJS.Marshal.Pure
#endif

import qualified Data.HashMap.Strict as Map

import Data.Coerce
import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce

data SyndicateVault where
  SyndicateVault ::
    IORef (Map.HashMap Txt (Syndicate Obj))
    -> SyndicateVault

{-# NOINLINE windowSyndicates__ #-}
windowSyndicates__ :: SyndicateVault
windowSyndicates__ = SyndicateVault (unsafePerformIO (newIORef Map.empty))

getWindowSyndicate :: (MonadIO c) => Txt -> c (Syndicate Obj)
getWindowSyndicate ev = liftIO $
  case windowSyndicates__ of
    SyndicateVault wn__ -> do
      nw :: Syndicate Obj <- syndicate
      enw <- atomicModifyIORef wn__ $ \ws ->
        case Map.lookup ev ws of
          Nothing -> (Map.insert ev (unsafeCoerce nw) ws,Left nw)
          Just nw -> (ws,Right $ unsafeCoerce nw)
      case enw of
        Left nw -> do
#ifdef __GHCJS__
          callback <- CB.syncCallback1 CB.ContinueAsync (publish nw . pFromJSVal)
          addEventListener (toJSV window) ev callback True
#endif
          return nw
        Right nw -> return nw

triggerWindowEvent :: (MonadIO c) => Txt -> Obj -> c ()
triggerWindowEvent ev e = do
  nw <- getWindowSyndicate ev
  publish nw e

onWindowSyndicate :: (MonadIO c, ms <: '[Evented])
                  => Txt
                  -> (Obj -> Ef '[Event Obj] (Ef ms c) ())
                  -> Ef ms c (IO ())
onWindowSyndicate en f = do
  nw <- getWindowSyndicate en
  connect nw f

{-# NOINLINE documentSyndicates__ #-}
documentSyndicates__ :: SyndicateVault
documentSyndicates__ = SyndicateVault (unsafePerformIO (newIORef Map.empty))

getDocumentSyndicate :: (MonadIO c) => Txt -> c (Syndicate Obj)
getDocumentSyndicate ev = liftIO $
  case windowSyndicates__ of
    SyndicateVault wn__ -> do
      nw :: Syndicate Obj <- syndicate
      enw <- atomicModifyIORef wn__ $ \ws ->
        case Map.lookup ev ws of
          Nothing -> (Map.insert ev (unsafeCoerce nw) ws,Left nw)
          Just nw -> (ws,Right $ unsafeCoerce nw)
      case enw of
        Left nw -> do
#ifdef __GHCJS__
          callback <- CB.syncCallback1 CB.ContinueAsync (publish nw . pFromJSVal)
          addEventListener (toJSV document) ev callback True
#endif
          return nw
        Right nw -> return nw

onDocumentSyndicate :: (MonadIO c, ms <: '[Evented])
                    => Txt
                    -> (Obj -> Ef '[Event Obj] (Ef ms c) ())
                    -> Ef ms c (IO ())
onDocumentSyndicate en f = do
  nw <- getDocumentSyndicate en
  connect nw f

{-# NOINLINE windowSyndicatesPreventDefault__ #-}
windowSyndicatesPreventDefault__ :: SyndicateVault
windowSyndicatesPreventDefault__ = SyndicateVault (unsafePerformIO (newIORef Map.empty))

getWindowSyndicatePreventDefault :: (MonadIO c) => Txt -> c (Syndicate Obj)
getWindowSyndicatePreventDefault ev = liftIO $
  case windowSyndicates__ of
    SyndicateVault wn__ -> do
      nw :: Syndicate Obj <- syndicate
      enw <- atomicModifyIORef wn__ $ \ws ->
        case Map.lookup ev ws of
          Nothing -> (Map.insert ev (unsafeCoerce nw) ws,Left nw)
          Just nw -> (ws,Right $ unsafeCoerce nw)
      case enw of
        Left nw -> do
#ifdef __GHCJS__
          callback <- CB.syncCallback1 CB.ContinueAsync $ \jsv -> do
                        preventDefault jsv
                        publish nw $ pFromJSVal jsv
          addEventListener (toJSV window) ev callback False
#endif
          return nw
        Right nw -> return nw

onWindowSyndicatePreventDefault :: (MonadIO c, ms <: '[Evented])
                                => Txt
                                -> (Obj -> Ef '[Event Obj] (Ef ms c) ())
                                -> Ef ms c (IO ())
onWindowSyndicatePreventDefault en f = do
  nw <- getWindowSyndicatePreventDefault en
  connect nw f

triggerWindowPreventDefaultEvent :: (MonadIO c) => Txt -> Obj -> c ()
triggerWindowPreventDefaultEvent ev e = do
  nw <- getWindowSyndicatePreventDefault ev
  publish nw e
