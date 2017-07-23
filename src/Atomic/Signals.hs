{-# language CPP #-}
{-# language ConstraintKinds #-}
module Atomic.Signals where

import Ef.Base

import Atomic.Component (Win,Doc,getDocument,getWindow)

#ifdef __GHCJS__
import qualified GHCJS.DOM.EventM as Ev
import qualified GHCJS.DOM.Types as T
import qualified GHCJS.DOM.EventTargetClosures as ETC
#endif

import qualified Data.HashMap.Strict as Map

import Data.Txt

import Data.Coerce
import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce

type EVName k e =
#ifdef __GHCJS__
  ETC.EventName k e
#else
  Txt
#endif

type IsEv e =
#ifdef __GHCJS__
  T.IsEvent e
#else
  Num Int
#endif

data SyndicateVault where
  SyndicateVault :: IORef (Map.HashMap Txt (Syndicate a)) -> SyndicateVault

{-# NOINLINE windowSyndicates__ #-}
windowSyndicates__ :: SyndicateVault
windowSyndicates__ = SyndicateVault (unsafePerformIO (newIORef Map.empty))

getWindowSyndicate :: (IsEv e, MonadIO c) => EVName Win e -> c (Syndicate e)
getWindowSyndicate
#ifdef __GHCJS__
  en@(ETC.EventName doms)
#else
  e
#endif
  = liftIO $
      case windowSyndicates__ of
        SyndicateVault wn__ -> do
#ifdef __GHCJS__
          let e = unsafeCoerce doms
#endif
          nw :: Syndicate e <- syndicate
          enw <- atomicModifyIORef wn__ $ \ws ->
            case Map.lookup e ws of
              Nothing -> (Map.insert e (unsafeCoerce nw) ws,Left nw)
              Just nw -> (ws,Right $ unsafeCoerce nw)
          case enw of
            Left nw -> do
              win <- getWindow
#ifdef __GHCJS__
              Ev.on win en $ do
                e <- Ev.event
                publish nw e
#endif
              return nw
            Right nw -> return nw

triggerWindowEvent :: (IsEv e, MonadIO c) => EVName Win e -> e -> c ()
triggerWindowEvent ev e = do
  nw <- getWindowSyndicate ev
  publish nw e

onWindowSyndicate :: (IsEv e, MonadIO c, ms <: '[Evented])
                  => EVName Win e
                  -> (e -> Ef '[Event e] (Ef ms c) ())
                  -> Ef ms c (IO ())
onWindowSyndicate en f = do
  nw <- getWindowSyndicate en
  connect nw f

{-# NOINLINE documentSyndicates__ #-}
documentSyndicates__ :: SyndicateVault
documentSyndicates__ = SyndicateVault (unsafePerformIO (newIORef Map.empty))

getDocumentSyndicate :: (IsEv e, MonadIO c) => EVName Doc e -> c (Syndicate e)
getDocumentSyndicate
#ifdef __GHCJS__
  en@(ETC.EventName doms)
#else
  e
#endif
  = liftIO $
      case documentSyndicates__ of
        SyndicateVault dn__ -> do
#ifdef __GHCJS__
          let e = unsafeCoerce doms
#endif
          nw <- syndicate
          enw <- atomicModifyIORef dn__ $ \ws ->
            case Map.lookup e ws of
              Nothing -> (Map.insert e (unsafeCoerce nw) ws,Left nw)
              Just nw -> (ws,Right $ unsafeCoerce nw)
          case enw of
            Left nw -> do
              doc <- getDocument
#ifdef __GHCJS__
              Ev.on doc en $ do
                e <- Ev.event
                publish nw e
#endif
              return nw
            Right nw -> return nw

onDocumentSyndicate :: (IsEv e, MonadIO c, ms <: '[Evented])
                    => EVName Doc e
                    -> (e -> Ef '[Event e] (Ef ms c) ())
                    -> Ef ms c (IO ())
onDocumentSyndicate en f = do
  nw <- getDocumentSyndicate en
  connect nw f

{-# NOINLINE windowSyndicatesPreventDefault__ #-}
windowSyndicatesPreventDefault__ :: SyndicateVault
windowSyndicatesPreventDefault__ = SyndicateVault (unsafePerformIO (newIORef Map.empty))

getWindowSyndicatePreventDefault :: (IsEv e, MonadIO c) => EVName Win e -> c (Syndicate e)
getWindowSyndicatePreventDefault
#ifdef __GHCJS__
  en@(ETC.EventName doms)
#else
  e
#endif
  = liftIO $
      case windowSyndicatesPreventDefault__ of
        SyndicateVault wn__ -> do
#ifdef __GHCJS__
          let e = unsafeCoerce doms
#endif
          nw :: Syndicate e <- syndicate
          enw <- atomicModifyIORef wn__ $ \ws ->
            case Map.lookup e ws of
              Nothing -> (Map.insert e (unsafeCoerce nw) ws,Left nw)
              Just nw -> (ws,Right $ unsafeCoerce nw)
          case enw of
            Left nw -> do
              win <- getWindow
#ifdef __GHCJS__
              Ev.on win en $ do
                Ev.preventDefault
                e <- Ev.event
                publish nw e
#endif
              return nw
            Right nw -> return nw

onWindowSyndicatePreventDefault :: (IsEv e, MonadIO c, ms <: '[Evented])
                                => EVName Win e
                                -> (e -> Ef '[Event e] (Ef ms c) ())
                                -> Ef ms c (IO ())
onWindowSyndicatePreventDefault en f = do
  nw <- getWindowSyndicatePreventDefault en
  connect nw f

triggerWindowPreventDefaultEvent :: (IsEv e, MonadIO c) => EVName Win e -> e -> c ()
triggerWindowPreventDefaultEvent ev e = do
  nw <- getWindowSyndicatePreventDefault ev
  publish nw e
