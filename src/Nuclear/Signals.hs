{-# language CPP #-}
{-# language ConstraintKinds #-}
module Nuclear.Signals where

import Ef.Base

import Nuclear.Revent
import Nuclear.Atom (Win,Doc,getDocument,getWindow)

#ifdef __GHCJS__
import qualified GHCJS.DOM.EventM as Ev
import qualified GHCJS.DOM.Types as T
import qualified GHCJS.DOM.EventTargetClosures as ETC
#endif

import qualified Data.HashMap.Strict as Map

import Data.JSText

import Data.Coerce
import Data.IORef
import System.IO.Unsafe
import Unsafe.Coerce

type EVName k e =
#ifdef __GHCJS__
  ETC.EventName k e
#else
  JSText
#endif

type IsEv e =
#ifdef __GHCJS__
  T.IsEvent e
#else
  Num Int
#endif

data NetworkVault =
  forall a. NetworkVault (IORef (Map.HashMap JSText (Network a)))

{-# NOINLINE windowNetworks__ #-}
windowNetworks__ :: NetworkVault
windowNetworks__ = NetworkVault (unsafePerformIO (newIORef Map.empty))

getWindowNetwork :: forall e c.
                   ( IsEv e
                   , MonadIO c
                   )
                => EVName Win e
                -> c (Network e)
getWindowNetwork
#ifdef __GHCJS__
  en@(ETC.EventName doms)
#else
  e
#endif
  = liftIO $
      case windowNetworks__ of
        NetworkVault wn__ -> do
#ifdef __GHCJS__
          let e = unsafeCoerce doms
#endif
          nw :: Network e <- network
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
                syndicate nw e
#endif
              return nw
            Right nw -> return nw

triggerWindowEvent :: (IsEv e, MonadIO c) => EVName Win e -> e -> c ()
triggerWindowEvent ev e = do
  nw <- getWindowNetwork ev
  syndicate nw e

onWindowNetwork :: ( IsEv e
                   , MonadIO c
                   , '[Revent] <: ms
                   )
                => EVName Win e
                -> (e -> Code '[Event e] (Code ms c) ())
                -> Code ms c (Subscription ms c e,Periodical ms c e)
onWindowNetwork en f = do
  rb <- getReventBuffer
  nw <- getWindowNetwork en
  p <- periodical
  Just s <- subscribe p f
  joinNetwork nw p rb
  return (s,p)

{-# NOINLINE documentNetworks__ #-}
documentNetworks__ :: NetworkVault
documentNetworks__ = NetworkVault (unsafePerformIO (newIORef Map.empty))

getDocumentNetwork :: forall e c.
                      ( IsEv e
                      , MonadIO c
                      )
                    => EVName Doc e
                    -> c (Network e)
getDocumentNetwork
#ifdef __GHCJS__
  en@(ETC.EventName doms)
#else
  e
#endif
  = liftIO $
      case documentNetworks__ of
        NetworkVault dn__ -> do
#ifdef __GHCJS__
          let e = unsafeCoerce doms
#endif
          nw <- network
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
                syndicate nw e
#endif
              return nw
            Right nw -> return nw

onDocumentNetwork :: ( IsEv e
                     , MonadIO c
                     , '[Revent] <: ms
                     )
                  => EVName Doc e
                  -> (e -> Code '[Event e] (Code ms c) ())
                  -> Code ms c (Subscription ms c e,Periodical ms c e)
onDocumentNetwork en f = do
  rb <- getReventBuffer
  nw <- getDocumentNetwork en
  p <- periodical
  Just s <- subscribe p f
  joinNetwork nw p rb
  return (s,p)

getWindowSignalPreventDefault :: (IsEv e, MonadIO c, '[Revent] <: ms)
                              => EVName Win e -> Code ms c (Signal ms c e)
getWindowSignalPreventDefault
#ifdef __GHCJS__
    en@(ETC.EventName doms)
#else
    e
#endif
  = do
      gb <- getReventBuffer
      win <- getWindow
      sig <- Ef.Base.construct
#ifdef __GHCJS__
      liftIO $ Ev.on win en $ do
        Ev.preventDefault
        e <- Ev.event
        lift $ buffer gb sig e
#endif
      return sig
