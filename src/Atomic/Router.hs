{-# language CPP #-}
{-# language OverloadedStrings #-}
module Atomic.Router where

import Atomic.Route hiding (route)
import Atomic.Signals

import Data.IORef

import Data.Txt (Txt)
import qualified Data.Txt as Txt
import Data.JSON

#ifdef __GHCJS__
import qualified GHCJS.DOM.Window as W
import qualified GHCJS.DOM.History as H
import qualified GHCJS.Marshal.Pure as M
import qualified GHCJS.DOM.Location as L
import qualified GHCJS.DOM.EventM as Ev
import qualified GHCJS.DOM.EventTargetClosures as Ev
#endif

import Atomic.Component

import Ef.Base

import System.IO.Unsafe
import Unsafe.Coerce

data Router r
  = Router
      { router :: forall ms c. Ef '[Route] (Ef ms c) r
      , currentRoute :: Maybe r
      , routeSyndicate :: Syndicate r
      }

getRouter :: (MonadIO c, ms <: '[State () (Router r)], e ~ Ef ms c)
          => e (Ef '[Route] e r)
getRouter = router <$> get

setRouter :: forall r ms c e. (MonadIO c, ms <: '[State () (Router r)], e ~ Ef ms c)
          => Ef '[Route] e r -> e ()
setRouter r = void $ modify (\rtr -> (rtr { router = unsafeCoerce r } :: Router r,()))

getRoute :: (MonadIO c, ms <: '[State () (Router r)], e ~ Ef ms c)
         => e (Maybe r)
getRoute = currentRoute <$> get

setRoute :: (MonadIO c, ms <: '[State () (Router r)], e ~ Ef ms c)
         => Maybe r -> Ef ms c ()
setRoute mr = void $ modify (\rtr -> (rtr { currentRoute = mr },()))

getRouteSyndicate :: (MonadIO c, ms <: '[State () (Router r)], e ~ Ef ms c)
                  => e (Syndicate r)
getRouteSyndicate = routeSyndicate <$> get

setRouteSyndicate :: forall r ms c e. (MonadIO c, ms <: '[State () (Router r)], e ~ Ef ms c)
                  => Syndicate r -> e ()
setRouteSyndicate rn = void $ modify (\rtr -> (rtr { routeSyndicate = rn } :: Router r,()))

mkRouter :: (Monad c, Eq r, rtr ~ State () (Router r), ts <. '[rtr], ms <: '[rtr], ts <=> ms)
         => Syndicate r -> Ef '[Route] (Ef ms c) r -> rtr (Action ts c)
mkRouter nw rtr = state (Router (unsafeCoerce rtr) Nothing nw)

-- Note that `route` /should not/ be called within the first 500 milliseconds
-- of the application loading or it may be ignored. This is to work around
-- a browser disparity in the triggering of popstate events on page load.
route :: MonadIO c => Txt -> c ()
route rt = do
  pushPath rt
#ifdef __GHCJS__
  liftIO triggerPopstate_js
#else
  triggerWindowPreventDefaultEvent popstate (mempty :: Obj)
#endif

pushPath :: MonadIO c => Txt -> c ()
pushPath pth = do
  win <- getWindow
#ifdef __GHCJS__
  liftIO $ do
    Just hist <- W.getHistory win
    H.pushState hist (M.pToJSVal (0 :: Int)) (mempty :: Txt) pth
#else
  let (pathname,search) = Txt.span (/= '?') pth
  liftIO $ do
    writeIORef pathname_ pathname
    writeIORef search_ search
#endif

popstate :: EVName Win Obj
popstate =
#ifdef __GHCJS__
  (Ev.unsafeEventName "popstate" :: EVName Win Obj)
#else
  "popstate"
#endif

#ifndef __GHCJS__
{-# NOINLINE pathname_ #-}
pathname_ :: IORef Txt
pathname_ = unsafePerformIO (newIORef mempty)
#endif

#ifndef __GHCJS__
{-# NOINLINE search_ #-}
search_ :: IORef Txt
search_ = unsafePerformIO (newIORef mempty)
#endif

getPathname :: MonadIO c => c Txt
getPathname = do
  loc <- getLocation
#ifdef __GHCJS__
  L.getPathname loc
#else
  liftIO $ readIORef pathname_
#endif

getSearch :: MonadIO c => c Txt
getSearch = do
  loc <- getLocation
#ifdef __GHCJS__
  L.getSearch loc
#else
  liftIO $ readIORef search_
#endif
