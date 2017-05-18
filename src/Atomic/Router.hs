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
      { router :: forall ms c. Code '[Route] (Code ms c) r
      , currentRoute :: Maybe r
      , routeSyndicate :: Syndicate r
      }

getRouter :: (MonadIO c, '[State () (Router r)] <: ms) => Code ms c (Code '[Route] (Code ms c) r)
getRouter = router <$> get

setRouter :: forall r ms c. (MonadIO c, '[State () (Router r)] <: ms) => Code '[Route] (Code ms c) r -> Code ms c ()
setRouter r = void $ modify (\rtr -> (rtr { router = unsafeCoerce r } :: Router r,()))

getRoute :: (MonadIO c, '[State () (Router r)] <: ms) => Code ms c (Maybe r)
getRoute = currentRoute <$> get

setRoute :: forall r ms c. (MonadIO c, '[State () (Router r)] <: ms) => Maybe r -> Code ms c ()
setRoute mr = void $ modify (\rtr -> (rtr { currentRoute = mr },()))

getRouteSyndicate :: (MonadIO c, '[State () (Router r)] <: ms) => Code ms c (Syndicate r)
getRouteSyndicate = routeSyndicate <$> get

setRouteSyndicate :: forall r ms c. (MonadIO c, '[State () (Router r)] <: ms) => Syndicate r -> Code ms c ()
setRouteSyndicate rn = void $ modify (\rtr -> (rtr { routeSyndicate = rn } :: Router r,()))

mkRouter :: forall ms c ts r.
            ( Monad c
            , Eq r
            , '[State () (Router r)] <. ts
            , '[State () (Router r)] <: ms
            , Delta (Modules ts) (Messages ms)
            )
         => Syndicate r -> Code '[Route] (Code ms c) r -> State () (Router r) (Action ts c)
mkRouter nw rtr = state (Router (unsafeCoerce rtr) Nothing nw)

-- Note that this /should not/ be called within the first 500 milliseconds
-- of the application loading or it may be ignored; this is to work around
-- a browser disparity in the triggering of popstate events on load.
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

