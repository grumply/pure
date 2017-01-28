{-# language CPP #-}
module Atomic.Router where

import Atomic.Route hiding (route)
import Atomic.Signals
import Atomic.Construct

import Data.Txt (Txt)
import Data.JSON

#ifdef __GHCJS__
import qualified GHCJS.DOM.Window as W
import qualified GHCJS.DOM.History as H
import qualified GHCJS.Marshal.Pure as M
#endif

import Ef.Base

import Unsafe.Coerce

data Router r
  = Router
      { router :: forall ms c. Code '[Route] (Code ms c) r
      , currentRoute :: Maybe r
      , routeNetwork :: Network r
      }

getRouter :: (MonadIO c, '[State () (Router r)] <: ms) => Code ms c (Code '[Route] (Code ms c) r)
getRouter = router <$> get

setRouter :: forall r ms c. (MonadIO c, '[State () (Router r)] <: ms) => Code '[Route] (Code ms c) r -> Code ms c ()
setRouter r = void $ modify (\rtr -> (rtr { router = unsafeCoerce r } :: Router r,()))

getRoute :: (MonadIO c, '[State () (Router r)] <: ms) => Code ms c (Maybe r)
getRoute = currentRoute <$> get

setRoute :: forall r ms c. (MonadIO c, '[State () (Router r)] <: ms) => Maybe r -> Code ms c ()
setRoute mr = void $ modify (\rtr -> (rtr { currentRoute = mr },()))

getRouteNetwork :: (MonadIO c, '[State () (Router r)] <: ms) => Code ms c (Network r)
getRouteNetwork = routeNetwork <$> get

setRouteNetwork :: forall r ms c. (MonadIO c, '[State () (Router r)] <: ms) => Network r -> Code ms c ()
setRouteNetwork rn = void $ modify (\rtr -> (rtr { routeNetwork = rn } :: Router r,()))

mkRouter :: forall ms c ts r.
            ( Monad c
            , Eq r
            , '[State () (Router r)] <. ts
            , '[State () (Router r)] <: ms
            , Delta (Modules ts) (Messages ms)
            )
         => Network r -> Code '[Route] (Code ms c) r -> State () (Router r) (Action ts c)
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
