{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE JavaScriptFFI #-}
module Pure.Router (module Pure.Router, module Export) where

import Ef.Base

import Pure.Route as Export hiding (route)
import qualified Pure.Data as Export

import Pure.Data
import Pure.Types
import Pure.Route hiding (route)
import Pure.Signals

import Pure.Lifted

import Data.IORef

import qualified Pure.Data.Txt as T

import System.IO.Unsafe
import Unsafe.Coerce

data Router r
  = Router
      { router :: forall c. Narrative Route c r
      , currentRoute :: Maybe r
      , routeSyndicate :: Syndicate r
      }

getRouter :: (MonadIO c, ms <: '[State () (Router r)], e ~ Ef ms c)
          => e (Narrative Route e r)
getRouter = router <$> get

setRouter :: forall r ms c e. (MonadIO c, ms <: '[State () (Router r)], e ~ Ef ms c)
          => Narrative Route e r -> e ()
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
         => Syndicate r -> Narrative Route (Ef ms c) r -> rtr (Action ts c)
mkRouter nw rtr = state (Router (unsafeCoerce rtr) Nothing nw)

{-# NOINLINE popped #-}
popped = unsafePerformIO $ newIORef False

setPopped = writeIORef popped True
getPopped = readIORef popped

route :: MonadIO c => T.Txt -> c ()
route rt = do
  liftIO $ do
    setPopped
    pushState rt
    popState
#ifndef __GHCJS__
  triggerWindowPreventDefaultEvent "popstate" (mempty :: Obj)
#endif

pushPath :: MonadIO c => T.Txt -> c ()
pushPath pth = do
  liftIO setPopped
  liftIO $ pushState pth
#ifndef __GHCJS__
  let (pathname,search) = T.span (/= '?') pth
  liftIO $ do
    writeIORef pathname_ pathname
    writeIORef search_ search
#endif

{-# NOINLINE pathname_ #-}
pathname_ :: IORef T.Txt
pathname_ = unsafePerformIO (newIORef mempty)

{-# NOINLINE search_ #-}
search_ :: IORef T.Txt
search_ = unsafePerformIO (newIORef mempty)
