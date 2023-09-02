{-# LANGUAGE CPP, MultiParamTypeClasses, ScopedTypeVariables, OverloadedStrings, RankNTypes, FlexibleContexts, TypeApplications, ConstraintKinds, AllowAmbiguousTypes, TypeOperators #-}
module Effect.Router
  ( Router, Route, Routes
  , routes_, Effect.Router.route, page
  , onRoute, lref, current, url
  , routed, subrouted, subrouter, router
  , routed', subrouted', subrouter', router'
  , module Export
  ) where

import Data.Default
import Data.DOM
import Data.Events
import Data.HTML
import Data.Router
import qualified Data.Router as Export hiding (route)
import qualified Data.Router as Router
import Data.Txt
import qualified Data.Txt as Txt
import Data.View hiding (On)

data Route rt = Route Txt rt
type Router rt = Exists (Route rt)

url :: forall rt. Router rt => Txt
url = let Route u _ = it :: Route rt in u

current :: Router rt => rt
current = let Route _ rt = it in rt

onRoute :: Router rt => (rt -> IO ()) -> View -> View
onRoute f = lazy (f current)

router :: forall rt. Typeable rt => (forall a. Routing rt a) -> (Router rt => View) -> View
router rtng v = do
  let
    runRouter :: Modify (Maybe (Txt,rt)) => IO ()
    runRouter = do
      pn  <- getPathname
      qps <- getSearch
      mrt <- Router.route (rtng >>= \x -> getPath >>= \p -> pure (p,x)) (pn <> qps)
      put (mrt :: Maybe (Txt,rt))

    initialize :: Modify (Maybe (Txt,rt)) => IO (Maybe (Txt,rt),Maybe (Txt,rt) -> IO ())
    initialize = do
      w <- getWindow
      stop <- onRaw (toNode w) "popstate" def (\_ _ -> runRouter)
      runRouter
      pure (Nothing,const stop)

  stateWith (const pure) initialize $ do
    case it of
      Nothing -> Null
      Just (sub,rt) -> with (Route @rt sub rt) v

subrouter :: forall rt' rt. (Typeable rt, Router rt') => (forall a. Routing rt a) -> (Router rt => View) -> View
subrouter rtng v = do
  let
    initialize :: (Router rt', Modify (Maybe (Txt,rt))) => IO (Maybe (Txt,rt),Maybe (Txt,rt) -> IO ())
    initialize = do
      let Route path _ = it :: Route rt'
      mrt <- Router.route (rtng >>= \x -> getPath >>= \p -> pure (p,x)) path
      pure (mrt :: Maybe (Txt,rt),\_ -> pure ())

  stateWith (const pure) initialize $ do
    case it of
      Nothing -> Null
      Just (sub,rt) -> with (Route @rt sub rt) v

router' :: forall rt. Typeable rt => (forall a. Routing rt a) -> (Router rt => View) -> View
router' rtng v = do
  let
    runRouter :: Modify (Maybe (Txt,rt)) => IO ()
    runRouter = do
      pn  <- getPathname
      qps <- getSearch
      mrt <- Router.route (rtng >>= \x -> getPath >>= \p -> pure (p,x)) (pn <> qps)
      put (mrt :: Maybe (Txt,rt))

    initialize :: Modify (Maybe (Txt,rt)) => IO (Maybe (Txt,rt),Maybe (Txt,rt) -> IO ())
    initialize = do
      w <- getWindow
      stop <- onRaw (toNode w) "popstate" def (\_ _ -> runRouter)
      runRouter
      pure (Nothing,const stop)

  stateWith' (const pure) initialize $ do
    case it of
      Nothing -> Null
      Just (sub,rt) -> with (Route @rt sub rt) v

subrouter' :: forall rt' rt. (Typeable rt, Router rt') => (forall a. Routing rt a) -> (Router rt => View) -> View
subrouter' rtng v = do
  let
    initialize :: (Router rt', Modify (Maybe (Txt,rt))) => IO (Maybe (Txt,rt),Maybe (Txt,rt) -> IO ())
    initialize = do
      let Route path _ = it :: Route rt'
      mrt <- Router.route (rtng >>= \x -> getPath >>= \p -> pure (p,x)) path
      pure (mrt :: Maybe (Txt,rt),\_ -> pure ())

  stateWith' (const pure) initialize $ do
    case it of
      Nothing -> Null
      Just (sub,rt) -> with (Route @rt sub rt) v

type Routes ctx = forall a. Routing (ctx |- View) a

routed :: forall ctx. Typeable ctx => Routes ctx -> (Exists (ctx |- View) => View) -> (ctx => View)
routed rs v = router rs (let (d :: ctx |- View) = current in with d v)

subrouted :: forall rt ctx. (Router rt, Typeable ctx) => Routes ctx -> (Exists (ctx |- View) => View) -> (ctx => View)
subrouted rs v = subrouter @rt rs (let (d :: ctx |- View) = current in with d v)

routed' :: forall ctx. Typeable ctx => Routes ctx -> (Exists (ctx |- View) => View) -> (ctx => View)
routed' rs v = router' rs (let (d :: ctx |- View) = current in with d v)

subrouted' :: forall rt ctx. (Router rt, Typeable ctx) => Routes ctx -> (Exists (ctx |- View) => View) -> (ctx => View)
subrouted' rs v = subrouter' @rt rs (let (d :: ctx |- View) = current in with d v)

page :: forall ctx. Exists (ctx |- View) => (ctx => View)
page = weak (it :: ctx |- View) (prove (it :: ctx |- View))

routes_ :: Routes () -> View
routes_ rs = routed @() rs (page @())

route :: forall ctx. (ctx |- View) -> (forall a. Routing (ctx |- View) a)
route = dispatch 

lref :: Txt -> View -> View
lref t a = OnClickWith intercept (\_ -> goto t) (Href t a)
