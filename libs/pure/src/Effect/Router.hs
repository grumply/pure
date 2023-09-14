{-# LANGUAGE CPP, MultiParamTypeClasses, ScopedTypeVariables, OverloadedStrings, RankNTypes, FlexibleContexts, TypeApplications, ConstraintKinds, AllowAmbiguousTypes, TypeOperators, DataKinds, TypeFamilies #-}
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
import Data.Type.Equality
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

subrouter :: forall from to. ((from == to) ~ False, Typeable to) => (forall a. Routing to a) -> (Router to => View) -> (Router from => View)
subrouter rtng v = do
  let
    initialize :: (Router from, Modify (Maybe (Txt,to))) => IO (Maybe (Txt,to),Maybe (Txt,to) -> IO ())
    initialize = do
      let Route path _ = it :: Route from
      mrt <- Router.route (rtng >>= \x -> getPath >>= \p -> pure (p,x)) path
      pure (mrt :: Maybe (Txt,to),\_ -> pure ())

  stateWith (const pure) initialize $ do
    case it of
      Nothing -> Null
      Just (sub,rt) -> with (Route @to sub rt) v

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

subrouter' :: forall from to. ((from == to) ~ False, Typeable to) => (forall a. Routing to a) -> (Router to => View) -> (Router from => View)
subrouter' rtng v = do
  let
    initialize :: (Router from, Modify (Maybe (Txt,to))) => IO (Maybe (Txt,to),Maybe (Txt,to) -> IO ())
    initialize = do
      let Route path _ = it :: Route from
      mrt <- Router.route (rtng >>= \x -> getPath >>= \p -> pure (p,x)) path
      pure (mrt :: Maybe (Txt,to),\_ -> pure ())

  stateWith' (const pure) initialize $ do
    case it of
      Nothing -> Null
      Just (sub,rt) -> with (Route @to sub rt) v

type Routes ctx = forall a. Routing (ctx |- View) a

routed :: forall ctx. Typeable ctx => Routes ctx -> (Router (ctx |- View) => View) -> (ctx => View)
routed = router

subrouted :: forall from to. ((from == to) ~ False, Typeable to) => Routes to -> (Router (to |- View) => View) -> ((Router (from |- View), from, to) => View)
subrouted = subrouter @(from |- View) @(to |- View)

routed' :: forall ctx. Typeable ctx => Routes ctx -> (Router (ctx |- View) => View) -> (ctx => View)
routed' = router' 

subrouted' :: forall from to. ((from == to) ~ False, Typeable to) => Routes to -> (Router (to |- View) => View) -> ((Router (from |- View), from, to) => View)
subrouted' = subrouter' @(from |- View) @(to |- View)

page :: forall ctx. Router (ctx |- View) => (ctx => View)
page = weak (current :: ctx |- View) (prove (current :: ctx |- View))

routes_ :: Routes () -> View
routes_ rs = routed @() rs (page @())

route :: forall ctx. (ctx => View) -> (forall a. Routing (ctx |- View) a)
route v = dispatch (proof v)

lref :: Txt -> View -> View
lref t a = OnClickWith intercept (\_ -> goto t) (Href t a)
