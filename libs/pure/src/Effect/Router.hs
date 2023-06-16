{-# LANGUAGE CPP, MultiParamTypeClasses, ScopedTypeVariables, OverloadedStrings, ViewPatterns, RankNTypes, FlexibleContexts, TypeApplications, FlexibleContexts, ConstraintKinds, AllowAmbiguousTypes #-}
module Effect.Router
  ( Router, Route
  , router, onRoute, subrouter, lref, current, url
  , module Data.Router
  ) where

import Control.Reader
import Control.State
import Data.Default
import Data.DOM
import Data.Events
import Data.HTML
import Data.Router
import qualified Data.Router as Router
import Data.Txt
import qualified Data.Txt as Txt
import Data.View hiding (On,ask)
import Effect.Async

data Route rt = Route Txt rt
type Router rt = Reader (Route rt)

url :: forall rt. Router rt => Txt
url = let Route u _ = ask :: Route rt in u

current :: Router rt => rt
current = let Route _ rt = ask in rt

onRoute :: Router rt => (rt -> IO ()) -> View -> View
onRoute f = async (f current)

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
      pure (Nothing,\_ -> stop) 

  stateWith (\_ -> pure) initialize $ do
    case ask of
      Nothing -> Null
      Just (sub,rt) -> reader (Route @rt sub rt) v

subrouter :: forall rt' rt. (Typeable rt, Router rt') => (forall a. Routing rt a) -> (Router rt => View) -> View
subrouter rtng v = do
  let
    initialize :: (Router rt', Modify (Maybe (Txt,rt))) => IO (Maybe (Txt,rt),Maybe (Txt,rt) -> IO ())
    initialize = do
      let Route path _ = ask :: Route rt'
      mrt <- Router.route (rtng >>= \x -> getPath >>= \p -> pure (p,x)) path
      pure (mrt :: Maybe (Txt,rt),\_ -> pure ())

  stateWith (\_ -> pure) initialize $ do
    case ask of
      Nothing -> Null
      Just (sub,rt) -> reader (Route @rt sub rt) v

lref :: HasFeatures a => Txt -> a -> a
lref t a = OnClickWith intercept (\_ -> goto t) (Href t a)
