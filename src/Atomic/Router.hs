module Atomic.Router where

import Atomic.Route

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
