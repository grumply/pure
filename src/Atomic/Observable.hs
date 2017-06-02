module Atomic.Observable where

import Ef.Base hiding (observe,watch)

type Observable o = State () (ObservableState o)

data ObservableState observable = Observable
  { observableState :: observable
  , observableUpdates :: Syndicate observable
  }

observable o = do
  onw <- syndicate
  return $ state (Observable o onw)

updateO :: (MonadIO c, '[Observable m] <: ms) => (m -> m) -> Ef ms c ()
updateO f = do
  m <- get
  let nm = f (observableState m)
  put $ m { observableState = nm }
  publish (observableUpdates m) nm

setO :: (MonadIO c, '[Observable m] <: ms) => m -> Ef ms c ()
setO nm = do
  m <- get
  put $ m { observableState = nm }
  publish (observableUpdates m) nm

getO :: (Monad c, '[Observable m] <: ms) => Ef ms c m
getO = do
  m <- get
  return (observableState m)

observe :: ( MonadIO c
           , MonadIO c'
           , With w (Ef ms c) IO
           , '[Evented] <: ms'
           , '[Observable m] <: ms
           )
        => w
        -> (m -> Ef '[Event m] (Ef ms' c') ())
        -> Ef ms' c' (Promise (IO ()))
observe c f = do
  buf <- get
  pr  <- promise
  with c $ do
    Observable {..} <- get
    sub <- subscribe observableUpdates (return buf)
    bhv <- listen sub f
    fulfill pr (stop bhv >> leaveSyndicate observableUpdates sub)
  return pr

observe' :: ( MonadIO c
            , MonadIO c'
            , With w (Ef ms c) IO
            , '[Evented] <: ms'
            , '[Observable m] <: ms
            )
        => w
        -> (m -> Ef ms' c' ())
        -> Ef ms' c' (Promise (IO ()))
observe' c f = do
  sp <- observe c (lift . f)
  with c getO >>= demandMaybe >>= mapM_ f
  return sp
