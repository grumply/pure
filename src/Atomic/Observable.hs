module Atomic.Observable where

import Ef.Base hiding (observe,watch)

import Atomic.Revent
import Atomic.With

type Observable o = State () (ObservableState o)

data ObservableState observable = Observable
  { observableState :: observable
  , observableUpdates :: Network observable
  }

observable o = do
  onw <- network
  return $ state (Observable o onw)

updateO :: (MonadIO c, '[Observable m] <: ms) => (m -> m) -> Code ms c ()
updateO f = do
  m <- get
  let nm = f (observableState m)
  put $ m { observableState = nm }
  syndicate (observableUpdates m) nm

setO :: (MonadIO c, '[Observable m] <: ms) => m -> Code ms c ()
setO nm = do
  m <- get
  put $ m { observableState = nm }
  syndicate (observableUpdates m) nm

getO :: (Monad c, '[Observable m] <: ms) => Code ms c m
getO = do
  m <- get
  return (observableState m)

observe_ :: ( MonadIO c
         , MonadIO c'
         , With w (Code ms c) IO
         , '[Revent] <: ms'
         , '[Observable m] <: ms
         )
      => w
      -> (m -> Code '[Event m] (Code ms' c') ())
      -> Code ms' c' (IO ())
observe_ c f = do
  p <- periodical
  Just s <- subscribe p f
  buf <- getReventBuffer
  Just leaveNW <- demandMaybe =<< with c (do
    Observable _ us <- get
    joinNetwork us p buf
    return (leaveNetwork us p))
  return (stop s >> leaveNW)

observe :: ( MonadIO c
         , MonadIO c'
         , With w (Code ms c) IO
         , '[Revent] <: ms'
         , '[Observable m] <: ms
         )
      => w
      -> (m -> Code ms' c' ())
      -> Code ms' c' (IO ())
observe c f = observe_ c (lift . f)

observe' :: ( MonadIO c
          , MonadIO c'
          , With w (Code ms c) IO
          , '[Revent] <: ms'
          , '[Observable m] <: ms
          )
       => w
       -> (m -> Code ms' c' ())
       -> Code ms' c' (IO ())
observe' c f = do
  sp <- observe c f
  with c getO >>= demandMaybe >>= mapM_ f
  return sp
