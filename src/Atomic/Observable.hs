module Atomic.Observable where

import Ef.Base

import Atomic.Revent
import Atomic.With

data Observable observable = Observable
  { observableState :: observable
  , observableUpdates :: Network observable
  }

updateO :: (MonadIO c, '[State () (Observable m)] <: ms) => (m -> m) -> Code ms c ()
updateO f = do
  m <- get
  let nm = f (observableState m)
  put $ m { observableState = nm }
  syndicate (observableUpdates m) nm

setO :: (MonadIO c, '[State () (Observable m)] <: ms) => m -> Code ms c ()
setO nm = do
  m <- get
  put $ m { observableState = nm }
  syndicate (observableUpdates m) nm

observeO :: (Monad c, '[State () (Observable m)] <: ms) => Code ms c m
observeO = do
  m <- get
  return (observableState m)

watch :: ( MonadIO c
         , MonadIO c'
         , With w (Code ms c) (Code ms' c')
         , '[Revent] <: ms'
         , '[State () (Observable m)] <: ms
         )
      => w
      -> (m -> Code '[Event m] (Code ms' c') ())
      -> Code ms' c' (Subscription ms' c' m,Periodical ms' c' m)
watch c f = do
  p <- periodical
  Just s <- subscribe p f
  buf <- getReventBuffer
  with_ c $ do
    Observable _ us <- get
    joinNetwork us p buf
  return (s,p)

