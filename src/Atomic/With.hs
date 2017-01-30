{-# language FunctionalDependencies #-}
module Atomic.With where

import Ef.Base

import Atomic.Revent

data Shutdown = Shutdown (Network ())

class With a m n | a -> m where
  using_ :: a -> n (m b -> n (Promise b))
  with_ :: a -> m b -> n (Promise b)
  shutdown_ :: a -> n ()

using :: (With a m IO, MonadIO n) => a -> n (m b -> n (Promise b))
using a = fmap (fmap liftIO) $ liftIO $ using_ a

with :: (With a m IO, MonadIO n) => a -> m b -> n (Promise b)
with a m = liftIO $ with_ a m

(#) :: (With a m IO, MonadIO n) => a -> m b -> n (Promise b)
(#) = with

shutdown :: (With a m IO, MonadIO n) => a -> n ()
shutdown a = liftIO $ shutdown_ a

initialize :: (With a m IO, Monad m, MonadIO n)
           => a -> n ()
initialize a = void $ with a (return ())

connectWith w networkGetter f = do
  buf <- getReventBuffer
  p <- periodical
  Just s <- subscribe p f
  with w $ do
    nw <- networkGetter
    joinNetwork nw p buf
  return (s,p)

-- Do not use this for a self shutdown network; it will block!
onShutdown :: ( With a (Code ms IO) IO
              , MonadIO c
              , '[State () Shutdown] <: ms
              , '[Revent] <: ms'
              )
           => a
           -> Code ms IO ()
           -> Code ms' c (IO ())
onShutdown c ons = do
  buf <- getReventBuffer
  p <- periodical
  Just s <- subscribe p (const $ lift ons)
  Just leaveNW <- demandMaybe =<< with c (do
    Shutdown sdn <- get
    joinNetwork sdn p buf
    return (leaveNetwork sdn p))
  return (stop s >> leaveNW)

onSelfShutdown :: ( MonadIO c
                , '[Revent,State () Shutdown] <: ms
                )
             => Code ms c ()
             -> Code ms c (IO ())
onSelfShutdown ons = do
  buf <- getReventBuffer
  p <- periodical
  Just s <- subscribe p (const $ lift ons)
  Shutdown sdn <- get
  joinNetwork sdn p buf
  return (stop s >> leaveNetwork sdn p)

shutdownSelf :: (MonadIO c,'[State () Shutdown] <: ms) => Code ms c ()
shutdownSelf = do
  Shutdown sdn <- get
  syndicate sdn ()
