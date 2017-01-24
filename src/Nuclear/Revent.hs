module Nuclear.Revent where

import Ef.Base

import Control.Exception
import Control.Concurrent

-- | Revent is the base trait for all services and components. It represents the
-- signal buffer upon which service and component event loops run.
data Revent k
  = Revent
      { reventBuffer :: (Signaled,k)
      }
  | GetReventBuffer (Signaled -> k)

instance Functor Revent where
  fmap f (Revent sk) = Revent (fmap f sk)
  fmap f (GetReventBuffer sk) = GetReventBuffer (fmap f sk)

-- | Get the local Revent buffer.
getReventBuffer :: (MonadIO c, '[Revent] <: ms)
                => Code ms c Signaled
getReventBuffer = Send (GetReventBuffer Return)

instance Delta Revent Revent where
  delta eval Revent{..} (GetReventBuffer sk) = delta eval reventBuffer sk

-- | Create a Revent Trait from a given `Signaled` buffer.
revent :: (MonadIO c, '[Revent] <. ts)
       => Signaled -> Revent (Action ts c)
revent buf = Revent (buf,return)

connect :: (MonadIO c,'[Revent] <: ms)
        => Network e
        -> (e -> Code '[Event e] (Code ms c) ())
        -> Code ms c (Subscription ms c e)
connect netwrk f = do
  buf <- getReventBuffer
  p <- periodical
  Just st <- subscribe p f
  joinNetwork netwrk p buf
  return st

-- | Using the local Revent Trait, delay execution of the given computation
-- by the given amount of microseconds. This is non-blocking.
delay :: forall ms c a.
         (MonadIO c, '[Revent] <: ms)
      => Int
      -> Code ms c a
      -> Code ms c (Behavior ms c (),Promise a)
delay uSeconds nar = do
  p <- promise
  sig :: Signal ms c () <- Ef.Base.construct
  buf <- getReventBuffer
  bt <- behavior sig $ \_ -> do
    lift $ nar >>= void . fulfill p
    Ef.Base.end
  if uSeconds == 0
    then buffer buf sig ()
    else liftIO $ void $ forkIO $ do
           threadDelay uSeconds
           buffer buf sig ()
  return (bt,p)

-- | Schedule a computation for execution by the local event loop. This
-- will run as soon as everything that is currently in the Revent buffer
-- has been handled.
schedule :: forall ms c a.
            (MonadIO c, '[Revent] <: ms)
         => Code ms c a
         -> Code ms c (Behavior ms c (),Promise a)
schedule = delay 0

asSelf :: forall ms c external.
          (MonadIO c, MonadIO external, '[Revent] <: ms)
       => Code ms c (As (Code ms c) external)
asSelf = do
  rb <- getReventBuffer
  sig <- runner
  return $ constructAs rb sig

onSuccess :: forall ms c status result.
             (MonadIO c, '[Revent] <: ms)
          => Process status result
          -> (result -> Code ms c ())
          -> Code ms c (ProcessListener status result)
onSuccess p f = do
  buf <- getReventBuffer
  sig :: Signal ms c result <- Ef.Base.construct
  behavior sig (lift . f)
  onComplete p (buffer buf sig)

onFailure :: forall ms c s r.
             (MonadIO c, '[Revent] <: ms)
          => Process s r
          -> (SomeException -> Code ms c ())
          -> Code ms c (ProcessListener s r)
onFailure p f = do
  buf <- getReventBuffer
  sig :: Signal ms c SomeException <- Ef.Base.construct
  behavior sig (lift . f)
  onAbort p (buffer buf sig)

onUpdate :: forall ms c status result.
            (MonadIO c, '[Revent] <: ms)
         => Process status result
         -> (status -> Code ms c ())
         -> Code ms c (ProcessListener status result)
onUpdate p f = do
  buf <- getReventBuffer
  sig :: Signal ms c status <- Ef.Base.construct
  behavior sig (lift . f)
  onNotify p (buffer buf sig)

onUpdate' :: forall ms c status result.
             (MonadIO c, '[Revent] <: ms)
          => Process status result
          -> (status -> Code ms c ())
          -> Code ms c (ProcessListener status result)
onUpdate' p f = do
  buf <- getReventBuffer
  sig :: Signal ms c status <- Ef.Base.construct
  behavior sig (lift . f)
  onNotify' p (buffer buf sig)
