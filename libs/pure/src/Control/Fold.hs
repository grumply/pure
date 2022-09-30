{-# language TypeFamilies, OverloadedStrings, FlexibleContexts, PartialTypeSignatures, ScopedTypeVariables, RankNTypes, AllowAmbiguousTypes, ConstraintKinds, DataKinds, GADTs, MultiParamTypeClasses, FlexibleInstances #-}
module Control.Fold (fold,foldM) where

import Data.Default
import Data.Effect
import Data.Exists
import Data.View

import Control.Concurrent (MVar,newEmptyMVar,putMVar,takeMVar,forkIO,killThread,ThreadId)
import qualified Control.Exception as E (catch,mask,AsyncException(ThreadKilled))
import Control.Monad (forever)
import Data.Function (fix)
import Data.Unique (Unique,newUnique,hashUnique)
import GHC.Exts (inline)
import Prelude hiding (Read,(.),id)

{-# INLINE fold #-}
fold :: (Typeable eff, Typeable a) => (Effect eff => eff -> a -> a) -> (Effect eff => a) -> ((Effect eff, Exists a) => View) -> View
fold step initial = foldM (\eff a -> pure (step eff a)) (pure (initial,\_ -> pure ()))

{-# INLINE foldM #-}
foldM :: forall eff a. (Typeable eff, Typeable a) => (Effect eff => eff -> a -> IO a) -> (Effect eff => IO (a,a -> IO ())) -> ((Effect eff,Exists a) => View) -> View
foldM step initial v = ComponentView witness Nothing Control.Fold.app (\self -> using (upd self) (Fold step initial (`using` v)))

{-# INLINE app #-}
app :: Ref (Fold eff a) (a,a -> IO ()) -> Comp (Fold eff a) (a,a -> IO ())
app self =
  def
    { onConstruct = ask self >>= \(Fold _ initial _) -> initial >>= \(st,shutdown) -> pure (st,shutdown)
    , onUnmounted = get self >>= \(st,shutdown) -> shutdown st
    , render = \(Fold _ _ v) (st,_) -> v st
    }

{-# INLINE upd #-}
upd :: Ref (Fold eff a) (a,a -> IO ()) -> Handler eff
upd self = handler
  where
    handler = Handler $ \msg after ->
      modifyM self $ \(Fold step _ _) (st,shutdown) -> do
        st' <- step msg st
        pure ((st',shutdown),after)

data Fold eff a = Fold (eff -> a -> IO a) (IO (a,a -> IO ())) (a -> View)

