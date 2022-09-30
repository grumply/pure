{-# language ScopedTypeVariables, ConstraintKinds, FlexibleContexts, RankNTypes, DeriveFunctor, TypeApplications, AllowAmbiguousTypes #-}
module Control.Error (Error,throw,catch,mask,pass,toError,toErrorWith,fromError,fromErrorWith) where

import Control.Producer (Producer,stream,yield,discard)
import Control.Fold (foldM)

import Data.Effect (Effect,(#))
import Data.Default (Default(..))
import Data.Typeable (Typeable)
import Data.View (View)

newtype Failure e = Failure e deriving Functor
type Error e = Producer (Failure e)

{-# INLINE throw #-}
throw :: Error e => e -> IO ()
throw = yield . Failure

{-# INLINE catch #-}
catch :: (e -> IO ()) -> (Error e => View) -> View
catch f = stream (\(Failure e) -> f e) 

{-# INLINE mask #-}
mask :: forall e. (Error e => View) -> View
mask = discard @(Failure e)

{-# INLINE pass #-}
pass :: (a -> b) -> (Error a => x) -> (Error b => x)
pass f = (fmap @Failure f #)

{-# INLINE toError #-}
toError :: forall e. (Producer e => View) -> (Error e => View)
toError = toErrorWith @e id

{-# INLINE toErrorWith #-}
toErrorWith :: (e -> e') -> (Producer e => View) -> (Error e' => View)
toErrorWith f = stream (yield . Failure . f) 

{-# INLINE fromError #-}
fromError :: forall e. (Error e => View) -> (Producer e => View)
fromError = fromErrorWith @e id

{-# INLINE fromErrorWith #-}
fromErrorWith :: (e -> e') -> (Error e => View) -> (Producer e' => View)
fromErrorWith f = stream (\(Failure e) -> yield (f e))