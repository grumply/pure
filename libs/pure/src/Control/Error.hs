{-# language ScopedTypeVariables, ConstraintKinds, FlexibleContexts, RankNTypes, DeriveFunctor, TypeApplications, AllowAmbiguousTypes #-}
module Control.Error (Throws,throw,catch,mask,pass,toError,toErrorWith,fromError,fromErrorWith) where

import Control.Producer (Producer,stream,yield,discard)
import Control.Fold (foldM)

import Data.Effect (Effect,(#))
import Data.Default (Default(..))
import Data.Typeable (Typeable)
import Data.View (View)

newtype Failure e = Failure e deriving Functor
type Throws e = Producer (Failure e)

{-# INLINE throw #-}
throw :: Throws e => e -> IO ()
throw = yield . Failure

{-# INLINE catch #-}
catch :: (e -> IO ()) -> (Throws e => View) -> View
catch f = stream (\(Failure e) -> f e) 

{-# INLINE mask #-}
mask :: forall e. (Throws e => View) -> View
mask = discard @(Failure e)

{-# INLINE pass #-}
pass :: (a -> b) -> (Throws a => x) -> (Throws b => x)
pass f = (fmap @Failure f #)

{-# INLINE toError #-}
toError :: forall e. (Producer e => View) -> (Throws e => View)
toError = toErrorWith @e id

{-# INLINE toErrorWith #-}
toErrorWith :: (e -> e') -> (Producer e => View) -> (Throws e' => View)
toErrorWith f = stream (yield . Failure . f) 

{-# INLINE fromError #-}
fromError :: forall e. (Throws e => View) -> (Producer e => View)
fromError = fromErrorWith @e id

{-# INLINE fromErrorWith #-}
fromErrorWith :: (e -> e') -> (Throws e => View) -> (Producer e' => View)
fromErrorWith f = stream (\(Failure e) -> yield (f e))