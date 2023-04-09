{-# language ConstraintKinds, TypeApplications, RankNTypes, ScopedTypeVariables, FlexibleContexts, AllowAmbiguousTypes, DerivingVia, DeriveFunctor #-}
module Control.Writer (Writer,tell,listen,silence,writer,translate,toWriter,toWriterWith) where

import Control.Fold (fold)
import Control.Producer (Producer,stream,yield,discard)
import Control.State (State,Modify,modify,get,ignore,zoom,state)
import Data.Coerce (coerce)
import Data.Effect (Effect,(#))
import Data.Exists (Exists,it,using)
import Data.Typeable (Typeable)
import Data.View (View)

newtype Log a = Log a deriving stock Functor deriving (Semigroup,Monoid) via a

type Writer a = State (Log a)

{-# INLINE tell #-}
tell :: (Semigroup a, Modify (Log a)) => a -> IO ()
tell a = modify (<> Log a)

-- Unlike monadic writers, where discretization of computation is achieved
-- through wrapping and composition, discretization of projected computational
-- views is achieved through nesting. It is therefore necessary to expose a
-- method of inspecting the log to achieve a dependence between the results of
-- a writer and its nested contexts. 
--
-- Without listen, writer x == silence x.
{-# INLINE listen #-}
listen :: Exists (Log a) => a
listen = let Log a = it in a

{-# INLINE silence #-}
silence :: forall a. (Modify (Log a) => View) -> View
silence = ignore @(Log a)

{-# INLINE writer #-}
writer :: forall a. Typeable a => Monoid a => (Writer a => View) -> View
writer = state (mempty :: Log a)

{-# INLINE translate #-}
translate :: forall a b x. (a -> b) -> (b -> a -> a) -> (Writer b => x) -> (Writer a => x)
translate f g = zoom f' g'
  where
    f' :: Log a -> Log b
    f' = coerce f

    g' :: Log b -> Log a -> Log a
    g' = coerce g

{-# INLINE toWriter #-}
toWriter :: forall a. Semigroup a => (Producer a => View) -> (Modify (Log a) => View)
toWriter = toWriterWith @a id

{-# INLINE toWriterWith #-}
toWriterWith :: Semigroup b => (a -> b) -> (Producer a => View) -> (Modify (Log b) => View)
toWriterWith f = stream (tell . f)
