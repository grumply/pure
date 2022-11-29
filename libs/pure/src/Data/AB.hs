{-# language RankNTypes, FlexibleContexts, ScopedTypeVariables, AllowAmbiguousTypes, ConstraintKinds #-}
module Data.AB (AB,A,B,ab,a,b) where

import Data.Exists

type AB x = (Exists (A x),Exists (B x))

newtype A x = A x
newtype B x = B x

ab :: x -> x -> (AB x => r) -> r
ab x1 x2 f = with (A x1) (with (B x2) f)

a :: forall x r. AB x => (Exists x => r) -> r
a f = let A (x :: x) = it in with x f

b :: forall x r. AB x => (Exists x => r) -> r
b f = let B (x :: x) = it in with x f