{-# language UndecidableInstances #-}
module Atomic.Cond where

import Control.Lens.Empty
import Control.Lens.Prism

import Data.Maybe
import Data.Txt

class Cond a where
  nil :: a
  default nil :: (Monoid a, Eq a) => a
  nil = mempty

infix 9 ?
(?) :: (Cond x, Eq x) => x -> a -> a -> a
(?) x t e = if notNil x then t else e

infix 9 ?&
(?&) :: (Cond x, Eq x) => x -> a -> a -> a
(?&) x t e = x ? e $ t

cond :: (Cond x, Eq x, Cond a) => x -> a -> a
cond b t = b ? t $ nil

may :: Cond a => (b -> a) -> Maybe b -> a
may = maybe nil

ncond :: (Cond x, Eq x, Cond a) => x -> a -> a
ncond b = cond (notNil b)

isNil :: (Cond a, Eq a) => a -> Bool
isNil = (== nil)

notNil :: (Cond a, Eq a) => a -> Bool
notNil = (/= nil)

instance Cond [a] where
  nil = []

instance Cond (Maybe a) where
  nil = Nothing

instance Cond Bool where
  nil = False

instance Cond Txt

instance Cond Int where
  nil = 0

instance Cond Integer where
  nil = 0

instance Cond Double where
  nil = 0

instance Cond Float where
  nil = 0

instance Cond (a -> a) where
+  nil = id
