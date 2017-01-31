module Atomic.Cond where

import Data.Maybe

class Cond a where
  nil :: a

(?) :: Cond a => Bool -> a -> a -> a
(?) b t e = if b then t else e

cond :: Cond a => Bool -> a -> a
cond b t = b ? t $ nil

may :: Cond a => (b -> a) -> Maybe b -> a
may = maybe nil

ncond :: Cond a => Bool -> a -> a
ncond b = cond (not b)

isNil :: (Cond a, Eq a) => a -> Bool
isNil = (== nil)

notNil :: (Cond a, Eq a) => a -> Bool
notNil = (/= nil)

instance Cond [a] where
  nil = []

instance Cond (Maybe a) where
  nil = Nothing

