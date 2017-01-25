module Atomic.Cond where

import Data.Maybe

class Cond a where
  nil :: a

(?) :: Cond a => Bool -> a -> a -> a
(?) b t e = if b then t else e

cond :: Cond a => Bool -> a -> a
cond b t = b ? t $ nil

may :: Cond a => Maybe a -> a
may = fromMaybe nil

ncond :: Cond a => Bool -> a -> a
ncond b = cond (not b)

instance Cond [a] where
  nil = []

instance Cond (Maybe a) where
  nil = Nothing

