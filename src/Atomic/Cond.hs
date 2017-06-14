{-# language UndecidableInstances #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language OverloadedStrings #-}
module Atomic.Cond where

import Ef

import Data.Maybe
import Data.Txt

class Cond a where
  nil :: a
  default nil :: (Monoid a, Eq a) => a
  nil = mempty

  isNil :: a -> Bool
  default isNil :: (Eq a) => a -> Bool
  isNil = (== nil)

  notNil :: a -> Bool
  notNil = not . isNil

infix 9 ?
(?) :: (Cond x) => x -> a -> a -> a
(?) x t e = if notNil x then t else e

infix 9 ?&
(?&) :: (Cond x) => x -> a -> a -> a
(?&) x t e = x ? e $ t

may :: Cond a => (b -> a) -> Maybe b -> a
may = maybe nil

cond :: (Cond x, Cond a) => x -> a -> a
cond b t = b ? t $ nil

instance Cond (Ef ms IO ()) where
  nil = Return ()
  isNil (Return ()) = True
  isNil _ = False

instance Cond [a] where
  nil = []
  isNil [] = True
  isNil _ = False

instance Cond (Maybe a) where
  nil = Nothing
  isNil Nothing = True
  isNil _ = False

instance Cond Bool where
  nil = False
  isNil = not

instance Cond Txt where
  nil = ""
  isNil "" = True
  isNil _  = False

instance Cond Int where
  nil = 0
  isNil = (== 0)

instance Cond Integer where
  nil = 0
  isNil = (== 0)

instance Cond Double where
  nil = 0
  isNil = (== 0)

instance Cond Float where
  nil = 0
  isNil = (== 0)

pattern Nil <- (isNil -> True) where
  Nil = nil
