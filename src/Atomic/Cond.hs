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
  {-# INLINE nil #-}

  isNil :: a -> Bool
  default isNil :: (Eq a) => a -> Bool
  isNil = (== nil)
  {-# INLINE isNil #-}

  notNil :: a -> Bool
  notNil = not . isNil
  {-# INLINE notNil #-}

infix 9 ?
(?) :: (Cond x) => x -> a -> a -> a
(?) !x !t !e = if notNil x then t else e
{-# INLINE (?) #-}

infix 9 ?&
(?&) :: (Cond x) => x -> a -> a -> a
(?&) x t e = x ? e $ t
{-# INLINE (?&) #-}

may :: Cond a => (b -> a) -> Maybe b -> a
may = maybe nil
{-# INLINE may #-}

cond :: (Cond x, Cond a) => x -> a -> a
cond b t = b ? t $ nil
{-# INLINE cond #-}

instance Cond (Ef ms IO ()) where
  nil = Return ()
  {-# INLINE nil #-}
  isNil (Return ()) = True
  isNil _ = False
  {-# INLINE isNil #-}

instance Cond [a] where
  nil = []
  {-# INLINE nil #-}
  isNil [] = True
  isNil _ = False
  {-# INLINE isNil #-}

instance Cond (Maybe a) where
  nil = Nothing
  {-# INLINE nil #-}
  isNil Nothing = True
  isNil _ = False
  {-# INLINE isNil #-}

instance Cond Bool where
  nil = False
  {-# INLINE nil #-}
  isNil = not
  {-# INLINE isNil #-}

instance Cond Txt where
  nil = ""
  {-# INLINE nil #-}
  isNil "" = True
  isNil _  = False
  {-# INLINE isNil #-}

instance Cond Int where
  nil = 0
  {-# INLINE nil #-}
  isNil = (== 0)
  {-# INLINE isNil #-}

instance Cond Integer where
  nil = 0
  {-# INLINE nil #-}
  isNil = (== 0)
  {-# INLINE isNil #-}

instance Cond Double where
  nil = 0
  {-# INLINE nil #-}
  isNil = (== 0)
  {-# INLINE isNil #-}

instance Cond Float where
  nil = 0
  {-# INLINE nil #-}
  isNil = (== 0)
  {-# INLINE isNil #-}

pattern Nil <- (isNil -> True) where
  Nil = nil
