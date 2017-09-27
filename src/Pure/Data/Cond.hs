{-# LANGUAGE CPP #-}
{-# LANGUAGE UndecidableInstances, TypeSynonymInstances, FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Pure.Data.Cond where

import Pure.Data.Default
import Pure.Data.JSON
import Pure.Data.Micros
import Pure.Data.Millis
import Pure.Data.Try
import Pure.Data.Txt

import Control.Applicative

import Data.Complex
import Data.Int
import Data.Monoid
import Data.Ratio
import Data.Word

class Cond a where
  nil :: a
  default nil :: (Monoid a, Eq a) => a
  nil = mempty

  isNil :: a -> Bool
  default isNil :: (Eq a) => a -> Bool
  isNil = (== nil)

  notNil :: a -> Bool
  notNil = not . isNil

infixl 9 ?
(?) :: (Cond x) => x -> a -> a -> a
(?) x t e = if notNil x then t else e

infixl 9 !?
(!?) :: (Cond x) => x -> a -> a -> a
(!?) x t e = if isNil x then t else e

may :: Cond a => (b -> a) -> Maybe b -> a
may = maybe nil

infixl 9 #
(#) :: (Cond x, Default a) => x -> a -> a
(#) b t = b ? t $ def

cond :: (Cond a, Default a) => a -> a
cond a = a # a

-- instance Cond a => Cond (Ef ms IO a) where
--   nil = Return nil
--   isNil (Return a) = isNil a
--   isNil _ = False

instance Cond [a] where
  nil = []
  isNil [] = True
  isNil _ = False

instance Cond (Maybe a) where
  nil = Nothing
  isNil Nothing = True
  isNil _ = False

instance Cond (Try a) where
  nil = Failed
  isNil Failed = True
  isNil _ = False

instance Cond () where
  nil = ()
  isNil _ = False

instance Cond Any where
  nil = Any True

instance Cond All where
  nil = All False

-- questionable
instance Cond Ordering where
  nil = LT

instance Cond (Last a) where
  nil = Last Nothing
  isNil (Last Nothing) = True
  isNil _ = False

instance (Num a,Eq a) => Cond (Sum a) where
  nil = Sum 0

instance (Num a,Eq a) => Cond (Product a) where
  nil = Product 0 -- should this differ from def?

instance (Cond a) => Cond (Const a b) where
  nil = Const nil
  isNil (Const a) = isNil a

instance Cond Bool where
  nil = False
  isNil = not

instance Cond Txt where nil = ""

instance Cond Int where nil = 0
instance Cond Int8 where nil = 0
instance Cond Int16 where nil = 0
instance Cond Int32 where nil = 0
instance Cond Int64 where nil = 0
instance Cond Word where nil = 0
instance Cond Word8 where nil = 0
instance Cond Word16 where nil = 0
instance Cond Word32 where nil = 0
instance Cond Word64 where nil = 0
instance Cond Integer where nil = 0
instance Cond Float where nil = 0
instance Cond Double where nil = 0

instance Cond Millis where nil = 0
instance Cond Micros where nil = 0

instance (Integral a) => Cond (Ratio a) where nil = 0
instance (Cond a,RealFloat a) => Cond (Complex a) where nil = nil :+ nil

instance Cond a => Cond (Dual a) where
  nil = Dual nil
  isNil (Dual a) = isNil a

instance Cond Obj where
  nil = mempty
  isNil o =
#ifdef __GHCJS__
    Prelude.null $ objectAssocs o
#else
    o == mempty
#endif

instance Cond Value where
  nil =
#ifdef __GHCJS__
    nullValue
#else
    Null
#endif

pattern Nil <- (isNil -> True) where
  Nil = nil
