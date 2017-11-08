{-# LANGUAGE MagicHash #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DefaultSignatures #-}
module Pure.Data.UnsafeEq where

import GHC.Prim
import Unsafe.Coerce

class UnsafeEq a where
  (===) :: a -> a -> Bool
  default (===) :: (Eq a) => a -> a -> Bool
  (===) = prettyUnsafeEq
  {-# INLINE (===) #-}

{-# INLINE (/==) #-}
(/==) :: UnsafeEq a => a -> a -> Bool
(/==) a b = not (a === b)

{-# INLINE reallyVeryUnsafeEq #-}
reallyVeryUnsafeEq :: a -> b -> Bool
reallyVeryUnsafeEq a b =
  case reallyUnsafePtrEquality# a (unsafeCoerce b) of
    1# -> True
    _  -> False

{-# INLINE reallyUnsafeEq #-}
reallyUnsafeEq :: a -> a -> Bool
reallyUnsafeEq a b =
  case reallyUnsafePtrEquality# a b of
    1# -> True
    _  -> False

{-# INLINE prettyUnsafeEq #-}
prettyUnsafeEq :: Eq a => a -> a -> Bool
prettyUnsafeEq a b =
  case reallyUnsafePtrEquality# a b of
    1# -> True
    _  -> a == b
