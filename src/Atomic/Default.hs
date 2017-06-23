{-# language CPP #-}
module Atomic.Default where

import Ef.Base

import Atomic.Try
import Data.Millis
import Data.Micros
import Data.JSON
import Data.Txt

import Data.Int
import Data.Word
import Data.Ratio
import Data.Complex
import Data.Monoid

import GHC.Generics

class Default a where
  def :: a
  default def :: (Generic a, GDefault (Rep a)) => a
  def = to gdef

instance Default Bool where def = False -- like other languages with construction defaults

instance Default () where def = ()
instance Default Ordering where def = EQ
instance Default Any where def = Any False
instance Default All where def = All True
instance Default (Last a) where def = Last Nothing
instance (Num a) => Default (Sum a) where def = Sum 0
instance (Num a) => Default (Product a) where def = Product 1
instance Default (Endo a) where def = Endo id
instance Default a => Default (Const a b) where def = Const def
instance Default (Maybe a) where def = Nothing
instance Default [a] where def = []

-- Note that (def :: Try a) /= mempty
instance Default (Try a) where def = Trying

instance Default Txt where def = mempty

instance Default Value where
  def =
#ifdef __GHCJS__
    nullValue
#else
    Null
#endif

instance Default Obj where def = object []

instance Default Micros where def = 0
instance Default Millis where def = 0
instance Default Int where def = 0
instance Default Int8 where def = 0
instance Default Int16 where def = 0
instance Default Int32 where def = 0
instance Default Int64 where def = 0
instance Default Word where def = 0
instance Default Word8 where def = 0
instance Default Word16 where def = 0
instance Default Word32 where def = 0
instance Default Word64 where def = 0
instance Default Integer where def = 0
instance Default Float where def = 0
instance Default Double where def = 0
instance (Integral a) => Default (Ratio a) where def = 0
instance (Default a,RealFloat a) => Default (Complex a) where def = def :+ def
instance Default r => Default (x -> r) where def = const def
instance Default a => Default (IO a) where def = return def
instance (Default a) => Default (Ef ms c a) where def = Return def

instance (Default a) => Default (Dual a) where def = Dual def
instance (Default a, Default b) => Default (a, b) where def = (def, def)
instance (Default a, Default b, Default c) => Default (a, b, c) where def = (def, def, def)
instance (Default a, Default b, Default c, Default d) => Default (a, b, c, d) where def = (def, def, def, def)
instance (Default a, Default b, Default c, Default d, Default e) => Default (a, b, c, d, e) where def = (def, def, def, def, def)
instance (Default a, Default b, Default c, Default d, Default e, Default f) => Default (a, b, c, d, e, f) where def = (def, def, def, def, def, def)
instance (Default a, Default b, Default c, Default d, Default e, Default f, Default g) => Default (a, b, c, d, e, f, g) where def = (def, def, def, def, def, def, def)

instance (Monad c) => Default (Callback_ status result c) where
  def = Callback (\_ -> return ()) (\_ -> return ()) (\_ -> return ())

-- Inspired by Lukas Mai's data-default-class with a default instance for
-- sum types based on lexicographical order - similar to the Ord and Enum
-- instances
class GDefault f where
  gdef :: f a

instance GDefault U1 where
  gdef = U1

instance (Default a) => GDefault (K1 i a) where
  gdef = K1 def

instance (GDefault a, GDefault b) => GDefault (a :*: b) where
  gdef = gdef :*: gdef

instance (GDefault a, GDefault b) => GDefault (a :+: b) where
  gdef = L1 gdef

instance (GDefault a) => GDefault (M1 i c a) where
  gdef = M1 gdef
