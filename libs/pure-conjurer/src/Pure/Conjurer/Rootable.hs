module Pure.Conjurer.Rootable where

import Pure.Conjurer.Rep

import Data.Txt as Txt

import Data.Typeable

class Rootable a where
  root :: Txt
  default root :: Typeable a => Txt
  root = "/" <> Txt.toLower (rep @a)

instance {-# OVERLAPPABLE #-} Typeable a => Rootable a