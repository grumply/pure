{-# language OverloadedStrings #-}
module Pure.Data.Key where

import Pure.Data.Txt

import Data.Hashable
import Data.String
import Data.Monoid

data Key phantom = Key { getKey :: {-# UNPACK #-}!Txt, getKeyHash :: {-# UNPACK #-}!Int }
  deriving (Show,Eq)

instance FromTxt (Key phantom) where
  fromTxt t = Key t (hash t)

instance ToTxt (Key phantom) where
  toTxt = getKey

instance IsString (Key a) where
  fromString str =
    let h = hash str
    in h `seq` Key (fromString str) h

instance Monoid (Key phantom) where
  mempty = fromTxt ""
  mappend (Key t0 _) (Key t1 _) =
    let 
      t = t0 <> t1
      h = hash t
    in Key t h

