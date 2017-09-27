{-# language OverloadedStrings #-}
module Pure.Data.Key where

import Pure.Data.Txt

import Data.Hashable
import Data.String
import Data.Monoid

newtype Key phantom = Key { getKey :: (Txt,Int) }
  deriving (Show,Eq)

instance FromTxt (Key phantom) where
  fromTxt t = Key (t,hash t)

instance ToTxt (Key phantom) where
  toTxt = fst . getKey

instance IsString (Key a) where
  fromString str =
    let h = hash str
    in h `seq` Key (fromString str,h)

instance Monoid (Key phantom) where
  mempty = Key ("",hash ("" :: Txt))
  mappend (Key (jss0,_)) (Key (jss1,_)) =
    let h = hash (jss0 <> jss1)
    in Key (jss0 <> jss1,h)

