module Nuclear.Key where

import Data.JSText

import Nuclear.FromText
import Nuclear.ToText

import Data.Hashable
import Data.String
import Data.Monoid

newtype Key phantom = Key { getKey :: (JSText,Int) }
  deriving (Show)

instance FromText (Key phantom) where
  fromText t = Key (t,hash t)

instance ToText (Key phantom) where
  toText = fst . getKey

instance IsString (Key a) where
  fromString str =
    let h = hash str
    in h `seq` Key (fromString str,h)

instance Monoid (Key phantom) where
  mempty = Key (fromString "",hash "")
  mappend (Key (jss0,_)) (Key (jss1,_)) =
    let h = hash (fromText jss0 ++ fromText jss1 :: String)
    in Key (jss0 <> jss1,h)

