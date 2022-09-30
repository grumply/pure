module Pure.Conjurer.Rep where

import Data.Txt as Txt

import Data.Char
import Data.Typeable

rep :: forall p. (Typeable p) => Txt
rep = Txt.map limit $ go (typeRep (Proxy :: Proxy p))
  where
    limit c | isAscii c && isAlphaNum c = c | otherwise = '_'
    go tr =
      let tc = toTxt (show (typeRepTyCon tr))
          trs = typeRepArgs tr
      in Txt.intercalate "_" (tc : fmap go trs)
