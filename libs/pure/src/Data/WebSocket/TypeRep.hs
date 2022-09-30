{-# LANGUAGE CPP #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Websocket.TypeRep where

import Data.List as L
import Data.Monoid
import Data.Txt as Txt (Txt,ToTxt(..),FromTxt(..),intercalate)
import Data.Typeable

-- simple textual type rep - covers most use-cases and leaves endpoints
-- available for simpler external availability.
{-# INLINE rep #-}
rep :: (Typeable p) => Proxy p -> Txt
rep p = go (typeRep p)
  where
    go tr =
      let tc = toTxt (show (typeRepTyCon tr))
          trs = typeRepArgs tr
      in Txt.intercalate " " (tc : fmap go trs)
