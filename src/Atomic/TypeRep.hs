{-# language OverloadedStrings #-}
{-# language CPP #-}
module Atomic.TypeRep where

import Data.Typeable
import Data.Proxy

import Data.Txt
import Data.Monoid

import Data.List as L

import Atomic.ToTxt

-- | rep is a type representation without package or module qualification.
{-# INLINE rep #-}
rep :: forall (p :: *). (Typeable p) => Proxy p -> Txt
rep _ =
  toTxt $ go False (typeOf (undefined :: p))
  where
    go surround tr =
      let tc = typeRepTyCon tr
          trs = typeRepArgs tr
          r = tyConName tc
      in if L.null trs then
           r
         else if surround then
           " (" <> L.intercalate " " (r:map (go True) trs) <> ")"
         else
           L.intercalate " " (r:map (go True) trs)

-- | fullRep is a type representation with package and package version,
-- and full module qualification. Use this only when expecting to create
-- versioned APIs with full backwards compatibility; you'll know when you
-- want this, so assume you don't as the bytes-across-the-wire cost is high.
{-# INLINE fullRep #-}
fullRep :: forall (p :: *). (Typeable p) => Proxy p -> Txt
fullRep _ =
  toTxt $ go False (typeOf (undefined :: p))
  where
    go surround tr =
      let tc = typeRepTyCon tr
          trs = typeRepArgs tr
          r = tyConPackage tc <> ('.':tyConModule tc) <> ('.':tyConName tc)
      in if L.null trs then
           r
         else if surround then
           " (" <> L.intercalate " " (r:map (go True) trs) <> ")"
         else
           L.intercalate " " (r:map (go True) trs)

-- | qualRep is a type representation with full module qualification.
{-# INLINE qualRep #-}
qualRep :: forall (p :: *). (Typeable p) => Proxy p -> Txt
qualRep _ =
  toTxt $ go False (typeOf (undefined :: p))
  where
    go surround tr =
      let tc = typeRepTyCon tr
          trs = typeRepArgs tr
          r = tyConModule tc <> ('.':tyConName tc)
      in if L.null trs then
           r
         else if surround then
           " (" <> L.intercalate " " (r:map (go True) trs) <> ")"
         else
           L.intercalate " " (r:map (go True) trs)

