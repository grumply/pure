{-# language KindSignatures, DataKinds, RoleAnnotations, DerivingStrategies, DeriveGeneric, DerivingVia #-}
module Pure.Auth.Data.Hash where

import Data.Txt (Txt,ToTxt,FromTxt)
import Data.JSON (ToJSON,FromJSON)

import GHC.Generics (Generic)

import GHC.TypeLits (Nat)

newtype Hash (rounds :: Nat) hashOf = Hash Txt
  deriving (Generic,Show,Eq,Ord)
  deriving (ToJSON,FromJSON,ToTxt,FromTxt) via Txt

type role Hash nominal nominal