{-# language DerivingStrategies, DeriveGeneric, DerivingVia, KindSignatures, RoleAnnotations #-}
module Pure.Auth.Data.Token where

import Pure.Auth.Data.Key (Key)
import Pure.Auth.Data.Username (Username)

import Data.JSON (ToJSON,FromJSON)

import GHC.Generics (Generic)

newtype Token (_role :: *) = Token (Username,Key)
  deriving (Generic,Eq,Ord)
  deriving (ToJSON,FromJSON) via (Username,Key) 
  -- role is ignored in the JSON interface, be careful!

type role Token nominal