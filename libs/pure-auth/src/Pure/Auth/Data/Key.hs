{-# language DerivingStrategies, DeriveGeneric, DerivingVia #-}
module Pure.Auth.Data.Key where

import Data.JSON (ToJSON,FromJSON)
import Data.Txt as Txt

import GHC.Generics (Generic)

newtype Key = Key Txt
  deriving (Generic,Eq,Ord)
  deriving (ToJSON,FromJSON,ToTxt,FromTxt) via Txt