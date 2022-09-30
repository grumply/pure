{-# language DerivingStrategies, DeriveGeneric, DerivingVia #-}
module Pure.Auth.Data.Password where

import Data.Txt (Txt(),ToTxt,FromTxt)
import Data.JSON (ToJSON,FromJSON)

import GHC.Generics (Generic)

newtype Password = Password Txt
  deriving stock (Generic,Eq,Ord)
  deriving (ToJSON,FromJSON,ToTxt,FromTxt) via Txt
