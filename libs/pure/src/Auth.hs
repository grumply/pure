{-# language DerivingStrategies, DeriveAnyClass, DeriveGeneric #-}
module Auth where

import Data.JSON
import Data.Txt
import GHC.Generics
import Endpoint

data Token = Token
  { claims :: [Txt]
  , proof :: Txt
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

type Authenticated x = Endpoint (Token -> x)
