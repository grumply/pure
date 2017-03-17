{-# language TemplateHaskell #-}
module Atomic.Try where

import Data.Txt
import Data.JSON
import GHC.Generics

import Control.Lens.TH

data Try a
  = Trying
  | Failed
  | Done a
  deriving (Eq,Generic,ToJSON,FromJSON)
makePrisms ''Try

isTrying :: Try a -> Bool
isTrying Trying = True
isTrying _ = False

isFailed :: Try a -> Bool
isFailed Failed = True
isFailed _ = False

isDone :: Try a -> Bool
isDone (Done _) = True
isDone _ = False
