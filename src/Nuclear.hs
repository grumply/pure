module Nuclear
  ( module Data.Aeson
  , module Data.Typeable
  , module GHC.Generics
  , module Export
  ) where

import Data.Aeson
import Data.Typeable
import GHC.Generics

import Data.Text as Export (Text) 

import Nuclear.ToBS as Export
import Nuclear.FromBS as Export
import Nuclear.ToText as Export
import Nuclear.FromText as Export
import Nuclear.Msg as Export
import Nuclear.Request as Export
import Nuclear.Message as Export
import Nuclear.TypeRep as Export
import Nuclear.Indexed as Export
import Nuclear.API as Export
import Nuclear.Endpoint as Export

import Data.JSTime as Export
import Data.MicroTime as Export

-- import Nuclear.Header -- needs to be imported qualified or things get messy

-- Note: This module, and, thus, its descendants, defaults to preferring
-- lazy bytestrings and strict text.

jsTimeToMicroTime :: JSTime -> MicroTime
jsTimeToMicroTime jt = MicroTime $ (millis jt) * 1000

-- truncate rather than round
microTimeToJSTime :: MicroTime -> JSTime
microTimeToJSTime mt = JSTime $ (micros mt) `div` 1000
