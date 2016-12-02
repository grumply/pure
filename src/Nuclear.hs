module Nuclear
  ( module Data.Aeson
  , module Data.Typeable
  , module Data.Hashable
  , module GHC.Generics
  , module Export
  , LazyByteString
  , LazyText
  ) where

import Data.Aeson
import Data.Hashable
import Data.Typeable
import GHC.Generics

import Data.Text as Export (Text)
import Data.ByteString as Export (ByteString)

import Nuclear.ToBS as Export
import Nuclear.FromBS as Export
import Nuclear.ToText as Export
import Nuclear.FromText as Export
import Nuclear.Strict as Export
import Nuclear.Msg as Export
import Nuclear.Request as Export
import Nuclear.Message as Export
import Nuclear.TypeRep as Export
import Nuclear.Indexed as Export
import Nuclear.API as Export
import Nuclear.Endpoint as Export

import Data.JSTime as Export
import Data.MicroTime as Export

import qualified Data.Text.Lazy as TL (Text) 
import qualified Data.ByteString.Lazy as BSL (ByteString)

type LazyByteString = BSL.ByteString
type LazyText = TL.Text

-- import Nuclear.Header -- needs to be imported qualified or things get messy

-- Note: This module, and, thus, its descendants, defaults to preferring
-- lazy bytestrings and strict text.

instance FromJSTime MicroTime where
 --  fromJSTime :: JSTime -> MicroTime
  fromJSTime jt = MicroTime $ (millis jt) * 1000

instance FromMicroTime JSTime where
  -- fromMicrotime :: MicroTime -> JSTime
  -- truncate rather than round
  fromMicroTime mt = JSTime $ (micros mt) `div` 1000
