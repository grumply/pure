{-# language CPP #-}
module Atomic.FromBS where

import Data.Txt
import Data.JSON

import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

#ifdef __GHCJS__
foreign import javascript unsafe
  "JSON.parse($1)" js_JSON_parse :: Txt -> Value
#endif

class FromBS a where
  fromBS :: BSL.ByteString -> Either String a
#ifndef __GHCJS__
  default fromBS :: FromJSON a => BSL.ByteString -> Either String a
  fromBS = eitherDecode'
#endif

instance FromBS T.Text where
  fromBS = Right . TL.toStrict . TL.decodeUtf8

instance FromBS TL.Text where
  fromBS = Right . TL.decodeUtf8
