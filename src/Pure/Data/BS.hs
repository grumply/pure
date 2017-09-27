{-# LANGUAGE CPP #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Pure.Data.BS where

import Pure.Data.JSON
import Pure.Data.Txt

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

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

class ToBS a where
  toBS :: a -> BSL.ByteString
  default toBS :: ToJSON a => a -> BSL.ByteString
#ifdef __GHCJS__
  toBS = toBS . encode . toJSON
#else
  toBS = encode
#endif

instance ToBS ()

instance ToBS BSL.ByteString where
  toBS = id

#ifdef __GHCJS__
instance ToBS Txt where
  toBS = BSLC.pack . unpack
#endif

