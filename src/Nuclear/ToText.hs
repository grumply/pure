{-# language OverloadedStrings #-}
module Nuclear.ToText where

import Nuclear.ToBS

import Numeric

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import Data.Text as T
import Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Text.Lazy.Builder as Builder
import Data.Text.Lazy.Builder.Int as Builder

-- ToText is representational and is thus uni-directional. For a fully
-- bidirectional encoding, see ToBS/FromBS where the expectation is that
-- fromBS . toBS = id and toBS . fromBS = id
--
-- ToText is used to construct, possibly unique, resource identifiers.
--
-- Note the default instance uses a ToBS instance to construct a text value
-- from a full encoding of the term; this is slow since ToBS generates a
-- lazy bytestring and we must use lazy decoding and subsequent strictness
-- conversion. For small terms, which is the intended use-case for the
-- default instance, this won't matter much.
class ToText a where
  toText :: a -> Text
  {-# INLINE toText #-}
  default toText :: ToBS a => a -> Text
  -- can this fail at runtime from a bad encoding?
  toText = TL.toStrict . TL.decodeUtf8 . toBS

instance ToText ()

instance ToText BSL.ByteString where
  -- can this fail at runtime from a bad encoding?
  toText = toText . TL.decodeUtf8

instance ToText B.ByteString where
  -- can this fail at runtime from a bad encoding?
  toText = T.decodeUtf8

instance ToText Text where
  toText = id

instance ToText TL.Text where
  toText = TL.toStrict

instance ToText Char where
  toText = T.singleton

instance ToText String where
  toText = pack

instance ToText Int where
  toText = shortText . Builder.decimal

instance ToText Integer where
  toText = shortText . Builder.decimal

instance ToText Double where
  toText = toText . ($ "") . showFFloat Nothing

instance ToText Bool where
    toText True  = "true"
    toText False = "false"

shortText :: Builder -> Text
shortText = TL.toStrict . Builder.toLazyTextWith 32

