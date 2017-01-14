{-# language OverloadedStrings #-}
{-# language CPP #-}
module Nuclear.ToText where

import Nuclear.ToBS

import Numeric

import Data.JSText

#ifdef __GHCJS__
import Data.JSString
import GHCJS.Types
import GHCJS.Marshal.Pure
import GHCJS.Marshal
import System.IO.Unsafe
#endif

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder

-- ToText is representational and is thus uni-directional. For a fully
-- bidirectional encoding, see ToBS/FromBS where the expectation is that
-- fromBS (toBS a) = Right and fmap toBS . fromBS = Right
--
-- ToText is used to construct, possibly unique, resource identifiers.
--
-- Note the default instance uses a ToBS instance to construct a text value
-- from a full encoding of the term; this is slow since ToBS generates a
-- lazy bytestring and we must use lazy decoding and subsequent strictness
-- conversion. For small terms, which is the intended use-case for the
-- default instance, this won't matter much.
class ToText a where
  toText :: a -> JSText
  default toText :: ToBS a => a -> JSText
#ifdef __GHCJS__
  toText = pack . BSLC.unpack . toBS
#else
  toText = TL.toStrict . TL.decodeUtf8 . toBS
#endif

instance ToText Value where
#ifdef __GHCJS__
  toText = encode
#else
  toText = toText . encode
#endif

instance ToText () where
  toText _ = "()"

instance ToText BSL.ByteString where
  -- can this fail at runtime from a bad encoding?
  toText = toText . TL.decodeUtf8

instance ToText B.ByteString where
  -- can this fail at runtime from a bad encoding?
  toText = toText . T.decodeUtf8

instance ToText JSText where
  toText = id

#ifdef __GHCJS__
instance ToText T.Text where
  toText = textToJSString
#endif

instance ToText TL.Text where
#ifdef __GHCJS__
  toText = lazyTextToJSString
#else
  toText = TL.toStrict
#endif

instance ToText Char where
#ifdef __GHCJS__
  toText = singleton
#else
  toText = T.singleton
#endif

instance ToText String where
#ifdef __GHCJS__
  toText = pack
#else
  toText = T.pack
#endif

instance ToText Int where
#ifdef __GHCJS__
  toText = toText . show
#else
  toText = shortText . Builder.decimal
#endif

instance ToText Integer where
#ifdef __GHCJS__
  toText = toText . show
#else
  toText = shortText . Builder.decimal
#endif

instance ToText Double where
#ifdef __GHCJS__
  toText = toText . show
#else
  toText = toText . ($ "") . showFFloat Nothing
#endif

instance ToText Bool where
  toText True  = "true"
  toText False = "false"

shortText :: Builder.Builder -> T.Text
shortText = TL.toStrict . Builder.toLazyTextWith 32

