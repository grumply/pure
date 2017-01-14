{-# language CPP #-}
module Nuclear.FromText where

#ifdef __GHCJS__
import Data.JSString
#endif

import Data.JSText

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL

class FromText a where
  fromText :: JSText -> a

instance FromText TL.Text where
#ifdef __GHCJS__
  fromText = lazyTextFromJSString
#else
  fromText = TL.fromStrict
#endif

instance FromText T.Text where
#ifdef __GHCJS__
  fromText = textFromJSString
#else
  fromText = id
#endif

#ifdef __GHCJS__
instance FromText JSString where
  fromText = id
#endif

instance FromText [Char] where
#ifdef __GHCJS__
  fromText = unpack
#else
  fromText = T.unpack
#endif

instance FromText B.ByteString where
#ifdef __GHCJS__
  fromText = BC.pack . unpack
#else
  fromText = T.encodeUtf8
#endif

instance FromText BSL.ByteString where
#ifdef __GHCJS__
  fromText = BSLC.pack . unpack
#else
  fromText = BSL.fromStrict . T.encodeUtf8
#endif

