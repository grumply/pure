{-# language CPP #-}
module Nuclear.FromBS where

import Data.JSText
import Nuclear.ToText
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

#ifdef __GHCJS__
foreign import javascript unsafe
  "JSON.parse($1)" js_JSON_parse :: JSText -> Value
#endif

class FromBS a where
  fromBS :: BSL.ByteString -> Either String a
  default fromBS :: FromJSON a => BSL.ByteString -> Either String a
#ifdef __GHCJS__
  fromBS = parseEither parseJSON . js_JSON_parse . toText
#else
  fromBS = eitherDecode'
#endif

instance FromBS T.Text where
  fromBS = Right . TL.toStrict . TL.decodeUtf8

instance FromBS TL.Text where
  fromBS = Right . TL.decodeUtf8
