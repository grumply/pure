module Nuclear.FromBS where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import Data.Text

class FromBS a where
  fromBS :: BSL.ByteString -> Either String a
  {-# INLINE fromBS #-}
  default fromBS :: FromJSON a => BSL.ByteString -> Either String a
  fromBS = eitherDecode'

instance FromBS Text where
  fromBS = Right . TL.toStrict . TL.decodeUtf8

instance FromBS TL.Text where
  fromBS = Right . TL.decodeUtf8

