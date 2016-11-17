module Nuclear.ToBS where

import Data.Aeson
import qualified Data.ByteString.Lazy as BSL

class ToBS a where
  toBS :: a -> BSL.ByteString
  default toBS :: ToJSON a => a -> BSL.ByteString
  toBS = encode

instance ToBS ()

instance ToBS BSL.ByteString where
  toBS = id

