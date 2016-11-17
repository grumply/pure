module Nuclear.FromText where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import Data.Text
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL

-- FromText is a convenience class that should be considered closed.
class FromText a where
  fromText :: Text -> a

instance FromText TL.Text where
  fromText = TL.fromStrict

instance FromText [Char] where
  fromText = unpack

instance FromText B.ByteString where
  fromText = T.encodeUtf8

instance FromText BSL.ByteString where
  fromText = BSL.fromStrict . T.encodeUtf8

