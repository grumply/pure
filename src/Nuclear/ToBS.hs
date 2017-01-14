{-# language CPP #-}
module Nuclear.ToBS where

import Data.JSText
#ifdef __GHCJS__
import Data.JSString
#endif
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC

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
instance ToBS JSText where
  toBS = BSLC.pack . unpack
#endif
