{-# language CPP #-}
module Atomic.ToBS where

import Data.Txt
import Data.JSON
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
instance ToBS Txt where
  toBS = BSLC.pack . unpack
#endif
