{-# language CPP #-}
module Data.Sorcerer.JSON (encode_,decode_,ToJSON(..),FromJSON(..)) where

import Data.Txt (fromTxt,toTxt)

import Data.ByteString.Lazy as BSL

#ifdef __GHCJS__
import Data.JSON
#else
import Data.Aeson
#endif

{-# INLINE encode_ #-}
{-# INLINE decode_ #-}
#ifdef __GHCJS__
encode_ :: ToJSON a => a -> BSL.ByteString
encode_ = fromTxt . encode . toJSON
decode_ :: FromJSON a => BSL.ByteString -> Maybe a
decode_ = decode . toTxt
#else
encode_ :: ToJSON a => a -> BSL.ByteString
encode_ = encode
decode_ :: FromJSON a => BSL.ByteString -> Maybe a
decode_ = decode
#endif