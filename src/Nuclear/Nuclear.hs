{-# language CPP #-}
module Nuclear.Nuclear where

import qualified Data.ByteString.Lazy as BSL
import Data.JSText
import GHC.Generics

import Nuclear.ToBS
import Nuclear.FromBS
import Nuclear.ToText
import Nuclear.FromText

data Nuclear
  = Nuclear
    { ep :: JSText
    , pl :: Value
    } deriving (Generic)
instance ToJSON Nuclear
instance FromJSON Nuclear
instance ToBS Nuclear
instance ToText Nuclear
#ifndef __GHCJS__
instance FromBS Nuclear
  where
    fromBS = eitherDecode' . BSL.takeWhile (/= 0)
  -- NOTE on BSL.takeWhile (/= 0):
  -- fixes a padding bug when used with Fusion. Is \NUL
  -- valid in a text component of an encoded message?
  -- Probably need to filter \NUL from encoded messages
  -- in Fusion. Any use of a null byte is liable to clobber
  -- a message and force a disconnect since Fission doesn't
  -- permit malformed messages.
#endif

{-# INLINE encodeNuclear #-}
encodeNuclear :: ToJSON a => JSText -> a -> Nuclear
encodeNuclear ep a =
  let pl = toJSON a
  in Nuclear {..}

{-# INLINE decodeNuclear #-}
decodeNuclear :: FromJSON a => Nuclear -> Maybe a
decodeNuclear Nuclear {..} =
  case fromJSON pl of
    Error _ -> Nothing
    Success a -> Just a
