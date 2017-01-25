{-# language CPP #-}
module Atomic.Dispatch where

import qualified Data.ByteString.Lazy as BSL
import Data.Txt
import GHC.Generics

import Atomic.ToBS
import Atomic.FromBS
import Atomic.ToTxt
import Atomic.FromTxt

data Dispatch
  = Dispatch
    { ep :: Txt
    , pl :: Value
    } deriving (Generic)
instance ToJSON Dispatch
instance FromJSON Dispatch
instance ToBS Dispatch
instance ToTxt Dispatch
#ifndef __GHCJS__
instance FromBS Dispatch
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

{-# INLINE encodeDispatch #-}
encodeDispatch :: ToJSON a => Txt -> a -> Dispatch
encodeDispatch ep a =
  let pl = toJSON a
  in Dispatch {..}

{-# INLINE decodeDispatch #-}
decodeDispatch :: FromJSON a => Dispatch -> Maybe a
decodeDispatch Dispatch {..} =
  case fromJSON pl of
    Error _ -> Nothing
    Success a -> Just a
