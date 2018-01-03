{-# language CPP #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE StandaloneDeriving #-}
module Pure.WebSocket.Dispatch where

import qualified Data.ByteString.Lazy as BSL
import GHC.Generics

import Pure.Data

#if defined(DEBUGAPI) || defined(DEVEL)
import Debug.Trace
#endif

data Dispatch
  = Dispatch
    { ep :: Txt
    , pl :: Value
    } deriving (Generic,ToJSON,FromJSON)

deriving instance ToBS Dispatch
#ifndef __GHCJS__
instance FromBS Dispatch
  where
    fromBS = eitherDecode' . BSL.takeWhile (/= 0)
  -- NOTE on BSL.takeWhile (/= 0):
  -- fixes a padding bug when used with ghcjs/pure. Is \NUL
  -- valid in a text component of an encoded message?
  -- Probably need to filter \NUL from encoded messages
  -- in ghcjs/pure. Any use of a null byte is liable to clobber
  -- a message and force a disconnect since ghc/pure doesn't
  -- permit malformed messages.
#else
deriving instance FromBS Dispatch
#endif

{-# INLINE encodeDispatch #-}
encodeDispatch :: ToJSON a => Txt -> a -> Dispatch
encodeDispatch ep a =
  let pl = toJSON a
  in Dispatch {..}

{-# INLINE decodeDispatch #-}
decodeDispatch :: FromJSON a => Dispatch -> Maybe a
decodeDispatch d@Dispatch {..} =
  case fromJSON pl of
    Error err ->
#if defined(DEBUGAPI) || defined(DEVEL)
      traceShow ("decodeDispatch:fromJSON => Error",err,toBS d) Nothing
#else
      Nothing
#endif
    Success a -> Just a
