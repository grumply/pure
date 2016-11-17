module Nuclear.Msg where

import qualified Data.ByteString.Lazy as BSL
import Data.Aeson
import Data.Text
import GHC.Generics

import Nuclear.ToBS
import Nuclear.FromBS
import Nuclear.ToText
import Nuclear.FromText

data Msg
  = Msg
    -- I'd prefer header/body but they overlap with the HTML elements.
    { endpoint :: Text
    , payload :: Value
    } deriving Generic
instance ToJSON Msg
instance FromJSON Msg
instance ToBS Msg
instance ToText Msg
instance FromBS Msg where
  fromBS = eitherDecode' . BSL.takeWhile (/= 0)
  -- NOTE on BSL.takeWhile (/= 0):
  -- fixes a padding bug when used with Fusion. Is \NUL
  -- valid in a text component of an encoded message?
  -- Probably need to filter \NUL from encoded messages
  -- in Fusion. Any use of a null byte is liable to clobber
  -- a message and force a disconnect since Fission doesn't
  -- permit malformed messages.

{-# INLINE encodeMsg #-}
encodeMsg :: ToJSON a => Text -> a -> Msg
encodeMsg endpoint a =
  let payload = toJSON a
  in Msg {..}

{-# INLINE decodeMsg #-}
decodeMsg :: FromJSON a => Msg -> Maybe a
decodeMsg Msg {..} =
  case fromJSON payload of
    Error _ -> Nothing
    Success a -> Just a


  
