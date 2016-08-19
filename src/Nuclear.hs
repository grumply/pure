{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-# language CPP #-}
module Nuclear
  ( Msg(..), Name, Body
  , fromBS, toBS
  , encodeMsg, decodeMsg
  , LazyByteString, Text
  , ToJSON(..), FromJSON(..)
  , module GHC.Generics
  ) where

import Data.Aeson
import GHC.Generics
import Data.Text
import Data.Monoid
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as Lazy
import qualified Data.ByteString.Lazy as LBS

type LazyByteString = LBS.ByteString

type Name = Text
type Body = Lazy.Text

data Msg
  = Msg
    { name :: !Name
    , body :: !Body
    } deriving (Show,Eq,Ord,Generic)

instance FromJSON Msg
instance ToJSON Msg

-- keep an eye on this takeWhile; the other option is
-- conversion to strict ByteString with spanEnd. This
-- implementation could be problematic if sending a
-- bytestring of some pre-encoded value with a null
-- byte. If sticking to ToJSON/FromJSON this should
-- work out, I think.
fromBS :: LazyByteString -> Either String Msg
fromBS = eitherDecode . LBS.takeWhile (/= 0)

toBS :: Msg -> LazyByteString
toBS = encode

encodeMsg :: ToJSON a => Text -> a -> Msg
encodeMsg name a =
  let body = Lazy.decodeUtf8 $ encode a
  in Msg {..}

decodeMsg :: FromJSON a => Msg -> Maybe a
decodeMsg Msg {..} = decode $ Lazy.encodeUtf8 body
