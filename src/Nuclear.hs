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
    { name :: Name
    , body :: Body
    } deriving (Show,Eq,Ord,Generic)

instance FromJSON Msg
instance ToJSON Msg

fromBS :: LazyByteString -> Either String Msg
fromBS = eitherDecode

toBS :: Msg -> LazyByteString
toBS = encode

encodeMsg :: ToJSON a => Text -> a -> Msg
encodeMsg method a =
  let body = Lazy.decodeUtf8 $ encode a
  in Msg {..}

decodeMsg :: FromJSON a => Msg -> Maybe a
decodeMsg Msg {..} = decode $ Lazy.encodeUtf8 body
