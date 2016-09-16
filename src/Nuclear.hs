{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-# language CPP #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language TypeFamilies #-}
module Nuclear
  ( Msg(..), Name, Body
  , fromBS, toBS
  , encodeMsg, decodeMsg
  , LazyByteString, Text
  , ToJSON(..), FromJSON(..)
  , Request(..), Response(..), Message(..)
  , module GHC.Generics
  , module Data.Typeable
  , Proxy(..)
  ) where

import Data.Aeson
import GHC.Generics
import Data.Text
import Data.Monoid
import qualified Data.Text.Lazy as Lazy
import qualified Data.Text.Lazy.Encoding as Lazy
import qualified Data.ByteString.Lazy as LBS

import Data.Typeable
import Data.Proxy

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

class (ToJSON a,FromJSON a,ToJSON b,FromJSON b,Typeable a,Typeable b) => Response a b | a -> b, b -> a where
  requestName :: Proxy b -> Text
  requestName p =
    append (pack "Req::")
           (pack $ show $ typeRepTyCon $ typeOf (undefined :: b))

  responseName :: Proxy a -> Text
  responseName p =
    append (pack "Res::")
           (pack $ show $ typeRepTyCon $ typeOf (undefined :: a))

class (ToJSON a,FromJSON a,ToJSON b,FromJSON b,Typeable a,Typeable b) => Request b a | a -> b, b -> a
instance (Request b a) => Response a b

-- class of unsollicited message types
class (ToJSON m,FromJSON m,Typeable m) => Message m where
  messageName :: Proxy m -> Text
  messageName p =
    append (pack "Msg::")
           (pack $ show $ typeRepTyCon $ typeOf (undefined :: m))
