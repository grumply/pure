{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
module Nuclear
  ( Msg(..)
  , fromBS, toBS
  , encodeMsg, decodeMsg
  , Text
  , Request(..), Response(..), Message(..)
  , module Data.Aeson
  , module Data.Typeable
  , module GHC.Generics
  , Proxy(..)
  ) where

import Data.Aeson
import Data.Proxy
import Data.Text
import Data.Typeable

import GHC.Generics

import qualified Data.ByteString.Lazy as BSL

data Msg
  = Msg
    { name :: {-# UNPACK #-} !Text
    , body :: {-# UNPACK #-} !Value
    } deriving Generic
instance ToJSON Msg
instance FromJSON Msg

fromBS :: BSL.ByteString -> Either String Msg
fromBS = eitherDecode

toBS :: Msg -> BSL.ByteString
toBS = encode

encodeMsg :: ToJSON a => Text -> a -> Msg
encodeMsg name a =
  let body = toJSON a
  in Msg {..}

decodeMsg :: FromJSON a => Msg -> Maybe a
decodeMsg Msg {..} =
  case fromJSON body of
    Error _ -> Nothing
    Success a -> Just a

class (ToJSON a,FromJSON a,ToJSON b,FromJSON b,Typeable a,Typeable b)
  => Response a b | a -> b, b -> a
  where
  requestName :: Proxy b -> Text
  requestName p =
     append "Req::" $ pack (show $ typeRepTyCon $ typeOf (undefined :: b))

  responseName :: Proxy a -> Text
  responseName p =
    append "Res::" $ pack (show $ typeRepTyCon $ typeOf (undefined :: a))

class (ToJSON a,FromJSON a,ToJSON b,FromJSON b,Typeable a,Typeable b)
  => Request b a | a -> b, b -> a
instance (Request b a) => Response a b

-- class of unsollicited message types
class (ToJSON m,FromJSON m,Typeable m) => Message m where
  messageName :: Proxy m -> Text
  messageName p =
    append "Msg::" $ pack (show $ typeRepTyCon $ typeOf (undefined :: m))
