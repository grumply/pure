{-# language DeriveGeneric #-}
{-# language KindSignatures #-}
{-# language DataKinds #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
module Nuclear.Data.Msg
  ( Msg(..)
  , Header
  , Body
  , fromBS
  , module Data.Binary
  , LazyByteString
  , Text
  , Endpoint
  , module GHC.TypeLits
  ) where

import Data.Binary
import GHC.Generics
import Data.Text
import qualified Data.ByteString.Lazy as LBS
import GHC.TypeLits

type LazyByteString = LBS.ByteString

type Header = String
type Body = LazyByteString

data Msg
  = Msg
    { msgHeader :: Header
    , msgBody :: Body
    } deriving (Show,Eq,Ord,Generic)

instance Binary Msg

fromBS :: LazyByteString -> Either String Msg
fromBS lbs =
  case decodeOrFail lbs of
    Left (_,_,msg) -> Left msg
    Right (_,_,a) -> Right a

data Endpoint (sym :: Symbol) a = Endpoint a
