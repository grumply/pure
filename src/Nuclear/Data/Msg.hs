{-# language DeriveGeneric #-}
module Nuclear.Data.Msg
  ( Msg(..)
  , Header
  , Body
  , fromBS
  , module Nuclear.Data.Text
  , module Nuclear.Data.ByteString
  ) where

import Data.Binary
import GHC.Generics
import Nuclear.Data.Text
import Nuclear.Data.ByteString

type Header = Text
type Body = ByteString

data Msg
  = Msg
    { msgHeader :: Header
    , msgBody :: Body
    } deriving (Show,Eq,Ord,Generic)

instance Binary Msg

instance ToByteString Msg where
  toBS = toBS . encode

fromBS :: LazyByteString -> Either String Msg
fromBS lbs =
  case decodeOrFail lbs of
    Left (_,_,msg) -> Left msg
    Right (_,_,a) -> Right a
