{-# language DeriveGeneric #-}
module Nuclear.Data.Msg where

import Data.Binary
import GHC.Generics
import Nuclear.Data.Text
import Nuclear.Data.ByteString
import qualified Data.ByteString.Lazy         as LBS
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
