module Endpoint where

import Data.Proxy
import Data.String
import Data.Txt

newtype Endpoint a = Endpoint (Txt,Proxy a)

instance Show (Endpoint a) where
  show (Endpoint (path,_)) = fromTxt path

instance IsString (Endpoint a) where
  fromString str = Endpoint (fromString str,Proxy)

instance ToTxt (Endpoint a) where
  toTxt (Endpoint (path,_)) = path

instance FromTxt (Endpoint a) where
  fromTxt path = Endpoint (path,Proxy)

endpoint :: Txt -> Endpoint a
endpoint path = Endpoint (path,Proxy)
