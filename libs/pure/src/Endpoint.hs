{-# language DerivingVia #-}
module Endpoint where

import Control.Exception
import Data.Proxy
import Data.JSON
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

instance Monoid (Endpoint a) where
  mempty = Endpoint (mempty,Proxy)

instance Semigroup (Endpoint a) where
  (<>) (Endpoint (pl,_)) (Endpoint (pr,_)) = Endpoint (pl <> pr,Proxy)

newtype Host = Host Txt
  deriving (ToJSON,FromJSON,ToTxt,FromTxt,Show,Eq,Ord) via Txt

newtype Agent = Agent Txt
  deriving (ToJSON,FromJSON,ToTxt,FromTxt,Show,Eq,Ord) via Txt
 
data Unauthorized = Unauthorized deriving Show
instance Exception Unauthorized

unauthorized :: a
unauthorized = throw Unauthorized
