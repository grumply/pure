{-# language DerivingVia, TypeApplications, ScopedTypeVariables, TypeFamilies, GADTs, MultiParamTypeClasses, AllowAmbiguousTypes, OverloadedStrings, DataKinds, UndecidableInstances #-}
module Endpoint where

import Control.Exception
import Data.Char as Char
import Data.List as List
import Data.Proxy
import Data.JSON hiding (Result)
import Data.String
import Data.Theme
import Data.Txt as Txt
import Data.Typeable
import Data.ByteString
import Data.Void

data Method = GET | HEAD | POST | PUT | PATCH | DELETE | OPTIONS | CONNECT

data Endpoint (method :: Method) a = Endpoint !(Proxy a) Txt

type GET = Endpoint 'GET
type HEAD = Endpoint 'HEAD 
type PATCH = Endpoint 'PATCH
type POST = Endpoint 'POST 
type PUT = Endpoint 'PUT 
type DELETE = Endpoint 'DELETE 
type CONNECT = Endpoint 'CONNECT 

instance Show (Endpoint method a) where
  show (Endpoint _ t) = fromTxt t

instance IsString (Endpoint method a) where
  fromString str = Endpoint Proxy (fromString str)

instance ToTxt (Endpoint method a) where
  toTxt (Endpoint _ path) = path

instance FromTxt (Endpoint method a) where
  fromTxt = Endpoint Proxy

endpoint :: Txt -> Endpoint method a
endpoint = fromTxt

instance Monoid (Endpoint method a) where
  mempty = fromTxt mempty

instance Semigroup (Endpoint method a) where
  (<>) (Endpoint _ pl) (Endpoint _ pr) = fromTxt (pl <> pr)

newtype Host = Host Txt
  deriving (ToJSON,FromJSON,ToTxt,FromTxt,Show,Eq,Ord) via Txt

newtype Agent = Agent Txt
  deriving (ToJSON,FromJSON,ToTxt,FromTxt,Show,Eq,Ord) via Txt
 
data Unauthorized = Unauthorized deriving Show
instance Exception Unauthorized

unauthorized :: a
unauthorized = throw Unauthorized

class API r where
  api :: Txt

class Typeable r => Methods r where
 
  type Create r :: *
  type Create r = Void

  type Update r :: *
  type Update r = Void

  type Query r :: *
  type Query r = Void

  type Place r :: *
  type Place r = Void

  type Delete r :: *
  type Delete r = Void

  {-# NOINLINE base #-}
  base :: Endpoint method x
  base = defaultBase @r
    
  create :: POST (Create r)
  create = base @r

  update :: PATCH (Update r)
  update = base @r

  query :: GET (Query r)
  query = base @r

  place :: PUT (Place r)
  place = base @r

  delete :: DELETE (Delete r)
  delete = base @r

defaultBase :: forall r method x. Typeable r => Endpoint method x
defaultBase = fromString ('/' : fmap Char.toLower rep)
  where
    rep = limit <$> go (typeRep (Proxy :: Proxy r))
      where
        limit c | isAscii c && isAlphaNum c = c | otherwise = '_'
        go tr =
          let tc = show (typeRepTyCon tr)
              trs = typeRepArgs tr
          in List.intercalate "_" (tc : fmap go trs)

