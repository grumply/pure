{-# language DerivingVia, TypeApplications, ScopedTypeVariables, TypeFamilies, GADTs, MultiParamTypeClasses, AllowAmbiguousTypes, OverloadedStrings #-}
module Endpoint where

import Control.Exception
import Data.Char
import Data.Proxy
import Data.JSON 
import Data.String
import Data.Theme
import Data.Txt as Txt
import Data.Typeable

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

class API r where
  api :: Txt

class Typeable r => Resource r where

  type Auth r :: *
  type Name r :: *

  data Event r :: *

  data Product r :: *
  data Preview r :: *

  type Index r :: *
  type Index r = [Preview r]

  {-# INLINE base #-}
  base :: Endpoint x
  base = ("/" <>) $ fromTxt $! Txt.toLower rep
    where
      rep = Txt.map limit $ go (typeRep (Proxy :: Proxy r))
        where
          limit c | isAscii c && isAlphaNum c = c | otherwise = '_'
          go tr =
            let tc = toTxt (show (typeRepTyCon tr))
                trs = typeRepArgs tr
            in Txt.intercalate "_" (tc : fmap go trs)

  create :: Endpoint (Auth r -> r -> IO (Maybe (Name r)))
  create = base @r <> "/create"

  raw :: Endpoint (Auth r -> Name r -> IO (Maybe r))
  raw = base @r <> "/raw"

  read :: Endpoint (Maybe (Auth r) -> Name r -> IO (Maybe (Product r)))
  read = base @r <> "/read"

  update :: Endpoint (Auth r -> Name r -> Event r -> IO ())
  update = base @r <> "/update"

  index :: Endpoint (Maybe (Auth r) -> IO (Index r))
  index = base @r <> "/index"


