{-# LANGUAGE DerivingVia, KindSignatures, DataKinds, RoleAnnotations, DeriveGeneric, DeriveAnyClass, FlexibleContexts, RankNTypes, AllowAmbiguousTypes, ScopedTypeVariables, ConstraintKinds, TypeApplications, CPP, RecordWildCards, NamedFieldPuns, OverloadedStrings, BlockArguments, ViewPatterns, LambdaCase #-}
module Pure.Auth.Data where

import Data.JSON hiding (Key)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Char
import Data.Either
import Data.Exists
import Data.Hashable
import Data.List as List
import Data.Maybe
import Data.String
import Data.Time
import Data.Txt as Txt
import Data.View
import Endpoint
import GHC.Generics
import GHC.TypeLits
import System.IO.Unsafe

newtype Hash (rounds :: Nat) hashOf = Hash Txt
  deriving (ToJSON,FromJSON,ToTxt,FromTxt,Show,Eq,Ord) via Txt

type role Hash nominal nominal

newtype Password = Password Txt
  deriving (ToJSON,FromJSON,ToTxt,FromTxt,Show,Eq,Ord) via Txt

newtype Username c = Username Txt
  deriving 
    (ToJSON,ToTxt,Show,Eq,Ord,Hashable
#ifndef __GHCJS__
    ,ToJSONKey, FromJSONKey
#endif
    ) via Txt

type role Username nominal

instance FromTxt (Username c) where
  fromTxt = Username . Txt.filter (\c -> isAsciiLower c || isDigit c) . Txt.take 255 . Txt.toLower

instance FromJSON (Username c) where
  parseJSON = fmap fromTxt . parseJSON 

instance IsString (Username c) where
  fromString = fromTxt . fromString

newtype Key = Key Txt
  deriving (ToJSON,FromJSON,ToTxt,FromTxt,Show,Eq,Ord) via Txt

newtype Email = Emal Txt
  deriving (ToJSON,FromJSON,ToTxt,FromTxt,Show,Eq,Ord) via Txt

data Token (c :: *) = Token
  { owner   :: Username c
  , expires :: Time
  , claims  :: [(Txt,Txt)]
  , proof   :: Txt
  } deriving stock (Generic,Eq,Ord)
    deriving anyclass (ToJSON,FromJSON)
type role Token nominal

newtype Pool_ c = Pool FilePath
type role Pool_ nominal
type Pool c = Exists (Pool_ c)

pool :: forall c. Pool c => FilePath
pool = let Pool fp = it :: Pool_ c in fp

newtype Secret_ c = Secret BS.ByteString deriving Show
instance ToJSON (Secret_ c) where toJSON (Secret sec) = toJSON (show sec)
instance FromJSON (Secret_ c) where parseJSON v = Secret . Prelude.read <$> parseJSON v
type role Secret_ nominal
type Secret c = Exists (Secret_ c)

secret :: forall c. Secret c => BS.ByteString
secret = let Secret s = it :: Secret_ c in s
