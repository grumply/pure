{-# LANGUAGE DerivingVia, KindSignatures, DataKinds, RoleAnnotations, DeriveGeneric, DeriveAnyClass, FlexibleContexts, RankNTypes, AllowAmbiguousTypes, ScopedTypeVariables, ConstraintKinds, TypeApplications, CPP, RecordWildCards, NamedFieldPuns, OverloadedStrings #-}
module Pure.Auth.Data where

import Data.JSON hiding (Key)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Char
import Data.Exists
import Data.Hashable
import Data.List as List
import Data.String
import Data.Time
import Data.Txt as Txt
import Endpoint
import GHC.Generics
import GHC.TypeLits
import System.IO.Unsafe
#ifndef __GHCJS__
import System.Directory
import Data.Aeson as JSON
import Crypto.Hash
#endif

newtype Hash (rounds :: Nat) hashOf = Hash Txt
  deriving (ToJSON,FromJSON,ToTxt,FromTxt,Show,Eq,Ord) via Txt

type role Hash nominal nominal

newtype Password = Password Txt
  deriving (ToJSON,FromJSON,ToTxt,FromTxt,Show,Eq,Ord) via Txt

newtype Username c = Username Txt
  deriving (ToJSON,ToTxt,Show,Eq,Ord,Hashable) via Txt

type role Username nominal

instance FromTxt (Username c) where
  fromTxt = Username . Txt.toLower . Txt.filter isPrint

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

newtype Secret_ c = Secret BS.ByteString deriving Show
instance ToJSON (Secret_ c) where toJSON (Secret sec) = toJSON (show sec)
instance FromJSON (Secret_ c) where parseJSON v = Secret . Prelude.read <$> parseJSON v
type role Secret_ nominal
type Secret c = Exists (Secret_ c)

secret :: forall c. Secret c => BS.ByteString
secret = let Secret s = it :: Secret_ c in s

newtype Pool_ c = Pool FilePath
type role Pool_ nominal
type Pool c = Exists (Pool_ c)

pool :: forall c. Pool c => FilePath
pool = let Pool fp = it :: Pool_ c in fp

newtype User c = User (Token c)
type Authenticated c = Exists (User c)

token :: forall c. Authenticated c => Token c
token = let User t = it :: User c in t

user :: forall c. Authenticated c => Username c
user = owner (token @c)

role :: forall c. Authenticated c => Maybe Txt
role = List.lookup "role" (claims (token @c))

#ifdef __GHCJS__
authenticated :: (Authenticated c => r) -> (Token c -> r)
authenticated r t = with (User t) r
#else
authenticated :: forall c r. (Pool c, Secret c) => (Authenticated c => r) -> (Token c -> r)
authenticated r t@Token {..} 
  | Secret s <- it :: Secret_ c
  , h <- show (hashWith SHA256 (s <> BSL.toStrict (JSON.encode (owner,expires,claims))))
  , h == fromTxt proof
  , unsafePerformIO (doesFileExist (pool @c <> h))
  , unsafePerformIO ((expires >) <$> time)
  = with (User t) r

  | otherwise 
  = unauthorized
#endif

authorized :: forall c r. Authenticated c => Txt -> (Txt -> r) -> r
authorized c r =
  let User a = it :: User c
  in case List.lookup c (claims a) of
      Just x -> r x
      _      -> unauthorized

#ifdef __GHCJS__
authorized' :: forall c r. Authenticated c => Txt -> (Txt -> r) -> r
authorized' c r = authorized @c c r
#else
authorized' :: forall c r. (Pool c, Authenticated c) => Txt -> (Txt -> r) -> r
authorized' c r
  | User Token { proof } <- it :: User c
  , unsafePerformIO (doesFileExist (pool @c <> fromTxt proof)) 
  = authorized @c c r

  | otherwise 
  = unauthorized
#endif
