{-# LANGUAGE DerivingVia, KindSignatures, DataKinds, RoleAnnotations, DeriveGeneric, DeriveAnyClass, FlexibleContexts, RankNTypes, AllowAmbiguousTypes, ScopedTypeVariables, ConstraintKinds, TypeApplications, CPP, RecordWildCards, NamedFieldPuns, OverloadedStrings, BlockArguments, ViewPatterns #-}
module Pure.Auth.Data where

import Data.JSON hiding (Key)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Char
import Data.Either
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
#ifndef __GHCJS__
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), nullIV, KeySizeSpecifier(..), IV, makeIV)
import Crypto.Error (CryptoFailable(..), CryptoError(..))
import Crypto.Hash
import Crypto.Random
import Crypto.MAC.HMAC
import Data.Aeson as JSON
import Data.ByteString.Char8 as B
import qualified Data.ByteString.Base64 as B64
import System.Directory
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

newtype User c = User (Token c)
type Authenticated c = Exists (User c)

token :: forall c. Authenticated c => Token c
token = let User t = it :: User c in t

user :: forall c. Authenticated c => Username c
user = owner (token @c)

role :: forall c. Authenticated c => Maybe Txt
role = List.lookup "role" (claims (token @c))

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

#ifdef __GHCJS__
authenticated :: (Authenticated c => r) -> (Token c -> r)
authenticated r t = with (User t) r
#else
authenticated :: forall c r. (Pool c, Secret c) => (Authenticated c => r) -> (Token c -> r)
authenticated r t@Token {..} 
  | Secret s <- it :: Secret_ c
  , h <- show (hmacGetDigest (hmac @B.ByteString @B.ByteString @SHA256 s (BSL.toStrict (JSON.encode (owner,expires,claims)))))
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

#ifdef __GHCJS__
decrypted :: forall c r. Authenticated c => Txt -> (Txt -> r) -> r
decrypted c r = authorized @c c r
#else
decrypted :: forall c r. (Secret c, Authenticated c) => Txt -> (Txt -> r) -> r
decrypted c r = authorized @c c (maybe unauthorized r . decrypt @c)

encrypt :: forall c. (Authenticated c, Secret c) => Txt -> IO (Maybe Txt)
encrypt msg = do
  let Secret s = it :: Secret_ c 
  k :: B.ByteString <- getRandomBytes 16
  case cipherInit @AES256 s of
    CryptoPassed c -> do
      let 
        Just iv = makeIV k
        ct = ctrCombine c iv (fromTxt msg)
        m = B64.encode k <> B.cons '.' (B64.encode ct)
      pure (Just (toTxt m))
    _ -> 
      pure Nothing

decrypt :: forall c. Secret c => Txt -> Maybe Txt
decrypt msg = do
  let Secret s = it :: Secret_ c 
  case cipherInit @AES256 s of
    CryptoPassed c -> 
        let 
          (key,Txt.uncons -> Just ('.',pay)) = Txt.break (== '.') msg
          Right k = B64.decode (fromTxt key)
          Right m = B64.decode (fromTxt pay)
          Just iv = makeIV k
        in
          Just (toTxt (ctrCombine c iv m))
    _ -> 
      Nothing
#endif
