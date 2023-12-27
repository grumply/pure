{-# language ScopedTypeVariables, TypeApplications, CPP, LambdaCase, RecordWildCards, ViewPatterns, NamedFieldPuns, ConstraintKinds, FlexibleContexts, RankNTypes, AllowAmbiguousTypes, OverloadedStrings, PolyKinds #-}
module Pure.Auth.Auth 
  (Authenticated
  ,User
  ,token,name,role,asRole
  ,user
  ,authenticated,authorized,authorized'

  ,Access
  ,Authentication
  ,Authenticating
  ,mtoken
  ,guarded
  ,as

  ,Username(..)
  ,Token

#ifndef __GHCJS__
  ,decrypted
  ,encrypt,decrypt
  ,encryptFile,decryptFile
  ,Secret,secret
  ,Pool,pool
#endif

  ) where

{-

This module witnesses the security approach that `pure-auth` employs:

Server:
  1. `Token` cannot be manually constructed in server code.
  2. Only `authenticated` can construct an `Authenticated` context.

Client:
  1. Assumes validity of any `Token`. 

The result are a few conveniences:

  1. Any `Authenticated` context server-side witnesses a valid `Token`, and can be assumed safe.
  2. No validation is required client-side, potentially improving performance in critical startup scenarios.

While this guarantees static (non-temporal) safety of authenticating and authorizing code 
server-side, it /does not/ guarantee the /privacy/ of client-side authenticated/authorized 
code, as doing so is impossible with JS being user-available and user-modifiable.

As an example, the following server-side code is guaranteed safe: 

> greet :: Token MyApp -> Txt
> greet = 
>   authenticated @MyApp do
>     let u = toTxt (user @App)
>     in "Hello, " <> u

If the token is invalid, `greet` is expected to throw `Unauthorized`.

-}

import Pure.Auth.Data (Secret,Secret_(..),secret,Pool,pool,Token(..),Username,pool)

import Data.ByteString.Lazy as BSL (toStrict)
import Data.JSON (Value)
import Data.List as List (lookup)
import Data.Log (Logging)
import Data.Time (time)
import Data.Txt (Txt)
import Data.Typeable (Typeable)
import Data.View (with,Exists(..),State)
import Endpoint (API,unauthorized)
import System.IO.Unsafe (unsafePerformIO)

#ifndef __GHCJS__
import Crypto.Cipher.AES (AES256)
import Crypto.Cipher.Types (BlockCipher(..), Cipher(..), nullIV, KeySizeSpecifier(..), IV, makeIV)
import Crypto.Error (CryptoFailable(..), CryptoError(..))
import Crypto.Hash (SHA256)
import Crypto.Random ( MonadRandom(getRandomBytes))
import Crypto.MAC.HMAC (hmac,HMAC(hmacGetDigest))
import Data.Aeson as JSON (encode)
import Data.ByteString.Char8 as B (ByteString,cons)
import Data.ByteString.Base64 as B64 (encode,decode)
import Data.Txt as Txt (FromTxt(..),ToTxt(..),break,uncons,readFile,writeFile)
import System.Directory (doesFileExist)
#endif

newtype User domain = User (Token domain)

type Authenticated c = Exists (User c)

token :: forall c. Authenticated c => Token c
token = let User t = it @(User c) in t

name :: forall c. Authenticated c => Username c
name = owner (token @c)

role :: forall c. Authenticated c => Maybe Txt
role = List.lookup "role" (claims (token @c))

asRole :: forall c a. Authenticated c => Txt -> a -> a
asRole r f 
  | Just r == role @c = f 
  | otherwise         = unauthorized

user :: Authenticated c => Username c -> Bool
user = (== name)

type Access domain = Maybe (Token domain)
type Authentication domain = Exists (Access domain)
type Authenticating domain = (API domain, State (Access domain), Typeable domain, Logging Value)

-- | A generic authorization primitive for a given authentication domain. 
guarded :: forall domain a. Authentication domain => a -> (Authenticated domain => a) -> a
guarded unauthd authd = maybe unauthd (\t -> with (User t) authd) (mtoken @domain)

mtoken :: forall domain. Authentication domain => Maybe (Token domain)
mtoken = it

as :: forall c a. Authentication c => Username c -> a -> (Authenticated c => a) -> a
as u fallback desired = guarded @c fallback (if user u then desired else fallback)

#ifdef __GHCJS__
-- Clients assume valid tokens! 
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
  = with (User t) r -- this is the critical point where /User/ is constructed

  | otherwise 
  = unauthorized
#endif

authorized :: forall c r. Authenticated c => Txt -> (Txt -> r) -> r
authorized c r =
  case List.lookup c (claims (token @c)) of
    Just x -> r x
    _      -> unauthorized

#ifdef __GHCJS__
authorized' :: forall c r. Authenticated c => Txt -> (Txt -> r) -> r
authorized' c r = authorized @c c r
#else
authorized' :: forall c r. (Pool c, Authenticated c) => Txt -> (Txt -> r) -> r
authorized' c r = revalidated @c @r (authorized @c c r)
#endif

#ifdef __GHCJS__
{-# INLINE revalidated #-}
revalidated :: forall c r. r -> r
revalidated = id
#else
{-# NOINLINE revalidated #-}
revalidated :: forall c r. (Pool c, Authenticated c) => r -> r
revalidated r  
  -- Keep an eye on this. 
  --
  -- It seems possible that multiple `revalidated` calls could be deduplicated, 
  -- even in the face of noinline.
  --
  -- The code I'm worried about is something like this:
  --
  -- > loop = revalidated @MyApp do { ... ; loop }
  --
  | Token { proof, expires } <- token :: Token c
  , unsafePerformIO (doesFileExist (pool @c <> fromTxt proof)) 
  , unsafePerformIO (time >>= \now -> pure (now < expires)) 
  = r

  | otherwise
  = unauthorized
#endif

#ifndef __GHCJS__
decrypted :: forall c r. (Secret c, Authenticated c) => Txt -> (Txt -> r) -> r
decrypted c r = authorized @c c (maybe unauthorized r . decrypt @c)

encrypt :: forall c. Secret c => Txt -> IO (Maybe Txt)
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

decryptFile :: forall c. Secret c => FilePath -> IO (Maybe Txt)
decryptFile fp = decrypt @c <$> Txt.readFile fp

encryptFile :: forall c. Secret c => FilePath -> Txt -> IO ()
encryptFile fp cnt = do
  encrypt @c cnt >>= \case
    Just e -> Txt.writeFile fp e
    _      -> error "Cipher initialization failed." -- this isn't recoverable? Shouldn't encrypt throw this, then?
#endif