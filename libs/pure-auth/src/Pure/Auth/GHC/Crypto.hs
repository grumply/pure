{-# language BlockArguments, RankNTypes, TypeApplications, KindSignatures, DataKinds, ScopedTypeVariables, RecordWildCards, NamedFieldPuns, FlexibleContexts, AllowAmbiguousTypes, BangPatterns #-}
module Pure.Auth.GHC.Crypto where

import Control.Concurrent (MVar,newMVar,modifyMVar)
import Control.Exception
import Control.State
import Crypto.Hash
import Crypto.PasswordStore as PW (makePassword,verifyPassword)
import Crypto.Random
import qualified Crypto.Random.Types as CRT
import Data.Aeson as JSON (encode,encodeFile,decode,decodeFileStrict)
import Data.ByteString as BS (ByteString)
import Data.ByteString.Char8 as BS (filter)
import Data.ByteString.Lazy as BSL
import Data.Char (isHexDigit)
import Data.Exists
import Data.JSON (encode)
import Data.List as List
import Data.String
import Data.Time
import Data.Typeable
import Data.Maybe (listToMaybe,mapMaybe)
import Data.Proxy (Proxy(..))
import Data.Txt as Txt (Txt,ToTxt(..),FromTxt(..),toLower,length,null)
import Data.View
import Effect.Async
import GHC.TypeNats (Nat,KnownNat(..),natVal)
import Pure.Auth.Data
import System.Directory
import System.IO
import System.IO.Unsafe (unsafePerformIO)
import Server (unauthorized)

import Debug.Trace

{- NOTES

Our hashing method uses 2^10 rounds of pbkdf2, taking on the order of 20ms to 
generate. I would prefer to use the suggested 2^17 rounds, but running this on a
single server makes that prohibitively expensive. We amortize this cost with a 
Token (256-bit cryptographically secure random hex value) that can rapidly 
verify.

Note that `newKey` and its derivative `newToken` share a global SystemRandom.
Authentication is, therefore, a global lock for all authentication actions!

TODO: It might be worth generating a new SystemRandom per newKey/newToken since 
      the cost will be dwarfed by the cost of 2^n hash rounds.

TODO: Make the hash rounds configurable.

TODO: Consider making the generator per-connection to improve performance
      and remove a global lock! What's the cost of `newGenIO`? Can `SystemRandom`
      be used in a multi-threaded fashion, or does it incur a global lock while
      accessing its entropy source?

A token is a valid proof of authentication for the associated token owner.



-}

--------------------------------------------------------------------------------
-- Generic low-level hashing using pwstore-fast for pbkdf2 hashing.

hashTxt :: forall n x. KnownNat n => Txt -> IO (Hash n x)
hashTxt bs = fmap build make
  where
    rounds = fromIntegral (natVal (Proxy :: Proxy n))
    tbs    = fromTxt bs
    build  = fromTxt . toTxt
    make   = PW.makePassword tbs rounds

unsafeCheckHash :: forall n a b. ToTxt a => a -> Hash n b -> Bool
unsafeCheckHash a h =
  let
    abs = fromTxt (toTxt a)
    hbs = fromTxt (toTxt h)
  in
    PW.verifyPassword abs hbs

checkHash :: forall n a. ToTxt a => a -> Hash n a -> Bool
checkHash = unsafeCheckHash

unsafeCheckHashes :: forall n a b. ToTxt a => a -> [Hash n b] -> Maybe (Hash n b)
unsafeCheckHashes a = listToMaybe . mapMaybe (\h -> if unsafeCheckHash a h then Just h else Nothing) 

checkHashes :: forall n a. ToTxt a => a -> [Hash n a] -> Maybe (Hash n a)
checkHashes = unsafeCheckHashes

--------------------------------------------------------------------------------
-- Passwords

hashPassword :: forall n. (KnownNat n) => Password -> IO (Hash n Password)
hashPassword = hashTxt . toTxt

--------------------------------------------------------------------------------
-- Key
--   Used internally as a generic cryptographic primitive for one-time keys and
--   larger structures, like tokens.

newKey :: Int -> IO Key
newKey n = fromTxt . toTxt . show . (hash :: BS.ByteString -> Digest SHA3_512) <$> getRandomBytes n

hashKey :: forall n. KnownNat n => Key -> IO (Hash n Key)
hashKey = hashTxt . toTxt

newHashKey :: forall n. KnownNat n => Int -> IO (Hash n Key)
newHashKey n = hashKey =<< newKey n


--------------------------------------------------------------------------------
-- Email

hashEmail :: forall n. (KnownNat n) => Email -> IO (Hash n Email)
hashEmail = hashTxt . toTxt

--------------------------------------------------------------------------------
-- Username

hashUsername :: forall n c. (KnownNat n) => Username c -> IO (Hash n (Username c))
hashUsername = hashTxt . toTxt

--------------------------------------------------------------------------------
-- Token

hashToken :: Token c -> Hash 1 (Token c)
hashToken = fromTxt . proof

--------------------------------------------------------------------------------

withPool :: forall c. Typeable c => FilePath -> (Pool c => View) -> View
withPool fp = stateIO (createDirectoryIfMissing True fp >> pure (Pool fp :: Pool_ c))

withSecret :: forall c. Typeable c => IO (Secret_ c) -> (Secret c => View) -> View
withSecret = async 

-- | Warning! Assumes that if a token is materialized, it is valid.
withProofs :: forall c a. Token c -> (Proofs c => a) -> a
withProofs token = with (Proofs token :: Proofs_ c)

newSecret :: IO (Secret_ c)
newSecret = Secret <$> CRT.getRandomBytes 16

secretFile :: FilePath -> IO (Secret_ c)
secretFile fp = do
  fe <- doesFileExist fp
  if fe then do
    !ms <- decodeFileStrict fp
    case ms of
      Nothing -> do
        !sec <- newSecret
        encodeFile fp sec
        pure sec
      Just s -> do
        pure s
  else do
    !sec <- newSecret
    encodeFile fp sec
    pure sec

pool :: forall c. Pool c => FilePath
pool = let Pool p = it :: Pool_ c in p

sign :: forall c. (Pool c, Secret c) => Username c -> Time -> [(Txt,Txt)] -> Token c
sign owner expires@(Seconds i _) claims = 
  let 
    t = fromIntegral (round i :: Int)
    f = pool @c <> fromTxt proof
  in
    unsafePerformIO (openFile f WriteMode >>= hClose) `seq` Token {..}
  where
    Secret s = it :: Secret_ c
    proof = toTxt (show (hashWith SHA256 (s <> BSL.toStrict (JSON.encode (owner,expires,claims)))))

revoke :: forall c. Pool c => Txt -> IO ()
revoke proof = removeFile (pool @c <> fromTxt proof)

authenticated :: forall c r. (Pool c, Secret c) => (Proofs c => r) -> (Token c -> r)
authenticated r t@Token {..} 
  | Secret s <- it :: Secret_ c
  , h <- show (hashWith SHA256 (s <> BSL.toStrict (JSON.encode (owner,expires,claims))))
  , h == fromTxt proof
  , unsafePerformIO (doesFileExist (pool @c <> h))
  , unsafePerformIO ((expires >) <$> time)
  = withProofs @c t r

  | otherwise 
  = trace "unauthorized" unauthorized

authorized :: forall c r. Proofs c => Txt -> r -> r
authorized c r =
  case proove @c c of
    Just x -> r
    _      -> unauthorized

authorized' :: forall c r. (Pool c, Proofs c) => Txt -> r -> r
authorized' c r =
  case proove' @c c of
    Just x -> r
    _      -> unauthorized

proofs :: forall c. Proofs c => [(Txt,Txt)]
proofs = let Proofs Token { claims } = it :: Proofs_ c in claims

proove :: forall c. Proofs c => Txt -> Maybe Txt
proove claim = List.lookup claim (proofs @c)

proove' :: forall c. (Pool c, Proofs c) => Txt -> Maybe Txt
proove' claim
  | Proofs Token { claims, proof } :: Proofs_ c <- it
  , unsafePerformIO (doesFileExist (pool @c <> fromTxt proof)) 
  = List.lookup claim claims

  | otherwise 
  = Nothing

