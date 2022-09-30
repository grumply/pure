{-# language BlockArguments, RankNTypes, TypeApplications, KindSignatures, DataKinds, ScopedTypeVariables #-}
module Pure.Auth.GHC.Crypto where

import Pure.Auth.Data.Email (Email(..))
import Pure.Auth.Data.Hash (Hash(..))
import Pure.Auth.Data.Key (Key(..))
import Pure.Auth.Data.Password (Password(..))
import Pure.Auth.Data.Token (Token(..))
import Pure.Auth.Data.Username (Username)

import Data.Txt as Txt (Txt,ToTxt(..),FromTxt(..),toLower,length)

import Crypto.Random (SystemRandom,newGenIO,genBytes)
import Crypto.PasswordStore as PW (makePassword,verifyPassword)
import Data.ByteString.Char8 as BS (filter)

import Control.Monad.IO.Class (MonadIO(..))

import Control.Concurrent (MVar,newMVar,modifyMVar)
import Data.Char (isHexDigit)
import Data.Maybe (listToMaybe,mapMaybe)
import Data.Proxy (Proxy(..))
import GHC.TypeNats (Nat,KnownNat(..),natVal)
import System.IO.Unsafe (unsafePerformIO)

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

-}

--------------------------------------------------------------------------------
-- Global generator

{-# NOINLINE generator #-}
generator :: MVar SystemRandom
generator = unsafePerformIO do
  sr <- newGenIO @SystemRandom
  newMVar sr

--------------------------------------------------------------------------------
-- Generic low-level hashing using pwstore-fast for pbkdf2 hashing.

hashTxt :: forall n m x. (KnownNat n, MonadIO m) => Txt -> m (Hash n x)
hashTxt bs = fmap build make 
  where
    rounds = fromIntegral (natVal (Proxy :: Proxy n))
    tbs    = fromTxt bs
    build  = Hash . toTxt
    make   = liftIO (PW.makePassword tbs rounds)

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

hashPassword :: forall n m. (KnownNat n, MonadIO m) => Password -> m (Hash n Password)
hashPassword = hashTxt . toTxt

--------------------------------------------------------------------------------
-- Key
--   Used internally as a generic cryptographic primitive for one-time keys and
--   larger structures, like tokens.

newKey :: MonadIO m => Int -> m Key
newKey n = fmap build make
  where
    build = Key 
    make = liftIO (modifyMVar generator (go [] n))
      where
        go acc 0 gen = return (gen,Txt.toLower $ mconcat acc)
        go acc m gen =
          case genBytes m gen of
            Left _ -> newGenIO >>= go acc m
            Right (bs,gen') ->
              let t = toTxt (BS.filter isHexDigit bs)
              in go (t : acc) (m - Txt.length t) gen' 

hashKey :: forall n m. (KnownNat n, MonadIO m) => Key -> m (Hash n Key)
hashKey = hashTxt . toTxt

newHashKey :: forall n m. (KnownNat n, MonadIO m) => Int -> m (Hash n Key)
newHashKey n = hashKey =<< newKey n


--------------------------------------------------------------------------------
-- Email

hashEmail :: forall n m. (KnownNat n, MonadIO m) => Email -> m (Hash n Email)
hashEmail = hashTxt . toTxt

--------------------------------------------------------------------------------
-- Token
--   Used to authenticate sessions.

newToken :: MonadIO m => Username -> m (Token _role)
newToken un = ((Token .) . (,)) <$> pure un <*> newKey 64

hashToken :: (KnownNat n, MonadIO m) => Token _role -> m (Hash n (Token _role))
hashToken (Token (_,token)) = hashTxt (toTxt token)

checkToken :: Token _role -> [Hash n (Token _role)] -> Maybe (Hash n (Token _role))
checkToken (Token (_,token)) = unsafeCheckHashes (toTxt token)