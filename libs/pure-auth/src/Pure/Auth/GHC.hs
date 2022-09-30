{-# language AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables, RecordWildCards, PatternSynonyms, DataKinds #-}
module Pure.Auth.GHC (module Export,authDB,tryCreateUser) where

import Pure.Auth.Data.Email
import Pure.Auth.Data.Password
import Pure.Auth.Data.Username
import Pure.Auth.GHC.API as Export
import Pure.Auth.GHC.Auth as Export (AuthEvent(Deleted),Stream(AuthEventStream),Auth)
import Pure.Auth.GHC.Auth
import Pure.Auth.GHC.Crypto

import Data.Sorcerer (sorcerer,read,write,observe,pattern Added)

import Data.Typeable

import Prelude hiding (read)

authDB :: forall _role. Typeable _role => IO ()
authDB = sorcerer @(AuthEvent _role) @'[(Auth _role)]

tryCreateUser :: forall _role. Typeable _role => Username -> Email -> Password -> IO Bool
tryCreateUser un em pw =
  read (AuthEventStream un :: Stream (AuthEvent _role)) >>= \a ->
    case a of

      Just (_ :: Auth _role) -> 
        pure False

      _ -> do
        k     <- newKey 64
        email <- hashEmail em
        key   <- hashKey k
        pass  <- hashPassword pw
        r     <- let username = un in observe (AuthEventStream un :: Stream (AuthEvent _role)) (Registered {..} :: AuthEvent _role)
        case r of
          Added (_ :: Auth _role) -> do
            write (AuthEventStream un :: Stream (AuthEvent _role)) (Activated :: AuthEvent _role)
            pure True
          _ -> 
            pure False