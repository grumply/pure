{-# language AllowAmbiguousTypes, TypeApplications, ScopedTypeVariables, RecordWildCards, PatternSynonyms, DataKinds, LambdaCase, ViewPatterns, OverloadedStrings #-}
module Pure.Auth.GHC (module Export,authDB,tryCreateUser) where

import Data.Sorcerer (sorcerer,read,write,observe,pattern Added)
import Data.Txt (FromTxt(..))
import Data.Typeable
import qualified Data.Time as Time
import Prelude hiding (read)
import Pure.Auth.Data
import Pure.Auth.GHC.API as Export
import Pure.Auth.GHC.Auth as Export (AuthEvent(Deleted),Stream(AuthEventStream),Auth)
import Pure.Auth.GHC.Auth
import Pure.Auth.GHC.Crypto as Export

authDB :: forall c. Typeable c => IO ()
authDB = sorcerer @(AuthEvent c) @'[Auth c]

tryCreateUser :: forall c. Typeable c => Username c -> Email -> Password -> IO Bool
tryCreateUser un em pw = do
  let host = fromTxt "localhost"
      agent = fromTxt ""
  read (AuthEventStream un :: Stream (AuthEvent c)) >>= \case

    Just (_ :: Auth c) -> do
      pure False

    _ -> do
      time     <- Time.time
      k        <- newKey 64
      email    <- hashEmail em
      key      <- hashKey k
      pass     <- hashPassword pw
      username <- hashUsername un
      observe (AuthEventStream un :: Stream (AuthEvent c)) (Registered {..} :: AuthEvent c) >>= \case
        Added (_ :: Auth c) -> do
          write (AuthEventStream un :: Stream (AuthEvent c)) (Activated time host agent :: AuthEvent c)
          pure True
        _ -> do
          pure False
