{-# LANGUAGE LambdaCase, NamedFieldPuns, RecordWildCards, BlockArguments, DuplicateRecordFields, PartialTypeSignatures, ScopedTypeVariables, OverloadedStrings, FlexibleContexts, ViewPatterns, AllowAmbiguousTypes, RankNTypes, ConstraintKinds, KindSignatures, TypeApplications, UndecidableInstances #-}
module Pure.Auth.GHC.API where

import Control.Exception as E
import Control.Log
import Control.Monad
import qualified Data.Bloom.Limiter as Limiter
import Data.Default
import Data.Exists
import Data.Foldable
import Data.IORef
import qualified Data.List as List
import Data.Maybe
import Data.Sorcerer as Sorcerer hiding (event,Deleted)
import Data.Time as Time
import Data.Txt as Txt
import Data.Typeable
import Endpoint
import Prelude hiding (read)
import qualified Pure.Auth.API as API
import Pure.Auth.Data
import Pure.Auth.GHC.Auth
import Pure.Auth.GHC.Crypto
import Server

newtype Limiter_ (c :: *) = Limiter { allowed :: Host -> Username c -> IO Bool }
type Limiter c = Exists (Limiter_ c)

simpleLimiter :: Int -> Time -> Limiter_ c
simpleLimiter per delta = Limiter \host un ->
  liftM2 (&&)
    (Limiter.allowed per "pure-auth" (toTxt host) delta)
    (Limiter.allowed per "pure-auth" (toTxt un) delta)

register :: forall c. (Typeable c, Limiter c) => (Username c -> IO Bool) -> (Host -> Agent -> Username c -> Email -> Key -> IO () -> IO ()) -> Host -> Agent -> Username c -> Email -> Password -> IO Bool
register checkUsername onSuccess host agent un e p = do

  allow <- allowed it host un

  if allow then do

    valid <- checkUsername un

    if valid then do

      read (AuthEventStream un :: Stream (AuthEvent c)) >>= \case

        Just (_ :: Auth c) -> pure False

        _ -> do
          time <- Time.time
          username <- hashUsername un
          k <- newKey 64
          email <- hashEmail e
          key <- hashKey k
          pass <- hashPassword p
          r <- observe (AuthEventStream un :: Stream (AuthEvent c)) (Registered {..} :: AuthEvent c)
          let activate = write (AuthEventStream un :: Stream (AuthEvent c)) (Activated {..} :: AuthEvent c)
          case r of
            -- Check if `Added` to prevent re-initialization after deletion.
            Added (_ :: Auth c) -> onSuccess host agent un e k activate >> pure True
            _ -> pure False

    else
      pure False

  else
    pure False

startRecover :: forall c. (Typeable c, Limiter c) => (Host -> Agent -> Username c -> Email -> Key -> IO ()) -> Host -> Agent -> Username c -> Email -> IO ()
startRecover onSuccess host agent un e = do

  allow <- allowed it host un

  if allow then

    read (AuthEventStream un :: Stream (AuthEvent c)) >>= \case

      Just (Auth { email, key = Nothing } :: Auth c) | checkHash e email -> do
        time <- Time.time
        k    <- newKey 64
        key  <- hashKey k
        write (AuthEventStream un :: Stream (AuthEvent c)) (StartedRecovery {..} :: AuthEvent c)
        onSuccess host agent un e k

      _ -> pure ()

  else
    pure ()

activate :: forall c. (Typeable c, Limiter c, Pool c, Secret c) => (Host -> Agent -> Username c -> Email -> IO (Token c)) -> Time -> Host -> Agent -> Key -> Username c -> Email -> IO (Maybe (Token c))
activate onSuccess dur host agent key un email = do

  allow <- allowed it host un

  if allow then

    read (AuthEventStream un :: Stream (AuthEvent c)) >>= \case

      Just (Auth { key = Just a, email = e } :: Auth c)
        | checkHash key a, checkHash email e -> do
          t <- onSuccess host agent un email
          let token = proof t
          time <- Time.time
          write (AuthEventStream un :: Stream (AuthEvent c)) (Activated {..} :: AuthEvent c)
          write (AuthEventStream un :: Stream (AuthEvent c)) (LoggedIn {..} :: AuthEvent c)
          pure (Just t)

      _ ->
        pure Nothing

  else
    pure Nothing

updateEmail :: forall c. (Typeable c, Limiter c, Secret c) => (Host -> Agent -> Username c -> Email -> IO ()) -> Host -> Agent -> Username c -> Password -> Email -> Email -> IO Bool
updateEmail onSuccess host agent un password old new = do

  allow <- allowed it host un

  if allow then

    read (AuthEventStream un :: Stream (AuthEvent c)) >>= \case

      -- User must be activated.
      Just (Auth { key = Nothing, email, pass } :: Auth c)
        | checkHash password pass, checkHash old email -> do
          time  <- Time.time
          email <- hashEmail new
          write (AuthEventStream un :: Stream (AuthEvent c)) (ChangedEmail {..} :: AuthEvent c)
          onSuccess host agent un new
          pure True

      _ ->
        pure False

  else
    pure False

logout :: forall c. (Typeable c, Limiter c, Pool c) => (Host -> Agent -> Username c -> IO ()) -> Host -> Agent -> Token c -> IO ()
logout onSuccess host agent t@(owner -> un) = do

  allow <- allowed it host un

  if allow then

    read (AuthEventStream un :: Stream (AuthEvent c)) >>= \case

      Just (Auth { key = Nothing, tokens } :: Auth c)
        | proof t `elem` tokens -> do
          time <- Time.time
          let token = proof t
          write (AuthEventStream un :: Stream (AuthEvent c)) (LoggedOut {..} :: AuthEvent c)
          revoke @c token
          onSuccess host agent un

      _ ->
        pure ()

  else
    pure ()

logoutAll :: forall c. (Typeable c, Limiter c, Pool c) => (Host -> Agent -> Username c -> IO ()) -> Host -> Agent -> Username c -> Password -> IO ()
logoutAll onSuccess host agent un password = do

  allow <- allowed it host un

  if allow then

    read (AuthEventStream un :: Stream (AuthEvent c)) >>= \case

      Just (Auth { key = Nothing, pass , tokens } :: Auth c)
        | checkHash password pass -> do
          time <- Time.time
          write (AuthEventStream un :: Stream (AuthEvent c)) (LoggedOutAll {..} :: AuthEvent c)
          for_ tokens (revoke @c)
          onSuccess host agent un

      _ ->
        pure ()

  else
    pure ()


login :: forall c. (Typeable c, Limiter c, Pool c, Secret c) => (Host -> Agent -> Username c -> IO (Token c)) -> Time -> Host -> Agent -> Username c -> Password -> IO (Maybe (Token c))
login onSuccess dur host agent un password = do

  allow <- allowed it host un

  if allow then do

    time <- Time.time

    read (AuthEventStream un :: Stream (AuthEvent c)) >>= \case

      Just (Auth { key = Nothing, pass = p, tokens } :: Auth c) | checkHash password p -> do
        t <- onSuccess host agent un
        let token = proof t
            (_,expired) = List.splitAt 9 tokens
        for_ expired (revoke @c)
        write (AuthEventStream un :: Stream (AuthEvent c)) (LoggedIn {..} :: AuthEvent c)
        pure (Just t)

      _ -> do
        write (AuthEventStream un :: Stream (AuthEvent c)) (FailedLogin {..} :: AuthEvent c)
        pure Nothing

  else
    pure Nothing

updatePassword :: forall c. (Typeable c, Limiter c, Pool c, Secret c) => (Host -> Agent -> Username c -> Email -> IO ()) -> Time -> Host -> Agent -> Username c -> Password -> Email -> Password -> IO Bool
updatePassword onSuccess dur host agent un old email new = do

  allow <- allowed it host un

  if allow then

    read (AuthEventStream un :: Stream (AuthEvent c)) >>= \case

      Just (Auth { key = Nothing, email = e, pass = p } :: Auth c)
        | checkHash email e, checkHash old p -> do
          time <- Time.time
          let t = sign un (time + dur) []

          pass <- hashPassword new
          write (AuthEventStream un :: Stream (AuthEvent c)) (ChangedPassword {..} :: AuthEvent c)

          let token = proof t
          write (AuthEventStream un :: Stream (AuthEvent c)) (LoggedIn {..} :: AuthEvent c)

          onSuccess host agent un email
          pure True

      _ ->
        pure False

  else
    pure False

recover :: forall c. (Typeable c, Limiter c, Pool c, Secret c) => (Host -> Agent -> Username c -> Email -> IO (Token c)) -> Time -> Host -> Agent -> Username c -> Email -> Password -> Key -> IO (Maybe (Token c))
recover onSuccess dur host agent un email password key = do

  allow <- allowed it host un

  if allow then do

    key <- hashKey key

    read (AuthEventStream un :: Stream (AuthEvent c)) >>= \case

      Just (Auth { key = Just r, email = e } :: Auth c)
        | checkHash email e, key == r -> do
          t <- onSuccess host agent un email

          time <- Time.time
          pass <- hashPassword password
          write (AuthEventStream un :: Stream (AuthEvent c)) (ChangedPassword {..} :: AuthEvent c)

          let token = proof t
          write (AuthEventStream un :: Stream (AuthEvent c)) (LoggedIn {..} :: AuthEvent c)

          pure (Just t)

      _ ->
        pure Nothing

  else
    pure Nothing

startDelete :: forall c. (Typeable c, Limiter c) => (Host -> Agent -> Username c -> Email -> Key -> IO ()) -> Host -> Agent -> Username c -> Email -> Password -> IO ()
startDelete onSuccess host agent un e p = do

  allow <- allowed it host un

  if allow then

    read (AuthEventStream un :: Stream (AuthEvent c)) >>= \case

      Just (Auth { email, pass, deletion = Nothing } :: Auth c)
        | checkHash p pass, checkHash e email -> do
          time <- Time.time
          k    <- newKey 64
          key  <- hashKey k
          write (AuthEventStream un :: Stream (AuthEvent c)) (StartedDeletion {..} :: AuthEvent c)
          onSuccess host agent un e k

      _ ->
        pure ()

  else
    pure ()

delete :: forall c. (Typeable c, Limiter c) => (Host -> Agent -> Username c -> Email -> IO ()) -> Host -> Agent -> Username c -> Email -> Key -> IO Bool
delete onSuccess host agent un email key = do

  allow <- allowed it host un

  if allow then

    read (AuthEventStream un :: Stream (AuthEvent c)) >>= \case

      Just (Auth { email = e , deletion = Just k } :: Auth c)
        | checkHash key k, checkHash email e -> do
          time <- Time.time
          write (AuthEventStream un :: Stream (AuthEvent c)) (Deleted {..} :: AuthEvent c)
          onSuccess host agent un email
          pure True

      _ ->
        pure False

  else
    pure False

recentAuthEvents :: forall c. (Typeable c, Limiter c) => (Host -> Agent -> Username c -> IO ()) -> Host -> Agent -> Token c -> IO [API.AuthEvent]
recentAuthEvents onSuccess host agent t@(owner -> un) = do
  allow <- allowed it host un

  if allow then do

    time <- Time.time
    evs <- Sorcerer.events 0 (AuthEventStream un :: Stream (AuthEvent c))
    onSuccess host agent un
    let
      go FailedLogin {..} = Just (API.LoginFailure time host agent)
      go LoggedIn {..} = Just (API.LoginSuccess time host agent)
      go LoggedOut {..} = Just (API.LogoutSuccess time host agent)
      go _ = Nothing

    pure (List.take 100 $ List.takeWhile (\ev -> API.time ev > time - Month) (mapMaybe go (List.reverse evs)))

  else
    pure []

data Config c = Config
  { base :: forall x. Endpoint x
  , duration :: Time
  , validate :: Username c -> IO Bool -- You might want to blacklist certain user names. 
  , onRegister :: Host -> Agent -> Username c -> Email -> Key -> IO () -> IO ()
  , onActivate :: Host -> Agent -> Username c -> Email -> IO (Token c)
  , onLogin :: Host -> Agent -> Username c -> IO (Token c)
  , onLogout :: Host -> Agent -> Username c -> IO ()
  , onLogoutAll :: Host -> Agent -> Username c -> IO ()
  , onStartRecover :: Host -> Agent -> Username c -> Email -> Key -> IO ()
  , onRecover :: Host -> Agent -> Username c -> Email -> IO (Token c)
  , onStartDelete :: Host -> Agent -> Username c -> Email -> Key -> IO ()
  , onDelete :: Host -> Agent -> Username c -> Email -> IO ()
  , onUpdateEmail :: Host -> Agent -> Username c -> Email -> IO ()
  , onUpdatePassword :: Host -> Agent -> Username c -> Email -> IO ()
  , onListRecentAuthEvents :: Host -> Agent -> Username c -> IO ()
  }

instance (Secret c, Pool c) => Default (Config c) where
  def = Config
    { base = ""
    , duration = Month
    , validate = \un -> pure True
    , onRegister = \h ua un e pw activate -> activate
    , onActivate = \h ua un e -> Time.time >>= \t -> pure (sign un (t + Month) [])
    , onLogin = \h ua un -> Time.time >>= \t -> pure (sign un (t + Month) [])
    , onLogout = \h ua un -> pure ()
    , onLogoutAll = \h ua un -> pure ()
    , onStartRecover = \h ua un e k -> pure ()
    , onRecover = \h ua un e -> Time.time >>= \t -> pure (sign un (t + Month) [])
    , onStartDelete = \h ua un e k -> pure ()
    , onDelete = \h ua un e -> pure ()
    , onUpdateEmail = \h ua un e -> pure ()
    , onUpdatePassword = \h ua un e -> pure ()
    , onListRecentAuthEvents = \h ua un -> pure ()
    }

authentication :: forall c. (Typeable c, Secret c, Pool c, Limiter c) => Config c -> [Server.Handler]
authentication Config {..} =
  [ withIdentity (base <> API.register) (register validate onRegister)
  , withIdentity (base <> API.activate) (activate onActivate Month)
  , withIdentity (base <> API.login) (login onLogin Month)
  , withIdentity (base <> API.logout) (logout onLogout)
  , withIdentity (base <> API.logoutAll) (logoutAll onLogoutAll)
  , withIdentity (base <> API.startRecover) (startRecover onStartRecover)
  , withIdentity (base <> API.recover) (recover onRecover duration)
  , withIdentity (base <> API.startDelete) (startDelete onStartDelete)
  , withIdentity (base <> API.delete) (delete onDelete)
  , withIdentity (base <> API.updateEmail) (updateEmail onUpdateEmail)
  , withIdentity (base <> API.updatePassword) (updatePassword onUpdatePassword duration)
  , withIdentity (base <> API.recentAuthEvents) (recentAuthEvents onListRecentAuthEvents)
  ]

