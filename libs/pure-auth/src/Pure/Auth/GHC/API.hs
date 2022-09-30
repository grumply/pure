{-# language LambdaCase, NamedFieldPuns, RecordWildCards, BlockArguments, DuplicateRecordFields, PartialTypeSignatures, ScopedTypeVariables, DuplicateRecordFields, TypeApplications, OverloadedStrings #-}
module Pure.Auth.GHC.API (Config(..),auth,fromWebsocket) where

import Pure.Auth.API as API
import Pure.Auth.GHC.Auth
import Pure.Auth.GHC.Crypto
import Pure.Auth.Data.Email (Email(..))
import Pure.Auth.Data.Hash (Hash(..))
import Pure.Auth.Data.Key (Key(..))
import Pure.Auth.Data.Password (Password(..))
import Pure.Auth.Data.Token (Token(..))
import Pure.Auth.Data.Username (Username,normalize)

import qualified Data.Bloom.Limiter as Limiter
import Data.Time
import Data.Txt as Txt
import Data.Websocket as WS
import Data.Sorcerer hiding (event,Deleted)

import Control.Exception as E
import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad (when,liftM2)
import Data.IORef
import Data.Maybe
import Data.Typeable
import Prelude hiding (read)
import System.IO.Unsafe

import qualified Data.IP as IP (IP(..),fromSockAddr)

fromWebsocket :: Websocket -> Txt
fromWebsocket ws_ = unsafePerformIO $ do
  ws <- liftIO (readIORef ws_)

  let
    -- 192.0.2.0 is reserved for documentation and examples, 
    -- so should be safe as a default. Though I'm not sure
    -- that it should ever appear.
    ip = fromMaybe "192.0.2.0" do 
      (sa,_,_,_) <- wsSocket ws
      (ip,_) <- IP.fromSockAddr sa
      pure ip

  pure $!
    case ip of
      IP.IPv4 ipv4 -> toTxt (show ipv4)
      IP.IPv6 ipv6 -> toTxt (show ipv6)

-- TODO: use buffered to protect timing information
buffered :: MonadIO m => Time -> m a -> m a 
buffered t m = do
  start <- liftIO time
  a <- m
  end <- liftIO time
  let delta = end - start
  if delta < t then do
    liftIO (delay (t - delta))
    pure a
  else
    pure a

--------------------------------------------------------------------------------
-- Auth API implementation

data Config _role = Config
  { validateUsername :: Username -> Bool
  , onTokenChange    :: Maybe (Token _role) -> IO ()
  , onDeleted        :: Username -> Email -> IO ()
  , onRegister       :: Username -> Email -> Key -> IO () -> IO ()
  , onRecover        :: Username -> Email -> Key -> IO ()
  , onDelete         :: Username -> Email -> Key -> IO ()
  }

auth :: Typeable _role => Websocket -> Config _role -> Endpoints (AuthMessages _role) (AuthRequests _role) (AuthMessages _role) (AuthRequests _role)
auth ws config = Endpoints API.api msgs reqs
  where
    ip = fromWebsocket ws

    msgs = handleRegister ip config
       <:> handleInitiateRecovery ip config
       <:> handleLogout ip config
       <:> handleInitiateDelete ip config
       <:> WS.non

    reqs = handleLogin ip config
       <:> handleActivate ip config
       <:> handleVerify ip config
       <:> handleUpdateEmail ip config
       <:> handleUpdatePassword ip config
       <:> handleDelete ip config
       <:> handleRecover ip config
       <:> WS.non

-- TODO: add case-specific failure callbacks for when `allowed` returns False
allowed :: MonadIO m => Txt -> Username -> m Bool
allowed ip un = liftIO $ 
  liftM2 (&&)
    (Limiter.allowed 20 "pure-auth" ip (Minutes 10 0))
    (Limiter.allowed 20 "pure-auth" (toTxt un) (Minutes 10 0))

handleRegister :: forall _role. Typeable _role => Txt -> Config _role -> MessageHandler (API.Register _role)
handleRegister ip Config { onRegister, validateUsername } = awaiting do
  RegisterMessage { email = e, ..} <- acquire

  let un = normalize username

  allow <- allowed ip un

  if allow then

    when (validateUsername un) do
      read (AuthEventStream un :: Stream (AuthEvent _role)) >>= \case

        Just (_ :: Auth _role) -> 
          pure ()

        _ -> do
          k     <- newKey 64
          email <- hashEmail e
          key   <- hashKey k
          pass  <- hashPassword password
          r     <- let username = un in observe (AuthEventStream un :: Stream (AuthEvent _role)) (Registered {..} :: AuthEvent _role)
          let activate = write (AuthEventStream un :: Stream (AuthEvent _role)) (Activated :: AuthEvent _role)
          case r of
            -- Check if `Added` to prevent re-initialization after deletion.
            Added (_ :: Auth _role) -> liftIO (onRegister un e k activate)
            _ -> pure ()

  else
    pure ()

handleInitiateRecovery :: forall _role. Typeable _role => Txt -> Config _role -> MessageHandler (API.InitiateRecovery _role)
handleInitiateRecovery ip Config { onRecover } = awaiting do
  InitiateRecoveryMessage { email = e, ..} <- acquire

  let un = normalize username

  allow <- allowed ip un

  if allow then

    read (AuthEventStream un :: Stream (AuthEvent _role)) >>= \case

      Just (Auth { email, activation = Nothing } :: Auth _role) | checkHash e email -> do
        k   <- newKey 64
        key <- hashKey k
        write (AuthEventStream un :: Stream (AuthEvent _role)) (StartedRecovery {..} :: AuthEvent _role)
        liftIO (onRecover un e k)
      _ -> 
        pure ()

  else
    pure ()

handleUpdateEmail :: forall _role. Typeable _role => Txt -> Config _role -> RequestHandler (API.UpdateEmail _role)
handleUpdateEmail ip Config { onRecover } = responding do
  UpdateEmailRequest { email = e, ..} <- acquire

  let un = normalize username
  
  allow <- allowed ip un

  if allow then

    read (AuthEventStream un :: Stream (AuthEvent _role)) >>= \case

      Just (Auth { activation = Nothing, pass } :: Auth _role) | checkHash password pass -> do
        email <- hashEmail e
        write (AuthEventStream un :: Stream (AuthEvent _role)) (ChangedEmail {..} :: AuthEvent _role)
        reply True

      _ ->
        reply False

  else
    reply False

handleLogout :: forall _role. Typeable _role => Txt -> Config _role -> MessageHandler (API.Logout _role)
handleLogout ip Config { onTokenChange } = awaiting do
  LogoutMessage { token = t, ..} <- acquire

  let Token (username,_) = t
      un = normalize username

  allow <- allowed ip un

  if allow then

    read (AuthEventStream un :: Stream (AuthEvent _role)) >>= \case

      Just (Auth { activation = Nothing, tokens } :: Auth _role) | Just token <- checkToken t tokens -> do
        write (AuthEventStream un :: Stream (AuthEvent _role)) (LoggedOut {..} :: AuthEvent _role)
        liftIO (onTokenChange Nothing)

      _ ->
        pure ()
        
  else
    pure ()

handleLogin :: forall _role. Typeable _role => Txt -> Config _role -> RequestHandler (API.Login _role)
handleLogin ip Config { onTokenChange } = responding do
  LoginRequest {..} <- acquire

  let un = normalize username

  allow <- allowed ip un

  if allow then
    
    read (AuthEventStream un :: Stream (AuthEvent _role)) >>= \case

      Just (Auth { activation = Nothing, pass = p } :: Auth _role) | checkHash password p -> do
        t <- newToken un
        token <- hashToken t
        write (AuthEventStream un :: Stream (AuthEvent _role)) (LoggedIn {..} :: AuthEvent _role)
        liftIO (onTokenChange (Just t))
        reply (Just t)

      _ -> do
        reply Nothing

  else do
    reply Nothing

handleActivate :: forall _role. Typeable _role => Txt -> Config _role -> RequestHandler (API.Activate _role)
handleActivate ip Config { onTokenChange } = responding do
  ActivateRequest {..} <- acquire

  let un = normalize username

  allow <- allowed ip un

  if allow then

    read (AuthEventStream un :: Stream (AuthEvent _role)) >>= \case

      Just (Auth { activation = Just a } :: Auth _role) | checkHash key a -> do
        t <- newToken un
        reply (Just t)
        token <- hashToken t
        write (AuthEventStream un :: Stream (AuthEvent _role)) (Activated :: AuthEvent _role)
        write (AuthEventStream un :: Stream (AuthEvent _role)) (LoggedIn {..} :: AuthEvent _role)

      _ ->
        reply Nothing

  else
    reply Nothing

handleVerify :: forall _role. Typeable _role => Txt -> Config _role -> RequestHandler (API.Verify _role)
handleVerify ip Config { onTokenChange } = responding do
  VerifyRequest {..} <- acquire

  let Token (username,k) = token
      un = normalize username

  allow <- allowed ip un

  if allow then

    read (AuthEventStream un :: Stream (AuthEvent _role)) >>= \case

      Just (Auth { activation = Nothing, tokens } :: Auth _role) | Just _ <- unsafeCheckHashes k tokens -> do
        liftIO (onTokenChange (Just token))
        reply True

      _ ->
        reply False

  else
    reply False

handleUpdatePassword :: forall _role. Typeable _role => Txt -> Config _role -> RequestHandler (API.UpdatePassword _role)
handleUpdatePassword ip Config { onTokenChange } = responding do
  UpdatePasswordRequest {..} <- acquire

  let un = normalize username

  allow <- allowed ip un

  if allow then

    read (AuthEventStream un :: Stream (AuthEvent _role)) >>= \case

      Just (Auth { activation = Nothing, pass } :: Auth _role) | checkHash oldPassword pass -> do
        t <- newToken un

        pass <- hashPassword newPassword
        write (AuthEventStream un :: Stream (AuthEvent _role)) (ChangedPassword {..} :: AuthEvent _role)

        token <- hashToken t
        write (AuthEventStream un :: Stream (AuthEvent _role)) (LoggedIn {..} :: AuthEvent _role)

        liftIO (onTokenChange (Just t))
        reply (Just t)

      _ ->
        reply Nothing

  else
    reply Nothing

handleRecover :: forall _role. Typeable _role => Txt -> Config _role -> RequestHandler (API.Recover _role)
handleRecover ip Config { onTokenChange } = responding do
  RecoverRequest { key = k, ..} <- acquire

  let un = normalize username

  allow <- allowed ip un

  if allow then do

    key <- hashKey k
    read (AuthEventStream un :: Stream (AuthEvent _role)) >>= \case

      Just (Auth { activation = Nothing, recovery = Just r } :: Auth _role) | key == r -> do
        t <- newToken un

        pass <- hashPassword password
        write (AuthEventStream un :: Stream (AuthEvent _role)) (ChangedPassword {..} :: AuthEvent _role)

        token <- hashToken t
        write (AuthEventStream un :: Stream (AuthEvent _role)) (LoggedIn {..} :: AuthEvent _role)
        
        liftIO (onTokenChange (Just t))
        reply (Just t)

      _ ->
        reply Nothing

  else
    reply Nothing

handleInitiateDelete :: forall _role. Typeable _role => Txt -> Config _role -> MessageHandler (API.InitiateDeletion _role)
handleInitiateDelete ip Config { onDelete } = awaiting do
  InitiateDeletionMessage { email = e, ..} <- acquire
  
  let un = normalize username

  allow <- allowed ip un

  if allow then

    read (AuthEventStream un :: Stream (AuthEvent _role)) >>= \case

      Just (Auth { email, deletion = Nothing } :: Auth _role) | checkHash e email -> do
        k   <- newKey 64
        key <- hashKey k
        write (AuthEventStream un :: Stream (AuthEvent _role)) (StartedDeletion {..} :: AuthEvent _role)
        liftIO (onDelete un e k)

      _ -> 
        pure ()

  else
    pure ()

handleDelete :: forall _role. Typeable _role => Txt -> Config _role -> RequestHandler (API.Delete _role)
handleDelete ip Config { onDeleted } = responding do
  DeleteRequest { email = e, key = k, .. } <- acquire

  let un = normalize username

  allow <- allowed ip un

  if allow then

    read (AuthEventStream un :: Stream (AuthEvent _role)) >>= \case
      Just (Auth { email, deletion = Just k' } :: Auth _role) | checkHash k k' -> do
        write (AuthEventStream un :: Stream (AuthEvent _role)) (Deleted :: AuthEvent _role)
        liftIO (onDeleted un e)
        reply True
        
      _ ->
        reply False

  else
    reply False