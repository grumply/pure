{-# LANGUAGE LambdaCase, TypeApplications, RecordWildCards, NamedFieldPuns, RankNTypes, OverloadedStrings, DuplicateRecordFields, TypeFamilies, FlexibleContexts, ScopedTypeVariables, AllowAmbiguousTypes, UndecidableInstances, DataKinds, BlockArguments, ConstraintKinds, PatternSynonyms #-}
module Pure.Auth.Access
  ( Authentication
  , Authenticated
  , authentication
  , simple
  , guarded
  , user
  , logout
  , Warn
  , form
  , basic
  , FormState(..)
  , Form
  , login, register, activate, initiateRecovery, recover, initiateDeletion, delete, updateEmail, updatePassword
  ) where

import qualified Pure.Auth.API as Auth
import Pure.Auth.Data.Email
import Pure.Auth.Data.Key
import Pure.Auth.Data.Password
import Pure.Auth.Data.Token
import Pure.Auth.Data.Username

import Control.Cont
import qualified Data.Localstorage as LS
import Data.View (View,pattern Null,(<|),(<||>),(|>))
import Data.HTML (pattern Button,pattern Div,pattern H2,pattern Input,pattern P,pattern Placeholder,pattern TabIndex, pattern Type,pattern Value)
import Data.Events (pattern OnClick,pattern OnInput,value)
import Control.State (manage,modifyIO,put,state,zoom,Modify,State,modify)
import Data.Default (Default(..))
import Data.Txt (Txt,ToTxt(..),FromTxt(..))
import Data.Theme (Theme,pattern Themed)
import Data.Foldable (for_,traverse_)
import Effect.Fork (fork)
import Control.Reader (ask,reader,Reader)

import qualified Data.Websocket as WS
import Effect.Websocket as EWS

import Control.Concurrent
import Control.Monad
import Data.Maybe
import Data.Try
import Data.Typeable

-- Wrapper to avoid exposing a primitive `State (Try (Token domain))`.
-- Unexported.
newtype Access domain = Access { fromAccess :: Try (Token domain) }

type Authentication domain = State (Access domain)

setToken :: forall domain. Modify (Access domain) => Token domain -> IO ()
setToken = put . Access . Done

clearToken :: forall domain. Modify (Access domain) => IO ()
clearToken = put (Access Failed :: Access domain)

-- | Handle an Authentication context. Requires a Websocket context for the same
-- domain. Requires a type application of the domain for use. The domain allows
-- for multiple authentication contexts to coexist.
--
-- See `simple` for an explanation of common usage.
--
-- This approach should avoid flashing the login form before the asynchronous
-- token verification has completed, and shouldn't remove an active view in the
-- case that the server disconnected - it will only remove an authenticated view
-- if the server comes back up and the token does not verify.
-- 
authentication :: forall domain. (Websocket domain, Typeable domain) => (Authentication domain => View) -> View
authentication v = fork (stateWith (const pure) initialize v)
  where
    initialize :: Modify (Access domain) => IO (Access domain,Access domain -> IO ())
    initialize = do
      valid <- newEmptyMVar
      thread <- newEmptyMVar
      cleanup <-
        EWS.onStatus @domain \case

          -- Note that if the websocket is open or is re-opened,
          -- we must re/authenticate, as we have a new connection
          -- with a new session on the, possibly new, host.
          WS.Opened -> do
            mtid <- tryTakeMVar thread
            for_ mtid killThread
            tid <- forkIO do
              mv <- tryReadMVar valid
              authenticate >>= \case
                Nothing -> clearToken @domain
                Just t 
                  -- If we have previously authenticated, don't update the 
                  -- token on reconnection, or the view will re-render.
                  | Just () <- mv -> pure ()

                  | otherwise -> do
                    putMVar valid ()
                    setToken @domain t

            putMVar thread tid

          _ ->
            pure ()

      pure (Access Trying,\_ -> cleanup >> tryReadMVar thread >>= traverse_ killThread)

    authenticate :: IO (Maybe (Token domain))
    authenticate =
      getStoredToken @domain >>= \case
        Nothing -> pure Nothing
        Just t  -> do
          valid <- req @domain Uncached (Auth.api @domain) (Auth.verify @domain) (Auth.VerifyRequest t)
          if valid then pure (Just t) else do
            deleteStoredToken @domain
            pure Nothing

-- | Wraps up a common approach to authentication.
--
-- `simple @MyApp v` is equivalent to:
--
-- > authentication @MyApp (guarded @domain Null (basic @domain) v)
--
-- The downside of `simple` is that it requires authentication before rendering
-- the given View, regardless of the View! This is okay in some situations, but
-- you probably don't want such a naive approach applied globally. It often
-- makes more sense to introduce the `authentication` handler at the root of
-- your effect stack and then use `guarded` on a per-route or per-View basis.
simple :: forall domain. (Websocket domain, Typeable domain) => (Authenticated domain => View) -> View
simple v = authentication @domain (guarded @domain Null (basic @domain) v)

token :: Authentication domain => Maybe (Token domain)
token = try Nothing Nothing Just (fromAccess ask)

newtype User domain = User Username
type Authenticated domain = Reader (User domain)

-- The Username of the currently authenticated user for a given domain. Can
-- only be used within the authenticated branch of `guarded @domain`.
user :: forall domain. Authenticated domain => Username
user = let User un = ask :: User domain in un

-- | A generic authorization primitive for a given authentication domain. 
--
-- Given the following:
--
-- > guarded @MyApp x y z
--
-- `guarded` will evaluate to `x` only if `authentication @MyApp` has not yet
-- completed its first attempt at authentication. Otherwise, it will always
-- evaluate to `y` or `z`.
--
-- The `z` branch is granted access to the authenticated `Username` via `user`.
--
-- A common use is the display of an authentication form when unauthenticated:
--
-- > guarded @domain Null (basic @domain) do
-- >   { ... someProtectedView ... }
--
guarded :: forall domain a. Authentication domain => a -> a -> (Authenticated domain => a) -> a
guarded authenticating unauthenticated authenticated = 
  try authenticating unauthenticated 
    (\(Token (un,_)) -> reader (User @domain un) authenticated) 
    (fromAccess (ask :: Access domain))

--------------------------------------------------------------------------------
-- Basic fully-featured authentication form.

-- | The state of a fully-featured authentication form.
--
-- Note that: as used in the `basic` form, the field states are shared across
-- views so they don't clear when changing between, e.g. login and signup. 
--
-- This is exposed, but you could easily build your own, instead, if you need 
-- something more bespoke.
--
data FormState = FormState
  { username    :: Username
  , password    :: Password
  , newPassword :: Password
  , email       :: Email
  , activation  :: Key
  , recovery    :: Key
  , deletion    :: Key
  , problem     :: Bool
  }

-- | The context of a stateful form.
type Form = State FormState

-- | A simple `Form` handler.
form :: (Form => View) -> View
form = state (FormState (fromTxt def) (fromTxt def) (fromTxt def) (fromTxt def) (fromTxt def) (fromTxt def) (fromTxt def) False)

data Settings
instance Theme Settings
data Warn
instance Theme Warn
instance Theme Auth.UpdateEmail
instance Theme Auth.UpdatePassword
instance Theme Auth.Login
instance Theme Auth.Logout
instance Theme Auth.Register
instance Theme Auth.Activate
instance Theme Auth.InitiateDeletion
instance Theme Auth.Delete
instance Theme Auth.InitiateRecovery
instance Theme Auth.Recover
instance Theme Username
instance Theme Password
instance Theme Email
instance Theme Key

-- | A basic fully-featured authentication form. Customizable via theming. 
-- Includes, by default, login, registration, activation, recovery, password
-- change, email change, and account deletion. 
-- 
-- Account deletion by default means the server needs to handle that case for
-- generated user content. There is a callback in the default auth api server 
-- configuration for such a case. See: `Pure.Auth.GHC.API.Config.onDelete`.
--
-- Note: a domain is required via TypeApplication.
--
-- Note: some things, like timeouts, are not handled here natively, and you'll
-- need to build a custom form for that. We could split this out into a module
-- that exports each of the dynamic sub-forms to make constructing custom 
-- authentication forms slightly more convenient. It's not too difficult to
-- build a custom form without doing so, though; most of this form is just
-- boilerplate of sub-forms within a dynamic form continuation context.
-- 
-- TODO: Add aria roles/accessibility.
--
basic :: forall domain. (Typeable domain, Authentication domain) => View
basic = form (cont authenticating)
  where
    warning :: Reader FormState => View -> View
    warning = if problem ask then Themed @Warn else id

    usernameField :: Form => View
    usernameField =
      zoom (\FormState { username } -> username) (\un fs -> (fs :: FormState){ username = un, problem = False }) do
        Input <| Themed @Username . TabIndex 0 . OnInput (traverse_ (put . fromTxt @Username) . value) . Placeholder "Username" . Type "name" . Value (toTxt @Username ask)

    emailField :: Form => View
    emailField =
      zoom (\FormState { email } -> email) (\e fs -> (fs :: FormState) { email = e, problem = False }) do
        Input <| Themed @Email . TabIndex 0 . OnInput (traverse_ (put . fromTxt @Email) . value) . Placeholder "Email" . Type "email" . Value (toTxt @Email ask)

    passwordField :: Form => View
    passwordField =
      zoom (\FormState { password } -> password) (\pw fs -> (fs :: FormState) { password = pw, problem = False }) do
        Input <| Themed @Password . TabIndex 0 . OnInput (traverse_ (put . fromTxt @Password) . value) . Placeholder "Password" . Type "password" . Value (toTxt @Password ask)

    oldPasswordField :: Form => View
    oldPasswordField =
      zoom (\FormState { password } -> password) (\pw fs -> (fs :: FormState) { password = pw, problem = False }) do
        Input <| Themed @Password . TabIndex 0 . OnInput (traverse_ (put . fromTxt @Password) . value) . Placeholder "Old Password" . Type "password" . Value (toTxt @Password ask)

    newPasswordField :: Form => View
    newPasswordField =
      zoom (\FormState { newPassword } -> newPassword) (\pw fs -> (fs :: FormState) { newPassword = pw, problem = False }) do
        Input <| Themed @Password . TabIndex 0 . OnInput (traverse_ (put . fromTxt @Password) . value) . Placeholder "New Password" . Type "password" . Value (toTxt @Password ask)

    authenticating :: Dynamic Form
    authenticating = dynamic do
      let
        handleLogin :: Modify FormState => IO ()
        handleLogin = do
          modifyIO do
            let FormState {..} = ask
            success <- login @domain username password
            when success (unify settings)
            pure ask { problem = Prelude.not success }

      Div <| Themed @Auth.Login . warning |>
        [ H2 <||> [ "Logging In" ]
        , usernameField
        , passwordField
        , Button <| Themed @Auth.Login . TabIndex 0 . OnClick (const handleLogin) |> [ "Log In" ]
        , Button <| Themed @Auth.Register . TabIndex 0 . OnClick (\_ -> unify registering) |> [ "Sign Up" ]
        , Button <| Themed @Auth.InitiateRecovery . TabIndex 0 . OnClick (\_ -> unify recovering) |> [ "Recover" ]
        ]

    registering :: Dynamic Form
    registering = step1
      where
        step1 :: Dynamic Form
        step1 = dynamic do
          let
            handleRegister :: Modify FormState => IO ()
            handleRegister = modifyIO do
              let FormState {..} = ask
              register @domain username email password
              unify step2
              pure ask { problem = False }

          Div <| Themed @Auth.Register . warning |>
            [ H2 <||> [ "Signing Up" ]
            , emailField
            , usernameField
            , passwordField
            , Button <| Themed @Auth.Register . TabIndex 0 . OnClick (const handleRegister) |> [ "Sign Up" ]
            , Button <| Themed @Auth.Activate . TabIndex 0 . OnClick (\_ -> unify step2) |> [ "Have Code" ]
            , Button <| Themed @Auth.Login . TabIndex 0 . OnClick (\_ -> unify authenticating) |> [ "Log In" ]
            , Button <| Themed @Auth.InitiateRecovery . TabIndex 0 . OnClick (\_ -> unify recovering) |> [ "Recover" ]
            ]

        step2 :: Dynamic Form
        step2 = dynamic do
          let
            handleActivate :: Modify FormState => IO ()
            handleActivate = modifyIO do
              let FormState {..} = ask
              activated <- activate @domain username activation
              when activated (unify settings)
              pure ask { problem = False }

            activationKeyField :: Modify FormState => View
            activationKeyField =
              zoom (\FormState { activation } -> activation) (\a fs -> (fs :: FormState) { activation = a }) do
                Input <| Themed @Username . TabIndex 0 . OnInput (traverse_ (put . fromTxt @Key) . value) . Placeholder "Activation Code" . Type "text" . Value (toTxt @Key ask)

          Div <| Themed @Auth.Activate . warning |>
            [ H2 <||> [ "Activating" ]
            , P <||> [ "Check your email for an activation code." ]
            , usernameField
            , activationKeyField
            , Button <| Themed @Auth.Activate . TabIndex 0 . OnClick (const handleActivate) |> [ "Activate" ]
            , Button <| Themed @Auth.Login . TabIndex 0 . OnClick (\_ -> unify authenticating) |> [ "Log In" ]
            , Button <| Themed @Auth.InitiateRecovery . TabIndex 0 . OnClick (\_ -> unify recovering) |> [ "Recover" ]
            ]

    recovering :: Dynamic Form
    recovering = step1
      where
        step1 :: Dynamic Form
        step1 = dynamic do
          let
            handleRecover :: Modify FormState => IO ()
            handleRecover = modifyIO do
              let FormState {..} = ask
              initiateRecovery @domain username email
              unify step2
              pure ask { problem = False }

          Div <| Themed @Auth.InitiateRecovery . warning |>
            [ H2 <||> [ "Recovering" ]
            , emailField
            , usernameField
            , Button <| Themed @Auth.InitiateRecovery . TabIndex 0 . OnClick (const handleRecover) |> [ "Start Recovery" ]
            , Button <| Themed @Auth.Recover . TabIndex 0 . OnClick (\_ -> unify step2) |> [ "Have Code" ]
            , Button <| Themed @Auth.Login . TabIndex 0 . OnClick (\_ -> unify authenticating) |> [ "Log In" ]
            , Button <| Themed @Auth.Register . TabIndex 0 . OnClick (\_ -> unify registering) |> [ "Sign Up" ]
            ]

        step2 :: Dynamic Form
        step2 = dynamic do
          let
            handleValidate :: Modify FormState => IO ()
            handleValidate = modifyIO do
              let FormState {..} = ask
              recovered <- recover @domain username password recovery
              when recovered (unify settings)
              pure ask { problem = False }

            recoveryKeyField :: Modify FormState => View
            recoveryKeyField =
              zoom (\FormState { recovery } -> recovery) (\r fs -> (fs :: FormState) { recovery = r }) do
                Input <| Themed @Key . TabIndex 0 . OnInput (traverse_ (put . fromTxt @Key) . value) . Placeholder "Recovery Code" . Type "text" . Value (toTxt @Key ask)

          Div <| Themed @Auth.Recover . warning |>
            [ H2 <||> [ "Validating" ]
            , P <||> [ "Check your email for a recovery code." ]
            , usernameField
            , newPasswordField
            , recoveryKeyField
            , Button <| Themed @Auth.Recover . TabIndex 0 . OnClick (const handleValidate)  |> [ "Validate" ]
            , Button <| Themed @Auth.Login . TabIndex 0 . OnClick (\_ -> unify authenticating) |> [ "Log In" ]
            , Button <| Themed @Auth.Register . TabIndex 0 . OnClick (\_ -> unify registering) |> [ "Sign Up" ]
            ]

    deleting :: Dynamic Form
    deleting = step1
      where
        step1 :: Dynamic Form
        step1 = dynamic do
          let
            handleDelete :: Modify FormState => IO ()
            handleDelete = modifyIO do
              let FormState {..} = ask
              initiateDeletion @domain username email password
              unify step2
              pure ask { problem = False }

          Div <| Themed @Auth.InitiateDeletion . warning |>
            [ H2 <||> [ "Deleting" ]
            , emailField
            , usernameField
            , passwordField
            , Button <| Themed @Auth.InitiateDeletion . TabIndex 0 . OnClick (const handleDelete) |> [ "Start Deleting" ]
            , Button <| Themed @Auth.Delete . TabIndex 0 . OnClick (\_ -> unify step2) |> [ "Have Code" ]
            , Button <| Themed @Auth.Login . TabIndex 0 . OnClick (\_ -> unify authenticating) |> [ "Log In" ]
            , Button <| Themed @Auth.Register . TabIndex 0 . OnClick (\_ -> unify registering) |> [ "Sign Up" ]
            ]

        step2 :: Dynamic Form
        step2 = dynamic do
          let
            handleDelete :: Modify FormState => IO ()
            handleDelete = modifyIO do
              let FormState {..} = ask
              recovered <- delete @domain username password email recovery
              when recovered (unify authenticating)
              pure ask { problem = False }

            deletionKeyField :: Modify FormState => View
            deletionKeyField =
              zoom (\FormState { recovery } -> recovery) (\r fs -> (fs :: FormState) { recovery = r }) do
                Input <| Themed @Key . TabIndex 0 . OnInput (traverse_ (put . fromTxt @Key) . value) . Placeholder "Verification Code" . Type "text" . Value (toTxt @Key ask)

          Div <| Themed @Auth.Delete . warning |>
            [ H2 <||> [ "Validating" ]
            , P <||> [ "Check your email for a verification code." ]
            , emailField
            , usernameField
            , passwordField
            , deletionKeyField
            , Button <| Themed @Auth.Delete . TabIndex 0 . OnClick (const handleDelete)  |> [ "Delete" ]
            , Button <| Themed @Auth.Login . TabIndex 0 . OnClick (\_ -> unify authenticating) |> [ "Log In" ]
            , Button <| Themed @Auth.Register . TabIndex 0 . OnClick (\_ -> unify registering) |> [ "Sign Up" ]
            ]

    updatingPassword :: Dynamic Form
    updatingPassword = dynamic do
      let
        handleUpdate :: Modify FormState => IO ()
        handleUpdate = modifyIO do
          let FormState {..} = ask
          updated <- updatePassword @domain username password newPassword
          when updated (unify settings)
          pure ask { problem = False }

      Div <| Themed @Auth.UpdateEmail . warning |>
        [ H2 <||> [ "Updating Password" ]
        , usernameField
        , oldPasswordField
        , newPasswordField
        , Button <| Themed @Auth.UpdateEmail . TabIndex 0 . OnClick (const handleUpdate) |> [ "Update" ]
        , Button <| Themed @Settings . TabIndex 0 . OnClick (\_ -> unify settings) |> [ "Settings" ]
        ]

    updatingEmail :: Dynamic Form
    updatingEmail = dynamic do
      let
        handleUpdate :: Modify FormState => IO ()
        handleUpdate = modifyIO do
          let FormState {..} = ask
          updated <- updateEmail @domain username email password
          when updated (unify settings)
          pure ask { problem = False }

      Div <| Themed @Auth.UpdateEmail . warning |>
        [ H2 <||> [ "Updating Email" ]
        , emailField
        , usernameField
        , passwordField
        , Button <| Themed @Auth.UpdateEmail . TabIndex 0 . OnClick (const handleUpdate) |> [ "Update" ]
        , Button <| Themed @Settings . TabIndex 0 . OnClick (\_ -> unify settings) |> [ "Settings" ]
        ]

    -- Most uses will probably show something else upon authentication.
    settings :: Dynamic Form
    settings = dynamic do
      Div <| Themed @Settings |>
        [ H2 <||> [ "Account Settings" ]
        , Button <| Themed @Auth.UpdatePassword . TabIndex 0 . OnClick (\_ -> unify updatingPassword) |> [ "Update Password" ]
        , Button <| Themed @Auth.UpdateEmail . TabIndex 0 . OnClick (\_ -> unify updatingEmail) |> [ "Update Email" ]
        , Button <| Themed @Auth.InitiateRecovery . TabIndex 0 . OnClick (\_ -> unify recovering) |> [ "Recover" ]
        , Button <| Themed @Auth.InitiateDeletion . TabIndex 0 . OnClick (\_ -> unify deleting) |> [ "Delete" ]
        ]

--------------------------------------------------------------------------------
-- Internal communication API
-- 
-- These methods are used to construct the form, and are exposed for the case of
-- custom authentication forms.
--

login :: forall domain. (Typeable domain, Authentication domain) => Username -> Password -> IO Bool
login username password =
  req @domain Uncached (Auth.api @domain) (Auth.login @domain) Auth.LoginRequest {..} >>= \case
    Nothing -> pure False
    Just t  -> do
      storeToken @domain t
      setToken t
      pure True

logout :: forall domain. (Typeable domain, Authentication domain) => IO ()
logout = do
  deleteStoredToken @domain
  clearToken @domain
  for_ (token @domain) $ \t ->
    msg @domain (Auth.api @domain) (Auth.logout @domain) (Auth.LogoutMessage t)

register :: forall domain. (Typeable domain, Authentication domain) => Username -> Email -> Password -> IO ()
register username email password =
  msg @domain (Auth.api @domain) (Auth.register @domain) Auth.RegisterMessage {..}

activate :: forall domain. (Typeable domain, Authentication domain) => Username -> Key -> IO Bool
activate username key =
  req @domain Uncached (Auth.api @domain) (Auth.activate @domain) Auth.ActivateRequest {..} >>= \case
    Nothing -> pure False
    Just t -> do
      storeToken @domain t
      setToken t
      pure True

initiateRecovery :: forall domain. (Typeable domain, Authentication domain) => Username -> Email -> IO ()
initiateRecovery username email =
  msg @domain (Auth.api @domain) (Auth.initiateRecovery @domain) Auth.InitiateRecoveryMessage {..}

recover :: forall domain. (Typeable domain, Authentication domain) => Username -> Password -> Key -> IO Bool
recover username password key = do
  req @domain Uncached (Auth.api @domain) (Auth.recover @domain) Auth.RecoverRequest {..} >>= \case
    Nothing -> pure False
    Just t -> do
      storeToken @domain t
      setToken t
      pure True

initiateDeletion :: forall domain. (Typeable domain, Authentication domain) => Username -> Email -> Password -> IO ()
initiateDeletion username email password =
  msg @domain (Auth.api @domain) (Auth.initiateDeletion @domain) Auth.InitiateDeletionMessage {..}

delete :: forall domain. (Typeable domain, Authentication domain) => Username -> Password -> Email -> Key -> IO Bool
delete username password email key = do
  deleted <- req @domain Uncached (Auth.api @domain) (Auth.delete @domain) Auth.DeleteRequest {..}
  when deleted do
    deleteStoredToken @domain
    clearToken @domain
  pure deleted

updateEmail :: forall domain. (Typeable domain, Authentication domain) => Username -> Email -> Password -> IO Bool
updateEmail username email password =
  req @domain Uncached (Auth.api @domain) (Auth.updateEmail @domain) Auth.UpdateEmailRequest {..}

updatePassword :: forall domain. (Typeable domain, Authentication domain) => Username -> Password -> Password -> IO Bool
updatePassword username oldPassword newPassword =
  req @domain Uncached (Auth.api @domain) (Auth.updatePassword @domain) Auth.UpdatePasswordRequest {..} >>= \case
    Nothing -> pure False
    Just t -> do
      storeToken @domain t
      setToken t
      pure True

--------------------------------------------------------------------------------
-- Internal Storage API

session :: forall domain. Typeable domain => Txt
session = "pure-auth-session-" <> tc
  where tc = toTxt (show (typeRepTyCon (typeRep (Proxy :: Proxy domain))))

storeToken :: forall domain. Typeable domain => Token domain -> IO Bool
storeToken = LS.put (session @domain)

deleteStoredToken :: forall domain. Typeable domain => IO ()
deleteStoredToken = LS.delete (session @domain)

getStoredToken :: forall domain. Typeable domain => IO (Maybe (Token domain))
getStoredToken = LS.get (session @domain)
