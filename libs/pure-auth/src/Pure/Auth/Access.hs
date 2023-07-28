{-# LANGUAGE TypeApplications, RankNTypes, OverloadedStrings, DuplicateRecordFields, TypeFamilies, FlexibleContexts, ScopedTypeVariables, AllowAmbiguousTypes, UndecidableInstances, DataKinds, BlockArguments, ConstraintKinds, PatternSynonyms, DerivingVia, PolyKinds #-}
module Pure.Auth.Access
  ( Authentication
  , Authenticated
  , access
  , simple
  , guarded
  , mtoken
  , token
  , user
  , role
  , authenticated
  , authorized
  , authorized'
  , upgraded
  , Token
  , auth
  ) where

import qualified Pure.Auth.API as Auth
import Pure.Auth.Data
import Client hiding (user)
import Endpoint (API(..))
import Control.Cont
import Control.Log
import Control.Producer as Producer
import Data.Kind
import Data.Exists
import Data.List as List
import qualified Data.Localstorage as LS
import Data.String
import Data.View (View,pattern Null,(<|),(<||>),(|>))
import Data.HTML (pattern Button,pattern Div,pattern H2,pattern Input,pattern P,pattern Placeholder,pattern TabIndex, pattern Type,pattern Value)
import Data.Events (pattern OnClick,pattern OnInput,value)
import Control.State (stateWith,modifyIO,put,state,zoom,Modify,State,modify)
import Data.Default (Default(..))
import Data.Txt (Txt,ToTxt(..),FromTxt(..))
import Data.Theme (Theme,pattern Themed,rep)
import Data.Foldable (for_,traverse_)
import Effect.Fork (fork)

import Control.Concurrent
import Control.Monad
import Data.Maybe
import Data.Try
import Data.Typeable

import qualified Data.Localstorage as Localstorage

readToken :: forall domain. Typeable domain => IO (Maybe (Token domain))
readToken = Localstorage.get ("token/" <> rep @domain) 

storeToken :: forall domain. Typeable domain => Token domain -> IO ()
storeToken = void . Localstorage.put ("token/" <> rep @domain)

deleteToken :: forall domain. Typeable domain => IO ()
deleteToken = void (Localstorage.delete ("token/" <> rep @domain))

newtype Access domain = Access { fromAccess :: Maybe (Token domain) }

type Authentication domain = (Typeable domain, State (Access domain), Logging)

setToken :: forall domain. Modify (Access domain) => Token domain -> IO ()
setToken = put . Access . Just

clearToken :: forall domain. Modify (Access domain) => IO ()
clearToken = put (Access Nothing :: Access domain)

access :: forall domain. (Typeable domain, Logging) => (Authentication domain => View) -> View
access = stateIO (Access <$> readToken @domain)

-- | Wraps up a common approach to authentication.
--
-- `simple @domain v` is equivalent to:
--
-- > authentication @domain (guarded @domain Null (basic @domain) v)
--
simple :: forall domain. (Logging, Endpoint.API domain, Typeable domain) => Txt -> (Authenticated domain => View) -> View
simple api v = access @domain (guarded @domain (auth @domain) v)

mtoken :: forall domain. Authentication domain => Maybe (Token domain)
mtoken = fromAccess it

-- | A generic authorization primitive for a given authentication domain. 
guarded :: forall domain a. Authentication domain => a -> (Authenticated domain => a) -> a
guarded unauthenticated authenticated = 
  maybe unauthenticated 
    (\t -> with (User @domain t) authenticated) 
    (fromAccess (it :: Access domain))

logout :: forall domain. (Typeable domain, Authenticated domain, State (Access domain)) => Txt -> IO ()
logout api = do
  post api Auth.logout (token @domain)
  deleteToken @domain
  clearToken @domain

upgraded :: Authentication c => Token c -> IO ()
upgraded t = do
  storeToken t
  setToken t

newtype API = API Txt
  deriving (ToTxt,FromTxt,IsString,Eq,Ord) via Txt

auth :: forall domain. (Endpoint.API domain, Typeable domain) => Authentication domain => View
auth = 
  stream (upgraded @domain) do
    cont @(Producer (Token domain)) do
      loginForm @domain (api @domain)

logoutForm :: forall domain. (Typeable domain, Authenticated domain, State (Access domain)) => Txt -> View
logoutForm api = Div <||> [ Button <| clicks (logout @domain api) |> [ "Logout" ] ]

loginForm :: forall domain. Typeable domain => Txt -> Dynamic (Producer (Token domain)) 
loginForm api = dynamic do
  state (def :: Txt,def:: Txt) do
    let 
      (un,pw) = it

      setUsername,setPassword :: Exists Input => IO ()
      setUsername = let In InputEvent { value = un } = it in put (un,pw)
      setPassword = let In InputEvent { value = pw } = it in put (un,pw)

      login = post api (Auth.login @domain) (fromTxt un) (fromTxt pw) >>= maybe def Producer.yield

    Div <||>
      [ Label <| Display block |> [ "Username: ", Input <| inputs setUsername ]
      , Label <| Display block |> [ "Password: ", Input <| inputs setPassword ]
      , Div <||> [ Button <| clicks login |> [ "Login" ] ]
      , Div <||>
        [ Button <| clicks (unify (registerForm @domain api)) |> [ "Register" ]
        , Button <| clicks (unify (recoverForm @domain api)) |> [ "Recover" ]
        ]
      ]

recoverForm :: forall domain. Typeable domain => Txt -> Dynamic (Producer (Token domain)) 
recoverForm api = dynamic do
  state (def :: Txt,def:: Txt) do
    let 
      (un,em) = it

      setUsername,setEmail :: Exists Input => IO ()
      setUsername = let In InputEvent { value = un } = it in put (un,em)
      setEmail = let In InputEvent { value = em } = it in put (un,em)

      recover = post api (Auth.startRecover @domain) (fromTxt un) (fromTxt em)

    Div <||>
      [ Label <| Display block |> [ "Username: ", Input <| inputs setUsername ]
      , Label <| Display block |> [ "Email: ", Input <| inputs setEmail ]
      , Div <||> [ Button <| clicks recover |> [ "Recover" ] ]
      , Div <||>
        [ Button <| clicks (unify (loginForm @domain api)) |> [ "Login" ]
        , Button <| clicks (unify (registerForm @domain api)) |> [ "Register" ]
        ]
      ]

registerForm :: forall domain. Typeable domain => Txt -> Dynamic (Producer (Token domain))
registerForm api = dynamic do
  state (def :: Txt,def :: Txt,def :: Txt) do
    let
      (un,pw,em) = it

      setUsername,setPassword,setEmail :: Exists Input => IO ()
      setUsername = let In InputEvent { value = un } = it in put (un,pw,em)
      setPassword = let In InputEvent { value = pw } = it in put (un,pw,em)
      setEmail    = let In InputEvent { value = em } = it in put (un,pw,em)

      register = do
        post api (Auth.register @domain) (fromTxt un) (fromTxt em) (fromTxt pw) 
        -- TODO
        -- post api (Auth.login @domain) (fromTxt un) (fromTxt pw) >>= maybe def Producer.yield

    Div <||>
      [ Label <| Display block |> [ "Username: ", Input <| inputs setUsername ]
      , Label <| Display block |> [ "Password: ", Input <| inputs setPassword ]
      , Label <| Display block |> [ "Email: ", Input <| inputs setEmail ]
      , Div <||> [ Button <| clicks (void register) |> [ "Register" ] ]
      , Div <||> [ Button <| clicks (unify (loginForm @domain api)) |> [ "Login" ] ]
      ]

{-
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
    warning :: Exists FormState => View -> View
    warning = if problem it then Themed @Warn else id

    usernameField :: Form => View
    usernameField =
      zoom (\FormState { username } -> username) (\un fs -> (fs :: FormState){ username = un, problem = False }) do
        Input <| Themed @Username . TabIndex 0 . OnInput (traverse_ (put . fromTxt @Username) . value) . Placeholder "Username" . Type "name" . Value (toTxt @Username it)

    emailField :: Form => View
    emailField =
      zoom (\FormState { email } -> email) (\e fs -> (fs :: FormState) { email = e, problem = False }) do
        Input <| Themed @Email . TabIndex 0 . OnInput (traverse_ (put . fromTxt @Email) . value) . Placeholder "Email" . Type "email" . Value (toTxt @Email it)

    passwordField :: Form => View
    passwordField =
      zoom (\FormState { password } -> password) (\pw fs -> (fs :: FormState) { password = pw, problem = False }) do
        Input <| Themed @Password . TabIndex 0 . OnInput (traverse_ (put . fromTxt @Password) . value) . Placeholder "Password" . Type "password" . Value (toTxt @Password it)

    oldPasswordField :: Form => View
    oldPasswordField =
      zoom (\FormState { password } -> password) (\pw fs -> (fs :: FormState) { password = pw, problem = False }) do
        Input <| Themed @Password . TabIndex 0 . OnInput (traverse_ (put . fromTxt @Password) . value) . Placeholder "Old Password" . Type "password" . Value (toTxt @Password it)

    newPasswordField :: Form => View
    newPasswordField =
      zoom (\FormState { newPassword } -> newPassword) (\pw fs -> (fs :: FormState) { newPassword = pw, problem = False }) do
        Input <| Themed @Password . TabIndex 0 . OnInput (traverse_ (put . fromTxt @Password) . value) . Placeholder "New Password" . Type "password" . Value (toTxt @Password it)

    authenticating :: Dynamic Form
    authenticating = dynamic do
      let
        handleLogin :: Modify FormState => IO ()
        handleLogin = do
          modifyIO \FormState {..} -> do
            success <- login @domain username password
            when success (unify settings)
            pure FormState { problem = Prelude.not success, .. }

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
            handleRegister = modifyIO \FormState {..} -> do
              register @domain username email password
              unify step2
              pure FormState { problem = False, .. }

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
            handleActivate = modifyIO \FormState {..} -> do
              activated <- activate @domain username activation
              when activated (unify settings)
              pure FormState { problem = False, .. }

            activationKeyField :: Modify FormState => View
            activationKeyField =
              zoom (\FormState { activation } -> activation) (\a fs -> (fs :: FormState) { activation = a }) do
                Input <| Themed @Username . TabIndex 0 . OnInput (traverse_ (put . fromTxt @Key) . value) . Placeholder "Activation Code" . Type "text" . Value (toTxt @Key it)

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
            handleRecover = modifyIO \FormState {..} -> do
              initiateRecovery @domain username email
              unify step2
              pure FormState { problem = False, .. }

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
            handleValidate = modifyIO \FormState {..} -> do
              when recovered (unify settings)
              pure FormState { problem = False, .. }

            recoveryKeyField :: Modify FormState => View
            recoveryKeyField =
              zoom (\FormState { recovery } -> recovery) (\r fs -> (fs :: FormState) { recovery = r }) do
                Input <| Themed @Key . TabIndex 0 . OnInput (traverse_ (put . fromTxt @Key) . value) . Placeholder "Recovery Code" . Type "text" . Value (toTxt @Key it)

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
            handleDelete = modifyIO \FormState {..} -> do
              initiateDeletion @domain username email password
              unify step2
              pure FormState { problem = False, .. }

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
            handleDelete = modifyIO \FormState {..} -> do
              recovered <- delete @domain username password email recovery
              when recovered (unify authenticating)
              pure FormState { problem = False, .. }

            deletionKeyField :: Modify FormState => View
            deletionKeyField =
              zoom (\FormState { recovery } -> recovery) (\r fs -> (fs :: FormState) { recovery = r }) do
                Input <| Themed @Key . TabIndex 0 . OnInput (traverse_ (put . fromTxt @Key) . value) . Placeholder "Verification Code" . Type "text" . Value (toTxt @Key it)

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
        handleUpdate = modifyIO \FormState {..} -> do
          updated <- updatePassword @domain username password newPassword
          when updated (unify settings)
          pure FormState { problem = False, .. }

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
        handleUpdate = modifyIO \FormState {..} -> do
          updated <- updateEmail @domain username email password
          when updated (unify settings)
          pure FormState { problem = False, .. }

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
-}


