{-# language TemplateHaskell, DerivingStrategies, DeriveGeneric, DeriveAnyClass, DuplicateRecordFields, TypeFamilies, ScopedTypeVariables, TypeApplications, PartialTypeSignatures, DataKinds #-}
module Pure.Auth.API where

import Data.JSON (ToJSON,FromJSON)
import qualified Data.Websocket as WS
import Data.Websocket (mkMessage,mkRequest,(<:>),Identify(..),Request(..),Message(..))

import Pure.Auth.Data.Email
import Pure.Auth.Data.Key
import Pure.Auth.Data.Password
import Pure.Auth.Data.Token
import Pure.Auth.Data.Username

import Data.Proxy
import Data.Typeable
import GHC.Generics

--------------------------------------------------------------------------------
-- Login

data LoginRequest = LoginRequest
  { username :: Username
  , password :: Password
  } deriving stock (Generic,Eq,Ord)
    deriving anyclass (ToJSON,FromJSON)
data Login (_role :: *)
instance Typeable _role => Identify (Login _role) 
instance Typeable _role => Request (Login _role) where
  type Req (Login _role) = (Int,LoginRequest)
  type Rsp (Login _role) = Maybe (Token _role)

login :: Proxy (Login _role)
login = Proxy

--------------------------------------------------------------------------------
-- Activate

data ActivateRequest = ActivateRequest
  { username :: Username
  , key      :: Key
  } deriving stock (Generic,Eq,Ord)
    deriving anyclass (ToJSON,FromJSON)

data Activate (_role :: *)
instance Typeable _role => Identify (Activate _role)
instance Typeable _role => Request (Activate _role) where
  type Req (Activate _role) = (Int,ActivateRequest)
  type Rsp (Activate _role) = Maybe (Token _role)

activate :: Proxy (Activate _role)
activate = Proxy

--------------------------------------------------------------------------------
-- Verify

data VerifyRequest _role = VerifyRequest
  { token :: Token _role
  } deriving stock (Generic,Eq,Ord)
    deriving anyclass (ToJSON,FromJSON)

data Verify (_role :: *)
instance Typeable _role => Identify (Verify _role)
instance Typeable _role => Request (Verify _role) where
  type Req (Verify _role) = (Int,VerifyRequest _role)
  type Rsp (Verify _role) = Bool
  
verify :: Proxy (Verify _role)
verify = Proxy

--------------------------------------------------------------------------------
-- Register

data RegisterMessage = RegisterMessage
  { username :: Username
  , email    :: Email
  , password :: Password
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data Register (_role :: *) 
instance Typeable _role => Identify (Register _role)
instance Typeable _role => Message (Register _role) where
  type M (Register _role) = RegisterMessage

register :: Proxy (Register _role)
register = Proxy

--------------------------------------------------------------------------------
-- Initiate Recovery

-- Note that you must have a known username to initiate recovery, as users
-- are indexed by username.
data InitiateRecoveryMessage = InitiateRecoveryMessage
  { email    :: Email
  , username :: Username
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data InitiateRecovery (_role :: *)
instance Typeable _role => Identify (InitiateRecovery _role)
instance Typeable _role => Message (InitiateRecovery _role) where
  type M (InitiateRecovery _role) = InitiateRecoveryMessage

initiateRecovery :: Proxy (InitiateRecovery _role)
initiateRecovery = Proxy

--------------------------------------------------------------------------------
-- Recover

data RecoverRequest = RecoverRequest
  { username :: Username
  , password :: Password
  , key      :: Key
  } deriving stock (Generic,Eq,Ord)
    deriving anyclass (ToJSON,FromJSON)

data Recover (_role :: *)
instance Typeable _role => Identify (Recover _role) 
instance Typeable _role => Request (Recover _role) where
  type Req (Recover _role) = (Int,RecoverRequest)
  type Rsp (Recover _role) = Maybe (Token _role)

recover :: Proxy (Recover _role)
recover = Proxy

--------------------------------------------------------------------------------
-- Update Email

data UpdateEmailRequest = UpdateEmailRequest
  { username :: Username
  , email    :: Email
  , password :: Password
  } deriving stock (Generic,Eq,Ord)
    deriving anyclass (ToJSON,FromJSON)

data UpdateEmail (_role :: *)
instance Typeable _role => Identify (UpdateEmail _role)
instance Typeable _role => Request (UpdateEmail _role) where
  type Req (UpdateEmail _role) = (Int,UpdateEmailRequest)
  type Rsp (UpdateEmail _role) = Bool

updateEmail :: Proxy (UpdateEmail _role)
updateEmail = Proxy

--------------------------------------------------------------------------------
-- Update Password

data UpdatePasswordRequest = UpdatePasswordRequest
  { username    :: Username
  , oldPassword :: Password
  , newPassword :: Password
  } deriving stock (Generic,Eq,Ord)
    deriving anyclass (ToJSON,FromJSON)

data UpdatePassword (_role :: *)
instance Typeable _role => Identify (UpdatePassword _role)
instance Typeable _role => Request (UpdatePassword _role) where
  type Req (UpdatePassword _role) = (Int,UpdatePasswordRequest)
  type Rsp (UpdatePassword _role) = Maybe (Token _role)

updatePassword :: Proxy (UpdatePassword _role)
updatePassword = Proxy

--------------------------------------------------------------------------------
-- Logout

data LogoutMessage _role = LogoutMessage
  { token :: Token _role
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data Logout (_role :: *)
instance Typeable _role => Identify (Logout _role)
instance Typeable _role => Message (Logout _role) where
  type M (Logout _role) = LogoutMessage _role

logout :: Proxy (Logout _role)
logout = Proxy

--------------------------------------------------------------------------------
-- Initiate Deletion

data InitiateDeletionMessage = InitiateDeletionMessage
  { username :: Username
  , password :: Password
  , email    :: Email 
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

-- This is a message so we don't leak information about the existence of a user.
data InitiateDeletion (_role :: *)
instance Typeable _role => Identify (InitiateDeletion _role)
instance Typeable _role => Message (InitiateDeletion _role) where
  type M (InitiateDeletion _role) = InitiateDeletionMessage

initiateDeletion :: Proxy (InitiateDeletion _role)
initiateDeletion = Proxy

--------------------------------------------------------------------------------
-- Delete

data DeleteRequest = DeleteRequest
  { username :: Username
  , password :: Password
  , email    :: Email
  , key      :: Key
  } deriving stock (Generic,Eq,Ord)
    deriving anyclass (ToJSON,FromJSON)

data Delete (_role :: *)
instance Typeable _role => Identify (Delete _role)
instance Typeable _role => Request (Delete _role) where
  type Req (Delete _role) = (Int,DeleteRequest)
  type Rsp (Delete _role) = Bool

delete :: Proxy (Delete _role)
delete = Proxy

--------------------------------------------------------------------------------
-- API

type AuthMessages _role =
  [ Register _role
  , InitiateRecovery _role
  , Logout _role
  , InitiateDeletion _role
  ]

type AuthRequests _role = 
  [ Login _role
  , Activate _role
  , Verify _role
  , UpdateEmail _role
  , UpdatePassword _role
  , Delete _role
  , Recover _role
  ]

api :: forall _role. Typeable _role => WS.API (AuthMessages _role) (AuthRequests _role)
api = WS.api msgs reqs
  where
    msgs = register @_role
       <:> initiateRecovery @_role
       <:> logout @_role
       <:> initiateDeletion @_role
       <:> WS.non

    reqs = login @_role
       <:> activate @_role
       <:> verify @_role
       <:> updateEmail @_role
       <:> updatePassword @_role
       <:> delete @_role
       <:> recover @_role
       <:> WS.non
