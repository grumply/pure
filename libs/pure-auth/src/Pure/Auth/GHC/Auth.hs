{-# language DerivingStrategies, DeriveGeneric, DeriveAnyClass, DuplicateRecordFields, RecordWildCards, DataKinds, MultiParamTypeClasses, TypeFamilies #-}
module Pure.Auth.GHC.Auth where

import Pure.Auth.Data.Email (Email)
import Pure.Auth.Data.Hash (Hash)
import Pure.Auth.Data.Password (Password)
import Pure.Auth.Data.Key (Key)
import Pure.Auth.Data.Token (Token)
import Pure.Auth.Data.Username (Username)

import Data.Txt (ToTxt(..),FromTxt(..))
import Data.JSON (ToJSON,FromJSON)
import Data.Sorcerer hiding (Event,Deleted)

import Data.Hashable

import qualified Data.List as List
import Data.Typeable
import GHC.Generics (Generic)

-- Avoids storing raw primitives, like passwords, keys, and tokens, by storing their
-- weakly PBKDF1-hashed derivatives. We rely on the fact that keys and tokens have
-- high entropy and, thus, don't need excessive rounds of hashing for our purpose 
-- (avoiding storing the raw primitive on disk). Since passwords have low average
-- entropy, we guarantee 2^10 rounds of PBKDF1. Hashing the low-entropy email simply
-- obfuscates the record on disk - it is not a cryptographically secure hash in any 
-- sense, as searching for a record for a particular email would be exceptionally
-- easy. 
data Auth _role = Auth
  { username   :: Username
  , email      :: Hash 1 Email
  , pass       :: Hash 10 Password
  , activation :: Maybe (Hash 1 Key)
  , recovery   :: Maybe (Hash 1 Key)
  , deletion   :: Maybe (Hash 1 Key)
  , tokens     :: [Hash 1 (Token _role)]
  } deriving stock (Generic,Eq,Ord)
    deriving anyclass (ToJSON,FromJSON)

data AuthEvent _role
  = LoggedIn
    { token :: Hash 1 (Token _role)
    }
  | LoggedOut
    { token :: Hash 1 (Token _role)
    }
  | Registered
    { username :: Username
    , pass  :: Hash 10 Password
    , email :: Hash 1 Email
    , key   :: Hash 1 Key
    }
  | Activated
  | StartedRecovery
    { key :: Hash 1 Key
    }
  | ChangedPassword
    { pass :: Hash 10 Password
    }
  | ChangedEmail
    { email :: Hash 1 Email
    }
  | StartedDeletion
    { key :: Hash 1 Key
    }
  | Deleted -- No key needed so it can be used for banning.
    deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

instance Typeable _role => Streamable (AuthEvent _role) where
  data Stream (AuthEvent _role) = AuthEventStream Username
    deriving stock (Generic,Ord,Eq)
    deriving anyclass Hashable
    
  stream (AuthEventStream un) = 
    "users/" <> fromTxt (toTxt un) <> ".stream"

instance Typeable _role => Aggregable (AuthEvent _role) (Auth _role) where
  update Registered {..} Nothing = 
    Update Auth
      { username   = username
      , email      = email
      , pass       = pass
      , activation = Just key
      , recovery   = Nothing
      , deletion   = Nothing
      , tokens     = []
      }

  -- The server must guarantee the activation key before
  -- triggering `Activated`.
  update Activated {} (Just a) =
    Update a { activation = Nothing }
    
  update LoggedIn {..} (Just a) =
    Update a { tokens = List.take 10 (token : tokens a) }

  update LoggedOut {..} (Just a) =
    Update a { tokens = List.filter (/= token) (tokens a) }

  update StartedRecovery {..} (Just a) =
    -- Anyone can trigger an account recovery, so 
    -- we don't invalidate existing tokens here.
    Update a { recovery = Just key }

  update ChangedPassword { pass = p } (Just a) =
    Update a { tokens = [], recovery = Nothing, pass = p }
      
  update ChangedEmail { email = e } (Just a) =
    Update a { tokens = [], email = e }
    
  update (StartedDeletion k) (Just a) =
    Update a { deletion = Just k }
    
  update Deleted (Just _) = 
    Delete

  update _ _ =
    Ignore
    
  aggregate = "user.aggregate"

