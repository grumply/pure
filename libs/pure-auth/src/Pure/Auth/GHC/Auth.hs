{-# language DerivingStrategies, DeriveGeneric, DeriveAnyClass, DuplicateRecordFields, RecordWildCards, DataKinds, MultiParamTypeClasses, TypeFamilies, ScopedTypeVariables, NamedFieldPuns #-}
module Pure.Auth.GHC.Auth where

import Data.Hashable
import Data.JSON (ToJSON,FromJSON)
import qualified Data.List as List
import Data.Sorcerer hiding (Event,Deleted)
import Data.Time
import Data.Txt (Txt,ToTxt(..),FromTxt(..))
import Data.Typeable
import GHC.Generics (Generic)
import Pure.Auth.Data
import Server (Host(..),Agent(..))

-- Avoids storing raw primitives, like passwords, keys, and tokens, by storing their
-- weakly PBKDF1-hashed derivatives. We rely on the fact that keys and tokens have
-- high entropy and, thus, don't need excessive rounds of hashing for our purpose 
-- (avoiding storing the raw primitive on disk). Since passwords have low average
-- entropy, we guarantee 2^10 rounds of PBKDF1. Hashing the low-entropy email simply
-- obfuscates the record on disk - it is not a cryptographically secure hash in any 
-- sense, as searching for a record for a particular email would be exceptionally
-- easy. 
data Auth c = Auth
  { username :: Hash 1 (Username c)
  , email    :: Hash 1 Email
  , pass     :: Hash 10 Password
  , key      :: Maybe (Hash 1 Key)
  , deletion :: Maybe (Hash 1 Key)
  , tokens   :: [Txt]
  } deriving stock (Generic,Eq,Ord)
    deriving anyclass (ToJSON,FromJSON)

data AuthEvent c
  = LoggedIn
    { time  :: Time
    , host  :: Host
    , agent :: Agent
    , token :: Txt
    }
  | FailedLogin
    { time  :: Time
    , host  :: Host
    , agent :: Agent
    }
  | LoggedOut
    { time  :: Time
    , host  :: Host
    , agent :: Agent
    , token :: Txt
    }
  | LoggedOutAll
    { time  :: Time
    , host  :: Host
    , agent :: Agent
    }
  | Registered
    { time  :: Time
    , host  :: Host
    , agent :: Agent
    , username :: Hash 1 (Username c)
    , pass  :: Hash 10 Password
    , email :: Hash 1 Email
    , key   :: Hash 1 Key
    }
  | Activated
    { time  :: Time
    , host :: Host
    , agent :: Agent
    }
  | StartedRecovery
    { time  :: Time
    , host  :: Host
    , agent :: Agent
    , key   :: Hash 1 Key
    }
  | ChangedPassword
    { time  :: Time
    , host  :: Host
    , agent :: Agent
    , pass  :: Hash 10 Password
    }
  | ChangedEmail
    { time  :: Time
    , host  :: Host
    , agent :: Agent
    , email :: Hash 1 Email
    }
  | StartedDeletion
    { time  :: Time
    , host  :: Host
    , agent :: Agent
    , key   :: Hash 1 Key
    }
  | Deleted -- No key needed so it can be used for banning.
    { time  :: Time
    , host  :: Host
    , agent :: Agent
    }
    deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

instance Typeable c => Streamable (AuthEvent c) where
  data Stream (AuthEvent c) = AuthEventStream (Username c)
    deriving stock (Generic,Ord,Eq)
    deriving anyclass Hashable
    
  stream (AuthEventStream un) = 
    "users/" <> fromTxt (toTxt un) <> ".stream"

instance Typeable c => Aggregable (AuthEvent c) (Auth c) where
  update Registered {..} Nothing = 
    Update Auth
      { username   = username
      , email      = email
      , pass       = pass
      , key        = Just key
      , deletion   = Nothing
      , tokens     = []
      }

  -- The server must guarantee the activation key before
  -- triggering `Activated`.
  update Activated {} (Just a) =
    Update (a :: Auth c) { key = Nothing }
    
  update LoggedIn {..} (Just a) =
    Update a { tokens = List.take 10 (token : tokens a) }

  update LoggedOut {..} (Just a) =
    Update a { tokens = List.filter (/= token) (tokens a) }

  update LoggedOutAll {} (Just a) =
    Update a { tokens = [] }

  update StartedRecovery {..} (Just a) =
    -- Anyone can trigger an account recovery, so 
    -- we don't invalidate existing tokens here.
    Update (a :: Auth c) { key = Just key }

  update ChangedPassword { pass = p } (Just a) =
    Update a { tokens = [], key = Nothing, pass = p }
      
  update ChangedEmail { email = e } (Just a) =
    Update a { tokens = [], email = e }
    
  update StartedDeletion { key } (Just a) =
    Update a { deletion = Just key }
 
  update Deleted {} (Just _) = 
    Delete

  update _ _ =
    Ignore
    
  aggregate = "user.aggregate"

