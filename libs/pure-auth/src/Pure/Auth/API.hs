{-# LANGUAGE DerivingStrategies, DuplicateRecordFields, TypeFamilies, ScopedTypeVariables, PartialTypeSignatures, DataKinds, OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Pure.Auth.API where

import Data.JSON (ToJSON,FromJSON)
import Data.Proxy
import Data.Time
import Data.Typeable
import Endpoint
import GHC.Generics
import Pure.Auth.Data

type Authenticated c x = Endpoint (Token c -> x)

register :: Endpoint (Username c -> Email -> Password -> IO Bool)
register = "/auth/register"

activate :: Endpoint (Key -> Username c -> Email -> IO (Maybe (Token c)))
activate = "/auth/activate"

login :: Endpoint (Username c -> Password -> IO (Maybe (Token c)))
login = "/auth/login"

logout :: Endpoint (Token c -> IO ())
logout = "/auth/logout"

logoutAll :: Endpoint (Username c -> Password -> IO ())
logoutAll = "/auth/logout/all"

startRecover :: Endpoint (Username c -> Email -> IO ())
startRecover = "/auth/recover/initiate"

recover :: Endpoint (Username c -> Email -> Password -> Key -> IO (Maybe (Token c)))
recover = "/auth/recover/confirm"

startDelete :: Endpoint (Username c -> Email -> Password -> IO ())
startDelete = "/auth/delete/initiate"

delete :: Endpoint (Username c -> Email -> Key -> IO Bool)
delete = "/auth/delete/confirm"

updateEmail :: Endpoint (Username c -> Password -> Email -> Email -> IO Bool)
updateEmail = "/auth/update/email"

updatePassword :: Endpoint (Username c -> Password -> Email -> Password -> IO Bool)
updatePassword = "/auth/update/password"

-- TODO
data AuthEvent 
  = LoginSuccess 
    { time :: Time 
    , host :: Host 
    , agent :: Agent 
    }
  | LogoutSuccess 
    { time  :: Time 
    , host  :: Host 
    , agent :: Agent 
    }
  | LoginFailure 
    { time  :: Time 
    , host  :: Host 
    , agent :: Agent
    }
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (ToJSON,FromJSON)
recentAuthEvents :: Endpoint (Token c -> IO [AuthEvent])
recentAuthEvents = "/auth/events/recent"
