{-# LANGUAGE DerivingStrategies, DuplicateRecordFields, TypeFamilies, ScopedTypeVariables, PartialTypeSignatures, DataKinds, OverloadedStrings, DeriveGeneric, DeriveAnyClass #-}
module Pure.Auth.API where

import Data.JSON (ToJSON,FromJSON)
import Data.Proxy
import Data.Time
import Data.Typeable
import Endpoint
import GHC.Generics
import Pure.Auth.Data

register :: POST (Username c -> Email -> Password -> IO Bool)
register = "/auth/register"

activate :: PATCH (Key -> Username c -> Email -> IO (Maybe (Token c)))
activate = "/auth/activate"

login :: PATCH (Username c -> Password -> IO (Maybe (Token c)))
login = "/auth/login"

logout :: PATCH (Token c -> IO ())
logout = "/auth/logout"

logoutAll :: PATCH (Username c -> Password -> IO ())
logoutAll = "/auth/logout/all"

startRecover :: PATCH (Username c -> Email -> IO ())
startRecover = "/auth/recover/initiate"

recover :: PATCH (Username c -> Email -> Password -> Key -> IO (Maybe (Token c)))
recover = "/auth/recover/confirm"

startDelete :: PATCH (Username c -> Email -> Password -> IO ())
startDelete = "/auth/delete/initiate"

delete :: PATCH (Username c -> Email -> Key -> IO Bool)
delete = "/auth/delete/confirm"

updateEmail :: PATCH (Username c -> Password -> Email -> Email -> IO Bool)
updateEmail = "/auth/update/email"

updatePassword :: PATCH (Username c -> Password -> Email -> Password -> IO Bool)
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

recentAuthEvents :: GET (Token c -> IO [AuthEvent])
recentAuthEvents = "/auth/events/recent"
