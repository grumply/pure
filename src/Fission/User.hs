module Fission.User where

import Ef.Base

import Fission.WebSocket

import Nuclear.Revent
import Nuclear.Vault
import Nuclear.With

type IsUser ts ms =
  ( User_ <: ms
  , User_ <. ts
  , Delta (Modules ts) (Messages ms)
  )

newtype Origin = Origin SockAddr

type User_ =
  '[State () WebSocket
   ,Revent
   ,State () Origin
   ,State () Vault
   ,State () Shutdown
   ]
data User ts ms =
  User
    { build :: !(Modules User_ (Action ts IO) -> IO (Modules ts (Action ts IO)))
    , prime :: !(Code ms IO ())
    }

