module User (module User, module Export) where

import Ef.Base as Export hiding (Object,watch,transform)
import Nuclear as Export

import Nuclear.WebSocket

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

