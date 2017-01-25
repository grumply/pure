module Presence (module Presence, module Export) where

import Ef.Base as Export hiding (Object,watch,transform)
import Atomic as Export

import Atomic.WebSocket

import Atomic.Revent
import Atomic.Vault
import Atomic.With

type IsPresence ts ms =
  ( Presence_ <: ms
  , Presence_ <. ts
  , Delta (Modules ts) (Messages ms)
  )

newtype Origin = Origin SockAddr

type Presence_ =
  '[State () WebSocket
   ,State () Origin
   ,Revent
   ,State () Vault
   ,State () Shutdown
   ]
data Presence ts ms =
  Presence
    { build :: !(Modules Presence_ (Action ts IO) -> IO (Modules ts (Action ts IO)))
    , prime :: !(Code ms IO ())
    }

