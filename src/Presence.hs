module Presence (module Presence, module Export) where

import Ef.Base as Export hiding (As,Index,watch,transform,construct,uncons,distribute,embed,observe)
import Atomic as Export
import Prelude as Export hiding (all,exponent,div,head,span,tan,lookup,reverse)

import Atomic.WebSocket

import Atomic.Revent
import Atomic.Vault
import Atomic.With

type IsPresence' ts ms =
  ( PresenceBase <: ms
  , PresenceBase <. ts
  , Delta (Modules ts) (Messages ms)
  )
type IsPresence ms = IsPresence' ms ms

newtype Origin = Origin SockAddr

type PresenceBase =
  '[State () WebSocket
   ,State () Origin
   ,Revent
   ,State () Vault
   ,State () Shutdown
   ]
type PresenceBuilder' ts = Modules PresenceBase (Action ts IO) -> IO (Modules ts (Action ts IO))
type PresenceBuilder ts = PresenceBuilder' (Appended ts PresenceBase)
type PresencePrimer' ms = Code ms IO ()
type PresencePrimer ms = Code (Appended ms PresenceBase) IO ()
type Presence ms = Presence' (Appended ms PresenceBase) (Appended ms PresenceBase)

data Presence' ts ms =
  Presence
    { build :: !(PresenceBuilder' ts)
    , prime :: !(PresencePrimer' ms)
    }

