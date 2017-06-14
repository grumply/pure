module Connection (module Connection, module Export) where

import Ef.Base as Export hiding (As,Index,child,watch,transform,construct,uncons,distribute,embed,observe,End,Nat(..),maps)
import Atomic as Export hiding (Origin)
import Prelude as Export hiding (and,or,all,exponent,tan,lookup,reverse)

import Atomic.WebSocket

import Atomic.Vault

type IsConnection' ts ms = (Base <: ms, Base <. ts, Delta (Modules ts) (Messages ms))
type IsConnection ms = IsConnection' ms ms

newtype Origin = Origin SockAddr

type Base = '[State () WebSocket,State () Origin,Evented,State () Vault,State () Shutdown]

type ConnectionBuilder ts = Modules Base (Action (Appended ts Base) IO) -> IO (Modules (Appended ts Base) (Action (Appended ts Base) IO))
type ConnectionPrimer ms = Ef (Appended ms Base) IO ()

data Connection' ts ms =
  Connection
    { build :: !(Modules Base (Action ts IO) -> IO (Modules ts (Action ts IO)))
    , prime :: !(Ef ms IO ())
    }
type Connection ms = Connection' (Appended ms Base) (Appended ms Base)

