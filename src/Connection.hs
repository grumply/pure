module Connection (module Connection, module Export) where

import Ef.Base as Export hiding (As,Index,child,watch,transform,construct,uncons,distribute,embed,observe,End,Nat(..),maps)
import Atomic as Export hiding (Origin)
import Prelude as Export hiding (any,and,or,all,exponent,tan,lookup,reverse)

import Atomic.WebSocket

import Atomic.Vault

type Base = '[State () WebSocket,State () Origin,Evented,State () Vault,State () Shutdown]

type IsConnection' ts ms = (ms <: Base, ts <. Base, ts <=> ms)
type IsConnection ms = IsConnection' ms ms

newtype Origin = Origin SockAddr

type ConnectionBuilder ts = forall b a. (b ~ Appended ts Base, a ~ Action b IO)
                          => Modules Base a -> IO (Modules b a)

type ConnectionPrimer ms = forall b. b ~ Appended ms Base => Ef b IO ()

data Connection' ts ms =
  Connection
    { build :: !(Modules Base (Action ts IO) -> IO (Modules ts (Action ts IO)))
    , prime :: !(Ef ms IO ())
    }
type Connection ms = forall b. b ~ Appended ms Base => Connection' b b
