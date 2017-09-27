{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
module Pure.Connection (module Pure.Connection, module Export) where

import Ef.Base

import Network.Socket (SockAddr)

import Pure.Data as Export
import Pure.WebSocket as Export

type Base = '[WS,State () Origin,Evented,State () Vault,State () Shutdown]

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
