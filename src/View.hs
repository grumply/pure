{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PolyKinds #-}
module View (module Export) where

import Ef.Base              as Export hiding (As,Index,child,transform,watch,construct,uncons,distribute,embed,observe,End,Nat(..),initialize,maps)
import Atomic               as Export hiding (Alt)
import Atomic.DOM           as Export (forceDiff,currentHTML,ownHTML,setEagerDiff,setManualDiff,onModelChange,onOwnModelChange,onOwnModelChangeByProxy,getModel,putModel,modifyModel)
import Atomic.Types.View    as Export
import Atomic.Types.Feature as Export (Feature)
import Atomic.HTML          as Export
import Atomic.CSS           as Export
import Atomic.Attributes    as Export

import Atomic.Signals       as Export

type Static = Controller '[] (Const ())
-- static :: ControllerKey '[] () -> HTML [] () -> Static
static :: ControllerKey '[] (Const ()) -> View (Base (Const ())) -> Static
static key0 view0 = Controller {..}
  where
    key = key0
    build = return
    prime = return ()
    model = Const ()
    view _ = view0

