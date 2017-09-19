{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PolyKinds #-}
module View (module Export) where

import Atomic               as Export hiding (Alt)
import Atomic.DOM           as Export (forceDiff,currentHTML,ownHTML,setEagerDiff,setManualDiff,onModel,onOwnModel,getModel,putModel,modifyModel,onRaw,getHost,setState,setProps,unmountComponent,getView,getState,getProps)
import Atomic.Types.CB      as Export hiding (Callback)
import Atomic.Types.View    as Export
import Atomic.Types.Feature as Export (Feature,Options(..),Evt(..),Target(..))
import Atomic.Types.Lifted  as Export (scrollToTop,blurNode,clickNode,focusNode,requestAnimationFrame,cancelAnimationFrame,isNull,findById,findByTag)
import Atomic.Types.JSV     as Export hiding (Text)
import Atomic.HTML          as Export
import Atomic.CSS           as Export
import Atomic.CSS.Styles    as Export
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

