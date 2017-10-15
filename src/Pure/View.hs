{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE PolyKinds #-}
module Pure.View (module Export) where

import Pure.DOM           as Export (forceDiff,currentHTML,ownHTML,setEagerDiff,setManualDiff,onModel,onOwnModel,getModel,putModel,modifyModel,onRaw,getHost,setState,setProps,parent,unmountComponent,getView,getState,getProps)
import Pure.Data  as Export hiding (Text,Alt,hashed,to,from,end,wait,wrap)
import Pure.Types as Export hiding (Callback,Link,SVGLink,On,Text,Null,name,content)
import Pure.Types.View    as Export hiding (Null,content,intercept)
import Pure.Types.Feature as Export (Feature,Options(..),Target(..),Evt(..))
import Pure.Lifted  as Export (scrollToTop,blurNode,clickNode,focusNode,requestAnimationFrame,cancelAnimationFrame,isNull,findById,findByTag)
import Pure.HTML          as Export hiding (Text,hashed)
import Pure.CSS           as Export hiding (Text,Alt,hashed,to,from,end,wait,wrap,intercept)
import Pure.Styles        as Export hiding (intercept)
import Pure.Attributes    as Export
import Pure.Signals       as Export

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

