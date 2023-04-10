module Pure.Magician (module Export) where

import Pure.Auth as Export (Authentication,Authenticated,authentication,guarded,user,simple,Username(..),logout)
import Pure as Export hiding (Index,hex,Product,iterate,goto,user,Server,pattern Created,pattern Deleted,linear,meta,mod,next,root,run)
import Pure.Convoker as Export hiding (Root)
import Pure.Conjurer as Export hiding (root,publishing,List,Route,previews,stream,form)
import Pure.Conjurer.Analytics as Export hiding (count,Session(..))

import Data.Router as Export hiding (route,map)
import Effect.Router as Export hiding (map)
import Effect.Websocket as Export hiding (Cache)

import Pure.Magician.Resources as Export

#ifdef __GHCJS__
import Pure.Magician.Client as Export
import Pure.Magician.Client.Restore as Export
#else
import Pure.Magician.Client as Export (App,CURL,withRoute,client,basic,trivial,magic,Trivial,Viewables)
import Pure.Magician.Server as Export
#endif