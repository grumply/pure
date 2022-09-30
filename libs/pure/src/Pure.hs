{-# language CPP #-}
module Pure (module Pure,module Export) where

import Control.Applicative as Export hiding (empty,optional)
import Control.Concurrent as Export hiding (yield)
import Control.Monad as Export (void,join,when,unless,(>=>),(<=<),forever)
import Data.Bifunctor as Export (Bifunctor(..))
import Data.Bool as Export (bool)
import Data.Char as Export hiding (Space)
import Data.Coerce as Export (coerce,Coercible)
import Data.Foldable as Export (Foldable,for_,traverse_)
import Data.Function as Export ((&),fix,on)
import Data.Maybe as Export
import Data.Monoid as Export hiding (Alt)
import Data.Traversable as Export (Traversable,for,traverse)
import Data.Typeable as Export (Typeable,typeOf)
import Debug.Trace as Export (trace,traceShow)
import GHC.Generics as Export (Generic)
import GHC.Stack as Export (HasCallStack)
import System.IO as Export (stdout,hSetBuffering,BufferMode(..))
import Text.Read as Export (readMaybe,readEither)
import Text.Printf as Export (printf)

import Control.Cont as Export hiding (empty)
import Control.Error as Export
import Control.Producer as Export
import Control.Reader as Export hiding (lazy,eager)
import Control.State as Export hiding (flat)
import Control.Writer as Export hiding (translate)
import Control.Fold as Export
import Data.DOM as Export (body,Evt)
import Data.CSS as Export hiding (root,target,active,checked,wrap,select,empty,stylesheet,CSS_(..))
import Data.Default as Export
import Data.Effect as Export hiding (map)
import Data.Events as Export hiding (button,meta,Select,ContextMenu,Accept,Target)
import Data.Hashable as Export
import Data.HTML as Export hiding (Style,DateTime)
import Data.Exists as Export
import Data.JSON as Export hiding (Null,Key,lookup,match)
import Data.Marker as Export hiding (hex)
import Data.Random as Export hiding (normal,int,next,list) 
import Data.Styles as Export hiding (not,zoom,state,delay,fill,Left,Right)
import Data.Theme as Export
import Data.Time as Export hiding (Time_)
import Data.Try as Export hiding (try)
import Data.Txt as Export (Txt,ToTxt(..),FromTxt(..))
import Data.View as Export hiding (get,ask,look,modify,modify_,modifyM,modifyM_,setProps,queueComponentUpdate,getHost,initialize,content,Styles,On)
import Data.URI as Export
import Effect.Async as Export
import Effect.Fork as Export
import Effect.Lifecycles as Export
import Effect.Meta as Export
import Effect.Router as Export hiding (map)
import Effect.Script as Export
import Effect.Styles as Export hiding (css)
import Effect.Suspense as Export
import Effect.Sync as Export
import Effect.Title as Export
import Effect.Poll as Export

import Browser as Export
#ifndef __GHCJS__
import Server as Export
#endif

import Prelude as Export hiding (all,any,or,and,(^^),max,min,rem,odd,drop,break,even,repeat,span,tan,reverse)

{-# INLINE with #-}
with :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
with = traverse

{-# INLINE with_ #-}
with_ :: (Foldable t, Applicative f) => (a -> f b) -> t a -> f ()
with_ = traverse_

-- | A variant of `fix` 'fixed' to a function (`->`) type with a convenient
-- ordering for inline usage: `flip fix`. For introducing local anonymous
-- recursive functions.
-- 
-- > loop initialState \k currentState -> 
-- >   { ... k newState ... }
--
-- NOTE: the type of `loop`, while fixed to an arrow type, is still polymorphic
--       and remains covariant in the result type and can be used similarly
--       to the polymorphic `fix` to feed in extra parameters. That is: 
--
--       > loop2 :: x -> y -> ((x -> y -> a) -> x -> y -> a) -> a
--       > loop2 x = flip (loop x)
--
--       encodes the 2-variable recursive least fixed point of `a` and would be
--       used similarly to `loop` for introducing local anonymous recursive 
--       functions of two variables:
--
--       > loop2 x0 y0 ( \k x y -> ... k x' y' ... )
--
{-# INLINE loop #-}
loop :: state -> ((state -> a) -> state -> a) -> a
loop = flip fix

{-# INLINE run #-}
run :: View -> IO ()
run = inject body


