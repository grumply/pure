{-# language CPP, ScopedTypeVariables, DuplicateRecordFields #-}
module Pure 
  (module Pure
  ,module Export
  ) where

import Control.Applicative as Export hiding (empty,optional)
import Control.Concurrent as Export hiding (yield)
import Control.Monad as Export (void,join,when,unless,(>=>),(<=<),forever)
import Data.Bifunctor as Export (Bifunctor(..))
import Data.Bool as Export (bool)
import Data.Char as Export hiding (Space,Control)
import Data.Coerce as Export (coerce,Coercible)
import Data.Foldable as Export (Foldable,for_,traverse_)
import Data.Function as Export ((&),fix)
import Data.Maybe as Export
import Data.Monoid as Export hiding (Alt,Product)
import Data.Traversable as Export (Traversable,for,traverse)
import Data.Typeable as Export (Typeable,typeOf)
import Debug.Trace as Export (trace,traceShow)
import GHC.Generics as Export (Generic)
import GHC.Stack as Export (HasCallStack)
import System.IO as Export (stdout,hSetBuffering,BufferMode(..))
import Text.Read as Export (readMaybe,readEither)
import Text.Printf as Export (printf)

import Data.Retry as Export (retry,recover,recoverWith,recoverWithIO,recovering,retrying,limitRetries,limitTries,limitDelay,limitDuration,constant,exponential,jittered,fibonacci)
import Data.DOM as Export (body,Evt)
import Data.CSS as Export hiding (root,target,active,checked,wrap,select,empty,stylesheet,CSS_(..),focus,or)
import Data.Default as Export
import Data.Hashable as Export
import Data.HTML as Export hiding (Style,DateTime,Select,Accept,Alt,ContextMenu,Meta,Selected,Async,Form)
import Data.JSON as Export hiding (Null,Key,lookup,match,Number)
import Data.Marker as Export hiding (hex)
import Data.Random as Export hiding (normal,int,next,list,exponential) 
import Data.Scroll as Export
import Data.Slug as Export
import Data.Styles as Export hiding (not,zoom,state,delay,fill,Left,Right,true,false,url,Scroll,blur,copy,drop,scroll,select,repeat,resize,translate,events,empty)
import Data.Theme as Export hiding (within)
import Data.Time as Export hiding (Time_,every)
import Data.Try as Export hiding (try)
import Data.Txt as Export (Txt,ToTxt(..),FromTxt(..))
import Data.View as Export hiding (queueComponentUpdate,getHost,initialize,content,Styles,On,Raw,lifecycles,Lifecycles,attributes)
import Data.URI as Export
import Effect.Meta as Export
import Effect.Router as Export hiding (map)
import Effect.Script as Export
import Effect.Styles as Export hiding (css)
import Effect.Title as Export
import Web.Events as Export
-- import Web.Events.Resize as Export (Resize(Resize,borderBlockSize,borderInlineSize,contentBlockSize,contentInlineSize),resize,resizes)

import Data.View.Build as Export
import Prelude as Export hiding (all,any,or,and,(^^),max,min,rem,odd,drop,break,even,repeat,span,tan,reverse,read,map)

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

-- | A rearranged variant of `bool` with a default `id` false branch. Useful for
-- applying conditional styling if you have trouble remembering the order of
-- `bool`.
--
-- Equivalent to `flip (bool id)`
--
-- > Div <| true isVisible (Themed @Visible) |> { ... }
--
-- Equivalently:
--
-- > Div <| bool id (Themed @Visible) isVisible |> { ... }
-- 
-- With `true`, we can write the extracted conditional property in an 
-- aesthetically pleasing and intuitive way:
--
-- > visible :; Exists Visibility => View -> View
-- > visible =
-- >   true isVisible do
-- >     Themed @Visible
--
{-# INLINE true #-}
true :: Bool -> (a -> a) -> a -> a
true = flip (bool id)

-- | An intuitive synonym of `true` that reads more reactively.
--
-- > Div <| on isVisible (Themed @Visible) |> { ... }
--
{-# INLINE on #-}
on :: Bool -> (a -> a) -> a -> a
on = true

-- | A rearranged variant of `bool` with a default `id` true branch. Useful for
-- applying conditional styling if you have trouble remembering the order of
-- `bool`.
--
-- Equivalent to `flip (flip bool id)`
--
-- > Div <| false isVisible (Themed @Hidden) |> { ... }
-- 
-- Equivalently:
--
-- > Div <| bool id (Themed @Hidden) isVisible |> { .. }
-- 
-- With `false`, we can write the extracted conditional property in an 
-- aesthetically pleasing and intuitive way:
--
-- > hiding :: Exists Visibility => View -> View
-- > hiding = 
-- >   false isVisible do
-- >     Themed @Hidden
--
{-# INLINE false #-}
false :: Bool -> (a -> a) -> a -> a
false = flip (`bool` id)

-- | An intuitive synonym of `false` that reads more reactively.
--
-- > Div <| off isVisible (Themed @Hidden) |> { ... }
--
-- Equivalent to:
-- 
-- > Div <| on (not isVisible) (Themed @Hidden) |> { ... }
--
{-# INLINE off #-}
off :: Bool -> (a -> a) -> a -> a
off = false

{-# INLINE just #-}
just :: Maybe b -> (b -> a -> a) -> a -> a
just mb f = maybe id f mb

{-# INLINE nothing #-}
nothing :: Maybe b -> (a -> a) -> a -> a
nothing mb f = maybe f (const id) mb

{-# INLINE enum #-}
enum :: forall a. (Bounded a, Enum a) => [a]
enum = enumFromTo minBound (maxBound :: a)

infixr 0 ?
{-# INLINE (?) #-}
(?) :: Maybe a -> a -> a
(?) = flip fromMaybe

infixr 0 !?
{-# INLINE (!?) #-}
(!?) :: Eq a => [(a,b)] -> a -> Maybe b
(!?) = flip lookup

{-# INLINE present #-}
present :: [a] -> Bool
present = not . null
