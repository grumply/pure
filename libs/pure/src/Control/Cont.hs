{-# language TypeApplications, RankNTypes, ScopedTypeVariables, ConstraintKinds, FlexibleContexts, AllowAmbiguousTypes, TypeOperators, PatternSynonyms, PolyKinds, TypeFamilies #-}
module Control.Cont (C(..),Cont,Dynamic,reify,unify,codify,cont,compose,(<<-),(->>),(:=>),dynamic,fromDynamic,Surface,Shape,Template,surface,flat,hole,fill,empty,shape) where

import Control.Dynamic ((:=>),dynamic,fromDynamic)
import Control.State (Modify,State,state,put)
import Control.Producer (stream)
import Data.Effect (Effect,(#))
import Data.Exists (Exists,it,using)
import Data.Function (fix)
import Data.Kind (Constraint)
import Data.Typeable (Typeable)
import Data.View (eager,View,txt,pattern Null)

type family C (a :: k) :: Constraint
type instance C (a :: Constraint) = a

type Cont' c a = Modify (c :=> a)
type Cont c = Cont' c View

type Dynamic' c a = Cont' c a => c :=> a
type Dynamic c = Dynamic' c View

-- | Reify a View context.
reify :: forall c a. (Typeable c, Typeable a) => Dynamic' c a -> (State (c :=> a) => (c => View)) -> (c => View)
reify = state
{-# INLINE reify #-}

-- | Unify the supplied dynamic View with a matching `reify`d context.
--
-- Note that the need to wrap dynamic Views arises from the fact that unify is
-- often used in a context where those constraints are locally satisfied.
unify :: forall c a. Modify (c :=> a) => Dynamic' c a -> IO ()
unify = put
{-# INLINE unify #-}

-- | Call the dynamic value from a matching `reify`d context. 
-- Requires a type application for `c`.
call :: forall c a. Exists (c :=> a) => (c => a)
call = fromDynamic (it :: c :=> a)
{-# INLINE call #-}

-- | Codify the dynamic View from a matching `reify`d context. 
-- Requires a type application for `c`.
codify :: forall c. Exists (c :=> View) => (c => View)
codify = eager (it :: c :=> View) (call @c)
{-# INLINE codify #-}

-- | Reify a View context and call the initial continuation.
cont :: forall c. Typeable c => Dynamic' c View -> (c => View)
cont d = reify d (codify @c)
{-# INLINE cont #-}

--------------------------------------------------------------------------------
-- A physical interpretation for easier conceptualization. The abstraction is
-- largely trivial.

-- A surface is a contextualized `View`.
-- A surface is a gateway to functionality supplied by `c`.
type Surface c = c => View

-- A shape is a `View` with some unsatisfied constraints, `c`.
-- A shape is a reified surface; a shape is a reified contextualized `View`.
-- A shape can be reduced to a surface in any context.
type Shape c = Modify (c :=> View) => c :=> View

-- A template is a `View` with some 
-- A template is a surface with knowledge of a `c`-shape.
-- A template can be reduced to a surface in a valid context.
type Template c = State (c :=> View) => Surface c

-- Reduce a given template to a surface with a given default shape to fill the
-- possible holes in the template. The holes can be re-filled with `fill` from
-- within the shape or the template.
--
-- Requires type application.
--
{-# INLINE surface #-}
surface :: forall c. Typeable c => Template c -> Shape c -> Surface c
surface t s = reify @c s t

-- A self-modifying surface with no holes.
--
-- Requires type application.
--
-- Equivalent to `surface @c (hole @c)`.
--
-- Any use of `fill` inside a flat surface will force a full re-render rather
-- than a reconciliation. To allow reconciliation, use: 
--
-- > surface @c (call @c)
--
-- to avoid the `eager` within `hole`/`codify`. The `call` approach can be
-- convenient for mimicking state with continuations when it wouldn't make
-- sense to re-render on every `fill`.
--
-- Holes inside the template will be, by necessity, filled with the empty shape.
--
{-# INLINE flat #-}
flat :: forall c. Typeable c => Template c -> Surface c
flat t = surface @c (hole @c) (shape @c (using (empty @c) t))

-- Declare a hole in a surface. A hole is never empty, and will always be 
-- filled with, at least, the `empty` shape. A hole can be re-filled with 
-- `fill`.
--
-- Requires type application.
--
-- > type Graph = (Exists Dimensions,State Data)
-- > graph :: Shape Graph -> Surface Graph
-- > graph = surface @Graph do
-- >   Div <||>
-- >     [ hole @Graph 
-- >     ]
--
-- `hole` force a local re-render rather than a reconciliation through the
-- `eager` in `codify`.
{-# INLINE hole #-}
hole :: forall c. Exists (c :=> View) => Surface c
hole = codify @c

-- Fill a hole in a known surface. The current shape in the hole can be
-- accessed with `full` from within the containing surface.
--
{-# INLINE fill #-}
fill :: Modify (c :=> View) => Shape c -> IO ()
fill = unify

-- Construct a shape from a surface.
{-# INLINE shape #-}
shape :: Surface c -> Shape c
shape = dynamic

-- The empty shape. Fits any `c`-shaped hole.
{-# INLINE empty #-}
empty :: Shape c
empty = shape Null

--------------------------------------------------------------------------------
-- An implementation of ltr and rtl composition using view continuations. 

-- | Compose a producer and a reader within a dynamic context. Requires type application.
compose :: forall a c. (Typeable a, Typeable c) => ((c,Effect a) => View) -> ((Exists a,c) => View) -> (c => View)
compose v f = cont (dynamic @c (stream (\(a :: a) -> unify (dynamic @c (using a f))) v))
{-# INLINE compose #-}

infixr 0 <<-
(<<-) :: forall a c. (Typeable a, Typeable c) => (c => a -> View) -> (c => Effect a => View) -> (c => View)
(<<-) r p = compose @a @c p (r it)
{-# INLINE (<<-) #-}

infixl 1 ->>
(->>) :: forall a c. (Typeable a, Typeable c) => (c => Effect a => View) -> (c => a -> View) -> (c => View)
(->>) p r = compose @a @c p (r it)
{-# INLINE (->>) #-}

{-  

fmap :: (a -> b) -> (Effect a => View) -> (Effect b => View)

fail = txt

join :: (Effect View => View) -> View
pure a = exec (yield a) Null

(>>=) :: forall a. Typeable a => (Effect a => View) -> (a -> View) -> View
return = pure

(>>) :: forall a. Typeable a => (Effect a => View) -> View -> View
liftA2 f va vb = compose @a @() va (compose @b @() vb (pure (f ask ask)))

----------------------------------------

I wanted this to work, but I couldn't get constraint satisfaction to happen
during/before `do` desugaring.  That is:

pure a = exec (yield a) Null

works. But this:

> do { () <- pure (); "Done" } 

does not work. It complains (at runtime!) about an IsString instance, meaning
the rhs of the bind has had its constraints ignored?

-}