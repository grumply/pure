{-# language TypeApplications, RankNTypes, ScopedTypeVariables, ConstraintKinds, FlexibleContexts, AllowAmbiguousTypes, TypeOperators, PatternSynonyms #-}
module Control.Cont (Root,root,Cont,Dynamic,reify,unify,codify,cont,compose,(<<-),(->>),(:=>),dynamic,fromDynamic,Surface,Shape,surface,flat,hole,fill,empty,shape,refine) where

import Control.Dynamic ((:=>),dynamic,fromDynamic)
import Control.State (Modify,State,state,put)
import Control.Producer (stream)
import Data.Effect (Effect,(#))
import Data.Exists (Exists,it,using)
import Data.Typeable (Typeable)
import Data.View (eager,View,txt,pattern Null)

type Cont c = Modify (c :=> View)
type Dynamic c = Cont c => c :=> View

-- | Reify a View context.
reify :: forall c. Typeable c => Dynamic c -> (State (c :=> View) => (c => View)) -> (c => View)
reify = state
{-# INLINE reify #-}

-- | Unify the supplied dynamic View with a matching `reify`d context.
--
-- Note that the need to wrap dynamic Views arises from the fact that unify is
-- often used in a context where those constraints are locally satisfied.
unify :: forall c. Modify (c :=> View) => Dynamic c -> IO ()
unify = put
{-# INLINE unify #-}

-- | Codify the dynamic View from a matching `reify`d context. Requires type application.
codify :: forall c. Exists (c :=> View) => (c => View)
codify = eager (it :: c :=> View) (fromDynamic @c it)
{-# INLINE codify #-}

-- | Reify a View context and call the initial continuation.
cont :: forall c. Typeable c => Dynamic c -> (c => View)
cont d = reify d (codify @c)
{-# INLINE cont #-}

--------------------------------------------------------------------------------
-- A physical interpretation for easier conceptualization.

-- A surface is a `View` that knows how to satisfy some constraints, `c`.
-- A surface is a gateway to functionality supplied by `c`.
type Surface c = c => View

-- A shape is a `View` with some unsatisfied constraints, `c`.
-- A shape is a reified surface.
type Shape c = c :=> View

-- Given a default shape and a surface definition containing possible holes to
-- match the shape, constructs a surface with those holes filled. The holes can
-- be re-filled with `fill` from within the shape or the surface.
--
-- Requires type application.
--
{-# INLINE surface #-}
surface :: forall c. Typeable c => (Modify (Shape c) => Shape c) -> (State (Shape c) => Surface c) -> Surface c
surface = reify @c

-- Given a surface with fillable holes, constructs a surface with those holes
-- filled with the empty shape as a default. The holes can be re-filled with
-- `fill` from within the surface.
--
-- Requires type application.
--
{-# INLINE flat #-}
flat :: forall c. Typeable c => (State (Shape c) => Surface c) -> Surface c
flat = surface @c empty

-- Declare a hole in a surface. A hole is never empty, and will always be 
-- filled with, at least, the `empty` shape. A hole can be re-filled with 
-- `fill`.
--
-- Requires type application.
--
{-# INLINE hole #-}
hole :: forall c. Exists (Shape c) => Surface c
hole = codify @c

-- Fill a hole in a known surface. The current shape in the hole can be
-- accessed with `full` from within the containing surface.
--
{-# INLINE fill #-}
fill :: Modify (Shape c) => Shape c -> IO ()
fill = unify

-- Construct a shape from a surface.
{-# INLINE refine #-}
refine :: Surface c -> Shape c
refine = dynamic

-- The empty shape. Fits any hole of shape `c`.
{-# INLINE empty #-}
empty :: Shape c
empty = refine Null

-- The current shape in a known hole of shape `c`.
{-# INLINE shape #-}
shape :: Exists (Shape c) => Shape c
shape = it

--------------------------------------------------------------------------------
-- A root continuation.

type Root = Cont ()

{-# INLINE root #-}
root :: (Root => View) -> View
root r = cont @() (dynamic r)

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