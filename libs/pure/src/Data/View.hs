{-# language MagicHash, CPP, ScopedTypeVariables, PatternSynonyms, PolyKinds, DefaultSignatures, ViewPatterns, RecordWildCards, GADTs, FlexibleInstances, AllowAmbiguousTypes, OverloadedStrings, TypeApplications, BangPatterns, RankNTypes, FlexibleContexts, ConstraintKinds, BlockArguments, MultiWayIf, LambdaCase, DuplicateRecordFields, TypeOperators, DerivingVia, DataKinds, NamedFieldPuns, TypeFamilies, DeriveFunctor, UndecidableInstances, InstanceSigs, RoleAnnotations, ConstrainedClassMethods, DeriveAnyClass, DeriveGeneric #-}
{-# OPTIONS_GHC -O2 #-}
module Data.View (module Data.View, module Data.Exists, module Data.Consumer, Typeable()) where

import Debug.Trace
import Control.Arrow ((&&&))
import Control.Applicative (Applicative(..),Alternative(..),Const(..))
import Control.Comonad (Comonad(..),ComonadApply(..))
import Control.Concurrent (ThreadId,forkIO,killThread,myThreadId,MVar,newMVar,newEmptyMVar,readMVar,putMVar,takeMVar,tryPutMVar)
import Control.DeepSeq (NFData(..),deepseq)
import Control.Exception (mask,onException,evaluate,Exception,catch,throw,SomeException)
import Control.Monad (void,join,forever,unless,when,MonadPlus(..))
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Zip (MonadZip(..))
import Control.Monad.ST (ST)
import Data.Coerce (Coercible(),coerce)
import Data.Foldable (for_)
import Data.Functor.Contravariant (Contravariant(..))
import Data.Functor.Identity (Identity(..))
import Data.IORef (IORef,newIORef,atomicModifyIORef',readIORef,writeIORef)
import Data.List as List (null)
import Data.Maybe (listToMaybe,fromJust)
import Data.Monoid (Monoid(..),(<>))
import Data.Proxy (Proxy(..))
import Data.STRef (STRef)
import Data.String (IsString(..))
import Data.Try as Try (Try,try)
import Data.Time (Time,time,delay,timeout)
import Data.Traversable (for)
import Data.Type.Equality ( type (==) )
import Data.Typeable (TypeRep,Typeable,tyConName,typeRepTyCon,typeOf,typeRep,typeRepFingerprint,cast)
import Data.Unique (Unique,newUnique)
import GHC.Exts as Exts (IsList(..),Any,Constraint,reallyUnsafePtrEquality#,isTrue#,unsafeCoerce#,Proxy#,proxy#,inline)
import GHC.Fingerprint.Type (Fingerprint())
import GHC.Generics
import GHC.Magic (noinline)
import GHC.Stack (HasCallStack)
import System.IO.Unsafe (unsafePerformIO,unsafeDupablePerformIO)
import Type.Reflection (SomeTypeRep,someTypeRep)
import Unsafe.Coerce (unsafeCoerce)

import Data.Consumer 
import Data.Default (Default(..))
import Data.DOM (Evt,Element,Node(..),IsNode(..),Text,Options,nullJSV,JSV)
import Data.Exists
import Data.JSON (ToJSON,FromJSON)
import Data.Txt (FromTxt(..),ToTxt(..),Txt)
import Data.Queue (Queue,arrive)

import Data.Set (Set)
import Data.Set as Set (empty,fromList,null,empty,union,toList,insert)
import Data.Map.Lazy as Map (Map,fromList,null,empty,union,toList,insert,singleton)

data Target = ElementTarget | WindowTarget | DocumentTarget
  deriving (Eq,Generic,NFData)

newtype ListenerAction = ListenerAction (Evt -> IO ())
instance NFData ListenerAction where 
  rnf ListenerAction {} = ()

data Listener =
  On
    { eventName     :: Txt
    , eventTarget   :: Target
    , eventOptions  :: Options
    , eventAction   :: ListenerAction
    , eventUpdate   :: ListenerAction -> IO ()
    , eventStopper  :: IO ()
    }

instance NFData Listener where
  rnf On {..} =
    eventName `deepseq` eventTarget `deepseq` eventOptions `deepseq`  eventAction `deepseq` ()

data Lifecycle 
  = Created (Node -> IO (IO ())) (IO ()) 
  | Mounted (Node -> IO (IO ())) (IO ())
  
instance NFData Lifecycle where
  rnf Created {} = ()
  rnf Mounted {} = ()

data Comp props state = Comp
  { deferred       :: Bool
  , animated       :: Bool
  , construct      :: IO state
  , initialize     :: state -> IO state
  , onInitialized  :: IO ()
  , onMount        :: state -> IO state
  , onExecuting    :: state -> IO state
  , onMounted      :: IO ()
  , onReceive      :: props -> state -> IO state
  , force          :: props -> state -> IO Bool
  , onUpdate       :: props -> state -> IO ()
  , onUpdated      :: props -> state -> IO ()
  , onUnmounted    :: IO ()
  , render         :: props -> state -> View
  }

instance Default (Comp props state) where
  {-# INLINE def #-}
  def = Comp
    { deferred      = False
    , animated      = False
    , construct     = return (error "Comp.construct: no initial state supplied.")
    , initialize    = return
    , onInitialized = return ()
    , onMount       = return
    , onExecuting   = return
    , onMounted     = return ()
    , onReceive     = \_ -> return
    , force       = \_ _ -> return True
    , onUpdate      = \_ _ -> return ()
    , onUpdated     = \_ _ -> return ()
    , onUnmounted   = return ()
    , render        = \_ _ -> NullView Nothing
    }

data ComponentPatch props state
  = Unmount
  | UpdateProperties props
  | UpdateState (props -> state -> IO (state,IO ()))

data Ref props state
  = Ref
      { crView       :: IORef View
      , crProps      :: IORef props
      , crState      :: IORef state
      , crComponent  :: IORef (Comp props state)
      , crPatchQueue :: IORef (Maybe (Queue (ComponentPatch props state)))
      }

data Features =
  Features_
    { classes    :: Set Txt
    , styles     :: Map.Map Txt Txt
    , attributes :: Map.Map Txt Txt
    , properties :: Map.Map Txt Txt
    , listeners  :: [Listener]
    , lifecycles :: [Lifecycle]
    }
  deriving (Generic,NFData)

instance Monoid Features where
  {-# INLINE mempty #-}
  mempty = Features_ mempty mempty mempty mempty mempty mempty

instance Semigroup Features where
  {-# INLINE (<>) #-}
  (<>) (Features_ c1 s1 a1 p1 ls1 lc1) (Features_ c2 s2 a2 p2 ls2 lc2) =
    Features_ (c1 <> c2) (s2 <> s1) (a2 <> a1) (p2 <> p1) (ls1 <> ls2) (lc1 <> lc2)

instance Default Features where
  {-# INLINE def #-}
  def = mempty

data View where
  HTMLView ::
    { elementHost :: Maybe Element
    , tag         :: Txt
    , features    :: Features
    , children    :: [View]
    } -> View

  TextView ::
    { textHost :: Maybe Text
    , content  :: Txt
    } -> View

  NullView ::
    { elementHost :: Maybe Element
    } -> View

  RawView ::
    { elementHost:: Maybe Element
    , tag        :: Txt
    , features   :: Features
    , content    :: Txt
    } -> View

  SVGView ::
    { elementHost :: Maybe Element
    , tag         :: Txt
    , features    :: Features
    , xlinks      :: Map.Map Txt Txt
    , children    :: [View]
    } -> View

  KHTMLView ::
    { elementHost   :: Maybe Element
    , tag           :: Txt
    , features      :: Features
    , keyedChildren :: [(Int,View)]
    } -> View

  KSVGView ::
    { elementHost   :: Maybe Element
    , tag           :: Txt
    , features      :: Features
    , xlinks        :: Map.Map Txt Txt
    , keyedChildren :: [(Int,View)]
    } -> View

  ReactiveView ::
    { reactiveCmp  :: a -> a -> Bool
    , reactiveVal  :: a
    , reactiveView :: View
    } -> View

  WeakView ::
    { weakCmp  :: a -> a -> Bool
    , weakVal  :: a
    , weakView :: View
    } -> View

  PortalView ::
    { portalProxy :: Maybe Element
    , portalDestination :: Element
    , portalView :: View
    } -> View

  ComponentView ::
    { __rep  :: SomeTypeRep
    , record :: Maybe (Ref props state)
    , comp   :: Ref props state -> Comp props state
    , props  :: Ref props state -> props
    } -> View

  TaggedView ::
    { __tag :: SomeTypeRep
    , taggedView :: View 
    } -> View

  Prebuilt :: 
    { prebuilt :: View 
    } -> View

instance NFData View where
  rnf HTMLView {..} = tag `deepseq` features `deepseq` children `deepseq` ()
  rnf TextView {..} = content `deepseq` ()
  rnf NullView {..} = ()
  rnf RawView {..} = tag `deepseq` features `deepseq` content `deepseq` ()
  rnf SVGView {..} = tag `deepseq` features `deepseq` xlinks `deepseq` children `deepseq` ()
  rnf KHTMLView {..} = tag `deepseq` features `deepseq` keyedChildren `deepseq` ()
  rnf KSVGView {..} = tag `deepseq` features `deepseq` xlinks `deepseq` keyedChildren `deepseq` ()
  rnf ReactiveView {..} = reactiveView `deepseq` ()
  rnf WeakView {..} = weakView `deepseq` ()
  rnf PortalView {..} = portalDestination `deepseq` portalView `deepseq` ()
  rnf ComponentView {..} = __rep `deepseq` ()
  rnf TaggedView {..} = __tag `deepseq` rnf taggedView
  rnf Prebuilt {..} = ()

instance Default View where
  {-# NOINLINE def #-}
  def = NullView Nothing

instance IsString View where
  {-# INLINE fromString #-}
  fromString = TextView Nothing . toTxt

instance FromTxt View where
  {-# INLINE fromTxt #-}
  fromTxt = TextView Nothing

{-# INLINE (===) #-}
(===) :: a -> b -> Bool
x === y = isTrue# (unsafeCoerce# reallyUnsafePtrEquality# x y)

{-# INLINE (/==) #-}
(/==) :: a -> b -> Bool
x /== y = Prelude.not (x === y)

-- | A class for reference equality extended to work element-wise for tuples
-- and lists.
-- 
-- ## Overlapping
-- The catch-all instance is overlappable. You may construct custom instances,
-- but it is generally not recommended.
--
-- ## Variants
-- `req` is non-strict. `req'` is strict in both arguments.
-- 
-- ## Instances
-- For (<9)-tuples, `req'` defers to the element-wise `req'`.
-- For [], `req'` defers to the element-wise `req'`.
-- For JSV (including Txt) on GHCJS, defers to native reference equality.
-- For everything else, uses reallyUnsafePtrEquality#.
-- 
class Req a where
  req :: a -> a -> Bool
  req' :: a -> a -> Bool

instance {-# OVERLAPPABLE #-} Req a where
  req = (===)
  req' !a !b = a === b

instance {-# OVERLAPPING #-} (Req a, Req b) => Req (a,b) where
  req (a1,b1) (a2,b2) = a1 `req` a2 && b1 `req` b2
  req' (a1,b1) (a2,b2) = a1 `req'` a2 && b1 `req'` b2

instance {-# OVERLAPPING #-} (Req a, Req b, Req c) => Req (a,b,c) where
  req (a1,b1,c1) (a2,b2,c2) = a1 `req` a2 && b1 `req` b2 && c1 `req` c2
  req' (a1,b1,c1) (a2,b2,c2) = a1 `req'` a2 && b1 `req'` b2 && c1 `req'` c2

instance {-# OVERLAPPING #-} (Req a, Req b, Req c, Req d) => Req (a,b,c,d) where
  req (a1,b1,c1,d1) (a2,b2,c2,d2) = a1 `req` a2 && b1 `req` b2 && c1 `req` c2 && d1 `req` d2
  req' (a1,b1,c1,d1) (a2,b2,c2,d2) = a1 `req'` a2 && b1 `req'` b2 && c1 `req'` c2 && d1 `req'` d2

instance {-# OVERLAPPING #-} (Req a, Req b, Req c, Req d, Req e) => Req (a,b,c,d,e) where
  req (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) = a1 `req` a2 && b1 `req` b2 && c1 `req` c2 && d1 `req` d2 && e1 `req` e2
  req' (a1,b1,c1,d1,e1) (a2,b2,c2,d2,e2) = a1 `req'` a2 && b1 `req'` b2 && c1 `req'` c2 && d1 `req'` d2 && e1 `req'` e2

instance {-# OVERLAPPING #-} (Req a, Req b, Req c, Req d, Req e, Req f) => Req (a,b,c,d,e,f) where
  req (a1,b1,c1,d1,e1,f1) (a2,b2,c2,d2,e2,f2) = a1 `req` a2 && b1 `req` b2 && c1 `req` c2 && d1 `req` d2 && e1 `req` e2 && f1 `req` f2
  req' (a1,b1,c1,d1,e1,f1) (a2,b2,c2,d2,e2,f2) = a1 `req'` a2 && b1 `req'` b2 && c1 `req'` c2 && d1 `req'` d2 && e1 `req'` e2 && f1 `req'` f2

instance {-# OVERLAPPING #-} (Req a, Req b, Req c, Req d, Req e, Req f, Req g) => Req (a,b,c,d,e,f,g) where
  req (a1,b1,c1,d1,e1,f1,g1) (a2,b2,c2,d2,e2,f2,g2) = a1 `req` a2 && b1 `req` b2 && c1 `req` c2 && d1 `req` d2 && e1 `req` e2 && f1 `req` f2 && g1 `req` g2
  req' (a1,b1,c1,d1,e1,f1,g1) (a2,b2,c2,d2,e2,f2,g2) = a1 `req'` a2 && b1 `req'` b2 && c1 `req'` c2 && d1 `req'` d2 && e1 `req'` e2 && f1 `req'` f2 && g1 `req'` g2

instance {-# OVERLAPPING #-} (Req a, Req b, Req c, Req d, Req e, Req f, Req g, Req h) => Req (a,b,c,d,e,f,g,h) where
  req (a1,b1,c1,d1,e1,f1,g1,h1) (a2,b2,c2,d2,e2,f2,g2,h2) = a1 `req` a2 && b1 `req` b2 && c1 `req` c2 && d1 `req` d2 && e1 `req` e2 && f1 `req` f2 && g1 `req` g2 && h1 `req` h2
  req' (a1,b1,c1,d1,e1,f1,g1,h1) (a2,b2,c2,d2,e2,f2,g2,h2) = a1 `req'` a2 && b1 `req'` b2 && c1 `req'` c2 && d1 `req'` d2 && e1 `req'` e2 && f1 `req'` f2 && g1 `req'` g2 && h1 `req'` h2

instance {-# OVERLAPPING #-} Req a => Req [a] where
  req [] [] = True
  req (a1:as1) (a2:as2) = a1 `req` a2 && as1 `req` as2
  req _ _ = False
  
  req' [] [] = True
  req' (a1:as1) (a2:as2) = a1 `req'` a2 && as1 `req'` as2
  req' _ _ = False

{-# INLINE asProxyOf #-}
asProxyOf :: a -> Proxy a
asProxyOf _ = Proxy

{-# INLINE tyCon #-}
tyCon :: Typeable t => t -> String
tyCon = tyConName . typeRepTyCon . typeOf

{-# INLINE getref #-}
getref :: Ref props state -> IO state
getref = readIORef . crState

{-# INLINE askref #-}
askref :: Ref props state -> IO props
askref = readIORef . crProps

{-# INLINE lookref #-}
lookref :: Ref props state -> IO View
lookref = readIORef . crView

{-# INLINE modifyref #-}
modifyref :: Ref props state -> (props -> state -> state) -> IO Bool
modifyref r f = modifyrefM r (\p s -> return (f p s,return ()))

{-# INLINE modifyref_ #-}
modifyref_ :: Ref props state -> (props -> state -> state) -> IO ()
modifyref_ r f = void (modifyref r f)

{-# INLINE modifyrefM #-}
modifyrefM :: Ref props state -> (props -> state -> IO (state,IO ())) -> IO Bool
modifyrefM cr f = queueComponentUpdate cr (UpdateState f)

{-# INLINE modifyrefM_ #-}
modifyrefM_ :: Ref props state -> (props -> state -> IO (state,IO ())) -> IO ()
modifyrefM_ r f = void (modifyrefM r f)

{-# INLINE setProps #-}
setProps :: Ref props state -> props -> IO Bool
setProps cr = queueComponentUpdate cr . UpdateProperties

{-# INLINE queueComponentUpdate #-}
queueComponentUpdate :: Ref props state -> ComponentPatch props state -> IO Bool
queueComponentUpdate crec cp = do
  mq <- readIORef (crPatchQueue crec)
  case mq of
    Nothing -> return False
    Just q  -> do
      arrive q cp
      return True

{-# INLINE getHost #-}
getHost :: View -> Maybe Node
getHost ComponentView {..} = join $! for record (getHost . unsafePerformIO . readIORef . crView)
getHost TextView      {..} = fmap toNode textHost
getHost ReactiveView  {}   = Nothing
getHost WeakView      {}   = Nothing
getHost PortalView    {..} = fmap toNode portalProxy
getHost TaggedView    {..} = getHost taggedView
getHost Prebuilt      {..} = getHost prebuilt
getHost x                  = fmap toNode (elementHost x)

class Proof a where
  witness :: Props a |- View
  witness = proof Null

view :: forall a. (Proof a, Props a) => View
view = prove (witness @a)

{-# INLINE effect' #-}
effect' :: Effect eff => eff -> IO () -> IO Bool
effect' = runHandler it

{-# INLINE effect #-}
effect :: Effect eff => eff -> IO ()
effect eff = void (effect' eff (pure ()))

{-# INLINE reinterpret #-}
-- | Adapt producers to be more specific or varied.
reinterpret :: forall msg msg' a. (msg -> msg') -> (Effect msg => a) -> (Effect msg' => a)
reinterpret f = with (Handler (effect' . f)) 

-- {-# INLINE also #-}
-- also :: forall msg a. (msg -> IO ()) -> (Effect msg => a) -> (Effect msg => a)
-- also f = with (Handler (\m io -> effect' m (io >> f m)))

{-# INLINE (#) #-}
infixr 9 #
(#) :: forall a b x. (a -> b) -> (Effect a => x) -> (Effect b => x)
(#) = Data.View.reinterpret

-- This strictness is important for `Web`. Without it, many diverse instances
-- of `consume`/`yield` pairs will be non-productive!
--
-- Consider, if anything strange arises, that an implementation with a better
-- guarantee about evaluation order might be required: 
--
-- > yield a = a `pseq` effect a
--
-- or, equivalently:
--
-- > yield a = a `seq` lazy (effect a)
--
-- It is important to guarantee that a `Web =>` constraint has been fully
-- satisfied by `with` (and not thunk'd), before being yielded.
--
-- If this strictness requirement has a negative impact on your use, you can try
-- the non-strict version, `effect`. Many use-cases do not require the 
-- strictness. 
--
{-# INLINE yield #-}
yield :: Producer a => a -> IO ()
yield !a = effect a

{-# INLINE consume #-}
consume :: forall a b. (a -> IO ()) -> (Producer a => b) -> b
consume = stream

{-# INLINE events #-}
events :: forall a b. (Exists a => IO ()) -> (Producer a => b) -> b
events f = stream @a (`with` f)

{-# INLINE discard #-}
discard :: forall a b. (Producer a => b) -> b
discard = stream @a (\_ -> pure ())
-- Reconciler hints

{-# INLINE static #-}
-- Create a static un-reconcilable view. Note that this closes over everything
-- within the view, including existentials, like `Producer` consumers. 
static :: View -> View
static = reactive ()

-- | A hint to the reconciler that the view should only be reconciled when the
-- supplied value changes, where change is evaluated via referential equality
-- across product componenets using `Req`. If the old and new reactive values
-- are referentially equal, the reconciler short-circuites. 
--
-- ## Note on laziness
-- The reactive value is not forced to WHNF. See `reactive'` for a version that
-- forces the reactive value to WHNF. This can often be important to get expected
-- behavior. See `reactive'` for an example.
--
-- ## Note on non-composability
-- Operationally, `reactive a . reactive b /= reactive (a,b)`.
--
-- ## Note on safety and referential equality
-- Note that `static` is implemented as `reactive ()`, but, importantly,
-- it could be implemented as `reactive (Nothing @a)` for any `a`,
-- since GHC only has one `Nothing` value. Understanding the implications of
-- this is important when using `reactive`!
--
-- Given these two views:
-- 
-- > view1 :: Maybe Int -> View
-- > view1 mi = reactive mi "view1"
-- 
-- > view2 :: Maybe Char -> View
-- > view2 mc = reactive mc "view2"
--
-- Reconciliation of `view1 Nothing` and `view2 Nothing` is not expected to
-- replace `"view1"` with `"view2"`, as `Nothing @Int` and `Nothing @Char`
-- are referentially equal.
-- 
-- Thus, `reactive` is considered a low-level optimization that should be
-- avoided unless you are certain it is necessary, and then you should 
-- carefully control the contexts in which it is used. On way to guarantee
-- the correct reconciliation in such a case is `tagged`:
--
-- > view1 :: Maybe Int -> View
-- > view1 mi = tagged @(Maybe Int) (reactive mi "view1")
--
-- > view2 :: Maybe Char -> View
-- > view2 mc = tagged @(Maybe Char) (reactive mc "view2")
--
-- The reconciler in `Data.View.Build` is guaranteed to correctly reconcile
-- these views, but will not perform a short-circuiting reconciliation -
-- it will replace the old view with the new by fully building the new view
-- and then swapping the nodes.
--
reactive :: Req a => a -> View -> View
reactive = ReactiveView req

-- | A strict version of `reactive`. `reactive'` is a hint to the reconciler
-- that the view should only be reconciled when the supplied value changes,
-- where change is evaluated via referential equality. If the old and new
-- reactive values are referentially equal, the reconciler short-circuites.
--
-- ## Note on strictness and optimization
-- The strictness here can be useful for primitives, but keep in mind that
-- optimization level can affect the behavior.
--
-- ## Uses
-- There are some neat tricks that can be had with `reactive'`, like:
--
-- > reactive' (value > 1) ...
--
-- If `value` is strictly increasing from an initial value less than 1, this
-- `reactive'` call will update exactly once, when the value first exceeds 1. If
-- you attempt to do this same comparison with the non-strict `reactive`, you
-- will likely find it failing to achieve the same effect.
--
{-# INLINE reactive' #-}
reactive' :: Req a => a -> View -> View
reactive' = ReactiveView req'

-- | A hint to the reconciler that any changes to the supplied value -
-- any changes that result in referential inequality - should result in
-- replacing the view. Unlike `reactive`, if the value doesn't change,
-- reconciliation continues normally.
--
-- Operationally, `weak a . weak b == weak (a,b)`.
--
{-# INLINE weak #-}
weak :: Req a => a -> View -> View
weak = WeakView req

-- | A strict version of `weak` that forces the value to quasi-WHNF. `weak'` is a
-- hint to the reconciler that any changes to the supplied value - any changes
-- that result in referential inequality - should result in replacing the view.
-- Unlike `reactive` or `reactive'`, if the value doesn't change, reconciliation
-- continues normally.
--
-- Operationally, `weak' a . weak' b == weak' (a,b)`.
--
{-# INLINE weak' #-}
weak' :: Req a => a -> View -> View
weak' = WeakView req'

-- | A hint to the reconciler that only equivalently-tagged views should be
-- reconciled - any change to the tag should result in a full build-and-swap.
-- This can be especially useful in library code that exposes a `View`-level
-- interface, but needs to guarantee that it will not be reconciled with, or
-- reconciled against, any other code. If the type is not exported, no other
-- views can be equivalently tagged. 
--
-- Use can be either via `tagged` or the `Tag` pattern synonym.
--
-- ## Uses
-- Need for `tagged` sometimes arises in forms:
--
-- > view1 = Input
-- > view2 = Input
--
-- Reconciling these two views can be problematic. The value of `view1` at
-- reconciliation will not be reset when reconciled with `view2`, because
-- the reconciler does not know that they are different. One solution is:
--
-- > view1 = tagged @SomeViewTag Input
-- > view2 = tagged @SomeViewTag Input
--
-- Generally, this is not an issue because `Input` is usually inside some
-- state/reactivity wrapper. Occasionally, though, it will manifest when
-- the state/reactivity wrapper is wrapping the same type of value. Note that
-- `tagged` is not always a solution, though - sometimes you'll need `weak`:
--
-- > commentForm :: PostId -> View
-- > commentForm pid = weak pid (Form <||> [ Input, ... ])
--
-- Without `weak`, browsing from one page with a comment form to another might
-- result in the reconciler not replacing the `Input`. This is often not an
-- issue, but when it is, there are `tagged`, `weak`, and component-based 
-- (effectively equivalent to `tagged`) solutions available.
{-# INLINE tagged #-}
tagged :: forall t. Typeable t => View -> View
tagged = TaggedView (someTypeRep (Proxy :: Proxy t))

-- | A bi-directional pattern synonym for `TaggedView`s.
pattern Tag :: forall t. Typeable t => View -> View
pattern Tag v <- TaggedView ((==) (someTypeRep (Proxy :: Proxy t)) -> True) v where
  Tag t = tagged @t t

-- Txt

{-# INLINE txt #-}
{-# SPECIALIZE txt :: Txt -> View #-}
{-# SPECIALIZE txt :: String -> View #-}
txt :: ToTxt a => a -> View
txt a = TextView Nothing (toTxt a)

pattern Txt :: Txt -> View
pattern Txt t <- (TextView _ t) where
  Txt t = TextView Nothing t

{-# INLINE withType #-}
withType :: Typeable a => Proxy a -> SomeTypeRep -> b -> b -> b
withType p str bad good
  | (someTypeRep p) === str || someTypeRep p == str = good
  | otherwise                                       = bad

viewComponent :: forall props state. (Typeable props, Typeable state) => View -> Maybe (Maybe (Ref props state),Ref props state -> Comp props state,Ref props state -> props)
viewComponent (ComponentView rep f0 c0 ps0) =
  withType (Proxy @(props,state)) rep Nothing do
    Just (unsafeCoerce f0,unsafeCoerce c0,unsafeCoerce ps0)
viewComponent _ = Nothing

applyComponent :: Maybe (Maybe (Ref props state),Ref props state -> Comp props state,Ref props state -> props) -> Maybe (Ref props state -> Comp props state,props) 
applyComponent (Just (Just r,c,p)) = Just (c,p r)
applyComponent _ = Nothing

pattern ComponentWith :: forall props state. (Typeable props, Typeable state) => (Ref props state -> Comp props state) -> (Ref props state -> props) -> View
pattern ComponentWith v p <- (viewComponent -> Just (_,v,p)) where
  ComponentWith v p = ComponentView (someTypeRep (Proxy :: Proxy (props,state))) Nothing v p

pattern Component :: forall props state. (Typeable props, Typeable state) => (Ref props state -> Comp props state) -> props -> View
pattern Component v p <- (applyComponent . viewComponent -> Just (v,p)) where
  Component v p = ComponentWith v (const p)

{-# INLINE componentWith #-}
componentWith :: forall props state. (Typeable props, Typeable state) => (Ref props state -> Comp props state) -> (Ref props state -> props) -> View
componentWith = ComponentView (someTypeRep (Proxy :: Proxy (props,state))) Nothing

{-# INLINE component #-}
component :: forall props state. (Typeable props, Typeable state) => (Ref props state -> Comp props state) -> props -> View
component v p = ComponentView (someTypeRep (Proxy :: Proxy (props,state))) Nothing v (const p)

{-# INLINE nil #-}
nil :: View
nil = NullView Nothing

pattern Null :: View
pattern Null <- (NullView _) where
  Null = nil

{-# INLINE viewHTMLTag #-}
viewHTMLTag :: View -> Maybe Txt
viewHTMLTag (HTMLView _ tag _ _) = Just tag
viewHTMLTag (KHTMLView _ tag _ _) = Just tag
viewHTMLTag _ = Nothing

{-# INLINE [1] html #-}
html :: Txt -> View
html tag = HTMLView Nothing tag mempty mempty

pattern SimpleHTML :: Txt -> View
pattern SimpleHTML tag <- (viewHTMLTag -> Just tag) where
  SimpleHTML tag = html tag

{-# INLINE viewSVGTag #-}
viewSVGTag :: View -> Maybe Txt
viewSVGTag (SVGView _ tag _ _ _) = Just tag
viewSVGTag (KSVGView _ tag _ _ _) = Just tag
viewSVGTag _ = Nothing

pattern SimpleSVG :: Txt -> View
pattern SimpleSVG tag <- (viewSVGTag -> Just tag) where
  SimpleSVG tag = SVGView Nothing tag (Features_ mempty mempty mempty mempty mempty mempty) mempty mempty

{-# INLINE toRaw #-}
toRaw :: View -> View
toRaw HTMLView {..} = RawView { content = "", .. }
toRaw SVGView {..} = RawView { content = "", .. }
toRaw KHTMLView {..} = RawView { content = "", .. }
toRaw KSVGView {..} = RawView { content = "", .. }
toRaw PortalView {..} = PortalView { portalView = toRaw portalView, .. }
toRaw v = v

{-# INLINE setContent #-}
setContent :: Txt -> View -> View
setContent c RawView {..} = RawView { content = c, .. }
setContent _ v = v

pattern Raw :: View -> Txt -> View
pattern Raw v r <- (id &&& id -> (RawView _ _ _ r,v)) where
  Raw v r = setContent r (toRaw v)

pattern Portal :: Element -> View -> View
pattern Portal host v = PortalView Nothing host v

{-# INLINE isKeyed #-}
isKeyed :: View -> Bool
isKeyed KSVGView{} = True
isKeyed KHTMLView{} = True
isKeyed PortalView{..} = isKeyed portalView
isKeyed _ = False

{-# INLINE keyed #-}
keyed :: View -> View
keyed SVGView {..} = KSVGView { keyedChildren = [], .. }
keyed HTMLView {..} = KHTMLView { keyedChildren = [], .. }
keyed PortalView {..} = PortalView { portalView = keyed portalView, .. }
keyed v = v

pattern Keyed :: View -> View
pattern Keyed v <- ((isKeyed &&& id) -> (True,v)) where
  Keyed v = (keyed v)

-- Features

{-# INLINE [1] addFeatures #-}
addFeatures :: Features -> View -> View
addFeatures fs a = setFeatures (getFeatures a <> fs) a

{-# INLINE [1] getFeatures #-}
getFeatures :: View -> Features
getFeatures NullView {} = mempty
getFeatures TextView {} = mempty
getFeatures ComponentView {} = mempty
getFeatures TaggedView {..} = getFeatures taggedView
getFeatures ReactiveView {..} = getFeatures reactiveView
getFeatures WeakView {..} = getFeatures weakView
getFeatures PortalView{..} = getFeatures portalView
getFeatures Prebuilt {..}  = getFeatures prebuilt
getFeatures v = features v

{-# INLINE [1] setFeatures #-}
setFeatures :: Features -> View -> View
setFeatures _ v@NullView {} = v
setFeatures _ v@TextView {} = v
setFeatures _ v@ComponentView {} = v
setFeatures fs ReactiveView {..} = ReactiveView { reactiveView = setFeatures fs reactiveView, .. }
setFeatures fs WeakView {..} = WeakView { weakView = setFeatures fs weakView, .. }
setFeatures fs PortalView{..} = PortalView { portalView = setFeatures fs portalView, .. }
setFeatures fs TaggedView{..} = TaggedView { taggedView = setFeatures fs taggedView, .. }
setFeatures fs Prebuilt {..}  = Prebuilt { prebuilt = setFeatures fs prebuilt }
setFeatures fs v = v { features = fs }


{-# RULES 

  "features: set . set" forall fs fs' x. setFeatures fs (setFeatures fs' x) = setFeatures fs x;
  "features: get . set" forall fs x. getFeatures (setFeatures fs x) = fs;
  "features: add . add" forall fs gs x. addFeatures fs (addFeatures gs x) = addFeatures (gs <> fs) x;

  "getFeatures HTMLView" forall e t cs f     . getFeatures (HTMLView e t f cs)      = f;
  "getFeatures SVGView" forall e t cs ls f   . getFeatures (SVGView e t f ls cs)    = f;
  "getFeatures TextView" forall t c          . getFeatures (TextView t c)           = mempty;
  "getFeatures NullView" forall e            . getFeatures (NullView e)             = mempty;
  "getFeatures RawView" forall e t f c       . getFeatures (RawView e t f c)        = f;
  "getFeatures KHTMLView" forall e t cs f    . getFeatures (KHTMLView e t f cs)     = f;
  "getFeatures KSVGView" forall e t cs ls f  . getFeatures (KSVGView e t f ls cs)   = f;
  "getFeatures ReactiveView" forall c a v    . getFeatures (ReactiveView c a v)     = getFeatures v;
  "getFeatures WeakView" forall c a v        . getFeatures (WeakView c a v)         = getFeatures v;
  "getFeatures PortalView" forall p d v      . getFeatures (PortalView p d v)       = getFeatures v;
  "getFeatures ComponentView" forall _r r c p. getFeatures (ComponentView _r r c p) = mempty;
  "getFeatures TaggedView" forall _t v       . getFeatures (TaggedView _t v)        = getFeatures v;
  "getFeatures Prebuilt" forall v            . getFeatures (Prebuilt v)             = getFeatures v;


  "setFeatures HTMLView" forall e t cs f f'    . setFeatures f (HTMLView e t f' cs)     = HTMLView e t f cs;
  "setFeatures SVGView" forall e t cs ls f f'  . setFeatures f (SVGView e t f' ls cs)   = SVGView e t f ls cs;
  "setFeatures TextView" forall t c f          . setFeatures f (TextView t c)           = TextView t c;
  "setFeatures NullView" forall e f            . setFeatures f (NullView e)             = NullView e;
  "setFeatures RawView" forall e t f c f'      . setFeatures f (RawView e t f' c)       = RawView e t f c;
  "setFeatures KHTMLView" forall e t cs f f'   . setFeatures f (KHTMLView e t f' cs)    = KHTMLView e t f cs;
  "setFeatures KSVGView" forall e t cs ls f f' . setFeatures f (KSVGView e t f' ls cs)  = KSVGView e t f ls cs;
  "setFeatures ReactiveView" forall c a v f    . setFeatures f (ReactiveView c a v)     = ReactiveView c a (setFeatures f v);
  "setFeatures WeakView" forall c a v f        . setFeatures f (WeakView c a v)         = WeakView c a (setFeatures f v);
  "setFeatures PortalView" forall p d v f      . setFeatures f (PortalView p d v)       = PortalView p d (setFeatures f v);
  "setFeatures ComponentView" forall _r r c p f. setFeatures f (ComponentView _r r c p) = ComponentView _r r c p;
  "setFeatures TaggedView" forall _t v f       . setFeatures f (TaggedView _t v)        = TaggedView _t (setFeatures f v);
  "setFeatures Prebuilt" forall v f            . setFeatures f (Prebuilt v)             = Prebuilt (setFeatures f v);

  #-}


pattern SetFeatures :: Features -> View -> View
pattern SetFeatures fs a <- (getFeatures &&& id -> (fs,a)) where
  SetFeatures fs a = setFeatures fs a

pattern Features :: Features -> View -> View
pattern Features fs a <- (getFeatures &&& id -> (fs,a)) where
  Features fs a = addFeatures fs a

{-# INLINE [1] addClass #-}
addClass :: Txt -> View -> View
addClass c a = setFeatures ((getFeatures a) { classes = Set.insert c (classes (getFeatures a)) }) a

{-# INLINE [1] addClasses #-}
addClasses :: [Txt] -> View -> View
addClasses cs v = setFeatures ((getFeatures v) { classes = Set.union (Set.fromList cs) (classes (getFeatures v)) }) v

{-# INLINE [1] setClasses #-}
setClasses :: [Txt] -> View -> View
setClasses cs v = setFeatures (getFeatures v) { classes = Set.fromList cs } v


{-# RULES 
  "addClass . addClass == addClasses" forall c1 c2 v. addClass c1 (addClass c2 v) = addClasses [c1,c2] v;
  "addClass . addClasses == addClasses" forall c cs v. addClass c (addClasses cs v) = addClasses (c:cs) v;
  #-}

pattern Class :: HasCallStack => Txt -> View -> View
pattern Class c a <- (const (error "The Class pattern does not support matching, only construction. For pattern matching, use a combination of the Classes pattern with Data.List.elem.") &&& id -> (c,a)) where
  Class c a = addClass c a

pattern SetClasses :: [Txt] -> View -> View
pattern SetClasses cs a <- ((Set.toList . classes . getFeatures) &&& id -> (cs,a)) where
  SetClasses cs a = setClasses cs a

pattern Classes :: [Txt] -> View -> View
pattern Classes cs a <- SetClasses cs a where
  Classes cs a = addClasses cs a

{-# INLINE [1] addStyle #-}
addStyle :: Txt -> Txt -> View -> View
addStyle k v a = setFeatures ((getFeatures a) { styles = Map.insert k v (styles (getFeatures a)) }) a

{-# INLINE [1] addStyles #-}
addStyles :: [(Txt,Txt)] -> View -> View
addStyles kvs v = setFeatures ((getFeatures v) { styles = Map.union (Map.fromList kvs) (styles (getFeatures v)) }) v

{-# INLINE [1] setStyles #-}
setStyles :: [(Txt,Txt)] -> View -> View
setStyles kvs v = setFeatures (getFeatures v) { styles = Map.fromList kvs } v

{-# RULES 
  "addStyle . addStyle == addStyles" forall k1 v1 k2 v2 v. addStyle k1 v1 (addStyle k2 v2 v) = addStyles [(k1,v1),(k2,v2)] v;
  "addStyle . addStyles == addStyles" forall k v kvs x. addStyle k v (addStyles kvs x) = addStyles ((k,v):kvs) x;
  #-}

pattern Style :: Txt -> Txt -> View -> View
pattern Style k v a <- (const (error "The Style pattern does not support matching, only construction. For pattern matching, use a combination of the Styles pattern with Data.List.lookup.","") &&& id -> ((k,v),a)) where
  Style k v a = setFeatures ((getFeatures a) { styles = Map.insert k v (styles (getFeatures a)) }) a

pattern SetStyles :: [(Txt,Txt)] -> View -> View
pattern SetStyles ss v <- ((Map.toList . styles . getFeatures) &&& id -> (ss,v)) where
  SetStyles ss v = setFeatures ((getFeatures v) { styles = Map.fromList ss }) v

pattern Styles :: [(Txt,Txt)] -> View -> View
pattern Styles ss v <- SetStyles ss v where
  Styles ss v = setFeatures ((getFeatures v) { styles = Map.union (Map.fromList ss) (styles (getFeatures v)) }) v

pattern Listener :: Listener -> View -> View
pattern Listener l a <- (const (error "The Listener pattern does not support matching, only construction. For pattern matching on listeners, use the Listeners pattern.") &&& id -> (l,a)) where
  Listener l a = setFeatures ((getFeatures a) { listeners = l : listeners (getFeatures a) }) a

pattern SetListeners :: [Listener] -> View -> View
pattern SetListeners ls v <- ((listeners . getFeatures) &&& id -> (ls,v)) where
  SetListeners ls v = setFeatures ((getFeatures v) { listeners = ls }) v

pattern Listeners :: [Listener] -> View -> View
pattern Listeners ls v <- SetListeners ls v where
  Listeners ls v = setFeatures ((getFeatures v) { listeners = ls ++ listeners (getFeatures v) }) v

pattern Attribute :: Txt -> Txt -> View -> View
pattern Attribute k v a <- (const (error "The Attribute pattern does not support matching, only construction. For pattern matching on attributes, use the Attributes pattern with Data.List.lookup.","") &&& id -> ((k,v),a)) where
  Attribute k v a = addAttribute k v a

{-# INLINE [1] addAttribute #-}
addAttribute :: Txt -> Txt -> View -> View
addAttribute k v a = setFeatures ((getFeatures a) { attributes = Map.insert k v (attributes (getFeatures a)) }) a

{-# INLINE [1] addAttributes #-}
addAttributes :: [(Txt,Txt)] -> View -> View
addAttributes as v = setFeatures ((getFeatures v) { attributes = Map.union (Map.fromList as) (attributes (getFeatures v)) }) v

{-# INLINE [1] setAttributes #-}
setAttributes :: [(Txt,Txt)] -> View -> View
setAttributes as v = setFeatures (getFeatures v) { attributes = Map.fromList as } v

{-# RULES 
  "addAttribute . addAttribute == addAttributes" forall a1 v1 a2 v2 v. addAttribute a1 v1 (addAttribute a2 v2 v) = addAttributes [(a1,v1),(a2,v2)] v;
  "addAttribute . addAttributes == addAttributes" forall a1 v1 avs v. addAttribute a1 v1 (addAttributes avs v) = addAttributes ((a1,v1):avs) v;
  #-}

pattern SetAttributes :: [(Txt,Txt)] -> View -> View
pattern SetAttributes as v <- ((Map.toList . attributes . getFeatures) &&& id -> (as,v)) where
  SetAttributes as v = setFeatures ((getFeatures v) { attributes = Map.fromList as }) v

pattern Attributes :: [(Txt,Txt)] -> View -> View
pattern Attributes as v <- SetAttributes as v where
  Attributes as v = addAttributes as v

pattern Property :: Txt -> Txt -> View -> View
pattern Property k v a <- (const (error "The Property pattern does not support matching, only construction. For pattern matching on properties, use the Properties pattern with Data.List.lookup.","") &&& id -> ((k,v),a)) where
  Property k v a = setFeatures ((getFeatures a) { properties = Map.insert k v (properties (getFeatures a)) }) a

pattern SetProperties :: [(Txt,Txt)] -> View -> View
pattern SetProperties ps v <- ((Map.toList . properties . getFeatures) &&& id -> (ps,v)) where
  SetProperties ps v = setFeatures ((getFeatures v) { properties = Map.fromList ps <> properties (getFeatures v) }) v

pattern Properties :: [(Txt,Txt)] -> View -> View
pattern Properties ps v <- SetProperties ps v where
  Properties ps v = setFeatures ((getFeatures v) { properties = Map.union (Map.fromList ps) (properties (getFeatures v)) }) v

pattern Lifecycle :: Lifecycle -> View -> View
pattern Lifecycle l a <- (const (error "The Lifecycle pattern does not support matching, only construction. For pattern matching on lifecycle methods, use the Lifecycles pattern.") &&& id -> (l,a)) where
  Lifecycle l a = setFeatures ((getFeatures a) { lifecycles = l : lifecycles (getFeatures a) }) a

-- pattern SetLifecycles :: HasFeatures a => [Lifecycle] -> a -> a
-- pattern SetLifecycles lc v <- (((lifecycles . getFeatures) &&& id) -> (lc,v)) where
--   SetLifecycles lc v = (setFeatures ((getFeatures v) { lifecycles = lc }) v)
--
-- pattern Lifecycles :: HasFeatures a => [Lifecycle] -> a -> a
-- pattern Lifecycles lc v <- (((lifecycles . getFeatures) &&& id) -> (lc,v)) where
--   Lifecycles lc v =
--     let fs = getFeatures v
--     in (setFeatures (fs { lifecycles = lc ++ lifecycles fs }) v)
--
pattern OnCreated :: (Node -> IO (IO ())) -> View -> View
pattern OnCreated f a <- Lifecycle (Created f _) a where
  OnCreated f a = Lifecycle (Created f (pure ())) a

pattern OnMounted :: (Node -> IO (IO ())) -> View -> View
pattern OnMounted f a <- Lifecycle (Mounted f _) a where
  OnMounted f a = Lifecycle (Mounted f (pure ())) a

{-# INLINE [1] addXLinks #-}
addXLinks :: Map.Map Txt Txt -> View -> View
addXLinks ls v = setXLinks (getXLinks v <> ls) v

{-# INLINE [1] getXLinks #-}
getXLinks :: View -> Map.Map Txt Txt
getXLinks SVGView {..} = xlinks
getXLinks KSVGView {..} = xlinks
getXLinks PortalView {..} = getXLinks portalView
getXLinks TaggedView {..} = getXLinks taggedView
getXLinks Prebuilt {..} = getXLinks prebuilt
getXLinks ReactiveView {..} = getXLinks reactiveView
getXLinks WeakView {..} = getXLinks weakView
getXLinks _ = mempty

{-# INLINE [1] setXLinks #-}
setXLinks :: Map.Map Txt Txt -> View -> View
setXLinks xl khtml@SVGView {} = khtml { xlinks = xl }
setXLinks xl ksvg@KSVGView {} = ksvg { xlinks = xl }
setXLinks xl PortalView {..} = PortalView { portalView = setXLinks xl portalView, .. }
setXLinks xl TaggedView {..} = TaggedView { taggedView = setXLinks xl taggedView, .. }
setXLinks xl Prebuilt {..} = Prebuilt { prebuilt = setXLinks xl prebuilt }
setXLinks xl ReactiveView {..} = ReactiveView { reactiveView = setXLinks xl reactiveView, .. }
setXLinks xl WeakView {..} = WeakView { weakView = setXLinks xl weakView, .. }
setXLinks _ v = v

{-# RULES 

  "xlinks: set . set" forall ls ls' x. setXLinks ls (setXLinks ls' x) = setXLinks ls x;
  "xlinks: get . set" forall ls x.     getXLinks (setXLinks ls x)     = ls;
  "xlinks: add . add" forall ls ls' x. addXLinks ls (addXLinks ls' x) = addXLinks (ls' <> ls) x;

  "getXLinks HTMLView" forall e t cs f      . getXLinks (HTMLView e t f cs)      = mempty;
  "getXLinks SVGView" forall e t cs ls f    . getXLinks (SVGView e t f ls cs)    = ls;
  "getXLinks TextView" forall t c           . getXLinks (TextView t c)           = mempty;
  "getXLinks NullView" forall e             . getXLinks (NullView e)             = mempty;
  "getXLinks RawView" forall e t f c        . getXLinks (RawView e t f c)        = mempty;
  "getXLinks KHTMLView" forall e t cs f     . getXLinks (KHTMLView e t f cs)     = mempty;
  "getXLinks KSVGView" forall e t cs ls f   . getXLinks (KSVGView e t f ls cs)   = ls;
  "getXLinks ReactiveView" forall c a v     . getXLinks (ReactiveView c a v)     = getXLinks v;
  "getXLinks WeakView" forall c a v         . getXLinks (WeakView c a v)         = getXLinks v;
  "getXLinks PortalView" forall p d v       . getXLinks (PortalView p d v)       = getXLinks v;
  "getXLinks ComponentView" forall _r r c p . getXLinks (ComponentView _r r c p) = mempty;
  "getXLinks TaggedView" forall _t v        . getXLinks (TaggedView _t v)        = getXLinks v;
  "getXLinks Prebuilt" forall v             . getXLinks (Prebuilt v)             = getXLinks v;

  "setXLinks HTMLView" forall e t cs f ls      . setXLinks ls (HTMLView e t f cs)      = HTMLView e t f cs;
  "setXLinks SVGView" forall e t cs ls f ls'   . setXLinks ls (SVGView e t f ls' cs)   = SVGView e t f ls cs;
  "setXLinks TextView" forall t c ls           . setXLinks ls (TextView t c)           = TextView t c;
  "setXLinks NullView" forall e ls             . setXLinks ls (NullView e)             = NullView e;
  "setXLinks RawView" forall e t f c ls        . setXLinks ls (RawView e t f c)        = RawView e t f c;
  "setXLinks KHTMLView" forall e t cs f ls     . setXLinks ls (KHTMLView e t f cs)     = KHTMLView e t f cs;
  "setXLinks KSVGView" forall e t cs ls f ls'  . setXLinks ls (KSVGView e t f ls' cs)  = KSVGView e t f ls cs;
  "setXLinks ReactiveView" forall c a v ls     . setXLinks ls (ReactiveView c a v)     = ReactiveView c a (setXLinks ls v);
  "setXLinks WeakView" forall c a v ls         . setXLinks ls (WeakView c a v)         = WeakView c a (setXLinks ls v);
  "setXLinks PortalView" forall p d v ls       . setXLinks ls (PortalView p d v)       = PortalView p d (setXLinks ls v);
  "setXLinks ComponentView" forall _r r c p ls . setXLinks ls (ComponentView _r r c p) = ComponentView _r r c p;
  "setXLinks TaggedView" forall _t v ls        . setXLinks ls (TaggedView _t v)        = TaggedView _t (setXLinks ls v);
  "setXLinks Prebuilt" forall v ls             . setXLinks ls (Prebuilt v)             = Prebuilt (setXLinks ls v);

  #-}

pattern XLink :: Txt -> Txt -> View -> View
pattern XLink k v a <- (const (error "The XLink pattern does not support matching, only construction. For pattern matching on xlinks, use the XLinks pattern with Data.List.lookup.","") &&& id -> ((k,v),a)) where
  XLink k v a =
    let xls = getXLinks a
    in setXLinks (Map.singleton k v <> xls) a

pattern SetXLinks :: Map.Map Txt Txt -> View -> View
pattern SetXLinks xl v <- (getXLinks &&& id -> (xl,v)) where
  SetXLinks xl v = setXLinks xl v

pattern XLinks :: Map.Map Txt Txt -> View -> View
pattern XLinks xl v <- (getXLinks &&& id -> (xl,v)) where
  XLinks xl v = addXLinks xl v

{-# INLINE [1] getChildren #-}
getChildren :: View -> [View]
getChildren v@HTMLView {} = children v
getChildren v@SVGView {} = children v
getChildren PortalView {..} = getChildren portalView
getChildren TaggedView {..} = getChildren taggedView
getChildren Prebuilt {..} = getChildren prebuilt
getChildren ReactiveView {..} = getChildren reactiveView
getChildren WeakView {..} = getChildren weakView
getChildren _  = []

{-# INLINE [1] setChildren #-}
setChildren :: [View] -> View -> View
setChildren cs v@HTMLView {} = v { children = cs }
setChildren cs v@SVGView {} = v { children = cs }
setChildren cs PortalView {..} = PortalView { portalView = setChildren cs portalView, .. }
setChildren cs TaggedView {..} = TaggedView { taggedView = setChildren cs taggedView, .. }
setChildren cs Prebuilt {..} = Prebuilt { prebuilt = setChildren cs prebuilt }
setChildren cs ReactiveView {..} = ReactiveView { reactiveView = setChildren cs reactiveView , .. }
setChildren cs WeakView {..} = WeakView { weakView = setChildren cs weakView  , .. }
setChildren _ v = v

{-# INLINE [1] addChildren #-}
addChildren :: [View] -> View -> View
addChildren cs v = setChildren (getChildren v <> cs) v


{-# RULES 

  "children: set . set" forall fs fs' x. setChildren fs (setChildren fs' x) = setChildren fs x;
  "children: get . set" forall fs x. getChildren (setChildren fs x) = fs;
  "children: add . add" forall fs gs x. addChildren fs (addChildren gs x) = addChildren (gs <> fs) x;

  "getChildren HTMLView" forall e t cs f     . getChildren (HTMLView e t f cs)      = cs;
  "getChildren SVGView" forall e t cs ls f   . getChildren (SVGView e t f ls cs)    = cs;
  "getChildren TextView" forall t c          . getChildren (TextView t c)           = [];
  "getChildren NullView" forall e            . getChildren (NullView e)             = [];
  "getChildren RawView" forall e t f c       . getChildren (RawView e t f c)        = [];
  "getChildren KHTMLView" forall e t cs f    . getChildren (KHTMLView e t f cs)     = [];
  "getChildren KSVGView" forall e t cs ls f  . getChildren (KSVGView e t f ls cs)   = [];
  "getChildren ReactiveView" forall c a v    . getChildren (ReactiveView c a v)     = getChildren v;
  "getChildren WeakView" forall c a v        . getChildren (WeakView c a v)         = getChildren v;
  "getChildren PortalView" forall p d v      . getChildren (PortalView p d v)       = getChildren v;
  "getChildren ComponentView" forall _r r c p. getChildren (ComponentView _r r c p) = [];
  "getChildren TaggedView" forall _t v       . getChildren (TaggedView _t v)        = getChildren v;
  "getChildren Prebuilt" forall v            . getChildren (Prebuilt v)             = getChildren v;

  "setChildren HTMLView" forall e t cs f cs'    . setChildren cs (HTMLView e t f cs')     = HTMLView e t f cs;
  "setChildren SVGView" forall e t cs ls f cs'  . setChildren cs (SVGView e t f ls cs')   = SVGView e t f ls cs;
  "setChildren TextView" forall t c cs          . setChildren cs (TextView t c)           = TextView t c;
  "setChildren NullView" forall e cs            . setChildren cs (NullView e)             = NullView e;
  "setChildren RawView" forall e t f c cs       . setChildren cs (RawView e t f c)        = RawView e t f c;
  "setChildren KHTMLView" forall e t kcs f cs   . setChildren cs (KHTMLView e t f kcs)    = KHTMLView e t f kcs;
  "setChildren KSVGView" forall e t kcs ls f cs . setChildren cs (KSVGView e t f ls kcs)  = KSVGView e t f ls kcs;
  "setChildren ReactiveView" forall c a v cs    . setChildren cs (ReactiveView c a v)     = ReactiveView c a (setChildren cs v);
  "setChildren WeakView" forall c a v cs        . setChildren cs (WeakView c a v)         = WeakView c a (setChildren cs v);
  "setChildren PortalView" forall p d v cs      . setChildren cs (PortalView p d v)       = PortalView p d (setChildren cs v);
  "setChildren ComponentView" forall _r r c p cs. setChildren cs (ComponentView _r r c p) = ComponentView _r r c p;
  "setChildren TaggedView" forall _t v cs       . setChildren cs (TaggedView _t v)        = TaggedView _t (setChildren cs v);
  "setChildren Prebuilt" forall v cs            . setChildren cs (Prebuilt v)             = Prebuilt (setChildren cs v);

  #-}

pattern SetChildren :: [View] -> View -> View
pattern SetChildren cs v <- (getChildren &&& id -> (cs,v)) where
  SetChildren cs v = setChildren cs v

pattern Children :: [View] -> View -> View
pattern Children cs v <- (getChildren &&& id -> (cs,v)) where
  Children cs v = addChildren cs v

{-# INLINE [1] getKeyedChildren #-}
getKeyedChildren :: View -> [(Int,View)]
getKeyedChildren v@KHTMLView {} = keyedChildren v
getKeyedChildren v@SVGView {} = keyedChildren v
getKeyedChildren PortalView {..} = getKeyedChildren portalView
getKeyedChildren TaggedView {..} = getKeyedChildren taggedView
getKeyedChildren Prebuilt {..} = getKeyedChildren prebuilt
getKeyedChildren ReactiveView {..} = getKeyedChildren reactiveView
getKeyedChildren WeakView {..} = getKeyedChildren weakView
getKeyedChildren _ = []

{-# INLINE [1] setKeyedChildren #-}
setKeyedChildren :: [(Int,View)] -> View -> View
setKeyedChildren cs v@KHTMLView {} = v { keyedChildren = cs }
setKeyedChildren cs v@KSVGView {} = v { keyedChildren = cs }
setKeyedChildren cs PortalView {..} = PortalView { portalView = setKeyedChildren cs portalView, .. }
setKeyedChildren cs TaggedView {..} = TaggedView { taggedView = setKeyedChildren cs taggedView, .. }
setKeyedChildren cs Prebuilt {..} = Prebuilt { prebuilt = setKeyedChildren cs prebuilt, .. }
setKeyedChildren cs ReactiveView {..} = ReactiveView { reactiveView = setKeyedChildren cs reactiveView, .. }
setKeyedChildren cs WeakView {..} = WeakView { weakView = setKeyedChildren cs weakView, .. }
setKeyedChildren _ v = v

{-# INLINE [1] addKeyedChildren #-}
addKeyedChildren :: [(Int,View)] -> View -> View
addKeyedChildren kcs v = setKeyedChildren (getKeyedChildren v <> kcs) v

{-# RULES 

  "kchildren: set . set" forall fs fs' x. setKeyedChildren fs (setKeyedChildren fs' x) = setKeyedChildren fs x;
  "kchildren: get . set" forall fs x. getKeyedChildren (setKeyedChildren fs x) = fs;
  "kchildren: add . add" forall fs gs x. addKeyedChildren fs (addKeyedChildren gs x) = addKeyedChildren (gs <> fs) x;

  "getKeyedChildren HTMLView" forall e t cs f      . getKeyedChildren (HTMLView e t f cs)      = [];
  "getKeyedChildren SVGView" forall e t cs ls f    . getKeyedChildren (SVGView e t f ls cs)    = [];
  "getKeyedChildren TextView" forall t c           . getKeyedChildren (TextView t c)           = [];
  "getKeyedChildren NullView" forall e             . getKeyedChildren (NullView e)             = [];
  "getKeyedChildren RawView" forall e t f c        . getKeyedChildren (RawView e t f c)        = [];
  "getKeyedChildren KHTMLView" forall e t kcs f    . getKeyedChildren (KHTMLView e t f kcs)    = kcs;
  "getKeyedChildren KSVGView" forall e t kcs ls f  . getKeyedChildren (KSVGView e t f ls kcs)  = kcs;
  "getKeyedChildren ReactiveView" forall c a v     . getKeyedChildren (ReactiveView c a v)     = getKeyedChildren v;
  "getKeyedChildren WeakView" forall c a v         . getKeyedChildren (WeakView c a v)         = getKeyedChildren v;
  "getKeyedChildren PortalView" forall p d v       . getKeyedChildren (PortalView p d v)       = getKeyedChildren v;
  "getKeyedChildren ComponentView" forall _r r c p . getKeyedChildren (ComponentView _r r c p) = [];
  "getKeyedChildren TaggedView" forall _t v        . getKeyedChildren (TaggedView _t v)        = getKeyedChildren v;
  "getKeyedChildren Prebuilt" forall v             . getKeyedChildren (Prebuilt v)             = getKeyedChildren v;

  "setKeyedChildren HTMLView" forall e t cs f kcs      . setKeyedChildren kcs (HTMLView e t f cs)      = HTMLView e t f cs;
  "setKeyedChildren SVGView" forall e t cs ls f kcs    . setKeyedChildren kcs (SVGView e t f ls cs)    = SVGView e t f ls cs;
  "setKeyedChildren TextView" forall t c kcs           . setKeyedChildren kcs (TextView t c)           = TextView t c;
  "setKeyedChildren NullView" forall e kcs             . setKeyedChildren kcs (NullView e)             = NullView e;
  "setKeyedChildren RawView" forall e t f c kcs        . setKeyedChildren kcs (RawView e t f c)        = RawView e t f c;
  "setKeyedChildren KHTMLView" forall e t kcs f kcs'   . setKeyedChildren kcs (KHTMLView e t f kcs')   = KHTMLView e t f kcs;
  "setKeyedChildren KSVGView" forall e t kcs ls f kcs' . setKeyedChildren kcs (KSVGView e t f ls kcs') = KSVGView e t f ls kcs;
  "setKeyedChildren ReactiveView" forall c a v kcs     . setKeyedChildren kcs (ReactiveView c a v)     = ReactiveView c a (setKeyedChildren kcs v);
  "setKeyedChildren WeakView" forall c a v kcs         . setKeyedChildren kcs (WeakView c a v)         = WeakView c a (setKeyedChildren kcs v);
  "setKeyedChildren PortalView" forall p d v kcs       . setKeyedChildren kcs (PortalView p d v)       = PortalView p d (setKeyedChildren kcs v);
  "setKeyedChildren ComponentView" forall _r r c p kcs . setKeyedChildren kcs (ComponentView _r r c p) = ComponentView _r r c p;
  "setKeyedChildren TaggedView" forall _t v kcs        . setKeyedChildren kcs (TaggedView _t v)        = TaggedView _t (setKeyedChildren kcs v);
  "setKeyedChildren Prebuilt" forall v kcs             . setKeyedChildren kcs (Prebuilt v)             = Prebuilt (setKeyedChildren kcs v);

  #-}

pattern SetKeyedChildren :: [(Int,View)] -> View -> View
pattern SetKeyedChildren ks v <- (getKeyedChildren &&& id -> (ks,v)) where
  SetKeyedChildren ks v = setKeyedChildren ks v

pattern KeyedChildren :: [(Int,View)] -> View -> View
pattern KeyedChildren ks v <- (getKeyedChildren &&& id -> (ks,v)) where
  KeyedChildren ks v = addKeyedChildren ks v

{-# INLINE (<|) #-}
infixl 8 <|
(<|) :: a -> (a -> b) -> b
(<|) a f = f a

{-# INLINE (<||>) #-}
infixr 9 <||>
(<||>) :: View -> [View] -> View
(<||>) v cs = setChildren cs v

{-# INLINE (<||#>) #-}
infixr 9 <||#>
(<||#>) :: View -> [(Int,View)] -> View
(<||#>) v cs = setKeyedChildren cs v

{-# INLINE (|>) #-}
infixr 9 |>
(|>) :: (View -> View) -> [View] -> View -> View
(|>) f cs = setChildren cs . f

{-# INLINE (|#>) #-}
infixr 9 |#>
(|#>) :: (View -> View) -> [(Int,View)] -> View -> View
(|#>) f cs = setKeyedChildren cs . f

-- The following three functions are extraordinarily unsafe. 
-- People will hate them. I hate them. 
--
-- They allow for building complex UIs of dependent 
-- resources without the use of Component:
--
-- > someView fallback = build `or` fallback
-- >   where
-- >     build =
-- >       let SomeResource { someSubresource, .. } = unsafePerformIO (parse <$> <some http GET>)
-- >           SomeDependentResource {..} = unsafePerformIO (parse <$> <some http GET> someSubresource)
-- >       <use SomeResource and/or SomeDependentResource>
--
-- Staying out of IO here is extraordinarly convenient and,
-- in particular cases, can have large implications for 
-- performance. The one rule I attempt to stick to is: 
-- they only be used for GET requests. To that end, see
-- `Client` for unsafe GET request code that simplifies this
-- use-case.
--

within :: Time -> a -> a
within t a = unsafePerformIO (fromJust <$> timeout t (evaluate a))

caught :: Exception e => a -> (e -> a) -> a
caught a f = unsafePerformIO (Control.Exception.catch (evaluate a) (pure . f))

or :: a -> a -> a
or l r = Data.View.caught @SomeException l (const r)

-- {-# INLINE may #-}
-- may :: forall a b. Exists (Maybe a) => b -> (Exists a => b) -> b
-- may nothing just = maybe nothing (`with` just) (it :: Maybe a)

{-# INLINE may #-}
may :: Maybe a -> View -> (a -> View) -> View
may m n j = reactive m (maybe n j m)

{-# INLINE try #-}
try :: forall a b. Exists (Try.Try a) => b -> b -> (Exists a => b) -> b
try trying failed done = Try.try trying failed (`with` done) (it :: Try.Try a)

{-# INLINE unite #-}
unite :: forall a b c. Exists (Either a b) => (Exists a => c) -> (Exists b => c) -> c
unite left right = either (`with` left) (`with` right) (it :: Either a b)

{-# INLINE each #-}
each :: forall f a b. (Functor f, Exists (f a)) => (Exists a => b) -> f b 
each b = fmap (`with` b) (it :: f a)

data Fork

-- Fork an asynchronous rendering context. 
--
-- Consider:
-- 
-- > lazily action do
-- >   fork do
-- >     { ... await ... }
--
-- Without fork, await is render-blocking. With fork, the await is performed
-- within a new thread and, therefore, only blocks the fork.
--
{-# INLINE fork #-}
fork :: View -> View
fork = component @View @Fork (const def { deferred = True, render = const })

data Asynchronous a = Asynchronous (IO a) (Exists a |- View)

data Async a = Async 
  { action :: IO a
  , view_  :: Exists a |- View
  , thread :: ThreadId 
  , result :: MVar a
  }

-- | Await is semantically equivalent to `it`, but is meant to convey the fact
-- that the existential will take time to materialize. Thus, `await` is a
-- convenience often used with `lazy`.
--
-- > await = it
--
await :: Exists a => a
await = it

{-# NOINLINE lazily #-}
-- Note that only the asynchrony of the first action can be witnessed in View
-- To witness the asynchrony of reconciled effects, use `weak` to recreate the
-- `View` on each reconciliation.
lazily :: forall a. Typeable a => IO a -> (Exists a => View) -> View
lazily io v = go (Asynchronous io (proof v))
  where
    go = component \self -> def
      { deferred = True

      , construct = do
          Asynchronous action view_ <- askref self
          result <- newEmptyMVar
          thread <- forkIO (action >>= putMVar result)
          pure Async {..}

      , onReceive = \(Asynchronous new_action new_view) old@Async {..} ->
          case (req new_action action,req new_view view_) of
            (False,_) -> do
              let action = new_action
              let view_ = new_view
              thread <- forkIO do
                Control.Exception.mask $ \unmask -> do
                  a <- action 
                  unmask do
                    tid <- myThreadId
                    modifyrefM_ self $ \_ Async {..} ->
                      if tid == thread then do
                        result <- newMVar a
                        pure (Async {..},def)
                      else
                        pure (Async {..},def)
              pure Async {..}

            (_,False) -> do
              pure (old :: Async a) { view_ = new_view }

            _ -> do
              pure old

      , render = \_ Async { view_, result } -> 
          with (unsafePerformIO (readMVar result)) (prove view_)

      }

data Synchronous a = Synchronous (IO a) (Exists a |- View)
data Sync a = Sync 
  { action :: IO a
  , result :: a
  , view_  :: Exists a |- View
  }

{-# NOINLINE eagerly #-}
eagerly :: forall a. Typeable a => IO a -> (Exists a => View) -> View
eagerly io v = go (Synchronous io (proof v))
  where
    go = component \self -> def
      { construct = do
        Synchronous action view_ <- askref self
        result <- action
        pure Sync {..}

      , onReceive = \(Synchronous new_action new_view) old@Sync {..} -> do
          case (action === new_action,view_ === new_view) of
            (False,_) -> do
              let action = new_action
              let view_ = new_view
              result <- action
              pure Sync {..}
            (_,False) -> do
              pure (old :: Sync a) { view_ = new_view }
            _ -> do
              pure old

      , render = \_ Sync { view_, result } -> with result (prove view_)
      }

{-# INLINE conc #-}
conc :: View -> View -> View
conc a b = race [a,b] it

{-# INLINE race #-}
race :: [View] -> (Exists View => View) -> View
race vs v = lazily run v
  where
    run = do
      mv <- newEmptyMVar
      tids <- for vs \v -> forkIO (evaluate v >>= putMVar mv)
      v <- takeMVar mv
      for_ tids killThread
      pure v

-- A record of listeners to lifecycle events.
--
-- This approach reduces the component overhead.
--
data Lifecycles = Lifecycles
  { onStart :: IO ()
  , onLoad :: IO ()
  , onBefore :: IO ()
  , onAfter :: IO ()
  , onStop :: IO ()
  }

instance Semigroup Lifecycles where
  (<>) lc1 lc2 = Lifecycles 
    { onStart = onStart lc1 >> onStart lc2
    , onLoad = onLoad lc1 >> onLoad lc2
    , onBefore = onBefore lc1 >> onBefore lc2
    , onAfter = onAfter lc1 >> onAfter lc2
    , onStop = onStop lc1 >> onStop lc2
    }

instance Monoid Lifecycles where
  mempty = def

instance Default Lifecycles where
  def = Lifecycles def def def def def

{-# RULES 
  "lifecyle lc1 (lifecycle lc2) => lifecycle (lc1 <> lc2)"
    forall lc1 lc2 v. lifecycle lc1 (lifecycle lc2 v) = lifecycle (lc1 <> lc2) v
 #-}


{-# NOINLINE lifecycle #-}
lifecycle :: Lifecycles -> View -> View
lifecycle ls v = component go (ls,v)
  where
    go self = def
      { construct = askref self >>= onStart . fst
      , onMounted = askref self >>= onLoad . fst
      , onUpdate = \(_,v') _ -> do
          (ls,v) <- askref self
          unless (isTrue# (reallyUnsafePtrEquality# v v')) (onBefore ls)
      , onUpdated = \(_,v') _ -> do
          (ls,v) <- askref self
          unless (isTrue# (reallyUnsafePtrEquality# v v')) (onAfter ls)
      , onUnmounted = askref self >>= onStop . fst
      , render = \(_,v) _ -> v
      }

watch :: IO () -> View
watch = component @(IO ()) @() go
  where
    go self = def
      { onUpdate = const
      }

watch' :: IO () -> View
watch' = component go
  where
    go self = def
      { construct = join (askref self)
      , onUpdate    = const
      }

watching' :: (Typeable a, Typeable b) => a -> IO (b,IO ()) -> (Exists b => View) -> View
watching' a create v = component go (a,create,(`with` v))
  where
    go self = def
      { construct = askref self >>= \(_,create,_) -> create
      , onReceive = \(a,new,_) current@(_,cleanup) -> do
        (a',old,_) <- askref self
        if isTrue# (reallyUnsafePtrEquality# a a')
        then pure current
        else cleanup >> new
      , onUnmounted = getref self >>= snd
      , render = \(_,_,v) (b,_) -> v b
      }

watching :: Typeable b => IO b -> (Exists b => View) -> View
watching io = watching' io (io >>= \b -> pure (b,def))

data Polling a = Polling Time (IO a) (Exists a |- View)

data Poll a = Poll
  { interval :: Time
  , begin :: Time
  , action :: IO a 
  , view_  :: Exists a |- View
  , thread :: ThreadId
  , result :: MVar a
  }

{-# INLINE every #-}
every :: forall a. Typeable a => Time -> IO a -> (Exists a => View) -> View
every = poll

{-# NOINLINE poll #-}
-- Note that only the asynchrony of the first action can be witnessed in View
poll :: forall a. Typeable a => Time -> IO a -> (Exists a => View) -> View
poll t f v = go (Polling t f (proof v))
  where
    go = component \self -> def
      { deferred = True

      , construct = do
          begin <- time
          Polling interval action view_ <- askref self
          result <- newEmptyMVar
          thread <- forkIO do

            Control.Exception.mask $ \unmask -> do
              a <- action 
              unmask (putMVar result a)

            forever do
              delay interval
              Control.Exception.mask $ \unmask -> do
                a <- action
                unmask do
                  tid <- myThreadId
                  modifyrefM self $ \_ Poll {..} ->
                    if tid == thread then do
                      begin <- time
                      result <- newMVar a
                      pure (Poll {..},def)
                    else
                      pure (Poll {..},killThread tid)

          pure Poll {..}

      , onReceive = \(Polling new_interval new_action new_view) old@Poll {..} -> do
          let
            sameInterval = new_interval == interval
            sameAction   = new_action === action 
            sameView     = new_view === view_
          now <- time
          if 
            | not sameInterval || not sameAction -> do
              let interval = new_interval
              let begin = now
              let action = new_action
              let view_ = new_view
              thread <-
                forkIO do
                  when (not sameInterval && sameAction) do
                    delay ((begin + new_interval) - now)
                  forever do
                    Control.Exception.mask $ \unmask -> do
                      a <- action
                      unmask do
                        tid <- myThreadId
                        modifyrefM self $ \_ Poll {..} ->
                          if tid == thread then do
                            begin <- time
                            result <- newMVar a
                            pure (Poll {..},def)
                          else
                            pure (Poll {..},killThread tid)
                        delay interval
              pure Poll {..}

            | not sameView -> do
              pure (old :: Poll a) { view_ = new_view }
              
            | otherwise ->
              pure old

      , onUnmounted = getref self >>= \Poll { thread } -> killThread thread

      , render = \_ Poll { view_, result } -> 
          with (unsafePerformIO (readMVar result)) (prove view_)
      }

-- | Delay the materialization of a value. This can be useful for creating 
-- suspense where it might not always exist. Some UIs need to be slowed 
-- down to improve the user experience or to introduce consistency across
-- devices, and careful use of `delayed` is a simple primitive that can
-- serve that purpose. Note that `delayed` is synchronous! The delays 
-- introduced from multiple `delayed` calls within a view will be
-- additive.
-- 
-- The following is guaranteed to show `spinner` for at least 500ms:
--
-- > post slug =
-- >   suspense 500 spinner do
-- >     delayed 1000 do
-- >       let
-- >         Post { author } = get slug
-- >         Author { realName, bio } = get author
-- >       {...}
--
{-# INLINE delayed #-}
delayed :: Time -> a -> a
delayed t a = unsafePerformIO do
  mv <- newEmptyMVar
  forkIO do
    Control.Exception.catch (evaluate a >>= putMVar mv) \(se :: SomeException) -> 
      void (tryPutMVar mv (Control.Exception.throw se))
  delay t
  takeMVar mv

type Reader a = Exists a

type family Readers (xs :: [*]) :: Constraint where
  Readers (x ': xs) = (Reader x,Readers xs)
  Readers '[] = ()

{-# INLINE ask #-}
-- | `ask` is semantically equivalent to `it`, but is meant to convey the fact
-- that the existential is treated as a read-only value, even if that is not
-- the case. `Reader` does not guarantee a value to be static, it only
-- gurantees availability.
--
-- > ask = it
--
ask :: Reader a => a
ask = it

{-# INLINE asks #-}
asks :: (a -> b) -> (Reader a => b)
asks f = f ask

{-# INLINE reader #-}
reader :: a -> (Reader a => x) -> x
reader = with

{-# INLINE local #-}
local :: (a -> b) -> (Reader b => x) -> (Reader a => x)
local f = reader (f ask)

type Modify a = Producer (a -> IO a)
type State a = (Modify a, Exists a)

{-# INLINE state #-}
state :: Typeable a => (Modify a => a) -> (State a => View) -> View
state a = foldM ($) (pure (a,\_ -> pure ()))

{-# INLINE stateIO #-}
stateIO :: Typeable a => (Modify a => IO a) -> (State a => View) -> View
stateIO ioa = foldM ($) (ioa >>= \a -> pure (a,\_ -> pure ()))

{-# INLINE state' #-}
state' :: forall a. Typeable a => (Modify a => a) -> (State a => View) -> View
state' a v = weak (proof @(Modify a) a) (state a v)

{-# INLINE stateIO' #-}
stateIO' :: forall a. Typeable a => (Modify a => IO a) -> (State a => View) -> View
stateIO' ioa v = weak (proof @(Modify a) ioa) (stateIO ioa v)

{-# INLINE stateWith #-}
stateWith :: forall a. Typeable a => (Modify a => a -> a -> IO a) -> (Modify a => IO (a,a -> IO ())) -> (State a => View) -> View
stateWith f = foldM (\g a -> g a >>= f a) 

{-# INLINE stateWith' #-}
stateWith' :: forall a. Typeable a => (Modify a => a -> a -> IO a) -> (Modify a => IO (a,a -> IO ())) -> (State a => View) -> View
stateWith' f i v = weak (proof @(Modify a) f,proof @(Modify a) i) (stateWith f i v)

{-# INLINE modifyIO #-}
modifyIO :: forall a. Modify a => (a -> IO a) -> IO ()
modifyIO = yield 

{-# INLINE modify #-}
modify :: forall a. Modify a => (a -> a) -> IO ()
modify f = yield (pure @IO . f)

{-# INLINE modifyIt #-}
modifyIt :: forall a. Modify a => (Exists a => a) -> IO ()
modifyIt a = modifyItIO (pure a) 

{-# INLINE modifyItIO #-}
modifyItIO :: forall a. Modify a => (Exists a => IO a) -> IO ()
modifyItIO ioa = yield (\a -> with (a :: a) ioa)

{-# INLINE put #-}
put :: Modify a => a -> IO ()
put = modify . const

{-# INLINE get #-}
-- | `get` is semantically equivalent to `it`, but is meant to convey the fact
-- that the existential is within a read-write context. `get` does not guarantee
-- that a value is within a read-write context because doing so would incur a
-- structural dependency on the `Modify` context.
--
-- > get = it
--
get :: Exists a => a
get = it

{-# INLINE zooming #-}
zooming :: forall a b x. (forall f. Functor f => (b -> f b) -> a -> f a) -> (State b => x) -> (State a => x)
zooming f = zoom (getConst . f Const) (\b -> runIdentity . f (\_ -> Identity b))

{-# INLINE zoom #-}
zoom :: forall a b x. (a -> b) -> (b -> a -> a) -> (State b => x) -> (State a => x)
zoom f g v = with (f get) ((\h a -> h (f a) >>= \b -> pure @IO (g b a)) # v)

{-# INLINE zoomIO #-}
zoomIO :: forall a b x. (a -> b) -> (b -> a -> IO a) -> (State b => x) -> (State a => x)
zoomIO f g v = with (f get) ((\h a -> h (f a) >>= \b -> g b a) # v)

{-# INLINE ignore #-}
ignore :: forall b x. (Modify b => x) -> x
ignore = discard @(b -> IO b)

{-# INLINE toState #-}
toState :: forall a. (Producer a => View) -> (State a => View)
toState = stream @a put

{-# INLINE toStateWith #-}
toStateWith :: forall a b. (a -> b) -> (Producer a => View) -> (State b => View)
toStateWith f = stream @a (put . f)

{-# INLINE flat #-}
flat :: forall l a x. (IsList l, Item l ~ a, State l) => (State a => x) -> [x]
flat = flatBy @l @a (const True)

{-# INLINE flatBy #-}
flatBy :: forall l a x. (IsList l, Item l ~ a, State l) => (a -> Bool) -> (State a => x) -> [x]
flatBy pred v =
  [ zoom (const a) (replace n) v
  | (n,a) <- zip [0..] (Exts.toList (get :: l))
  , pred a
  ]
  where
    replace :: Int -> a -> l -> l
    replace n a l = Exts.fromList (go n (Exts.toList l))
      where
        go _ []     = []
        go 0 (_:as) = a : as
        go n (a:as) = a : go (n - 1) as
--
-- {-# INLINE some #-}
-- some :: (Typeable a, State a) => a -> (State a => View) -> View
-- some = state
--
-- {-# INLINE one #-}
-- one :: Exists a => a
-- one = it
--
-- {-# INLINE many #-}
-- many :: forall a. (Typeable a, State (Maybe a)) => (State (Maybe a) => View) -> View
-- many = state @(Maybe a) Nothing
--
-- {-# INLINE any #-}
-- any :: Exists (Maybe a) => Maybe a
-- any = it
--


newtype Log a = Log a deriving stock Functor deriving (Semigroup,Monoid) via a

type Writer a = State (Log a)

{-# INLINE tell #-}
tell :: (Semigroup a, Modify (Log a)) => a -> IO ()
tell a = modify (<> Log a)

-- Unlike monadic writers, where discretization of computation is achieved
-- through wrapping and composition, discretization of projected computational
-- views is achieved through nesting. It is therefore necessary to expose a
-- method of inspecting the log to achieve a dependence between the results of
-- a writer and its nested contexts. 
--
-- Without listen, writer x == silence x.
{-# INLINE listen #-}
listen :: Exists (Log a) => a
listen = let Log a = it in a

{-# INLINE silence #-}
silence :: forall a. (Modify (Log a) => View) -> View
silence = ignore @(Log a)

{-# INLINE writer #-}
writer :: forall a. Typeable a => Monoid a => (Writer a => View) -> View
writer = state (mempty :: Log a)

{-# INLINE translate #-}
translate :: forall a b x. (a -> b) -> (b -> a -> a) -> (Writer b => x) -> (Writer a => x)
translate f g = zoom f' g'
  where
    f' :: Log a -> Log b
    f' = coerce f

    g' :: Log b -> Log a -> Log a
    g' = coerce g

{-# INLINE toWriter #-}
toWriter :: forall a. Semigroup a => (Producer a => View) -> (Modify (Log a) => View)
toWriter = toWriterWith @a id

{-# INLINE toWriterWith #-}
toWriterWith :: Semigroup b => (a -> b) -> (Producer a => View) -> (Modify (Log b) => View)
toWriterWith f = stream (tell . f)

newtype Failure e = Failure e deriving Functor
type Throws e = Producer (Failure e)

{-# INLINE throw #-}
throw :: Throws e => e -> IO ()
throw = yield . Failure

{-# INLINE catch #-}
catch :: (e -> IO ()) -> (Throws e => View) -> View
catch f = stream (\(Failure e) -> f e) 

{-# INLINE mask #-}
mask :: forall e. (Throws e => View) -> View
mask = discard @(Failure e)

{-# INLINE pass #-}
pass :: (a -> b) -> (Throws a => x) -> (Throws b => x)
pass f = (fmap @Failure f #)

{-# INLINE toError #-}
toError :: forall e. (Producer e => View) -> (Throws e => View)
toError = toErrorWith @e id

{-# INLINE toErrorWith #-}
toErrorWith :: (e -> e') -> (Producer e => View) -> (Throws e' => View)
toErrorWith f = stream (yield . Failure . f) 

{-# INLINE fromError #-}
fromError :: forall e. (Throws e => View) -> (Producer e => View)
fromError = fromErrorWith @e id

{-# INLINE fromErrorWith #-}
fromErrorWith :: (e -> e') -> (Throws e => View) -> (Producer e' => View)
fromErrorWith f = stream (\(Failure e) -> yield (f e))

type Event a = Exists a
type Behavior a b = Event a => b

{-# INLINE behavior #-}
behavior :: forall a b. Event a => Behavior a b -> b
behavior = id

{-# INLINE event #-}
event :: a -> (Event a => b) -> b
event = with

{-# INLINE read #-}
-- | `read` is semantically equivalent to `it`, but is meant to convey the fact
-- that the existential is within a reactive context. 
--
-- > get = it
--
read :: Event a => a
read = it

{-# WARNING one, occs, ambs, improving, amb ["These are experimental; do not expect fixed or consistent operation."] #-}

{-# INLINE one #-}
one :: a -> a
one a = unsafePerformIO (readIORef ref)
  where
    {-# NOINLINE ref #-}
    ref = unsafePerformIO (newIORef a)

{-# INLINE occs #-}
occs :: a -> [a]
occs a = unsafePerformIO (atomicModifyIORef' ref (dup . (a:)))
  where
    {-# NOINLINE ref #-}
    ref = unsafePerformIO (newIORef [])

    dup x = (x,x)

{-# INLINE ambs #-}
ambs :: a -> b -> [Either a b]
ambs a b = changes (occs (a,b))
  where
    changes ((a1,b1):(a2,b2):xys)
      | isTrue# (reallyUnsafePtrEquality# a1 a2) = Right b1 : changes ((a2,b2):xys)
      | otherwise = Left a1 : changes ((a2,b2):xys)
    changes xs = []

{-# INLINE improving #-}
improving :: a -> b -> Maybe (Either a b)
improving a b = listToMaybe (ambs a b)

{-# INLINE amb #-}
amb :: a -> a -> Maybe a
amb l r = fmap (either id id) (improving l r)

data Fold eff a = Fold (eff -> a -> IO a) (IO (a,a -> IO ())) (a -> View)

{-# INLINE fold #-}
fold :: (Typeable eff, Typeable a) => (Effect eff => eff -> a -> a) -> (Effect eff => a) -> ((Effect eff, Exists a) => View) -> View
fold step initial = foldM (\eff a -> pure (step eff a)) (pure (initial,\_ -> pure ()))

{-# INLINE foldM #-}
foldM :: forall eff a. (Typeable eff, Typeable a) => (Effect eff => eff -> a -> IO a) -> (Effect eff => IO (a,a -> IO ())) -> ((Effect eff,Exists a) => View) -> View
foldM step initial v = componentWith folder (\self -> with (upd self) (Fold step initial (`with` v)))
  where
    {-# INLINE upd #-}
    upd :: Ref (Fold eff a) (a,a -> IO ()) -> Handler eff
    upd self = handler
      where
        handler = Handler $ \msg after ->
          modifyrefM self $ \(Fold step _ _) (st,shutdown) -> do
            st' <- step msg st
            pure ((st',shutdown),after)

    {-# INLINE folder #-}
    folder :: Ref (Fold eff a) (a,a -> IO ()) -> Comp (Fold eff a) (a,a -> IO ())
    folder self =
      def
        { construct = askref self >>= \(Fold _ initial _) -> initial >>= \(st,shutdown) -> pure (st,shutdown)
        , onUnmounted = getref self >>= \(st,shutdown) -> shutdown st
        , render = \(Fold _ _ v) (st,_) -> v st
        }

-- | The type of constraint-based entailments. Basically, a constrained Identity.
newtype c |- a = Proof { prove :: c => a }

{-# INLINE proof #-}
-- | Construct a constraint-based entailment.
proof :: (c => a) -> (c |- a)
proof = Proof

{-# INLINE generalize #-}
-- | Generalize an entailment. 
generalize :: (b -> a) -> (Exists a |- x) -> (Exists b |- x)
generalize f ax = proof (with (f it) (prove ax))

instance Functor ((|-) c) where
  fmap :: (a -> b) -> c |- a -> c |- b
  fmap f (Proof p) = Proof (f p) -- proof (f (prove a))

instance Applicative ((|-) c) where
  pure :: a -> c |- a
  pure = Proof -- proof a

  (<*>) :: c |- (a -> b) -> c |- a -> c |- b
  (<*>) (Proof f) (Proof a) = Proof (f a) -- proof ((prove f) (prove a))

  (*>) :: c |- a -> c |- b -> c |- b
  (*>) = const id

  (<*) :: c |- a -> c |- b -> c |- a
  (<*) = const

  liftA2 :: (a -> b -> x) -> (c |- a) -> (c |- b) -> (c |- x)
  liftA2 f (Proof a) (Proof b) = Proof (f a b) -- proof (f (prove a) (prove b))

instance Monad ((|-) c) where
  (>>=) :: (c |- a) -> (a -> (c |- b)) -> (c |- b)
  (>>=) (Proof a) f = Proof (let Proof p = f a in p) -- proof (prove (f (prove a)))

  (>>) :: (c |- a) -> (c |- b) -> (c |- b)
  (>>) = const id

instance Alternative ((|-) c) where
  empty = fail "empty"

  -- Yeah, this is awful and breaks bottom preservation. 
  -- Strongly consider removing.
  (<|>) l r = l `Data.View.or` r 

instance MonadPlus ((|-) c)

instance c => Comonad ((|-) c) where
  duplicate = proof
  extract = prove

instance c => ComonadApply ((|-) c)

instance Semigroup a => Semigroup (c |- a) where
  (<>) (Proof l) (Proof r) = Proof (l <> r)

instance Monoid a => Monoid (c |- a) where
  mempty = Proof mempty

instance IsList (c |- [a]) where

  type Item (c |- [a]) = c |- a

  -- a list of provable values of type 'a' can be turned into a provable list of values of type `a`
  fromList :: [c |- a] -> (c |- [a])
  fromList as = Proof (fmap prove as)

  toList :: (c |- [a]) -> [c |- a]
  toList (Proof as) = error "(|-).toList: cannot distribute entailment proof; the shape of the list might depend on the proof"

instance c => Foldable ((|-) c) where
  foldMap f (Proof p) = f p

instance IsString (c |- String) where
  fromString = Proof

instance Num n => Num (c |- n) where 
  (+) (Proof a) (Proof b) = Proof (a + b)
  (-) (Proof a) (Proof b) = Proof (a - b)
  (*) (Proof n) (Proof m) = Proof (n * m)
  abs (Proof n) = Proof (abs n)
  signum (Proof n) = Proof (signum n)
  fromInteger i = Proof (fromInteger i)

instance Fractional f => Fractional (c |- f) where
  (/) (Proof a) (Proof b) = Proof (a / b)
  fromRational r = Proof (fromRational r)

instance Floating f => Floating (c |- f) where
  pi = Proof pi
  exp (Proof i) = Proof (exp i)
  log (Proof n) = Proof (log n)
  sin (Proof a) = Proof (sin a)
  cos (Proof a) = Proof (cos a)
  asin (Proof a) = Proof (asin a)
  acos (Proof a) = Proof (acos a)
  atan (Proof a) = Proof (atan a)
  sinh (Proof h) = Proof (sinh h)
  cosh (Proof h) = Proof (cosh h)
  asinh (Proof h) = Proof (asinh h)
  acosh (Proof h) = Proof (acosh h)
  atanh (Proof h) = Proof (atanh h)

instance MonadFail ((|-) c) where
  fail = error

instance MonadFix ((|-) c) where
  mfix :: (a -> c |- a) -> c |- a
  mfix f = Proof (let Proof a = f a in a)

instance MonadZip ((|-) c)  where
  mzip :: c |- a -> c |- b -> c |- (a, b)
  mzip (Proof a) (Proof b) = Proof (a,b)

type family (xs :: [*]) |= (a :: *) where
  cs |= a = Collapse (CMap Exists cs) |- a

type family Collapse (cs :: [Constraint]) :: Constraint where
  Collapse '[] = ()
  Collapse '[c] = c
  Collapse (c ': cs) = (c,Collapse cs)

type family CMap (f :: k -> k') (xs :: [k]) where
  CMap f '[] = '[]
  CMap f (k ': ks) = f k ': CMap f ks
 
type es ->> a = es |= a

type Material (xs :: [*]) = xs |= View

{-# INLINE materialize #-}
materialize :: (xs |= View) -> (Collapse (CMap Exists xs) => View)
materialize = prove

-- properties or propositions; your choice
type family Props (a :: k) :: Constraint
type instance Props (a :: Constraint) = a

type Cont' c a = Modify (c |- a)
type Cont c = Cont' c View

type Dynamic' c a = Cont' c a => c |- a
type Dynamic c = Dynamic' c View

-- | Reify a View context.
reify :: forall c a. (Typeable c, Typeable a) => Dynamic' c a -> (State (c |- a) => (c => View)) -> (c => View)
reify = state
{-# INLINE reify #-}

-- | Unify the supplied dynamic View with a matching `reify`d context.
--
-- Note that the need to wrap dynamic Views arises from the fact that unify is
-- often used in a context where those constraints are locally satisfied.
unify :: forall c a. Modify (c |- a) => Dynamic' c a -> IO ()
unify = put
{-# INLINE unify #-}

-- | Call the dynamic value from a matching `reify`d context. 
-- Requires a type application for `c`.
call :: forall c a. Exists (c |- a) => (c => a)
call = prove (it :: c |- a)
{-# INLINE call #-}

-- | Codify the dynamic View from a matching `reify`d context. 
-- Requires a type application for `c`.
codify :: forall c. Exists (c |- View) => (c => View)
codify = weak (it :: c |- View) (call @c)
{-# INLINE codify #-}

-- | Reify a View context and call the initial continuation.
cont :: forall c. Typeable c => Dynamic' c View -> (c => View)
cont d = reify d (codify @c)
{-# INLINE cont #-}

type Surface c = c => View

type Shape c = Modify (c |- View) => c |- View

type Template c = State (c |- View) => Surface c

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
-- Any use of `fill` inside a full surface will force a full re-render rather
-- than a reconciliation. To allow reconciliation, use: 
--
-- > surface @c (call @c)
--
-- to avoid the `weak` within `hole`/`codify`. The `call` approach can be
-- convenient for mimicking state with continuations when it wouldn't make
-- sense to re-render on every `fill`.
--
-- Holes inside the template will be, by necessity, filled with the empty shape.
--
{-# INLINE full #-}
full :: forall c. Typeable c => Template c -> Surface c
full t = surface @c (hole @c) (shape @c (with (Data.View.empty @c) t))

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
-- `weak` in `codify`.
{-# INLINE hole #-}
hole :: forall c. Exists (c |- View) => Surface c
hole = codify @c

-- Fill a hole in a known surface. The current shape in the hole can be
-- accessed with `full` from within the containing surface.
--
{-# INLINE fill #-}
fill :: Modify (c |- View) => Shape c -> IO ()
fill = unify

-- Construct a shape from a surface.
{-# INLINE shape #-}
shape :: Surface c -> Shape c
shape = proof

-- The empty shape. Fits any `c`-shaped hole.
{-# INLINE empty #-}
empty :: Shape c
empty = shape Null

--------------------------------------------------------------------------------
-- An implementation of ltr and rtl composition using view continuations. 

-- | Compose a producer and a reader within a dynamic context. Requires type application.
compose :: forall a c. (Typeable a, Typeable c) => ((c,Effect a) => View) -> ((Exists a,c) => View) -> (c => View)
compose v f = cont (proof @c (stream (\(a :: a) -> unify (proof @c (with a f))) v))
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

type Write a = Modify a

channel :: Typeable a => a -> (Event a => View)-> View
channel = state

write :: Write a => a -> IO ()
write = put

-- read :: Event a => a
-- read = it

newtype Form ctx a = Form (Producer a => Dynamic ctx)

formlet :: forall a ctx. ((ctx, Producer a) => View) -> Form ctx a
formlet v = Form (proof v)

form :: forall ctx a. (Typeable ctx, Producer a, ctx) => Form ctx a -> View
form (Form f) = cont f

instance Functor (Form ctx) where
  fmap f (Form g) = Form (proof (stream (yield . f) (prove g)))

instance Applicative (Form ctx) where
  pure a = formlet (lifecycle def { onStart = yield a } Null)
  liftA2 = liftF2

instance Monad (Form ctx) where
  return = pure
  (Form f) >>= g = Form (proof (stream (\a -> let Form f = g a in unify f) (prove f)))

instance MonadFail (Form ctx) where
  fail str = Form (proof (txt str))

-- liftIO :: (ctx => IO a) -> Form ctx a
-- liftIO io = Form (proof (lifecycle def { onStart = io >>= yield } Null))

data Any2 = Any2 Any Any
liftF2 :: forall ctx a b c. (a -> b -> c) -> Form ctx a -> Form ctx b -> Form ctx c
liftF2 f (Form fa) (Form fb) =
  Form do
    let 
      commitA :: State Any2 => a -> IO ()
      commitA a =
        case it of
          Any2 _ (unsafeCoerce -> Just b) -> do
            yield (f a b)
            put (Any2 (unsafeCoerce (Just a)) (unsafeCoerce (Just b)))
          Any2 _ b -> 
            put (Any2 (unsafeCoerce (Just a)) b)
            
      commitB :: State Any2 => b -> IO ()
      commitB b =
        case it of
          Any2 (unsafeCoerce -> Just a) _ -> do
            yield (f a b)
            put (Any2 (unsafeCoerce (Just a)) (unsafeCoerce (Just b)))
          Any2 a _ -> 
            put (Any2 a (unsafeCoerce (Just b)))

    proof do
      state (Any2 (unsafeCoerce Nothing) (unsafeCoerce Nothing)) do
        SimpleHTML "span" <||>
          [ stream commitA (prove fa)
          , stream commitB (prove fb)                
          ]
