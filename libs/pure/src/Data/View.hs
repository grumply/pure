{-# language MagicHash, CPP, ScopedTypeVariables, PatternSynonyms, PolyKinds, DefaultSignatures, ViewPatterns, RecordWildCards, GADTs, FlexibleInstances, AllowAmbiguousTypes, OverloadedStrings, TypeApplications, BangPatterns, RankNTypes, FlexibleContexts, ConstraintKinds, BlockArguments, MultiWayIf, LambdaCase, DuplicateRecordFields, TypeOperators, DerivingVia, DataKinds, NamedFieldPuns, TypeFamilies, DeriveFunctor, UndecidableInstances, InstanceSigs, RoleAnnotations, ConstrainedClassMethods  #-}
module Data.View (module Data.View, Typeable()) where

import Data.Functor.Identity (Identity(..))
import Control.Applicative (Const(..))

import Control.Arrow ((&&&))
import Control.Applicative (Applicative(..),Alternative(..))
import Control.Comonad (Comonad(..),ComonadApply(..))
import Control.Concurrent (ThreadId,forkIO,killThread,myThreadId,MVar,newMVar,newEmptyMVar,readMVar,putMVar,takeMVar)
import Control.Exception (mask,onException,evaluate,Exception,catch,throw,SomeException)
import Control.Monad (void,join,forever,unless,when,MonadPlus(..))
import Control.Monad.Fix (MonadFix(..))
import Control.Monad.Zip (MonadZip(..))
import Control.Monad.ST (ST)
import Data.Coerce (Coercible(),coerce)
import Data.Foldable (for_)
import Data.Functor.Contravariant (Contravariant(..))
import Data.IORef (IORef,newIORef,atomicModifyIORef',readIORef)
import Data.List as List (null)
import Data.Maybe (listToMaybe,fromJust)
import Data.Monoid (Monoid(..),(<>))
import Data.Proxy (Proxy(..))
import Data.STRef (STRef)
import Data.String (IsString(..))
import Data.Try as Try (Try,try)
import Data.Time (Time,time,delay,timeout)
import Data.Traversable (for)
import Data.Type.Equality
import Data.Typeable (TypeRep,Typeable,tyConName,typeRepTyCon,typeOf,typeRep,typeRepFingerprint,cast)
import Data.Unique (Unique,newUnique)
import GHC.Exts as Exts (IsList(..),Any,Constraint,reallyUnsafePtrEquality#,isTrue#,unsafeCoerce#,Proxy#,proxy#)
import GHC.Fingerprint.Type (Fingerprint())
import GHC.Generics (Generic(..))
import GHC.Stack
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Data.Default (Default(..))
import Data.DOM (Evt,Element,Node(..),IsNode(..),Text,Options,nullJSV)
import Data.JSON (ToJSON,FromJSON)
import Data.Txt (FromTxt(..),ToTxt(..),Txt)
import Data.Queue (Queue,arrive)

import Data.Set (Set)
import Data.Set as Set (empty,fromList,null,empty,union,toList,insert)
import Data.Map.Lazy as Map (Map,fromList,null,empty,union,toList,insert,singleton)

data Target = ElementTarget | WindowTarget | DocumentTarget deriving Eq

newtype ListenerAction = ListenerAction (Evt -> IO ())

data Listener =
  On
    { eventName     :: Txt
    , eventTarget   :: Target
    , eventOptions  :: Options
    , eventAction   :: ListenerAction
    , eventUpdate   :: ListenerAction -> IO ()
    , eventStopper  :: IO ()
    }

data Lifecycle 
  = Created (Node -> IO (IO ())) (IO ()) 
  | Mounted (Node -> IO (IO ())) (IO ())

data Comp props state = Comp
  { deferred       :: Bool
  , onInitialize   :: state -> IO state
  , onInitialized  :: IO ()
  , onConstruct    :: IO state
  , onMount        :: state -> IO state
  , onExecuting    :: state -> IO state
  , onMounted      :: IO ()
  , onReceive      :: props -> state -> IO state
  , onForce        :: props -> state -> IO Bool
  , onUpdate       :: props -> state -> IO ()
  , onUpdated      :: props -> state -> IO ()
  , onUnmounted    :: IO ()
  , render         :: props -> state -> View
  }

instance Default (Comp props state) where
  {-# INLINE def #-}
  def = Comp
    { deferred      = False
    , onConstruct   = return (error "Comp.construct: no initial state supplied.")
    , onInitialize  = return
    , onInitialized = return ()
    , onMount       = return
    , onExecuting   = return
    , onMounted     = return ()
    , onReceive     = \_ -> return
    , onForce       = \_ _ -> return True
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
    { reactiveVal :: !a
    , reactiveView :: View
    } -> View

  WeakView ::
    { weakVal :: !a
    , weakView :: View
    } -> View

  PortalView ::
    { portalProxy :: Maybe Element
    , portalDestination :: Element
    , portalView :: View
    } -> View

  ComponentView ::
    { __rep  :: !Fingerprint
    , record :: Maybe (Ref props state)
    , comp   :: Ref props state -> Comp props state
    , props  :: Ref props state -> props
    } -> View

  TaggedView ::
    { __tag :: !Fingerprint
    , taggedView :: View 
    } -> View

  Prebuilt :: 
    { prebuilt :: View 
    } -> View

instance Default View where
  {-# INLINE def #-}
  def = NullView Nothing

instance IsString View where
  {-# INLINE fromString #-}
  fromString = TextView Nothing . toTxt

instance FromTxt View where
  {-# INLINE fromTxt #-}
  fromTxt = TextView Nothing

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

-- Reconciler hints

{-# INLINE static #-}
static :: View -> View
static = ReactiveView ()

-- Semantically, `reactive a . reactive b /= reactive (a,b)`.
{-# INLINE reactive #-}
reactive :: a -> View -> View
reactive = ReactiveView

-- The strictness here can be useful for primitives, but keep in mind that
-- optimization level can affect the behavior.
--
-- There are some neat tricks that can be had with `reactive'`, like:
--
-- > reactive' (value > 1) do
-- >   _
--
-- If `value` is strictly increasing from an initial value less than 1, this
-- `lazy'` call will update exactly once, when the value first exceeds 1.
--
{-# INLINE reactive' #-}
reactive' :: a -> View -> View
reactive' !a = ReactiveView a

-- If the given value is not the exact same object (reallyUnsafePtrEquality),
-- replace the View. You probably don't need this; you likely just need stronger
-- typing!
-- Semantically, `weak a . weak b == weak (a,b)`.
{-# INLINE weak #-}
weak :: a -> View -> View
weak = WeakView

-- If the given value is not the exact same object after forcing to WHNF 
-- and comparing with reallyUnsafePtrEquality, replace the View. You probably
-- don't need this; you likely just need stronger typing! The strictness here
-- can be useful for primitives, but keep in mind that optimization level can 
-- affect the behavior.
{-# INLINE weak' #-}
weak' :: a -> View -> View
weak' !a = weak a

{-# INLINE tagged #-}
tagged :: forall t. Typeable t => View -> View
tagged = TaggedView (typeRepFingerprint (typeRep (Proxy :: Proxy t)))

pattern Tag :: forall t. Typeable t => View -> View
pattern Tag v <- TaggedView ((==) (typeRepFingerprint (typeRep (Proxy :: Proxy t))) -> True) v where
  Tag t = tagged @t t

-- Txt

{-# INLINE txt #-}
txt :: ToTxt a => a -> View
txt a = reactive a (TextView Nothing (toTxt a))

pattern Txt :: Txt -> View
pattern Txt t <- (TextView _ t) where
  Txt t = TextView Nothing t

{-# INLINE withType #-}
withType :: Typeable a => Proxy a -> Fingerprint -> b -> b -> b
withType p fp bad good
  | typeRepFingerprint (typeRep p) == fp = good
  | otherwise                            = bad

viewComponent :: forall props state. (Typeable props, Typeable state) => View -> Maybe (Maybe (Ref props state),Ref props state -> Comp props state,Ref props state -> props)
viewComponent (ComponentView rep f0 c0 ps0) =
  withType (Proxy :: Proxy (props,state)) rep Nothing do
    Just (unsafeCoerce f0,unsafeCoerce c0,unsafeCoerce ps0)
viewComponent _ = Nothing

applyComponent :: Maybe (Maybe (Ref props state),Ref props state -> Comp props state,Ref props state -> props) -> Maybe (Ref props state -> Comp props state,props) 
applyComponent (Just (Just r,c,p)) = Just (c,p r)
applyComponent _ = Nothing

pattern ComponentWith :: forall props state. (Typeable props, Typeable state) => (Ref props state -> Comp props state) -> (Ref props state -> props) -> View
pattern ComponentWith v p <- (viewComponent -> Just (_,v,p)) where
  ComponentWith v p = ComponentView (typeRepFingerprint (typeRep (Proxy :: Proxy (props,state)))) Nothing v p

pattern Component :: forall props state. (Typeable props, Typeable state) => (Ref props state -> Comp props state) -> props -> View
pattern Component v p <- (applyComponent . viewComponent -> Just (v,p)) where
  Component v p = ComponentWith v (const p)

{-# INLINE componentWith #-}
componentWith :: forall props state. (Typeable props, Typeable state) => (Ref props state -> Comp props state) -> (Ref props state -> props) -> View
componentWith = ComponentView (typeRepFingerprint (typeRep (Proxy :: Proxy (props,state)))) Nothing

{-# INLINE component #-}
component :: forall props state. (Typeable props, Typeable state) => (Ref props state -> Comp props state) -> props -> View
component v p = ComponentView (typeRepFingerprint (typeRep (Proxy :: Proxy (props,state)))) Nothing v (const p)

pattern Null :: View
pattern Null <- (NullView _) where
  Null = NullView Nothing

{-# INLINE viewHTMLTag #-}
viewHTMLTag :: View -> Maybe Txt
viewHTMLTag (HTMLView _ tag _ _) = Just tag
viewHTMLTag (KHTMLView _ tag _ _) = Just tag
viewHTMLTag _ = Nothing

{-# INLINE [1] html #-}
html :: Txt -> View
html tag = HTMLView Nothing tag (Features_ mempty mempty mempty mempty mempty mempty) mempty

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
getFeatures PortalView{..} = getFeatures portalView
getFeatures TaggedView{..} = getFeatures taggedView
getFeatures Prebuilt {..}  = getFeatures prebuilt
getFeatures v = features v

{-# INLINE [1] setFeatures #-}
setFeatures :: Features -> View -> View
setFeatures _ v@NullView {} = v
setFeatures _ v@TextView {} = v
setFeatures _ v@ComponentView {} = v
setFeatures fs PortalView{..} = PortalView { portalView = setFeatures fs portalView, .. }
setFeatures fs TaggedView{..} = TaggedView { taggedView = setFeatures fs taggedView, .. }
setFeatures fs Prebuilt {..}  = Prebuilt { prebuilt = setFeatures fs prebuilt }
setFeatures fs v = v { features = fs }

{-# RULES 
  "features: set . set" forall fs fs' x. setFeatures fs (setFeatures fs' x) = setFeatures fs x;
  "features: get . set" forall fs x. getFeatures (setFeatures fs x) = fs;
  "features: add . add" forall fs gs x. addFeatures fs (addFeatures gs x)  = addFeatures (gs <> fs) x;
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

{-# INLINE [1] getXLinks #-}
getXLinks :: View -> [(Txt,Txt)]
getXLinks SVGView {..} = Map.toList xlinks
getXLinks KSVGView {..} = Map.toList xlinks
getXLinks PortalView {..} = getXLinks portalView
getXLinks TaggedView {..} = getXLinks taggedView
getXLinks Prebuilt {..} = getXLinks prebuilt
getXLinks _ = []

{-# INLINE [1] setXLinks #-}
setXLinks :: [(Txt,Txt)] -> View -> View
setXLinks xl khtml@SVGView {} = khtml { xlinks = Map.fromList xl }
setXLinks xl ksvg@KSVGView {} = ksvg { xlinks = Map.fromList xl }
setXLinks xl PortalView {..} = PortalView { portalView = setXLinks xl portalView, .. }
setXLinks xl TaggedView {..} = TaggedView { taggedView = setXLinks xl taggedView, .. }
setXLinks xl Prebuilt {..} = Prebuilt { prebuilt = setXLinks xl prebuilt }
setXLinks _ v = v

{-# INLINE [1] addXLinks #-}
addXLinks :: [(Txt,Txt)] -> View -> View
addXLinks xl v@SVGView {} = v { xlinks = Map.union (Map.fromList xl) (xlinks v) }
addXLinks xl v@KSVGView {} = v { xlinks = Map.union (Map.fromList xl) (xlinks v) }
addXLinks xl PortalView {..} = PortalView { portalView = addXLinks xl portalView, .. }
addXLinks xl TaggedView {..} = TaggedView { taggedView = addXLinks xl taggedView, .. }
addXLinks xl Prebuilt {..} = Prebuilt { prebuilt = addXLinks xl prebuilt }
addXLinks _ v = v

pattern XLink :: Txt -> Txt -> View -> View
pattern XLink k v a <- (const (error "The XLink pattern does not support matching, only construction. For pattern matching on xlinks, use the XLinks pattern with Data.List.lookup.","") &&& id -> ((k,v),a)) where
  XLink k v a =
    let xls = getXLinks a
    in setXLinks ((k,v):xls) a

pattern SetXLinks :: [(Txt,Txt)] -> View -> View
pattern SetXLinks xl v <- (getXLinks &&& id -> (xl,v)) where
  SetXLinks xl v = setXLinks xl v

pattern XLinks :: [(Txt,Txt)] -> View -> View
pattern XLinks xl v <- (getXLinks &&& id -> (xl,v)) where
  XLinks xl v = addXLinks xl v

{-# INLINE [1] getChildren #-}
getChildren :: View -> [View]
getChildren v@HTMLView {} = children v
getChildren v@SVGView {} = children v
getChildren PortalView {..} = getChildren portalView
getChildren TaggedView {..} = getChildren taggedView
getChildren Prebuilt {..} = getChildren prebuilt
getChildren _  = []

{-# INLINE [1] setChildren #-}
setChildren :: [View] -> View -> View
setChildren cs v@HTMLView {} = v { children = cs }
setChildren cs v@SVGView {} = v { children = cs }
setChildren cs PortalView {..} = PortalView { portalView = setChildren cs portalView, .. }
setChildren cs TaggedView {..} = TaggedView { taggedView = setChildren cs taggedView, .. }
setChildren cs Prebuilt {..} = Prebuilt { prebuilt = setChildren cs prebuilt }
setChildren _ v = v

{-# INLINE [1] addChildren #-}
addChildren :: [View] -> View -> View
addChildren cs v@HTMLView {} = v { children = children v ++ cs }
addChildren cs v@SVGView {} = v { children = children v ++ cs }
addChildren cs PortalView {..} = PortalView { portalView = addChildren cs portalView, .. }
addChildren cs TaggedView {..} = TaggedView { taggedView = addChildren cs taggedView, .. }
addChildren cs Prebuilt {..} = Prebuilt { prebuilt = addChildren cs prebuilt }
addChildren _ v = v

{-# RULES 
  "children: set . set" forall fs fs' x. setChildren fs (setChildren fs' x) = setChildren fs x;
  "children: get . set" forall fs x. getChildren (setChildren fs x) = fs;
  "children: add . add" forall fs gs x. addChildren fs (addChildren gs x) = addChildren (gs <> fs) x;
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
getKeyedChildren _ = []

{-# INLINE [1] setKeyedChildren #-}
setKeyedChildren :: [(Int,View)] -> View -> View
setKeyedChildren cs v@KHTMLView {} = v { keyedChildren = cs }
setKeyedChildren cs v@KSVGView {} = v { keyedChildren = cs }
setKeyedChildren cs PortalView {..} = PortalView { portalView = setKeyedChildren cs portalView, .. }
setKeyedChildren cs TaggedView {..} = TaggedView { taggedView = setKeyedChildren cs taggedView, .. }
setKeyedChildren cs Prebuilt {..} = Prebuilt { prebuilt = setKeyedChildren cs prebuilt, .. }
setKeyedChildren _ v = v

{-# INLINE [1] addKeyedChildren #-}
addKeyedChildren :: [(Int,View)] -> View -> View
addKeyedChildren cs v@KHTMLView {} = v { keyedChildren = keyedChildren v ++ cs }
addKeyedChildren cs v@KSVGView {} = v { keyedChildren = keyedChildren v ++ cs }
addKeyedChildren cs PortalView {..} = PortalView { portalView = addKeyedChildren cs portalView, .. }
addKeyedChildren cs TaggedView {..} = TaggedView { taggedView = addKeyedChildren cs taggedView, .. }
addKeyedChildren cs Prebuilt {..} = Prebuilt { prebuilt = addKeyedChildren cs prebuilt }
addKeyedChildren _ v = v

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

within :: Time -> a -> a
within t a = unsafePerformIO (fromJust <$> timeout t (evaluate a))

caught :: Exception e => a -> (e -> a) -> a
caught a f = unsafePerformIO (Control.Exception.catch (evaluate a) (pure . f))

or :: a -> a -> a
or l r = Data.View.caught @SomeException l (const r)

class Exists a where
  it :: a

{-
data Witness a r = Witness (Exists a => Proxy a -> r)

-- This configuration is not preferable, but it avoids a constraint satisfaction 
-- propagation issue. Using the `unsafeCoerce` approach for `with` along with
-- `GHC.Exts.inline` was able to completely remove dictionaries from the generated 
-- core, but calls to `with` were (sometimes) allowed to satisfy non-local 
-- `Exists` constraints! 
--
-- See: https://gitlab.haskell.org/ghc/ghc/-/issues/21575
--
{-# NOINLINE withWitness #-}
withWitness :: forall a r. (Exists a => Proxy a -> r) -> a -> Proxy a -> r
withWitness w a p = magicDict (Witness w) a p

{-# INLINE with' #-}
with' :: forall a r. a -> (Exists a => r) -> r
with' a w = withWitness (\_ -> w) a Proxy
-}

newtype Witness a r = Witness (Exists a => r)

-- This approach is likely to produce bugs, but the performance improvement over
-- the magicDict version can be massive. Keep `with` in mind if anything
-- inexplicable happens.
{-# INLINABLE with #-}
with :: forall a r. a -> (Exists a => r) -> r
with a w = unsafeCoerce (Witness w :: Witness a r) a

{-# INLINE using #-}
-- | Like `contramap` for existentials; existential refinement. 
--
-- NOTE: Be careful that `Exists a` is not resolvable by the calling context.
--       Homomorphisms are disallowed as a special case because it is guaranteed
--       that the existential constraint would be incorrectly resolved - the 
--       transformation would be ignored.
--
using :: (b == a) ~ False => (b -> a) -> (Exists a => r) -> (Exists b => r)
using f = with (f it)

{-# INLINE may #-}
may :: forall a b. Exists (Maybe a) => b -> (Exists a => b) -> b
may nothing just = maybe nothing (`with` just) (it :: Maybe a)

{-# INLINE try #-}
try :: forall a b. Exists (Try.Try a) => b -> b -> (Exists a => b) -> b
try trying failed done = Try.try trying failed (`with` done) (it :: Try.Try a)

{-# INLINE unite #-}
unite :: forall a b c. Exists (Either a b) => (Exists a => c) -> (Exists b => c) -> c
unite left right = either (`with` left) (`with` right) (it :: Either a b)

{-# INLINE each #-}
each :: forall f a b. (Functor f, Exists (f a)) => (Exists a => b) -> f b 
each b = fmap (`with` b) (it :: f a)

data Handler eff = Handler { runHandler :: eff -> IO () -> IO Bool }

type Effect eff = Exists (Handler eff)

{-# INLINE effect' #-}
effect' :: Effect eff => eff -> IO () -> IO Bool
effect' = runHandler it

{-# INLINE effect #-}
effect :: Effect eff => eff -> IO ()
effect eff = void (effect' eff (pure ()))

{-# INLINE [1] reinterpret #-}
-- | The covariant `reinterpret` allows you to adapt producers to be more specific or varied.
-- 
-- Related: see `refine`.
reinterpret :: forall msg msg' a. (msg -> msg') -> (Effect msg => a) -> (Effect msg' => a)
reinterpret f = with (Handler (effect' . f)) 

-- {-# INLINE also #-}
-- also :: forall msg a. (msg -> IO ()) -> (Effect msg => a) -> (Effect msg => a)
-- also f = with (Handler (\m io -> effect' m (io >> f m)))

{-# INLINE (#) #-}
infixr 9 #
(#) :: forall a b x. (a -> b) -> (Effect a => x) -> (Effect b => x)
(#) = Data.View.reinterpret

{-# RULES
  "Data.Effect.reinterpret id" forall x. Data.View.reinterpret id x = x
  #-}

type Producer a = Effect a

{-# INLINE yield #-}
yield :: Producer a => a -> IO ()
yield = effect

{-# INLINE stream #-}
stream :: (a -> IO ()) -> (Producer a => b) -> b
stream f = with (Handler (\a after -> f a >> after >> pure True))

{-# INLINE events #-}
events :: forall a b. (Exists a => IO ()) -> (Producer a => b) -> b
events f = stream @a (`with` f)

{-# INLINE discard #-}
discard :: forall a b. (Producer a => b) -> b
discard = stream @a (const def)

data Fork

-- Fork an asynchronous rendering context. 
--
-- Consider:
-- 
-- > lazy action do
-- >   fork do
-- >     { ... await ... }
--
-- Without fork, await is render-blocking. With fork, the await is performed
-- within a new thread and, therefore, only blocks the fork.
--
{-# INLINE fork #-}
fork :: View -> View
fork = component @View @Fork (const def { deferred = True, render = const })

data Asynchronous a = Asynchronous (IO a) (Material '[a])

data Async a = Async 
  { action :: IO a
  , view_  :: Material '[a]
  , thread :: ThreadId 
  , result :: MVar a
  }

await :: Exists a => a
await = it

parv :: Typeable a => a -> (Exists a => View) -> View
parv a = lazy (evaluate a)

{-# INLINE lazy #-}
-- Note that only the asynchrony of the first action can be witnessed in View
lazy :: forall a. Typeable a => IO a -> (Exists a => View) -> View
lazy io v = go (Asynchronous io (proof v))
  where
    go = component \self -> def
      { deferred = True

      , onConstruct = do
          Asynchronous action view_ <- askref self
          result <- newEmptyMVar
          thread <- forkIO (action >>= putMVar result)
          pure Async {..}

      , onForce = \(Asynchronous new_action new_view) old@Async {..} -> do
          let
            sameAction = isTrue# (reallyUnsafePtrEquality# new_action action) 
            sameView   = isTrue# (reallyUnsafePtrEquality# new_view view_)
          pure (not sameAction || not sameView)

      , onReceive = \(Asynchronous new_action new_view) old@Async {..} -> do
          let
            sameAction = isTrue# (reallyUnsafePtrEquality# new_action action) 
            sameView   = isTrue# (reallyUnsafePtrEquality# new_view view_)
          case (sameAction,sameView) of
            (False,_) -> do
              let action = new_action
              let view_ = new_view
              thread <- forkIO do
                Control.Exception.mask $ \unmask -> do
                  a <- action 
                  unmask do
                    tid <- myThreadId
                    modifyrefM self $ \_ Async {..} ->
                      if tid == thread then do
                        result <- newMVar a
                        pure (Async {..},def)
                      else
                        pure (Async {..},def)
                    pure ()
              pure Async {..}

            (_,False) -> do
              pure (old :: Async a) { view_ = new_view }

            _ -> do
              pure old

      , render = \_ Async { view_, result } -> 
          with (unsafePerformIO (readMVar result)) (prove view_)

      }

data Synchronous a = Synchronous (IO a) (Material '[a])
data Sync a = Sync 
  { action :: IO a
  , result :: a
  , view_  :: Material '[a]
  }

seqv :: Typeable a => a -> (Exists a => View) -> View
seqv a = eager (evaluate a)

{-# INLINE eager #-}
eager :: forall a. Typeable a => IO a -> (Exists a => View) -> View
eager io v = go (Synchronous io (proof v))
  where
    go = component \self -> def
      { onConstruct = do
        Synchronous action view_ <- askref self
        result <- action
        pure Sync {..}

      , onForce = \(Synchronous new_action new_view) Sync {..} -> do
          let
            sameAction = isTrue# (reallyUnsafePtrEquality# new_action action) 
            sameView   = isTrue# (reallyUnsafePtrEquality# new_view view_)
          pure (not sameAction || not sameView)

      , onReceive = \(Synchronous new_action new_view) old@Sync {..} -> do
          let
            sameAction = isTrue# (reallyUnsafePtrEquality# new_action action) 
            sameView   = isTrue# (reallyUnsafePtrEquality# new_view view_)
          case (sameAction,sameView) of
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
race vs v = lazy run v
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

{-# INLINE [1] lifecycle #-}
lifecycle :: Lifecycles -> View -> View
lifecycle ls v = component go (ls,v)
  where
    go self = def
      { onConstruct = askref self >>= onStart . fst
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
      { onConstruct = join (askref self)
      , onUpdate    = const
      }

watching' :: (Typeable a, Typeable b) => a -> IO (b,IO ()) -> (Exists b => View) -> View
watching' a create v = component go (a,create,\b -> with b v)
  where
    go self = def
      { onConstruct = askref self >>= \(_,create,_) -> create
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

data Polling a = Polling Time (IO a) (Material '[a])

data Poll a = Poll
  { interval :: Time
  , begin :: Time
  , action :: IO a 
  , view_  :: Material '[a]
  , thread :: ThreadId
  , result :: MVar a
  }

{-# INLINE every #-}
every :: forall a. Typeable a => Time -> IO a -> (Exists a => View) -> View
every = poll

{-# INLINE poll #-}
-- Note that only the asynchrony of the first action can be witnessed in View
poll :: forall a. Typeable a => Time -> IO a -> (Exists a => View) -> View
poll t f v = go (Polling t f (proof v))
  where
    go = component \self -> def
      { deferred = True

      , onConstruct = do
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

      , onForce = \(Polling new_interval new_action new_view) old@Poll {..} -> do
          let
            sameInterval = new_interval == interval
            sameAction   = isTrue# (reallyUnsafePtrEquality# new_action action)
            sameView     = isTrue# (reallyUnsafePtrEquality# new_view view_)
          pure (not sameInterval || not sameAction || not sameView)

      , onReceive = \(Polling new_interval new_action new_view) old@Poll {..} -> do
          let
            sameInterval = new_interval == interval
            sameAction   = isTrue# (reallyUnsafePtrEquality# new_action action) 
            sameView     = isTrue# (reallyUnsafePtrEquality# new_view view_)
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
  forkIO (evaluate a >>= putMVar mv)
  delay t
  takeMVar mv

type Reader a = Exists a

type family Readers (xs :: [*]) :: Constraint where
  Readers (x ': xs) = (Reader x,Readers xs)
  Readers '[] = ()

{-# INLINE ask #-}
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
state a = foldM ($!) (pure (a,\_ -> pure ()))

{-# INLINE stateIO #-}
stateIO :: Typeable a => (Modify a => IO a) -> (State a => View) -> View
stateIO ioa = foldM ($!) (ioa >>= \a -> pure (a,\_ -> pure ()))

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
read :: Event a => a
read = it

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
        { onConstruct = askref self >>= \(Fold _ initial _) -> initial >>= \(st,shutdown) -> pure (st,shutdown)
        , onUnmounted = getref self >>= \(st,shutdown) -> shutdown st
        , render = \(Fold _ _ v) (st,_) -> v st
        }

-- This seems to be an unfortunate necessity in some very particular circumstances,
-- so it's been made as convenient to use and understand as possible. 
--
-- There are a decent set of instances, as well: 
--
--   Unconstrained: Functor, Applicative, Monad, MonadFail, MonadFix, MonadZip, IsString, IsList, Alternative/MonadPlus (error)
--   Constrained/Lifted: Semigroup/Monoid, Num, Fractional, Floating
--
newtype c |- a = Proof { prove :: c => a }

{-# INLINE proof #-}
proof :: (c => a) -> (c |- a)
proof = Proof

{-# INLINE refine #-}
-- | The contravariant `refine` allows you to adapt consumers to be more general.
-- 
-- NOTE: Do not use `refine` with an inlined `proof` when `a == b`
--
-- Related: see `reinterpret`.
--
refine :: (b -> a) -> (Exists a |- x) -> (Exists b |- x)
refine f ax = proof (with (f it) (prove ax))

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

liftIO :: (ctx => IO a) -> Form ctx a
liftIO io = Form (proof (lifecycle def { onStart = io >>= yield } Null))

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
