{-# language MagicHash, CPP, ScopedTypeVariables, PatternSynonyms, PolyKinds, DefaultSignatures, ViewPatterns, RecordWildCards, GADTs, FlexibleInstances, AllowAmbiguousTypes, OverloadedStrings, TypeApplications, BangPatterns, RankNTypes, FlexibleContexts, ConstraintKinds, BlockArguments, MultiWayIf, LambdaCase, DuplicateRecordFields, TypeOperators, DerivingVia, DataKinds, NamedFieldPuns, TypeFamilies, DeriveFunctor #-}
module Data.View (module Data.View, Typeable()) where

import Control.Arrow ((&&&))
import Control.Applicative (Applicative(..))
import Control.Concurrent (ThreadId,forkIO,killThread,myThreadId,MVar,newMVar,newEmptyMVar,readMVar,putMVar,takeMVar)
import Control.Exception (mask,onException,evaluate,Exception,catch,throw,SomeException)
import Control.Monad (void,join,forever,unless,when)
import Control.Monad.ST (ST)
import Data.Coerce (Coercible(),coerce)
import Data.Foldable (for_)
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
import Data.Typeable (Typeable,tyConName,typeRepTyCon,typeOf,typeRep,typeRepFingerprint,cast)
import Data.Unique (Unique,newUnique)
import GHC.Exts as Exts (IsList(..),Any,Constraint,reallyUnsafePtrEquality#,isTrue#,unsafeCoerce#)
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
import Data.Map.Lazy (Map)
import Data.Map.Lazy as Map (fromList,null,empty,union,toList,insert)

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
    , styles     :: Map Txt Txt
    , attributes :: Map Txt Txt
    , properties :: Map Txt Txt
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

newtype TypeWitness (a :: k) = TypeWitness { unCompWitness :: Fingerprint }

witness :: forall a. Typeable a => TypeWitness a
witness = TypeWitness (typeRepFingerprint $ typeRep (Proxy :: Proxy a))

proxyWitness :: forall a. Typeable a => Proxy a -> TypeWitness a
proxyWitness p = TypeWitness (typeRepFingerprint (typeRep p)) 

sameTypeWitness :: TypeWitness a -> TypeWitness b -> Bool
sameTypeWitness (TypeWitness fp1) (TypeWitness fp2) =
  case reallyUnsafePtrEquality# fp1 fp2 of
    1# -> True
    _  -> fp1 == fp2

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
    , xlinks      :: Map Txt Txt
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
    , xlinks        :: Map Txt Txt
    , keyedChildren :: [(Int,View)]
    } -> View

  ReactiveView ::
    { reactiveVal :: a
    , reactiveView :: View
    } -> View

  WeakView ::
    { weakVal :: a
    , weakView :: View
    } -> View

  PortalView ::
    { portalProxy :: Maybe Element
    , portalDestination :: Element
    , portalView :: View
    } -> View

  ComponentView ::
    { __comp_witness :: TypeWitness (props,state)
    , record :: Maybe (Ref props state)
    , comp   :: Ref props state -> Comp props state
    , props  :: Ref props state -> props
    } -> View

  TaggedView :: forall tag.
    { __tag :: TypeWitness tag 
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

pattern EmptyMap <- (Map.null -> True) where
  EmptyMap = Map.empty

pattern EmptySet <- (Set.null -> True) where
  EmptySet = Set.empty

-- OverloadedLists makes this necessary
pattern EmptyList :: [a]
pattern EmptyList <- (List.null -> True) where
  EmptyList = []

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
tagged = TaggedView (proxyWitness (Proxy :: Proxy t))

pattern Tag :: forall t. Typeable t => View -> View
pattern Tag v <- TaggedView (sameTypeWitness (proxyWitness (Proxy :: Proxy t)) -> True) v where
  Tag t = tagged @t t

-- Txt

{-# INLINE txt #-}
txt :: ToTxt a => a -> View
txt a = reactive a (TextView Nothing (toTxt a))

pattern Txt :: Txt -> View
pattern Txt t <- (TextView _ t) where
  Txt t = (TextView Nothing t)

pattern Component :: forall props state. (Typeable props, Typeable state) => (Ref props state -> Comp props state) -> props -> View
pattern Component v p <- ComponentView ((sameTypeWitness (witness :: TypeWitness (props,state))) -> True) (unsafeCoerce -> p) _ (unsafeCoerce -> v) where
  Component v p = (ComponentView witness Nothing v (const p))

pattern Null :: View
pattern Null <- (NullView _) where
  Null = (NullView Nothing)

{-# INLINE viewHTMLTag #-}
viewHTMLTag :: View -> Maybe Txt
viewHTMLTag (HTMLView _ tag _ _) = Just tag
viewHTMLTag (KHTMLView _ tag _ _) = Just tag
viewHTMLTag _ = Nothing

pattern SimpleHTML :: Txt -> View
pattern SimpleHTML tag <- (viewHTMLTag -> Just tag) where
  SimpleHTML tag = (HTMLView Nothing tag (Features_ EmptySet EmptyMap EmptyMap EmptyMap EmptyList EmptyList) EmptyList)

{-# INLINE viewSVGTag #-}
viewSVGTag :: View -> Maybe Txt
viewSVGTag (SVGView _ tag _ _ _) = Just tag
viewSVGTag (KSVGView _ tag _ _ _) = Just tag
viewSVGTag _ = Nothing

pattern SimpleSVG :: Txt -> View
pattern SimpleSVG tag <- (viewSVGTag -> Just tag) where
  SimpleSVG tag = (SVGView Nothing tag (Features_ EmptySet EmptyMap EmptyMap EmptyMap EmptyList EmptyList) EmptyMap EmptyList)

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
pattern Raw v r <- ((id &&& id) -> (RawView _ _ _ r,v)) where
  Raw v r = (setContent r (toRaw v))

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

class HasFeatures a where
  getFeatures :: a -> Features
  setFeatures :: Features -> a -> a
  {-# INLINE addFeatures #-}
  addFeatures :: Features -> a -> a
  addFeatures fs a = setFeatures (getFeatures a <> fs) a

instance HasFeatures View where
  {-# INLINE getFeatures #-}
  getFeatures NullView {} = mempty
  getFeatures TextView {} = mempty
  getFeatures ComponentView {} = mempty
  getFeatures PortalView{..} = getFeatures portalView
  getFeatures TaggedView{..} = getFeatures taggedView
  getFeatures Prebuilt {..}  = getFeatures prebuilt
  getFeatures v = features v
  {-# INLINE setFeatures #-}
  setFeatures _ v@NullView {} = v
  setFeatures _ v@TextView {} = v
  setFeatures _ v@ComponentView {} = v
  setFeatures fs PortalView{..} = PortalView { portalView = setFeatures fs portalView, .. }
  setFeatures fs TaggedView{..} = TaggedView { taggedView = setFeatures fs taggedView, .. }
  setFeatures fs Prebuilt {..}  = Prebuilt { prebuilt = setFeatures fs prebuilt }
  setFeatures fs v = v { features = fs }

instance HasFeatures Features where
  {-# INLINE getFeatures #-}
  getFeatures = id
  {-# INLINE setFeatures #-}
  setFeatures = const
  {-# INLINE addFeatures #-}
  addFeatures = (<>)

pattern SetFeatures :: HasFeatures a => Features -> a -> a
pattern SetFeatures fs a <- ((getFeatures &&& id) -> (fs,a)) where
  SetFeatures fs a = (setFeatures fs a)

pattern Features :: HasFeatures a => Features -> a -> a
pattern Features fs a <- ((getFeatures &&& id) -> (fs,a)) where
  Features fs a = (addFeatures fs a)

pattern Class :: (HasCallStack, HasFeatures a) => Txt -> a -> a
pattern Class c a <- ((const (error "The Class pattern does not support matching, only construction. For pattern matching, use a combination of the Classes pattern with Data.List.elem.") &&& id) -> (c,a)) where
  Class c a =
    let fs = getFeatures a
        cs = Set.insert c (classes fs)
    in (setFeatures fs { classes = cs } a)

pattern SetClasses :: HasFeatures a => [Txt] -> a -> a
pattern SetClasses cs a <- (((Set.toList . classes . getFeatures) &&& id) -> (cs,a)) where
  SetClasses cs a =
    let cs' = foldr Set.insert Set.empty cs
        fs  = getFeatures a
    in (setFeatures fs { classes = cs' } a)

pattern Classes :: HasFeatures a => [Txt] -> a -> a
pattern Classes cs a <- SetClasses cs a where
  Classes cs a =
    let fs = getFeatures a
    in (setFeatures (fs { classes = Set.union (Set.fromList cs) (classes fs) }) a)

pattern Style :: (HasCallStack, HasFeatures a) => Txt -> Txt -> a -> a
pattern Style k v a <- ((const (error "The Style pattern does not support matching, only construction. For pattern matching, use a combination of the Styles pattern with Data.List.lookup.","") &&& id) -> ((k,v),a)) where
  Style k v a =
    let fs = getFeatures a
    in (setFeatures (fs { styles = Map.insert k v (styles fs) }) a)

pattern SetStyles :: HasFeatures a => [(Txt,Txt)] -> a -> a
pattern SetStyles ss v <- (((Map.toList . styles . getFeatures) &&& id) -> (ss,v)) where
  SetStyles ss v = (setFeatures ((getFeatures v) { styles = Map.fromList ss }) v)

pattern Styles :: HasFeatures a => [(Txt,Txt)] -> a -> a
pattern Styles ss v <- SetStyles ss v where
  Styles ss v =
    let fs = getFeatures v
    in (setFeatures (fs { styles = Map.union (Map.fromList ss) (styles fs) }) v)

pattern Listener :: HasFeatures a => Listener -> a -> a
pattern Listener l a <- ((const (error "The Listener pattern does not support matching, only construction. For pattern matching on listeners, use the Listeners pattern.") &&& id) -> (l,a)) where
  Listener l a =
    let fs = getFeatures a
    in (setFeatures (fs { listeners = l : listeners fs }) a)

pattern SetListeners :: HasFeatures a => [Listener] -> a -> a
pattern SetListeners ls v <- (((listeners . getFeatures) &&& id) -> (ls,v)) where
  SetListeners ls v = (setFeatures ((getFeatures v) { listeners = ls }) v)

pattern Listeners :: HasFeatures a => [Listener] -> a -> a
pattern Listeners ls v <- SetListeners ls v where
  Listeners ls v =
    let fs = getFeatures v
    in (setFeatures (fs { listeners = ls ++ listeners fs }) v)

pattern Attribute :: HasFeatures a => Txt -> Txt -> a -> a
pattern Attribute k v a <- ((const (error "The Attribute pattern does not support matching, only construction. For pattern matching on attributes, use the Attributes pattern with Data.List.lookup.","") &&& id) -> ((k,v),a)) where
  Attribute k v a =
    let fs = getFeatures a
    in (setFeatures (fs { attributes = Map.insert k v (attributes fs) }) a)

pattern SetAttributes :: HasFeatures a => [(Txt,Txt)] -> a -> a
pattern SetAttributes as v <- (((Map.toList . attributes . getFeatures) &&& id) -> (as,v)) where
  SetAttributes as v = (setFeatures ((getFeatures v) { attributes = Map.fromList as }) v)

pattern Attributes :: HasFeatures a => [(Txt,Txt)] -> a -> a
pattern Attributes as v <- SetAttributes as v where
  Attributes as v =
    let fs = getFeatures v
    in (setFeatures (fs { attributes = Map.union (Map.fromList as) (attributes fs) }) v)

pattern Property :: HasFeatures a => Txt -> Txt -> a -> a
pattern Property k v a <- ((const (error "The Property pattern does not support matching, only construction. For pattern matching on properties, use the Properties pattern with Data.List.lookup.","") &&& id) -> ((k,v),a)) where
  Property k v a =
    let fs = getFeatures a
        ps = properties fs
        ps' = Map.insert k v ps
    in (setFeatures fs { properties = ps' } a)

pattern SetProperties :: HasFeatures a => [(Txt,Txt)] -> a -> a
pattern SetProperties ps v <- (((Map.toList . properties . getFeatures) &&& id) -> (ps,v)) where
  SetProperties ps v =
    let fs = getFeatures v
        ps' = foldr (\(k,v) -> Map.insert k v) Map.empty ps
    in (setFeatures fs { properties = ps' } v)

pattern Properties :: HasFeatures a => [(Txt,Txt)] -> a -> a
pattern Properties ps v <- SetProperties ps v where
  Properties ps v =
    let fs = getFeatures v
    in (setFeatures (fs { properties = Map.union (Map.fromList ps) (properties fs) }) v)

pattern Lifecycle :: HasFeatures a => Lifecycle -> a -> a
pattern Lifecycle l a <- ((const (error "The Lifecycle pattern does not support matching, only construction. For pattern matching on lifecycle methods, use the Lifecycles pattern.") &&& id) -> (l,a)) where
  Lifecycle l a =
    let fs = getFeatures a
    in (setFeatures (fs { lifecycles = l : lifecycles fs }) a)

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
pattern OnCreated :: HasFeatures a => (Node -> IO (IO ())) -> a -> a
pattern OnCreated f a <- Lifecycle (Created f _) a where
  OnCreated f a = (Lifecycle (Created f (pure ())) a)

pattern OnMounted :: HasFeatures a => (Node -> IO (IO ())) -> a -> a
pattern OnMounted f a <- Lifecycle (Mounted f _) a where
  OnMounted f a = (Lifecycle (Mounted f (pure ())) a)

class HasXLinks a where
  getXLinks :: a -> [(Txt,Txt)]
  setXLinks :: [(Txt,Txt)] -> a -> a
  {-# INLINE addXLinks #-}
  addXLinks :: [(Txt,Txt)] -> a -> a
  addXLinks xl a = setXLinks (getXLinks a ++ xl) a

instance HasXLinks View where
  {-# INLINE getXLinks #-}
  getXLinks SVGView {..} = Map.toList xlinks
  getXLinks KSVGView {..} = Map.toList xlinks
  getXLinks PortalView {..} = getXLinks portalView
  getXLinks TaggedView {..} = getXLinks taggedView
  getXLinks Prebuilt {..} = getXLinks prebuilt
  getXLinks _ = []
  {-# INLINE setXLinks #-}
  setXLinks xl khtml@SVGView {} = khtml { xlinks = Map.fromList xl }
  setXLinks xl ksvg@KSVGView {} = ksvg { xlinks = Map.fromList xl }
  setXLinks xl PortalView {..} = PortalView { portalView = setXLinks xl portalView, .. }
  setXLinks xl TaggedView {..} = TaggedView { taggedView = setXLinks xl taggedView, .. }
  setXLinks xl Prebuilt {..} = Prebuilt { prebuilt = setXLinks xl prebuilt }
  setXLinks _ v = v
  {-# INLINE addXLinks #-}
  addXLinks xl v@SVGView {} = v { xlinks = Map.union (Map.fromList xl) (xlinks v) }
  addXLinks xl v@KSVGView {} = v { xlinks = Map.union (Map.fromList xl) (xlinks v) }
  addXLinks xl PortalView {..} = PortalView { portalView = addXLinks xl portalView, .. }
  addXLinks xl TaggedView {..} = TaggedView { taggedView = addXLinks xl taggedView, .. }
  addXLinks xl Prebuilt {..} = Prebuilt { prebuilt = addXLinks xl prebuilt }
  addXLinks _ v = v

pattern XLink :: HasXLinks a => Txt -> Txt -> a -> a
pattern XLink k v a <- ((const (error "The XLink pattern does not support matching, only construction. For pattern matching on xlinks, use the XLinks pattern with Data.List.lookup.","") &&& id) -> ((k,v),a)) where
  XLink k v a =
    let xls = getXLinks a
    in (setXLinks ((k,v):xls) a)

pattern SetXLinks :: HasXLinks a => [(Txt,Txt)] -> a -> a
pattern SetXLinks xl v <- ((getXLinks &&& id) -> (xl,v)) where
  SetXLinks xl v = (setXLinks xl v)

pattern XLinks :: HasXLinks a => [(Txt,Txt)] -> a -> a
pattern XLinks xl v <- ((getXLinks &&& id) -> (xl,v)) where
  XLinks xl v = (addXLinks xl v)

class HasChildren a where
  getChildren :: a -> [View]
  setChildren :: [View] -> a -> a
  {-# INLINE addChildren #-}
  addChildren :: [View] -> a -> a
  addChildren cs a = setChildren (getChildren a ++ cs) a

instance HasChildren View where
  {-# INLINE getChildren #-}
  getChildren v@HTMLView {} = children v
  getChildren v@SVGView {} = children v
  getChildren PortalView {..} = getChildren portalView
  getChildren TaggedView {..} = getChildren taggedView
  getChildren Prebuilt {..} = getChildren prebuilt
  getChildren _  = []
  {-# INLINE setChildren #-}
  setChildren cs v@HTMLView {} = v { children = cs }
  setChildren cs v@SVGView {} = v { children = cs }
  setChildren cs PortalView {..} = PortalView { portalView = setChildren cs portalView, .. }
  setChildren cs TaggedView {..} = TaggedView { taggedView = setChildren cs taggedView, .. }
  setChildren cs Prebuilt {..} = Prebuilt { prebuilt = setChildren cs prebuilt }
  setChildren _ v = v
  {-# INLINE addChildren #-}
  addChildren cs v@HTMLView {} = v { children = children v ++ cs }
  addChildren cs v@SVGView {} = v { children = children v ++ cs }
  addChildren cs PortalView {..} = PortalView { portalView = addChildren cs portalView, .. }
  addChildren cs TaggedView {..} = TaggedView { taggedView = addChildren cs taggedView, .. }
  addChildren cs Prebuilt {..} = Prebuilt { prebuilt = addChildren cs prebuilt }
  addChildren _ v = v

pattern SetChildren :: HasChildren a => [View] -> a -> a
pattern SetChildren cs v <- ((getChildren &&& id) -> (cs,v)) where
  SetChildren cs v = (setChildren cs v)

pattern Children :: HasChildren a => [View] -> a -> a
pattern Children cs v <- ((getChildren &&& id) -> (cs,v)) where
  Children cs v = (addChildren cs v)

class HasKeyedChildren a where
  getKeyedChildren :: a -> [(Int,View)]
  setKeyedChildren :: [(Int,View)] -> a -> a
  {-# INLINE addKeyedChildren #-}
  addKeyedChildren :: [(Int,View)] -> a -> a
  addKeyedChildren cs a = setKeyedChildren (getKeyedChildren a ++ cs) a

instance HasKeyedChildren View where
  {-# INLINE getKeyedChildren #-}
  getKeyedChildren v@KHTMLView {} = keyedChildren v
  getKeyedChildren v@SVGView {} = keyedChildren v
  getKeyedChildren PortalView {..} = getKeyedChildren portalView
  getKeyedChildren TaggedView {..} = getKeyedChildren taggedView
  getKeyedChildren Prebuilt {..} = getKeyedChildren prebuilt
  getKeyedChildren _ = []
  {-# INLINE setKeyedChildren #-}
  setKeyedChildren cs v@KHTMLView {} = v { keyedChildren = cs }
  setKeyedChildren cs v@KSVGView {} = v { keyedChildren = cs }
  setKeyedChildren cs PortalView {..} = PortalView { portalView = setKeyedChildren cs portalView, .. }
  setKeyedChildren cs TaggedView {..} = TaggedView { taggedView = setKeyedChildren cs taggedView, .. }
  setKeyedChildren cs Prebuilt {..} = Prebuilt { prebuilt = setKeyedChildren cs prebuilt, .. }
  setKeyedChildren _ v = v
  {-# INLINE addKeyedChildren #-}
  addKeyedChildren cs v@KHTMLView {} = v { keyedChildren = keyedChildren v ++ cs }
  addKeyedChildren cs v@KSVGView {} = v { keyedChildren = keyedChildren v ++ cs }
  addKeyedChildren cs PortalView {..} = PortalView { portalView = addKeyedChildren cs portalView, .. }
  addKeyedChildren cs TaggedView {..} = TaggedView { taggedView = addKeyedChildren cs taggedView, .. }
  addKeyedChildren cs Prebuilt {..} = Prebuilt { prebuilt = addKeyedChildren cs prebuilt }
  addKeyedChildren _ v = v

pattern SetKeyedChildren :: HasKeyedChildren a => [(Int,View)] -> a -> a
pattern SetKeyedChildren ks v <- ((getKeyedChildren &&& id) -> (ks,v)) where
  SetKeyedChildren ks v = (setKeyedChildren ks v)

pattern KeyedChildren :: HasKeyedChildren a => [(Int,View)] -> a -> a
pattern KeyedChildren ks v <- ((getKeyedChildren &&& id) -> (ks,v)) where
  KeyedChildren ks v = (addKeyedChildren ks v)

{-# INLINE (<|) #-}
infixl 8 <|
(<|) :: a -> (a -> b) -> b
(<|) a f = f a

{-# INLINE (<||>) #-}
{-# SPECIALIZE (<||>) :: View -> [View] -> View #-}
infixr 9 <||>
(<||>) :: HasChildren a => a -> [View] -> a
(<||>) v cs = setChildren cs v

{-# INLINE (<||#>) #-}
{-# SPECIALIZE (<||#>) :: View -> [(Int,View)] -> View #-}
infixr 9 <||#>
(<||#>) :: HasKeyedChildren a => a -> [(Int,View)] -> a
(<||#>) v cs = setKeyedChildren cs v

{-# INLINE (|>) #-}
{-# SPECIALIZE (|>) :: (View -> View) -> [View] -> View -> View #-}
infixr 9 |>
(|>) :: HasChildren a => (a -> a) -> [View] -> a -> a
(|>) f cs = setChildren cs . f

{-# INLINE (|#>) #-}
{-# SPECIALIZE (|#>) :: (View -> View) -> [(Int,View)] -> View -> View #-}
infixr 9 |#>
(|#>) :: HasKeyedChildren a => (a -> a) -> [(Int,View)] -> a -> a
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
-- propagation issue. Using the `unsafeCoerce` approach for `using` along with
-- `GHC.Exts.inline` was able to completely remove dictionaries from the generated 
-- core, but calls to `using` were (sometimes) allowed to satisfy non-local 
-- `Exists` constraints! 
--
-- See: https://gitlab.haskell.org/ghc/ghc/-/issues/21575
--
{-# NOINLINE withWitness #-}
withWitness :: forall a r. (Exists a => Proxy a -> r) -> a -> Proxy a -> r
withWitness w a p = magicDict (Witness w) a p

{-# INLINE using' #-}
using' :: forall a r. a -> (Exists a => r) -> r
using' a w = withWitness (\_ -> w) a Proxy
-}

newtype Witness a r = Witness (Exists a => r)

-- This approach is likely to produce bugs, but the performance improvement over
-- the magicDict version can be massive. Keep `using` in mind if anything
-- inexplicable happens.
{-# INLINABLE using #-}
using :: forall a r. a -> (Exists a => r) -> r
using a w = unsafeCoerce (Witness w :: Witness a r) a

{-# INLINE with #-}
with :: a -> (Exists a => r) -> r
with = using

{-# INLINE may #-}
may :: forall a b. Exists (Maybe a) => b -> (Exists a => b) -> b
may nothing just = maybe nothing (\a -> using a just) (it :: Maybe a)

{-# INLINE try #-}
try :: forall a b. Exists (Try.Try a) => b -> b -> (Exists a => b) -> b
try trying failed done = Try.try trying failed (\a -> using a done) (it :: Try.Try a)

{-# INLINE unite #-}
unite :: forall a b c. Exists (Either a b) => (Exists a => c) -> (Exists b => c) -> c
unite left right = either (\a -> using a left) (\b -> using b right) (it :: Either a b)

newtype Handler eff = Handler { runHandler :: eff -> IO () -> IO Bool }

type Effect eff = Exists (Handler eff)

{-# INLINE effect' #-}
effect' :: Effect eff => eff -> IO () -> IO Bool
effect' = runHandler it

{-# INLINE effect #-}
effect :: Effect eff => eff -> IO ()
effect eff = void (effect' eff (pure ()))

{-# INLINE map #-}
map :: forall msg msg' a. (msg -> msg') -> (Effect msg => a) -> (Effect msg' => a)
map f = using (Handler (\m io -> effect' (f m) io)) 

-- {-# INLINE also #-}
-- also :: forall msg a. (msg -> IO ()) -> (Effect msg => a) -> (Effect msg => a)
-- also f = using (Handler (\m io -> effect' m (io >> f m)))

{-# INLINE (#) #-}
infixr 9 #
(#) :: forall a b x. (a -> b) -> (Effect a => x) -> (Effect b => x)
(#) = Data.View.map

{-# RULES
  "Data.Effect.map id" forall x. Data.View.map id x = x
  #-}

type Viewable a = Exists (a -> View)

{-# INLINE toView #-}
toView :: Viewable a => a -> View
toView = it

type Producer a = Effect a

{-# INLINE yield #-}
yield :: Producer a => a -> IO ()
yield = effect

{-# INLINE stream #-}
stream :: (a -> IO ()) -> (Producer a => b) -> b
stream f = using (Handler (\a after -> f a >> after >> pure True))

{-# INLINE events #-}
events :: forall a b. (Exists a => IO ()) -> (Producer a => b) -> b
events f = stream @a (`using` f)

{-# INLINE discard #-}
discard :: forall a b. (Producer a => b) -> b
discard = stream @a (\_ -> pure ())

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
fork = Component @View @Fork (const def { deferred = True, render = const })

data Asynchronous a = Asynchronous (IO a) (Exists a :=> View)

data Async a = Async 
  { action :: IO a
  , view   :: Exists a :=> View
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
lazy io v = go (Asynchronous io (dynamic v))
  where
    go = Component \self -> def
      { deferred = True

      , onConstruct = do
          Asynchronous action view <- askref self
          result <- newEmptyMVar
          thread <- forkIO (action >>= putMVar result)
          pure Async {..}

      , onForce = \(Asynchronous new_action new_view) old@Async {..} -> do
          let
            sameAction = isTrue# (reallyUnsafePtrEquality# new_action action) 
            sameView   = isTrue# (reallyUnsafePtrEquality# new_view view)
          pure (not sameAction || not sameView)

      , onReceive = \(Asynchronous new_action new_view) old@Async {..} -> do
          let
            sameAction = isTrue# (reallyUnsafePtrEquality# new_action action) 
            sameView   = isTrue# (reallyUnsafePtrEquality# new_view view)
          case (sameAction,sameView) of
            (False,_) -> do
              let action = new_action
              let view = new_view
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
              pure (old :: Async a) { view = new_view }

            _ -> do
              pure old

      , render = \_ Async { view, result } -> 
          with (unsafePerformIO (readMVar result)) (fromDynamic view)

      }

data Synchronous a = Synchronous (IO a) (Exists a :=> View)
data Sync a = Sync 
  { action :: IO a
  , result :: a
  , view   :: Exists a :=> View
  }

seqv :: Typeable a => a -> (Exists a => View) -> View
seqv a = eager (evaluate a)

{-# INLINE eager #-}
eager :: forall a. Typeable a => IO a -> (Exists a => View) -> View
eager io v = go (Synchronous io (dynamic v))
  where
    go = Component $ \self -> def
      { onConstruct = do
        Synchronous action view <- askref self
        result <- action
        pure Sync {..}

      , onForce = \(Synchronous new_action new_view) Sync {..} -> do
          let
            sameAction = isTrue# (reallyUnsafePtrEquality# new_action action) 
            sameView   = isTrue# (reallyUnsafePtrEquality# new_view view)
          pure (not sameAction || not sameView)

      , onReceive = \(Synchronous new_action new_view) old@Sync {..} -> do
          let
            sameAction = isTrue# (reallyUnsafePtrEquality# new_action action) 
            sameView   = isTrue# (reallyUnsafePtrEquality# new_view view)
          case (sameAction,sameView) of
            (False,_) -> do
              let action = new_action
              let view = new_view
              result <- action
              pure Sync {..}
            (_,False) -> do
              pure (old :: Sync a) { view = new_view }
            _ -> do
              pure old

      , render = \_ Sync { view, result } -> with result (fromDynamic view)
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
lifecycle ls v = Component go (ls,v)
  where
    go self = def
      { onConstruct = askref self >>= onStart . fst
      , onMounted = askref self >>= onLoad . fst
      , onUpdate = \(ls',v') _ -> do
          (_,v) <- askref self
          unless (isTrue# (reallyUnsafePtrEquality# v v')) (onBefore ls')
      , onUpdated = \(ls,v) _ -> onAfter ls
      , onUnmounted = askref self >>= onStop . fst
      , render = \(_,v) _ -> v
      }

watch :: IO () -> View
watch = Component @(IO ()) @() go
  where
    go self = def
      { onUpdate = const
      }

watch' :: IO () -> View
watch' = Component go
  where
    go self = def
      { onConstruct = join (askref self)
      , onUpdate    = const
      }

data Polling a = Polling Time (IO a) (Exists a :=> View)

data Poll a = Poll
  { interval :: Time
  , begin :: Time
  , action :: IO a 
  , view :: Exists a :=> View
  , thread :: ThreadId
  , result :: MVar a
  }

{-# INLINE every #-}
every :: forall a. Typeable a => Time -> IO a -> (Exists a => View) -> View
every = poll

{-# INLINE poll #-}
-- Note that only the asynchrony of the first action can be witnessed in View
poll :: forall a. Typeable a => Time -> IO a -> (Exists a => View) -> View
poll t f v = go (Polling t f (dynamic v))
  where
    go = Component \self -> def
      { deferred = True

      , onConstruct = do
          begin <- time
          Polling interval action view <- askref self
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
            sameView     = isTrue# (reallyUnsafePtrEquality# new_view view)
          pure (not sameInterval || not sameAction || not sameView)

      , onReceive = \(Polling new_interval new_action new_view) old@Poll {..} -> do
          let
            sameInterval = new_interval == interval
            sameAction   = isTrue# (reallyUnsafePtrEquality# new_action action) 
            sameView     = isTrue# (reallyUnsafePtrEquality# new_view view)
          now <- time
          if 
            | not sameInterval || not sameAction -> do
              let interval = new_interval
              let begin = now
              let action = new_action
              let view = new_view
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
              pure (old :: Poll a) { view = new_view }
              
            | otherwise ->
              pure old

      , onUnmounted = getref self >>= \Poll { thread } -> killThread thread

      , render = \_ Poll { view, result } -> 
          with (unsafePerformIO (readMVar result)) (fromDynamic view)
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
reader = using

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
state' a v = weak (dynamic @(Modify a) a) (state a v)

{-# INLINE stateIO' #-}
stateIO' :: forall a. Typeable a => (Modify a => IO a) -> (State a => View) -> View
stateIO' ioa v = weak (dynamic @(Modify a) ioa) (stateIO ioa v)

{-# INLINE stateWith #-}
stateWith :: forall a. Typeable a => (Modify a => a -> a -> IO a) -> (Modify a => IO (a,a -> IO ())) -> (State a => View) -> View
stateWith f = foldM (\g a -> g a >>= f a) 

{-# INLINE stateWith' #-}
stateWith' :: forall a. Typeable a => (Modify a => a -> a -> IO a) -> (Modify a => IO (a,a -> IO ())) -> (State a => View) -> View
stateWith' f i v = weak (dynamic @(Modify a) f,dynamic @(Modify a) i) (stateWith f i v)

{-# INLINE modifyIO #-}
modifyIO :: forall a. Modify a => (a -> IO a) -> IO ()
modifyIO = yield 

{-# INLINE modify #-}
modify :: forall a. Modify a => (a -> a) -> IO ()
modify f = yield (\a -> pure @IO (f a))

{-# INLINE modifyIt #-}
modifyIt :: forall a. Modify a => (Exists a => a) -> IO ()
modifyIt a = modifyItIO (pure a) 

{-# INLINE modifyItIO #-}
modifyItIO :: forall a. Modify a => (Exists a => IO a) -> IO ()
modifyItIO ioa = yield (\a -> using (a :: a) ioa)

{-# INLINE put #-}
put :: Modify a => a -> IO ()
put = modify . const

{-# INLINE get #-}
get :: Exists a => a
get = it

{-# INLINE zoom #-}
zoom :: forall a b x. (a -> b) -> (b -> a -> a) -> (State b => x) -> (State a => x)
zoom f g v = using (f get) ((\h a -> h (f a) >>= \b -> pure @IO (g b a)) # v)

{-# INLINE zoomIO #-}
zoomIO :: forall a b x. (a -> b) -> (b -> a -> IO a) -> (State b => x) -> (State a => x)
zoomIO f g v = using (f get) ((\h a -> h (f a) >>= \b -> g b a) # v)

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
flat v = flatBy @l @a (const True) v

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

{-# INLINE fold #-}
fold :: (Typeable eff, Typeable a) => (Effect eff => eff -> a -> a) -> (Effect eff => a) -> ((Effect eff, Exists a) => View) -> View
fold step initial = foldM (\eff a -> pure (step eff a)) (pure (initial,\_ -> pure ()))

{-# INLINE foldM #-}
foldM :: forall eff a. (Typeable eff, Typeable a) => (Effect eff => eff -> a -> IO a) -> (Effect eff => IO (a,a -> IO ())) -> ((Effect eff,Exists a) => View) -> View
foldM step initial v = ComponentView witness Nothing folder (\self -> using (upd self) (Fold step initial (`using` v)))
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

data Fold eff a = Fold (eff -> a -> IO a) (IO (a,a -> IO ())) (a -> View)

newtype c :=> a = Dynamic { fromDynamic :: c => a }

dynamic :: (c => a) -> (c :=> a)
dynamic = Dynamic
{-# INLINE dynamic #-}

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
codify = weak (it :: c :=> View) (call @c)
{-# INLINE codify #-}

-- | Reify a View context and call the initial continuation.
cont :: forall c. Typeable c => Dynamic' c View -> (c => View)
cont d = reify d (codify @c)
{-# INLINE cont #-}


type Surface c = c => View

type Shape c = Modify (c :=> View) => c :=> View

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
full t = surface @c (hole @c) (shape @c (using (Data.View.empty @c) t))

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

type Write a = Modify a

channel :: Typeable a => a -> (Event a => View)-> View
channel = state

write :: Write a => a -> IO ()
write = put

-- read :: Event a => a
-- read = it

newtype Form ctx a = Form (Producer a => Dynamic ctx)

formlet :: forall a ctx. ((ctx, Producer a) => View) -> Form ctx a
formlet v = Form (dynamic v)

form :: forall ctx a. (Typeable ctx, Producer a, ctx) => Form ctx a -> View
form (Form f) = cont f

instance Functor (Form ctx) where
  fmap f (Form g) = Form (dynamic (stream (yield . f) (fromDynamic g)))

instance Applicative (Form ctx) where
  pure a = formlet (lifecycle def { onStart = yield a } Null)
  liftA2 = liftF2

instance Monad (Form ctx) where
  return = pure
  (Form f) >>= g = Form (dynamic (stream (\a -> let Form f = g a in unify f) (fromDynamic f)))

instance MonadFail (Form ctx) where
  fail str = Form (dynamic (txt str))

liftIO :: (ctx => IO a) -> Form ctx a
liftIO io = Form (dynamic (lifecycle def { onStart = io >>= yield } Null))

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

    dynamic do
      state (Any2 (unsafeCoerce Nothing) (unsafeCoerce Nothing)) do
        SimpleHTML "span" <||>
          [ stream commitA (fromDynamic fa)
          , stream commitB (fromDynamic fb)                
          ]
