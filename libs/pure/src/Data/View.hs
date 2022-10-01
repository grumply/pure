{-# language MagicHash, CPP, ScopedTypeVariables, PatternSynonyms, PolyKinds, DefaultSignatures, ViewPatterns, RecordWildCards, GADTs, FlexibleInstances, AllowAmbiguousTypes, OverloadedStrings, TypeApplications, BangPatterns, RankNTypes, FlexibleContexts, ConstraintKinds #-}
module Data.View (module Data.View,Typeable) where

import Control.Arrow ((&&&))
import Control.Concurrent (MVar)
import Control.Monad (void,join)
import Control.Monad.ST (ST)
import Data.Coerce (Coercible(),coerce)
import Data.Exists (Exists,it,using)
import Data.IORef (IORef,readIORef)
import Data.List as List (null)
import Data.Monoid (Monoid(..),(<>))
import Data.Proxy (Proxy(..))
import Data.STRef (STRef)
import Data.String (IsString(..))
import Data.Traversable (for)
import Data.Typeable (Typeable,tyConName,typeRepTyCon,typeOf,typeRep,typeRepFingerprint,cast)
import Data.Unique (Unique,newUnique)
import GHC.Exts (reallyUnsafePtrEquality#,isTrue#,unsafeCoerce#)
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

  LazyView ::
    { lazyVal :: a
    , lazyView :: View
    } -> View

  EagerView ::
    { eagerVal :: a
    , eagerView :: View
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

type Viewable a = Exists (a -> View)

{-# INLINE toView #-}
toView :: Viewable a => a -> View
toView = it

{-# INLINE get #-}
get :: Ref props state -> IO state
get = readIORef . crState

{-# INLINE ask #-}
ask :: Ref props state -> IO props
ask = readIORef . crProps

{-# INLINE look #-}
look :: Ref props state -> IO View
look = readIORef . crView

{-# INLINE modify #-}
modify :: Ref props state -> (props -> state -> state) -> IO Bool
modify r f = modifyM r (\p s -> return (f p s,return ()))

{-# INLINE modify_ #-}
modify_ :: Ref props state -> (props -> state -> state) -> IO ()
modify_ r f = void (modify r f)

{-# INLINE modifyM #-}
modifyM :: Ref props state -> (props -> state -> IO (state,IO ())) -> IO Bool
modifyM cr f = queueComponentUpdate cr (UpdateState f)

{-# INLINE modifyM_ #-}
modifyM_ :: Ref props state -> (props -> state -> IO (state,IO ())) -> IO ()
modifyM_ r f = void (modifyM r f)

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
getHost LazyView      {}   = Nothing
getHost EagerView     {}   = Nothing
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
static = LazyView ()

{-# INLINE lazy #-}
lazy :: a -> View -> View
lazy = LazyView

-- The strictness here can be useful for primitives, but keep in mind that
-- optimization level can affect the behavior.
--
-- There are some neat tricks that can be had with `lazy'`, like:
--
-- > lazy' (value > 1) do
-- >   _
--
-- If `value` is strictly increasing from an initial value less than 1, this
-- `lazy'` call will update exactly once, when the value first exceeds 1.
--
{-# INLINE lazy' #-}
lazy' :: a -> View -> View
lazy' !a = LazyView a

-- If the given value is not the exact same object (reallyUnsafePtrEquality),
-- replace the View. You probably don't need this; you likely just need stronger
-- typing!
{-# INLINE eager #-}
eager :: a -> View -> View
eager = EagerView

-- If the given value is not the exact same object after forcing to WHNF 
-- and comparing with reallyUnsafePtrEquality, replace the View. You probably
-- don't need this; you likely just need stronger typing! The strictness here
-- can be useful for primitives, but keep in mind that optimization level can 
-- affect the behavior.
{-# INLINE eager' #-}
eager' :: a -> View -> View
eager' !a = EagerView a

{-# INLINE tagged #-}
tagged :: forall t. Typeable t => View -> View
tagged = TaggedView (proxyWitness (Proxy :: Proxy t))

pattern Tag :: forall t. Typeable t => View -> View
pattern Tag v <- TaggedView ((sameTypeWitness (proxyWitness (Proxy :: Proxy t))) -> True) v where
  Tag t = (tagged @t t)

-- Txt

{-# INLINE txt #-}
txt :: ToTxt a => a -> View
txt a = lazy a (TextView Nothing (toTxt a))

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

pattern SetLifecycles :: HasFeatures a => [Lifecycle] -> a -> a
pattern SetLifecycles lc v <- (((lifecycles . getFeatures) &&& id) -> (lc,v)) where
  SetLifecycles lc v = (setFeatures ((getFeatures v) { lifecycles = lc }) v)

pattern Lifecycles :: HasFeatures a => [Lifecycle] -> a -> a
pattern Lifecycles lc v <- (((lifecycles . getFeatures) &&& id) -> (lc,v)) where
  Lifecycles lc v =
    let fs = getFeatures v
    in (setFeatures (fs { lifecycles = lc ++ lifecycles fs }) v)

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

