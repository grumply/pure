{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies, GADTs #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MagicHash #-}
module Pure.Types.View where

import Ef.Base

import TLC hiding (Construct,Update,Updated)

import Pure.Types.Feature

import Pure.Data.CB
import Pure.Data.Cond
import Pure.Data.Default
import Pure.Data.JSON
import Pure.Data.JSV
import Pure.Data.Txt hiding (map)
import Pure.Data.UnsafeEq
import Pure.Data.Key

import Control.Monad.ST
import Data.STRef

import Control.Concurrent
import Data.Bifunctor
import Data.String
import Data.Typeable
import Data.IORef
import Data.Unique
import GHC.Generics
import GHC.Exts
import System.IO.Unsafe

import Data.Hashable
import qualified Data.IntMap.Strict as HM

import Unsafe.Coerce

instance Default a => Default (Narrative f c a) where
  def = Return def

instance Cond a => Cond (Narrative f c a) where
  nil = Return nil
  isNil (Return a) = isNil a
  isNil _ = False

data View (ms :: [* -> *]) where
  -- NullView must have a presence on the page for proper diffing
  NullView
    :: { elementHost :: (Maybe Element)
       } -> View ms

  TextView
    ::  { textHost :: (Maybe Text)
        , content  :: Txt
        } -> View ms

  RawView
    :: { rawHost  :: (Maybe Node)
       , tag      :: Txt
       , features :: [Feature ms]
       , content  :: Txt
       } -> View ms

  HTMLView
    ::  { elementHost :: (Maybe Element)
        , tag         :: Txt
        , features    :: [Feature ms]
        , children    :: [View ms]
        } -> View ms

  KHTMLView
    ::  { elementHost   :: (Maybe Element)
        , tag           :: Txt
        , features      :: [Feature ms]
        , keyedChildren :: [(Int,View ms)]
        , childMap      :: (HM.IntMap (View ms))
        } -> View ms

  ComponentView
    :: { componentType   :: Txt
       , componentProps  :: props
       , componentRecord :: (Maybe (Ref ms props state))
       , componentView   :: (Ref ms props state -> Comp ms props state)
       } -> View ms

  SVGView
    ::  { elementHost :: (Maybe Element)
        , tag         :: Txt
        , features    :: [Feature ms]
        , children    :: [View ms]
        } -> View ms

  KSVGView
    ::  { elementHost   :: (Maybe Element)
        , tag           :: Txt
        , features      :: [Feature ms]
        , keyedChildren :: [(Int,View ms)]
        , childMap      :: (HM.IntMap (View ms))
        } -> View ms

  ManagedView
    ::  { elementHost :: (Maybe Element)
        , tag         :: Txt
        , features    :: [Feature ms]
        , controller  :: Controller_
        } -> View ms

  SomeView
    :: (Pure a ms, Typeable a, Typeable ms) => { renderable :: a ms } -> View ms

  -- StaticView
  --   :: { staticView :: View ms } -> View ms


type Builder e      = [Feature e] -> [View e] -> View e
type KeyedBuilder e = [Feature e] -> [(Int,View e)] -> View e

instance Eq (View e) where
  (==) v1 v2 =
    case reallyUnsafePtrEquality# v1 v2 of
      1# -> True
      _  -> go v1 v2
    where
      go (NullView _) (NullView _) =
          True
      go (TextView _ t) (TextView _ t') =
          prettyUnsafeEq t t'
      go (RawView _ t fs c) (RawView _ t' fs' c') =
          prettyUnsafeEq t t' && (reallyUnsafeEq fs fs' || fs == fs') && prettyUnsafeEq c c'
      go (KHTMLView _ t fs ks cm) (KHTMLView _ t' fs' ks' cm') =
          prettyUnsafeEq t t' && (reallyUnsafeEq fs fs' || fs == fs') && (reallyUnsafeEq ks ks' || ks == ks')
      go (HTMLView _ t fs cs) (HTMLView _ t' fs' cs') =
          prettyUnsafeEq t t' && (reallyUnsafeEq fs fs' || fs == fs') && (reallyUnsafeEq cs cs' || cs == cs')
      go (ComponentView _ p _ v) (ComponentView _ p' _ v') =
          reallyVeryUnsafeEq v v' && reallyVeryUnsafeEq p p'
      go (KSVGView _ t fs ks cm) (KSVGView _ t' fs' ks' cm') =
          prettyUnsafeEq t t' && (reallyUnsafeEq fs fs' || fs == fs') && (reallyUnsafeEq ks ks' || ks == ks')
      go (SVGView _ t fs cs) (SVGView _ t' fs' cs') =
          prettyUnsafeEq t t' && (reallyUnsafeEq fs fs' || fs == fs') && (reallyUnsafeEq cs cs' || cs == cs')
      go (ManagedView _ t fs c) (ManagedView _ t' fs' c') =
          prettyUnsafeEq t t' && (reallyUnsafeEq fs fs' || fs == fs') && prettyUnsafeEq c c'
      go (SomeView a) (SomeView b) =
          reallyVeryUnsafeEq a b
      go _ _ =
          False

instance Default (View ms) where
  def = nil

instance Cond (View e) where
  nil = NullView Nothing

instance IsString (View e) where
  fromString = TextView Nothing . fromString

instance FromTxt (View e) where
  fromTxt = TextView Nothing

instance {-# OVERLAPS #-} IsString [View e] where
  fromString s = [fromString s]

instance FromTxt [View e] where
  fromTxt t = [fromTxt t]

class Pure (a :: [* -> *] -> *) (ms :: [* -> *]) where
  -- TODO:
  --   build :: a ms -> IO (View ms)
  --   diff :: (Ef ms IO () -> IO ()) -> ENode -> View ms -> a ms -> a ms -> IO (View ms)
  -- With build and diff the only primitive view elements would be HTML, SVG, Managed, and View.
  -- Great avenue for extensibility and modularity, but I don't see that the expressivity gained
  -- would currently justify the work; it's mostly just a refactoring, but it is a major refactoring.
  render :: a ms -> View ms
  default render :: (Generic (a ms), GPure (Rep (a ms)) ms) => a ms -> View ms
  render = grender . from

instance Pure View ms where
  render (SomeView a) = render a
  render a = a

class GPure a ms where
  grender :: a x -> View ms

instance GPure GHC.Generics.U1 ms where
  grender GHC.Generics.U1 = nil

instance (Pure a ms) => GPure (GHC.Generics.K1 i (a ms)) ms where
  grender (GHC.Generics.K1 k) = render k

instance (GPure a ms, GPure b ms) => GPure (a :*: b) ms where
  grender (a :*: b) = mkHTML "div" [ ] [ grender a, grender b ]

instance (GPure a ms, GPure b ms) => GPure (a :+: b) ms where
  grender (L1 a) = grender a
  grender (R1 b) = grender b

type Dispatcher e = Ef e IO () -> IO ()

type PropsUpdater props = props -> IO Bool

type Unmounter = IO Bool

data DiffStrategy = Eager | Manual deriving (Eq)

data MVCView (ms :: [* -> *]) m where
  MVCView ::
    { mvcvCurrent     :: View ms
    , mvcvCurrentLive :: Maybe (View ms)
    , mvcvModel       :: m ms
    } -> MVCView ms m

data MVCRecord ms m where
  MVCRecord ::
    { mvcrAs      :: As (Ef ms IO)
    , mvcrView    :: IORef (MVCView ms m)
    , mvcrPatcher :: MVCPatcher
    } -> MVCRecord ms m

type IsMVC' ts ms m = (ms <: Base m, ts <. Base m, Delta (Modules ts) (Messages ms))
type IsMVC ms m = IsMVC' ms ms m

data MVCPatcher =
  MVCPatcher
    { mvcpThreadId :: ThreadId
    , mvcpAddPatch :: IO ()
    }

-- Top-level state cannot mention it's parent type; so `ms` is elided,
-- but we can always witness it in context - and thus safely unsafeCoerce to it.
data MVCState (m :: [* -> *] -> *) where
  MVCState ::
    { mvcsPatcher  :: MVCPatcher
    , mvcsStrategy :: IORef DiffStrategy
    , mvcsUpdates  :: Syndicate (m ms)
    , mvcsView     :: IORef (MVCView ms m)
    } -> MVCState m

type MVC m ms = (ms <: Base m)
type VC ms = ms <: '[ State () Shutdown, Evented]

type Base (m :: [* -> *] -> *)
  = '[ State () (MVCState m)
     , State () Shutdown
     , State () (IORef DiffStrategy)
     , State () MVCPatcher
     , Evented
     ]

data Controller_ where
  Controller_ :: (IsMVC' ts ms m) => Controller' ts ms m -> Controller_
instance Eq Controller_ where
 (==) (Controller_ c) (Controller_ c') =
  let Key _ k1 :: Key GHC.Exts.Any = unsafeCoerce (key c)
      Key _ k2 :: Key GHC.Exts.Any = unsafeCoerce (key c')
  in k1 == k2

type ControllerKey ms m = Key (MVCRecord (Appended ms (Base m)) m)

type ControllerBuilder ts m =
                      Modules (Base m)  (Action (Appended ts (Base m)) IO)
  -> IO (Modules (Appended ts (Base m)) (Action (Appended ts (Base m)) IO))

type ControllerPrimer ms m = Ef (Appended ms (Base m)) IO ()

data Controller' ts ms m = forall a. Pure a ms => Controller
  { key       :: !(Key (MVCRecord ms m))
  , build     :: !(Modules (Base m) (Action ts IO) -> IO (Modules ts (Action ts IO)))
  , prime     :: !(Ef ms IO ())
  , model     :: !(m ms)
  , view      :: !(m ms -> a ms)
  }
type Controller ms m = Controller' (Appended ms (Base m)) (Appended ms (Base m)) m

instance ToTxt (Controller' ts ms m) where
  toTxt = toTxt . key

instance Eq (Controller' ts ms m) where
  (==) (Controller k _ _ _ _) (Controller k' _ _ _ _) =
    let Key _ k1 = k
        Key _ k2 = k'
    in k1 == k2

instance Ord (Controller' ts ms m) where
  compare (Controller (Key _ k) _ _ _ _) (Controller (Key _ k') _ _ _ _) = compare k k'

pattern Null :: Typeable ms => View ms
pattern Null <- (NullView _) where
  Null = NullView Nothing

pattern Raw :: Txt -> [Feature ms] -> Txt -> View ms
pattern Raw t fs c <- (RawView _ t fs c) where
  Raw t fs c = RawView Nothing t fs c

pattern Text :: (ToTxt t, FromTxt t) => t -> View ms
pattern Text t <- (TextView _ (fromTxt -> t)) where
  Text t = TextView Nothing (toTxt t)

pattern Component nm p v <- ComponentView nm p _ v where
  Component nm p v = ComponentView nm p Nothing v

pattern View :: (Pure a e, Typeable a, Typeable e) => a e -> View e
pattern View ams <- (SomeView (cast -> Just ams)) where
  View ams = SomeView ams

mkHTML :: Txt -> [Feature e] -> [View e] -> View e
mkHTML tag features children =
  let elementHost = Nothing
  in HTMLView {..}

mkSVG :: Txt -> [Feature e] -> [View e] -> View e
mkSVG tag features children =
  let elementHost = Nothing
  in SVGView {..}

mvc :: ([Feature e] -> [View e] -> View e)
    -> (forall ts' ms' m. (IsMVC' ts' ms' m) => [Feature e] -> Controller' ts' ms' m -> View e)
mvc f = \as c ->
  case f [] [] of
    HTMLView _ t _ _ -> ManagedView Nothing t as (Controller_ c)
    _ -> error "Incorrect usage of construct; Controllers may only be embedded in plain html HTMLs."

hashed :: Hashable a => ([Feature e] -> [View e] -> View e) -> [Feature e] -> [(a,View e)] -> View e
hashed x features keyedChildren0 = list x features (map (first hash) keyedChildren0)

updateFeatures f v =
  case v of
    HTMLView    {..} -> HTMLView    { features = f features, .. }
    RawView     {..} -> RawView     { features = f features, .. }
    KHTMLView   {..} -> KHTMLView   { features = f features, .. }
    SVGView     {..} -> SVGView     { features = f features, .. }
    KSVGView    {..} -> KSVGView    { features = f features, .. }
    ManagedView {..} -> ManagedView { features = f features, .. }
    _             -> v

text :: Txt -> View e
text content =
  let textHost = Nothing
  in TextView {..}

raw :: ([Feature e] -> [View e] -> View e) -> [Feature e] -> Txt -> View e
raw x features content =
  case x [] [] of
    HTMLView _ tag _ _ ->
      let rawHost = Nothing
      in RawView {..}
    SVGView _ tag _ _ ->
      let rawHost = Nothing
      in RawView {..}
    _ -> error "HTMLic.Controller.raw: raw atoms may only be built from HTMLViews and SVGViews"

list :: ([Feature e] -> [View e] -> View e) -> [Feature e] -> [(Int,View e)] -> View e
list x features keyedChildren =
  case x [] [] of
    HTMLView _ tag _ _ ->
      let
        elementHost = Nothing
        childMap = mempty
      in
        KHTMLView {..}
    SVGView _ tag _ _ ->
      let
        elementHost = Nothing
        childMap = mempty
      in
        KSVGView {..}
    _ -> error "Atomic.Component.list: lists may only be built from HTMLViews and SVGViews"

witness :: View '[] -> View ms
witness = unsafeCoerce

witnesses :: [View '[]] -> [View ms]
witnesses = unsafeCoerce

mapPure :: (Typeable a, Typeable a', Typeable ms, Pure a ms, Pure a' ms) => (a ms -> a' ms) -> View ms -> View ms
mapPure f sa =
  case sa of
    View a -> View (f a)
    _ -> sa

forPure :: (Typeable a, Typeable a', Typeable ms, Pure a ms, Pure a' ms)
             => View ms -> (a ms -> a' ms) -> View ms
forPure = flip mapPure

infixl 9 %
(%) :: (Typeable a, Typeable a', Typeable ms, Pure a ms, Pure a' ms) => View ms -> (a ms -> a' ms) -> View ms
(%) = forPure

mapPures :: (Typeable a, Typeable a', Typeable ms, Pure a ms, Pure a' ms)
              => (a ms -> a' ms) -> [View ms] -> [View ms]
mapPures f as = map (mapPure f) as

forPures :: (Typeable a, Typeable a', Typeable ms, Pure a ms, Pure a' ms)
              => [View ms] -> (a ms -> a' ms) -> [View ms]
forPures = flip mapPures

data Mapping ms = forall a a'. (Typeable a, Typeable a', Typeable ms, Pure a ms, Pure a' ms)
                => Mapping (a ms -> a' ms)

maps :: View ms -> [Mapping ms] -> View ms
maps a mappings = Prelude.foldr tryMap a mappings
  where
    tryMap (Mapping m) res =
      case a of
        View a -> View (m a)
        _ -> res

data Comp (parent :: [* -> *]) (props :: *) (state :: *) =
    Comp
      { construct    :: (IO state)
      , initialize   :: (state -> IO state)
      , initialized  :: (IO ())
      , mount        :: (state -> IO state)
      , mounted      :: (IO ())
      , receiveProps :: (props -> state -> IO state)
      , forceUpdate  :: (props -> state -> IO Bool)
      , update       :: (props -> state -> IO ())
      , renderer     :: (props -> state -> View parent)
      , updated      :: (props -> state -> View parent -> IO ())
      , unmount      :: (IO ())
      , destruct     :: (IO ())
      }

instance (Typeable props, Typeable state) => Default (Comp parent props state) where
  def =
    Comp
      { renderer     = \_ _ -> nil
      , destruct     = return ()
      , unmount      = return ()
      , updated      = \_ _ _ -> return ()
      , update       = \_ _ -> return ()
      , forceUpdate  = \_ _ -> return True
      , receiveProps = \_ -> return
      , mounted      = return ()
      , mount        = return
      , initialized  = return ()
      , initialize   = return
      , construct    = return (error "Component.construct: no initial state.")
      }

type StateUpdate ps st = ps -> st -> IO (st,IO ())
type StateUpdater ps st = StateUpdate ps st -> IO Bool

data ComponentPatch (parent :: [* -> *]) props state where
  Unmount
    :: (STRef s [IO ()] -> View parent -> ST s ())
    -> MVar (IO ())
    -> ComponentPatch parent props state

  UpdateProperties
    :: props
    -> ComponentPatch parent props state

  UpdateState
    :: StateUpdate props state
    -> ComponentPatch parent props state

data Ref (parent :: [* -> *]) (props :: *) (state :: *) where
  Ref ::
    { crType       :: Txt
    , crView       :: (IORef (View parent))
    , crProps      :: (IORef props)
    , crState      :: (IORef state)
    , crDispatcher :: (Dispatcher parent)
    , crComponent  :: (Comp parent props state)
    , crPatchQueue :: (IORef (Maybe (Queue (ComponentPatch parent props state))))
    } -> Ref parent props state
