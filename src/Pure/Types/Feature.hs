{-# LANGUAGE GADTs #-}
module Pure.Types.Feature where

import Ef

import Pure.Data.CB
import Pure.Data.Cond
import Pure.Data.Default
import Pure.Data.JSV
import Pure.Data.Txt
import Pure.Data.UnsafeEq

import Data.Map.Strict as M

data Options = Options
  { preventDef :: Bool
  , stopProp   :: Bool
  , passive    :: Bool
  } deriving (Eq,Show)
instance Default Options where
  def = Options False False True

data Evt = Evt
  { evtObj            :: JSV
  , evtRemoveListener :: IO ()
  , evtTarget         :: JSV
  }

data Target = ElementTarget | WindowTarget | DocumentTarget
  deriving (Eq,Show)

data Feature ms where

  NullFeature :: Feature ms

  Attribute ::
    { name  :: Txt
    , value :: Txt
    } -> Feature ms

  Property ::
    { name  :: Txt
    , value :: Txt
    } -> Feature ms

  StyleMap ::
    { stylePairs :: !(M.Map Txt Txt)
    } -> Feature ms

  On ::
    { eventName     :: Txt
    , eventTarget   :: Target
    , eventOptions  :: Options
    , eventAction   :: Evt -> IO (Maybe (Ef ms IO ()))
    , eventCallback :: Maybe (CB (JSV -> IO ()))
    , eventStopper  :: IO ()
    } -> Feature ms

  HostRef ::
    { withHost :: Node -> IO (Maybe (Ef ms IO ()))
    } -> Feature ms

  Link ::
    { link         :: Txt
    , eventStopper :: IO ()
    } -> Feature ms

  SVGLink ::
    { link         :: Txt
    , eventStopper :: IO ()
    } -> Feature ms

  XLink ::
    { name  :: Txt
    , value :: Txt
    } -> Feature ms

  StaticFeature ::
    { feature :: Feature ms } -> Feature ms

  DiffFeatureOn ::
    { diffFeatureOn :: a, feature :: Feature ms } -> Feature ms

instance Eq (Feature ms) where
  (==) NullFeature NullFeature = True
  (==) (Property p v) (Property p' v') =
    prettyUnsafeEq p p' && prettyUnsafeEq v v'
  (==) (Attribute a v) (Attribute a' v') =
    prettyUnsafeEq a a' && prettyUnsafeEq v v'
  (==) (StyleMap ss) (StyleMap ss') =
    reallyUnsafeEq ss ss' || ss == ss'
  (==) (On e t os ev _ _) (On e' t' os' ev' _ _) =
    e == e' && t == t' && os == os' && reallyVeryUnsafeEq ev ev'
  (==) (HostRef f) (HostRef f') =
    reallyVeryUnsafeEq f f'
  (==) (Link t _) (Link t' _) =
    prettyUnsafeEq t t'
  (==) (SVGLink t _) (SVGLink t' _) =
    prettyUnsafeEq t t'
  (==) (XLink t _) (XLink t' _) =
    prettyUnsafeEq t t'
  (==) (StaticFeature _) (StaticFeature _) = True
  (==) (DiffFeatureOn a f) (DiffFeatureOn a' f') = reallyVeryUnsafeEq a a' || f == f'
  (==) _ _ = False

instance Cond (Feature ms) where
  nil = NullFeature
  isNil = (== NullFeature)

instance Default (Feature ms) where
  def = NullFeature

