{-# LANGUAGE CPP, PatternSynonyms, MultiParamTypeClasses, RecordWildCards,
   TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings, 
   FlexibleContexts, ViewPatterns, ConstraintKinds, RankNTypes, 
   DerivingVia, DerivingStrategies, TypeSynonymInstances #-}
module Effect.Intersection 
  (Options(..),Intersection(..),Rect(..)
  ,Intersectable
  ,intersection
  ,intersectable
  ,intersecting,intersectingWith
  ,viewportIntersecting
  ) where

import Control.Reader
import Control.State
import Control.Producer
import Data.Default
import Data.Effect
import Data.DOM hiding (Options)
import Data.HTML
import Data.JSON hiding (Null)
import Data.Txt
import Data.View hiding (modify,ask)

import Control.Arrow ((&&&))
import Control.Monad
import Data.Coerce
import Data.Foldable (for_,traverse_)
import Data.Function ((&))
import Data.IORef
import Data.Maybe
import GHC.Generics as G

#ifdef __GHCJS__
import GHCJS.Marshal.Internal
import JavaScript.Object.Internal as JS (Object(..),create,setProp)
#endif

data Options = Options
  { margin :: Txt
  , thresholds :: [Double]
  } deriving Show

instance Default Options where
  def = Options def def

data Intersection = Intersection
  { bounds         :: Rect
  , ratio          :: Double
  , rect           :: Rect
  , isIntersecting :: Bool
  , rootBounds     :: Maybe Rect
  , target         :: JSV
  , time           :: Double
  }

-- For why rootBounds can be null, see: 
-- https://developers.google.com/web/updates/2016/04/intersectionobserver#iframe_magic
mkIntersection :: JSV -> Intersection
mkIntersection jsv = 
#ifdef __GHCJS__
  fromMaybe (error "View.Intersection.mkIntersection: fromMaybe got Nothing") $ do
    bcr <- fmap mkRect $ jsv .# "boundingClientRect"
    ip  <- jsv .# "intersectionRatio"
    ir  <- fmap mkRect $ jsv .# "intersectionRect"
    ii  <- jsv .# "isIntersecting"
    let rb = fmap mkRect $ jsv .# "rootBounds"
    tr  <- jsv .# "target"
    tm  <- jsv .# "time"
    pure $ Intersection bcr ip ir ii rb tr tm
#else
  Intersection def def def def def def def
#endif

data IntersectionRoot = IntersectionRoot (Maybe Node)

root :: Reader IntersectionRoot => Maybe Node
root = let IntersectionRoot mn = ask in mn

setRoot :: Modify IntersectionRoot => Node -> IO ()
setRoot = put . IntersectionRoot . Just

type Intersectable = State IntersectionRoot

intersection :: (Intersectable => View) -> View
intersection = state (IntersectionRoot Nothing)

intersectable :: View -> (Intersectable => View)
intersectable = OnMounted (\node -> setRoot node >> def)

viewportIntersecting :: Options -> View -> (Effect Intersection => View)
viewportIntersecting options v = stateWith (\_ -> pure) (pure (pure (),id)) (OnMounted (\node -> intersectingWith (coerce nullJSV) node options yield >>= put >> def) v)

intersecting :: Intersectable => Options -> View -> (Effect Intersection => View)
intersecting options v
  | Just r <- root = stateWith (\_ -> pure) (pure (pure (),id)) (OnMounted (\node -> intersectingWith r node options yield >>= put >> def) v)
  | otherwise      = Null

intersectingWith :: Node -> Node -> Options -> (Intersection -> IO ()) -> IO (IO ())
intersectingWith root node Options {..} onIntersection = do
#ifdef __GHCJS__
  obj <- JS.create
  ts <- toJSValListOf thresholds
  JS.setProp "threshold" ts obj
  unless (isNull root) $ do
    JS.setProp "root" (coerce root) obj
  unless (margin == "") $ do
    JS.setProp "rootMargin" (pToJSVal margin) obj
  let options = coerce obj
  cb <- syncCallback1 ContinueAsync $ \jsv -> do
    Just (fmap mkIntersection -> is) <- fromJSValListOf jsv
    traverse_ onIntersection is
  obs <- observer_js cb (pFromJSVal options)
  observe_js obs (coerce node)
  pure (releaseCallback cb)
#else
  pure (pure ())
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "$r = new IntersectionObserver($1,$2)" observer_js :: Callback (JSV -> IO ()) -> JSV -> IO JSV
foreign import javascript unsafe
  "$1.observe($2)" observe_js :: JSV -> JSV -> IO ()
#endif
