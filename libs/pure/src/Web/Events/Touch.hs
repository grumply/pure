{-# language PatternSynonyms, RankNTypes, FlexibleContexts, OverloadedStrings, ViewPatterns, CPP, DuplicateRecordFields, TypeApplications #-}
module Web.Events.Touch where

import Control.Producer
import Control.Monad
import Data.Coerce
import Data.Default
import Data.DOM
import Data.Effect ((#))
import Data.Events (pattern OnWith)
import Data.Exists
import Data.JSON
import Data.Maybe
import Data.Txt
import Data.View
import System.IO.Unsafe

#ifdef __GHCJS__
import GHCJS.Marshal.Internal
#endif

data TouchType = Direct | Stylus
instance FromJSON TouchType where
  parseJSON v = do
    str <- parseJSON v
    case str :: Txt of
      "stylus" -> pure Stylus
      _        -> pure Direct

data Touch = Touch
  { eventObject :: JSV
  , identifier :: Int
  , target :: Node
  , screenX :: Double
  , screenY :: Double
  , clientX :: Double
  , clientY :: Double
  , radiusX :: Double
  , radiusY :: Double
  , rotationAngle :: Double
  , force :: Double
  , altitudeAngle :: Double
  , azimuthAngle :: Double
  , touchType :: TouchType
  } 

toTouch :: JSV -> Touch
toTouch o = let err = error "Invalid Touch Object." in
  Touch
    { eventObject = o
    , identifier = fromMaybe err (o .# "identifier")
    , target = maybe err (coerce :: JSV -> Node) (o .# "identifier")
    , screenX = fromMaybe err (o .# "screenX")
    , screenY = fromMaybe err (o .# "screenX")
    , clientX = fromMaybe err (o .# "clientX")
    , clientY = fromMaybe err (o .# "clientY")
    , radiusX = fromMaybe err (o .# "radiusX")
    , radiusY = fromMaybe err (o .# "radiusY")
    , force = fromMaybe err (o .# "force")
    , rotationAngle = fromMaybe err (o .# "rotationAngle")
    , altitudeAngle = fromMaybe err (o .# "altitudeAngle")
    , azimuthAngle = fromMaybe err (o .# "azimuthAngle")
    , touchType = 
      case fromMaybe err (o .# "rotationAngle") :: Txt of
        "stylus" -> Stylus
        _        -> Direct
    }

data TouchEvent = TouchEvent
  { eventObject :: JSV
  , touches :: [Touch]
  , targetTouches :: [Touch]
  , changedTouches :: [Touch]
  , altKey :: Bool
  , metaKey :: Bool
  , ctrlKey :: Bool
  , shiftKey :: Bool
  }

toTouchEvent :: Evt -> TouchEvent
toTouchEvent (evtObj -> o) = let err = error "Invalid TouchEvent Object." in
  TouchEvent 
    { eventObject = o
#ifdef __GHCJS__
    , touches        = fmap toTouch (fromMaybe err (join (fmap (unsafePerformIO . fromJSValListOf) (o .# "touches"))))
    , targetTouches  = fmap toTouch (fromMaybe err (join (fmap (unsafePerformIO . fromJSValListOf) (o .# "targetTouches"))))
    , changedTouches = fmap toTouch (fromMaybe err (join (fmap (unsafePerformIO . fromJSValListOf) (o .# "changedTouches"))))
#else
    , touches = []
    , targetTouches = []
    , changedTouches = []
#endif
    , altKey = fromMaybe err (o .# "altKey")
    , metaKey = fromMaybe err (o .# "metaKey")
    , ctrlKey = fromMaybe err (o .# "ctrlKey")
    , shiftKey = fromMaybe err (o .# "shiftKey")
    }

newtype TouchStart = TouchStart TouchEvent

touchStartWith :: Options -> (TouchStart -> IO ()) -> View -> View
touchStartWith opts f = OnWith opts "touchstart" (f . TouchStart . toTouchEvent)

touchStart :: View -> (Producer TouchStart => View)
touchStart = touchStartWith def yield

touchStarts :: (Exists TouchStart => IO ()) -> View -> View
touchStarts f = events @TouchStart f touchStart

newtype TouchEnd = TouchEnd TouchEvent

touchEndWith :: Options -> (TouchEnd -> IO ()) -> View -> View
touchEndWith opts f = OnWith opts "touchend" (f . TouchEnd . toTouchEvent)

touchEnd :: View -> (Producer TouchEnd => View)
touchEnd = touchEndWith def yield

touchEnds :: (Exists TouchEnd => IO ()) -> View -> View
touchEnds f = events @TouchEnd f touchEnd

newtype TouchMove = TouchMove TouchEvent

touchMoveWith :: Options -> (TouchMove -> IO ()) -> View -> View
touchMoveWith opts f = OnWith opts "touchmove" (f . TouchMove . toTouchEvent)

touchMove :: View -> (Producer TouchMove => View)
touchMove = touchMoveWith def yield

touchMoves :: (Exists TouchMove => IO ()) -> View -> View
touchMoves f = events @TouchMove f touchMove

newtype TouchCancel = TouchCancel TouchEvent

touchCancelWith :: Options -> (TouchCancel -> IO ()) -> View -> View
touchCancelWith opts f = OnWith opts "touchcancel" (f . TouchCancel . toTouchEvent)

touchCancel :: View -> (Producer TouchCancel => View)
touchCancel = touchCancelWith def yield

touchCancels :: (Exists TouchCancel => IO ()) -> View -> View
touchCancels f = events @TouchCancel f touchCancel


