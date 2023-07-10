{-# language PatternSynonyms, RankNTypes, FlexibleContexts, OverloadedStrings, ViewPatterns, CPP, DuplicateRecordFields, TypeApplications #-}
module Web.Events.Animation where

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
import Data.Time
import Data.Txt
import Data.View
import System.IO.Unsafe

data AnimationEvent = AnimationEvent
  { eventObject :: JSV
  , animationName :: Txt
  , elapsedTime :: Time
  , pseudoElement :: Txt
  }

toAnimationEvent :: Evt -> AnimationEvent
toAnimationEvent (evtObj -> o) = let err = error "Invalid Animation Event." in
  AnimationEvent
    { eventObject = o
    , animationName = fromMaybe err (o .# "animationName")
    , elapsedTime = maybe err (`Seconds` 0) (o .# "elapsedTime")
    , pseudoElement = fromMaybe err (o .# "pseudoElement")
    }

newtype AnimationStart = AnimationStart AnimationEvent

animationStartWith :: Options -> (AnimationStart -> IO ()) -> View -> View
animationStartWith opts f = OnWith opts "animationstart" (f . AnimationStart . toAnimationEvent)

animationStart :: View -> (Producer AnimationStart => View)
animationStart = animationStartWith def yield

animationStarts :: (Exists AnimationStart => IO ()) -> View -> View
animationStarts f = events @AnimationStart f animationStart

newtype AnimationIteration = AnimationIteration AnimationEvent

animationIterationWith :: Options -> (AnimationIteration -> IO ()) -> View -> View
animationIterationWith opts f = OnWith opts "animationiteration" (f . AnimationIteration . toAnimationEvent)

animationIteration :: View -> (Producer AnimationIteration => View)
animationIteration = animationIterationWith def yield

animationIterations :: (Exists AnimationIteration => IO ()) -> View -> View
animationIterations f = events @AnimationIteration f animationIteration

newtype AnimationCancel = AnimationCancel AnimationEvent

animationCancelWith :: Options -> (AnimationCancel -> IO ()) -> View -> View
animationCancelWith opts f = OnWith opts "animationcancel" (f . AnimationCancel . toAnimationEvent)

animationCancel :: View -> (Producer AnimationCancel => View)
animationCancel = animationCancelWith def yield

animationCancels :: (Exists AnimationCancel => IO ()) -> View -> View
animationCancels f = events @AnimationCancel f animationCancel

newtype AnimationEnd = AnimationEnd AnimationEvent

animationEndWith :: Options -> (AnimationEnd -> IO ()) -> View -> View
animationEndWith opts f = OnWith opts "animationend" (f . AnimationEnd . toAnimationEvent)

animationEnd :: View -> (Producer AnimationEnd => View)
animationEnd = animationEndWith def yield

animationEnds :: (Exists AnimationEnd => IO ()) -> View -> View
animationEnds f = events @AnimationEnd f animationEnd
