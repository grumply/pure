{-# language PatternSynonyms, RankNTypes, FlexibleContexts, OverloadedStrings, ViewPatterns, CPP, DuplicateRecordFields, TypeApplications #-}
module Web.Events.Transition where

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

data TransitionEvent = TransitionEvent
  { eventObject :: JSV
  , propertyName :: Txt
  , elapsedTime :: Time
  , pseudoElement :: Txt
  }

toTransitionEvent :: Evt -> TransitionEvent
toTransitionEvent (evtObj -> o) = let err = error "Invalid Transition Event." in
  TransitionEvent
    { eventObject = o
    , propertyName = fromMaybe err (o .# "propertyName")
    , elapsedTime = maybe err (`Seconds` 0) (o .# "elapsedTime")
    , pseudoElement = fromMaybe err (o .# "pseudoElement")
    }

newtype TransitionStart = TransitionStart TransitionEvent

transitionStartWith :: Options -> (TransitionStart -> IO ()) -> View -> View
transitionStartWith opts f = OnWith opts "transitionstart" (f . TransitionStart . toTransitionEvent)

transitionStart :: View -> (Producer TransitionStart => View)
transitionStart = transitionStartWith def yield

transitionStarts :: (Exists TransitionStart => IO ()) -> View -> View
transitionStarts f = events @TransitionStart f transitionStart

newtype TransitionEnd = TransitionEnd TransitionEvent

transitionEndWith :: Options -> (TransitionEnd -> IO ()) -> View -> View
transitionEndWith opts f = OnWith opts "transitionend" (f . TransitionEnd . toTransitionEvent)

transitionEnd :: View -> (Producer TransitionEnd => View)
transitionEnd = transitionEndWith def yield

transitionEnds :: (Exists TransitionEnd => IO ()) -> View -> View
transitionEnds f = events @TransitionEnd f transitionEnd

newtype TransitionRun = TransitionRun TransitionEvent

transitionRunWith :: Options -> (TransitionRun -> IO ()) -> View -> View
transitionRunWith opts f = OnWith opts "transitionrun" (f . TransitionRun . toTransitionEvent)

transitionRun :: View -> (Producer TransitionRun => View)
transitionRun = transitionRunWith def yield

transitionRuns :: (Exists TransitionRun => IO ()) -> View -> View
transitionRuns f = events @TransitionRun f transitionRun

newtype TransitionCancel = TransitionCancel TransitionEvent

transitionCancelWith :: Options -> (TransitionCancel -> IO ()) -> View -> View
transitionCancelWith opts f = OnWith opts "transitioncancel" (f . TransitionCancel . toTransitionEvent)

transitionCancel :: View -> (Producer TransitionCancel => View)
transitionCancel = transitionCancelWith def yield

transitionCancels :: (Exists TransitionCancel => IO ()) -> View -> View
transitionCancels f = events @TransitionCancel f transitionCancel

