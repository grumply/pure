{-# language PatternSynonyms, RankNTypes, FlexibleContexts, OverloadedStrings, ViewPatterns, CPP, DuplicateRecordFields, TypeApplications #-}
module Web.Events.Drag where

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
import Web.DataTransfer
import System.IO.Unsafe

data DragEvent = DragEvent
  { eventObject :: JSV
  , dataTransfer :: DataTransfer
  }

toDragEvent :: Evt -> DragEvent
toDragEvent (evtObj -> o) = let err = error "Invalid DragEvent." in
  DragEvent
    { eventObject = o 
    , dataTransfer = maybe err toDataTransfer (o .# "dataTransfer")
    }

newtype Drag = Drag DragEvent

dragWith :: Options -> (Drag -> IO ()) -> View -> View
dragWith opts f = OnWith opts "drag" (f . Drag . toDragEvent)

drag :: View -> (Producer Drag => View)
drag = dragWith def yield

drags :: (Exists Drag => IO ()) -> View -> View
drags f = events @Drag f drag

newtype DragEnd = DragEnd DragEvent

dragEndWith :: Options -> (DragEnd -> IO ()) -> View -> View
dragEndWith opts f = OnWith opts "dragend" (f . DragEnd . toDragEvent)

dragEnd :: View -> (Producer DragEnd => View)
dragEnd = dragEndWith def yield

dragEnds :: (Exists DragEnd => IO ()) -> View -> View
dragEnds f = events @DragEnd f dragEnd

newtype DragEnter = DragEnter DragEvent

dragEnterWith :: Options -> (DragEnter -> IO ()) -> View -> View
dragEnterWith opts f = OnWith opts "dragenter" (f . DragEnter . toDragEvent)

dragEnter :: View -> (Producer DragEnter => View)
dragEnter = dragEnterWith def yield

dragEnters :: (Exists DragEnter => IO ()) -> View -> View
dragEnters f = events @DragEnter f dragEnter

newtype DragLeave = DragLeave DragEvent

dragLeaveWith :: Options -> (DragLeave -> IO ()) -> View -> View
dragLeaveWith opts f = OnWith opts "dragleave" (f . DragLeave . toDragEvent)

dragLeave :: View -> (Producer DragLeave => View)
dragLeave = dragLeaveWith def yield

dragLeaves :: (Exists DragLeave => IO ()) -> View -> View
dragLeaves f = events @DragLeave f dragLeave

newtype DragOver = DragOver DragEvent

dragOverWith :: Options -> (DragOver -> IO ()) -> View -> View
dragOverWith opts f = OnWith opts "dragover" (f . DragOver . toDragEvent)

dragOver :: View -> (Producer DragOver => View)
dragOver = dragOverWith def yield

dragOvers :: (Exists DragOver => IO ()) -> View -> View
dragOvers f = events @DragOver f dragOver

newtype DragStart = DragStart DragEvent

dragStartWith :: Options -> (DragStart -> IO ()) -> View -> View
dragStartWith opts f = OnWith opts "dragstart" (f . DragStart . toDragEvent)

dragStart :: View -> (Producer DragStart => View)
dragStart = dragStartWith def yield

dragStarts :: (Exists DragStart => IO ()) -> View -> View
dragStarts f = events @DragStart f dragStart

newtype Drop = Drop DragEvent

dropWith :: Options -> (Drop -> IO ()) -> View -> View
dropWith opts f = OnWith opts "drop" (f . Drop . toDragEvent)

drop :: View -> (Producer Drop => View)
drop = dropWith def yield

drops :: (Exists Drop => IO ()) -> View -> View
drops f = events @Drop f Web.Events.Drag.drop


