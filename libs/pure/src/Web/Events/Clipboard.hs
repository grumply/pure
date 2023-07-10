{-# language PatternSynonyms, RankNTypes, FlexibleContexts, OverloadedStrings, ViewPatterns, CPP, DuplicateRecordFields, TypeApplications #-}
module Web.Events.Clipboard where

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

data ClipboardEvent = ClipboardEvent
  { eventObject :: JSV
  , clipboardData :: DataTransfer
  }

toClipboardEvent :: Evt -> ClipboardEvent
toClipboardEvent (evtObj -> o) = let err = error "Invalid Clipboard Event." in
  ClipboardEvent
    { eventObject = o
    , clipboardData = maybe err toDataTransfer (o .# "clipboardData")
    }

newtype Cut = Cut ClipboardEvent

cutWith :: Options -> (Cut -> IO ()) -> View -> View
cutWith opts f = OnWith opts "cut" (f . Cut . toClipboardEvent)

cut :: View -> (Producer Cut => View)
cut = cutWith def yield

cuts :: (Exists Cut => IO ()) -> View -> View
cuts f = events @Cut f cut

newtype Copy = Copy ClipboardEvent

copyWith :: Options -> (Copy -> IO ()) -> View -> View
copyWith opts f = OnWith opts "copy" (f . Copy . toClipboardEvent)

copy :: View -> (Producer Copy => View)
copy = copyWith def yield

copies :: (Exists Copy => IO ()) -> View -> View
copies f = events @Copy f Web.Events.Clipboard.copy

newtype Paste = Paste ClipboardEvent

pasteWith :: Options -> (Paste -> IO ()) -> View -> View
pasteWith opts f = OnWith opts "paste" (f . Paste . toClipboardEvent)

paste :: View -> (Producer Paste => View)
paste = pasteWith def yield

pastes :: (Exists Paste => IO ()) -> View -> View
pastes f = events @Paste f paste
