{-# language PatternSynonyms, RankNTypes, FlexibleContexts, OverloadedStrings, ViewPatterns, CPP, DuplicateRecordFields, TypeApplications #-}
module Web.Events.Selection where

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

data SelectionEvent = SelectionEvent
  { eventObject :: JSV
  }

toSelectionEvent :: Evt -> SelectionEvent
toSelectionEvent (evtObj -> o) =
  SelectionEvent
    { eventObject = o }

newtype Select = Select SelectionEvent

selectWith :: Options -> (Select -> IO ()) -> View -> View
selectWith opts f = OnWith opts "select" (f . Select . toSelectionEvent)

select :: View -> (Producer Select => View)
select = selectWith def yield

selects :: (Exists Select => IO ()) -> View -> View
selects f = events @Select f select


