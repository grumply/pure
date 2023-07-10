{-# language PatternSynonyms, RankNTypes, FlexibleContexts, OverloadedStrings, ViewPatterns, CPP, DuplicateRecordFields, TypeApplications #-}
module Web.Events.Scroll where

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

data ScrollEvent = ScrollEvent
  { eventObject :: JSV 
  }

toScrollEvent :: Evt -> ScrollEvent
toScrollEvent (evtObj -> o) = ScrollEvent { eventObject = o }

newtype Scroll = Scroll ScrollEvent

scrollWith :: Options -> (Scroll -> IO ()) -> View -> View
scrollWith opts f = OnWith opts "scroll" (f . Scroll . toScrollEvent)

scroll :: View -> (Producer Scroll => View)
scroll = scrollWith def yield

scrolls :: (Exists Scroll => IO ()) -> View -> View
scrolls f = events @Scroll f scroll

newtype ScrollEnd = ScrollEnd ScrollEvent

scrollEndWith :: Options -> (ScrollEnd -> IO ()) -> View -> View
scrollEndWith opts f = OnWith opts "scrollend" (f . ScrollEnd . toScrollEvent)

scrollEnd :: View -> (Producer ScrollEnd => View)
scrollEnd = scrollEndWith def yield

scrollEnds :: (Exists ScrollEnd => IO ()) -> View -> View
scrollEnds f = events @ScrollEnd f scrollEnd


