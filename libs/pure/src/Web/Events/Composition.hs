{-# language PatternSynonyms, RankNTypes, FlexibleContexts, OverloadedStrings, ViewPatterns, CPP, TypeApplications #-}
module Web.Events.Composition where

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

data CompositionEvent = CompositionEvent
  { eventObject :: JSV
  , dat :: Txt
  }

toCompositionEvent :: Evt -> CompositionEvent
toCompositionEvent (evtObj -> o) = let err = error "Invalid Composition Event." in
  CompositionEvent
    { eventObject = o
    , dat = fromMaybe err (o .# "data")
    }

newtype CompositionStart = CompositionStart CompositionEvent

compositionStartWith :: Options -> (CompositionStart -> IO ()) -> View -> View
compositionStartWith opts f = OnWith opts "compositionstart" (f . CompositionStart . toCompositionEvent)

compositionStart :: View -> (Producer CompositionStart => View)
compositionStart = compositionStartWith def yield

compositionStarts :: (Exists CompositionStart => IO ()) -> View -> View
compositionStarts f = events @CompositionStart f compositionStart

newtype CompositionUpdate = CompositionUpdate CompositionEvent

compositionUpdateWith :: Options -> (CompositionUpdate -> IO ()) -> View -> View
compositionUpdateWith opts f = OnWith opts "compositionupdate" (f . CompositionUpdate . toCompositionEvent)

compositionUpdate :: View -> (Producer CompositionUpdate => View)
compositionUpdate = compositionUpdateWith def yield

compositionUpdates :: (Exists CompositionUpdate => IO ()) -> View -> View
compositionUpdates f = events @CompositionUpdate f compositionUpdate

newtype CompositionEnd = CompositionEnd CompositionEvent

compositionEndWith :: Options -> (CompositionEnd -> IO ()) -> View -> View
compositionEndWith opts f = OnWith opts "compositionend" (f . CompositionEnd . toCompositionEvent)

compositionEnd :: View -> (Producer CompositionEnd => View)
compositionEnd = compositionEndWith def yield

compositionEnds :: (Exists CompositionEnd => IO ()) -> View -> View
compositionEnds f = events @CompositionEnd f compositionEnd

