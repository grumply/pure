{-# language PatternSynonyms, RankNTypes, FlexibleContexts, OverloadedStrings, ViewPatterns, CPP, DuplicateRecordFields, TypeApplications #-}
module Web.Events.Input where

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
import Web.DataTransfer
import Web.Range

#ifdef __GHCJS__
import GHCJS.Marshal.Internal
#endif

data InputEvent = InputEvent
  { eventObject :: JSV
  , dat :: Txt
  , value :: Txt
  , dataTransfer :: DataTransfer
  , inputType :: Txt
  , isComposing :: Bool
  , targetRanges :: [Range]
  }

toInputEvent :: Evt -> InputEvent
toInputEvent (evtObj -> o) = let err = error "Invalid InputEvent" in
  InputEvent
    { eventObject = o 
    , dat = fromMaybe err (o .# "data")
    , value = fromMaybe "" (o .# "target" >>= (.# "value"))
    , dataTransfer = maybe err toDataTransfer (o .# "dataTransfer")
    , inputType = fromMaybe err (o .# "inputType")
    , isComposing = fromMaybe err (o .# "isComposing")
#ifdef __GHCJS__
    , targetRanges = fmap toRange (maybe err (unsafePerformIO . fromJSValUncheckedListOf) (o .# "targetRanges"))
#else
    , targetRanges = []
#endif
    }

newtype BeforeInput = BeforeInput InputEvent

beforeInputWith :: Options -> (BeforeInput -> IO ()) -> View -> View
beforeInputWith opts f = OnWith opts "beforeinput" (f . BeforeInput . toInputEvent)

beforeInput :: View -> (Producer BeforeInput => View)
beforeInput = beforeInputWith def yield

beforeInputs :: (Exists BeforeInput => IO ()) -> View -> View
beforeInputs f = events @BeforeInput f beforeInput

newtype Input = In InputEvent

inputWith :: Options -> (Input -> IO ()) -> View -> View
inputWith opts f = OnWith opts "input" (f . In . toInputEvent)

input :: View -> (Producer Input => View)
input = inputWith def yield

inputs :: (Exists Input => IO ()) -> View -> View
inputs f = events @Input f input

