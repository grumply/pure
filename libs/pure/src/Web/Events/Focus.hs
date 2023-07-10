{-# language PatternSynonyms, RankNTypes, FlexibleContexts, OverloadedStrings, ViewPatterns, CPP, TypeApplications #-}
module Web.Events.Focus where

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

data FocusEvent = FocusEvent
  { eventObject :: JSV
  , relatedTarget :: Maybe Node
  }

toFocusEvent :: Evt -> FocusEvent
toFocusEvent (evtObj -> o) =
  FocusEvent
    { eventObject = o
    , relatedTarget = fmap (coerce :: JSV -> Node) (o .# "relatedTarget") 
    }

newtype Focus = Focus FocusEvent

focusWith :: Options -> (Focus -> IO ()) -> View -> View
focusWith opts f = OnWith opts "focus" (f . Focus . toFocusEvent)

focus :: View -> (Producer Focus => View)
focus = focusWith def yield

focuses :: (Exists Focus => IO ()) -> View -> View
focuses f = events @Focus f focus

newtype Blur = Blur FocusEvent

blurWith :: Options -> (Blur -> IO ()) -> View -> View
blurWith opts f = OnWith opts "blur" (f . Blur . toFocusEvent)

blur :: View -> (Producer Blur => View)
blur = blurWith def yield

blurs :: (Exists Blur => IO ()) -> View -> View
blurs f = events @Blur f blur

newtype FocusIn = FocusIn FocusEvent

focusInWith :: Options -> (FocusIn -> IO ()) -> View -> View
focusInWith opts f = OnWith opts "focusin" (f . FocusIn . toFocusEvent)

focusIn :: View -> (Producer FocusIn => View)
focusIn = focusInWith def yield

focusIns :: (Exists FocusIn => IO ()) -> View -> View
focusIns f = events @FocusIn f focusIn

newtype FocusOut = FocusOut FocusEvent

focusOutWith :: Options -> (FocusOut -> IO ()) -> View -> View
focusOutWith opts f = OnWith opts "focusout" (f . FocusOut . toFocusEvent)

focusOut :: View -> (Producer FocusOut => View)
focusOut = focusOutWith def yield

focusOuts :: (Exists FocusOut => IO ()) -> View -> View
focusOuts f = events @FocusOut f focusOut
