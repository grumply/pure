{-# language PatternSynonyms, RankNTypes, FlexibleContexts, OverloadedStrings, ViewPatterns, TypeApplications #-}
module Web.Events.Wheel where

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
import Data.Txt as Txt
import Data.View
import System.IO.Unsafe

data DeltaMode = Pixel | Line | Page

data WheelEvent = WheelEvent
  { eventObject :: JSV 
  , deltaX :: Double
  , deltaY :: Double
  , deltaZ :: Double
  , deltaMode :: DeltaMode
  }

toWheelEvent :: Evt -> WheelEvent
toWheelEvent (evtObj -> o) = let err = error "Invalid Wheel Event." in
  WheelEvent
    { eventObject = o
    , deltaX = fromMaybe err (o .# "deltaX")
    , deltaY = fromMaybe err (o .# "deltaY")
    , deltaZ = fromMaybe err (o .# "deltaZ")
    , deltaMode =
      case fromMaybe err (o .# "deltaMode") :: Int of
        0 -> Pixel
        1 -> Line
        2 -> Page
        _ -> err
    }

newtype Wheel = Wheel WheelEvent

wheelWith :: Options -> (Wheel -> IO ()) -> View -> View
wheelWith opts f = OnWith opts "wheel" (f . Wheel . toWheelEvent)

wheel :: View -> (Producer Wheel => View)
wheel = wheelWith def yield

wheels :: (Exists Wheel => IO ()) -> View -> View
wheels f = events @Wheel f wheel


