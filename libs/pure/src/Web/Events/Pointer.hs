{-# language PatternSynonyms, RankNTypes, FlexibleContexts, OverloadedStrings, ViewPatterns, DuplicateRecordFields, TypeApplications #-}
module Web.Events.Pointer where

import Control.Arrow
import Control.Producer
import Control.Monad
import Data.Coerce
import Data.Default
import Data.DOM
import Data.Effect ((#))
import Data.Events (pattern OnWith)
import Data.Exists
import Data.Txt
import Data.Maybe
import Data.View
import System.IO.Unsafe
import Prelude hiding (Either(..))
import Data.Bits

data PointerType = Mouse | Pen | Touch

data PointerEvent = PointerEvent
  { eventObject :: JSV
  , pointerId :: Int
  , width :: Double
  , height :: Double
  , pressure :: Double
  , tangentialPressure :: Double
  , tiltX :: Double
  , tiltY :: Double
  , twist :: Double
  , isPrimary :: Bool
  , pointerType :: PointerType
  }

toPointerEvent :: Evt -> PointerEvent
toPointerEvent (evtObj -> o) = let err = error "Invalid Pointer Event." in
  PointerEvent
    { eventObject = o
    , pointerId = fromMaybe err (o .# "pointerId")
    , width = fromMaybe err (o .# "width")
    , height = fromMaybe err (o .# "height") 
    , pressure = fromMaybe err (o .# "pressure")
    , tangentialPressure = fromMaybe err (o .# "tangentialPressure")
    , tiltX = fromMaybe err (o .# "tiltX")
    , tiltY = fromMaybe err (o .# "tiltY")
    , twist = fromMaybe err (o .# "twist")
    , isPrimary = fromMaybe err (o .# "isPrimary")
    , pointerType =
        case fromMaybe err (o .# "pointerType") :: Txt of
          "mouse" -> Mouse
          "pen" -> Pen
          "touch" -> Touch
          _ -> err
    }

newtype PointerOver = PointerOver PointerEvent

pointerOverWith :: Options -> (PointerOver -> IO ()) -> View -> View
pointerOverWith opts f = OnWith opts "pointerover" (f . PointerOver . toPointerEvent)

pointerOver :: View -> (Producer PointerOver => View)
pointerOver = pointerOverWith def yield

pointerOvers :: (Exists PointerOver => IO ()) -> View -> View
pointerOvers f = events @PointerOver f pointerOver

newtype PointerEnter = PointerEnter PointerEvent

pointerEnterWith :: Options -> (PointerEnter -> IO ()) -> View -> View
pointerEnterWith opts f = OnWith opts "pointerenter" (f . PointerEnter . toPointerEvent)

pointerEnter :: View -> (Producer PointerEnter => View)
pointerEnter = pointerEnterWith def yield

pointerEnters :: (Exists PointerEnter => IO ()) -> View -> View
pointerEnters f = events @PointerEnter f pointerEnter

newtype PointerDown = PointerDown PointerEvent

pointerDownWith :: Options -> (PointerDown -> IO ()) -> View -> View
pointerDownWith opts f = OnWith opts "pointerdown" (f . PointerDown . toPointerEvent)

pointerDown :: View -> (Producer PointerDown => View)
pointerDown = pointerDownWith def yield

pointerDowns :: (Exists PointerDown => IO ()) -> View -> View
pointerDowns f = events @PointerDown f pointerDown

newtype PointerMove = PointerMove PointerEvent

pointerMoveWith :: Options -> (PointerMove -> IO ()) -> View -> View
pointerMoveWith opts f = OnWith opts "pointermove" (f . PointerMove . toPointerEvent)

pointerMove :: View -> (Producer PointerMove => View)
pointerMove = pointerMoveWith def yield

pointerMoves :: (Exists PointerMove => IO ()) -> View -> View
pointerMoves f = events @PointerMove f pointerMove

newtype PointerRawUpdate = PointerRawUpdate PointerEvent

pointerRawUpdateWith :: Options -> (PointerRawUpdate -> IO ()) -> View -> View
pointerRawUpdateWith opts f = OnWith opts "pointerrawupdate" (f . PointerRawUpdate . toPointerEvent)

pointerRawUpdate :: View -> (Producer PointerRawUpdate => View)
pointerRawUpdate = pointerRawUpdateWith def yield

pointerRawUpdates :: (Exists PointerRawUpdate => IO ()) -> View -> View
pointerRawUpdates f = events @PointerRawUpdate f pointerRawUpdate

newtype PointerUp = PointerUp PointerEvent

pointerUpWith :: Options -> (PointerUp -> IO ()) -> View -> View
pointerUpWith opts f = OnWith opts "pointerup" (f . PointerUp . toPointerEvent)

pointerUp :: View -> (Producer PointerUp => View)
pointerUp = pointerUpWith def yield

pointerUps :: (Exists PointerUp => IO ()) -> View -> View
pointerUps f = events @PointerUp f pointerUp

newtype PointerCancel = PointerCancel PointerEvent

pointerCancelWith :: Options -> (PointerCancel -> IO ()) -> View -> View
pointerCancelWith opts f = OnWith opts "pointercancel" (f . PointerCancel . toPointerEvent)

pointerCancel :: View -> (Producer PointerCancel => View)
pointerCancel = pointerCancelWith def yield

pointerCancels :: (Exists PointerCancel => IO ()) -> View -> View
pointerCancels f = events @PointerCancel f pointerCancel

newtype PointerOut = PointerOut PointerEvent

pointerOutWith :: Options -> (PointerOut -> IO ()) -> View -> View
pointerOutWith opts f = OnWith opts "pointerout" (f . PointerOut . toPointerEvent)

pointerOut :: View -> (Producer PointerOut => View)
pointerOut = pointerOutWith def yield

pointerOuts :: (Exists PointerOut => IO ()) -> View -> View
pointerOuts f = events @PointerOut f pointerOut

newtype PointerLeave = PointerLeave PointerEvent

pointerLeaveWith :: Options -> (PointerLeave -> IO ()) -> View -> View
pointerLeaveWith opts f = OnWith opts "pointerleave" (f . PointerLeave . toPointerEvent)

pointerLeave :: View -> (Producer PointerLeave => View)
pointerLeave = pointerLeaveWith def yield

pointerLeaves :: (Exists PointerLeave => IO ()) -> View -> View
pointerLeaves f = events @PointerLeave f pointerLeave

newtype GotPointerCapture = GotPointerCapture PointerEvent

gotPointerCaptureWith :: Options -> (GotPointerCapture -> IO ()) -> View -> View
gotPointerCaptureWith opts f = OnWith opts "gotpointercapture" (f . GotPointerCapture . toPointerEvent)

gotPointerCapture :: View -> (Producer GotPointerCapture => View)
gotPointerCapture = gotPointerCaptureWith def yield

gotPointerCaptures :: (Exists GotPointerCapture => IO ()) -> View -> View
gotPointerCaptures f = events @GotPointerCapture f gotPointerCapture

newtype LostPointerCapture = LostPointerCapture PointerEvent

lostPointerCaptureWith :: Options -> (LostPointerCapture -> IO ()) -> View -> View
lostPointerCaptureWith opts f = OnWith opts "lostpointercapture" (f . LostPointerCapture . toPointerEvent)

lostPointerCapture :: View -> (Producer LostPointerCapture => View)
lostPointerCapture = lostPointerCaptureWith def yield

lostPointerCaptures :: (Exists LostPointerCapture => IO ()) -> View -> View
lostPointerCaptures f = events @LostPointerCapture f lostPointerCapture


