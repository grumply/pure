{-# language PatternSynonyms, RankNTypes, FlexibleContexts, OverloadedStrings, ViewPatterns, DuplicateRecordFields, TypeApplications #-}
module Web.Events.Mouse where

import Control.Arrow
import Control.Producer
import Control.Monad
import Data.Coerce
import Data.Default
import Data.DOM
import Data.Effect ((#))
import Data.Events (pattern OnWith)
import Data.Exists
import Data.Maybe
import Data.View
import System.IO.Unsafe
import Prelude hiding (Either(..))
import Data.Bits

data Button = Primary | Auxiliary | Secondary | Back | Forward | Other Int

data MouseEvent = MouseEvent
  { eventObject :: JSV
  , target :: Node
  , relatedTarget :: Maybe Node
  , clientX :: Double
  , clientY :: Double
  , screenX :: Double
  , screenY :: Double
  , pageX :: Double
  , pageY :: Double
  , offsetX :: Double
  , offsetY :: Double
  , movementX :: Double
  , movementY :: Double
  , altKey :: Bool
  , metaKey :: Bool
  , ctrlKey :: Bool
  , shiftKey :: Bool
  , button :: Button
  , buttons :: [Button]
  }

toMouseEvent :: Evt -> MouseEvent
toMouseEvent (evtObj -> o) = let err = error "Invalid MouseEvent." in
  MouseEvent 
    { eventObject = o
    , target = maybe err (coerce :: JSV -> Node) (o .# "target")
    , relatedTarget = fmap (coerce :: JSV -> Node) (o .# "relatedTarget")
    , clientX = fromMaybe err (o .# "clientX")
    , clientY = fromMaybe err (o .# "clientY")
    , pageX = fromMaybe err (o .# "pageX")
    , pageY = fromMaybe err (o .# "pageY")
    , screenX = fromMaybe err (o .# "screenX")
    , screenY = fromMaybe err (o .# "screenY")
    , offsetX = fromMaybe err (o .# "offsetX")
    , offsetY = fromMaybe err (o .# "offsetY")
    , movementX = fromMaybe err (o .# "movementX")
    , movementY = fromMaybe err (o .# "movementY")
    , altKey = fromMaybe err (o .# "altKey")
    , metaKey = fromMaybe err (o .# "metaKey")
    , ctrlKey = fromMaybe err (o .# "ctrlKey")
    , shiftKey = fromMaybe err (o .# "shiftKey")
    , button = 
        case fromMaybe err (o .# "button") of
          0 -> Primary
          1 -> Auxiliary
          2 -> Secondary
          3 -> Back
          4 -> Forward
          x -> Other x
    , buttons = 
        case fromMaybe err (o .# "buttons") :: Int of
          n -> let bit i r = if testBit n i then Just r else Nothing in
            catMaybes 
              [ bit 0 Primary
              , bit 1 Auxiliary
              , bit 2 Secondary
              , bit 3 Back
              , bit 4 Forward
              ]
    }

newtype Click = Click MouseEvent

clickWith :: Options -> (Click -> IO ()) -> View -> View
clickWith opts f = OnWith opts "click" (f . Click . toMouseEvent)

click :: View -> (Producer Click => View)
click = clickWith def yield

clicks :: (Exists Click => IO ()) -> View -> View
clicks f = events @Click f click

newtype DoubleClick = DoubleClick MouseEvent

doubleClickWith :: Options -> (DoubleClick -> IO ()) -> View -> View
doubleClickWith opts f = OnWith opts "dblclick" (f . DoubleClick . toMouseEvent)

doubleClick :: View -> (Producer DoubleClick => View)
doubleClick = doubleClickWith def yield

doubleClicks :: (Exists DoubleClick => IO ()) -> View -> View
doubleClicks f = events @DoubleClick f doubleClick

newtype MouseOver = MouseOver MouseEvent

mouseOverWith :: Options -> (MouseOver -> IO ()) -> View -> View
mouseOverWith opts f = OnWith opts "mouseover" (f . MouseOver . toMouseEvent)

mouseOver :: View -> (Producer MouseOver => View)
mouseOver = mouseOverWith def yield

mouseOvers :: (Exists MouseOver => IO ()) -> View -> View
mouseOvers f = events @MouseOver f mouseOver

newtype MouseEnter = MouseEnter MouseEvent

mouseEnterWith :: Options -> (MouseEnter -> IO ()) -> View -> View
mouseEnterWith opts f = OnWith opts "mouseenter" (f . MouseEnter . toMouseEvent)

mouseEnter :: View -> (Producer MouseEnter => View)
mouseEnter = mouseEnterWith def yield

mouseEnters :: (Exists MouseEnter => IO ()) -> View -> View
mouseEnters f = events @MouseEnter f mouseEnter

newtype MouseMove = MouseMove MouseEvent

mouseMoveWith :: Options -> (MouseMove -> IO ()) -> View -> View
mouseMoveWith opts f = OnWith opts "mousemove" (f . MouseMove . toMouseEvent)

mouseMove :: View -> (Producer MouseMove => View)
mouseMove = mouseMoveWith def yield

mouseMoves :: (Exists MouseMove => IO ()) -> View -> View
mouseMoves f = events @MouseMove f mouseMove

newtype MouseOut = MouseOut MouseEvent

mouseOutWith :: Options -> (MouseOut -> IO ()) -> View -> View
mouseOutWith opts f = OnWith opts "mouseout" (f . MouseOut . toMouseEvent)

mouseOut :: View -> (Producer MouseOut => View)
mouseOut = mouseOutWith def yield

mouseOuts :: (Exists MouseOut => IO ()) -> View -> View
mouseOuts f = events @MouseOut f mouseOut

newtype MouseLeave = MouseLeave MouseEvent

mouseLeaveWith :: Options -> (MouseLeave -> IO ()) -> View -> View
mouseLeaveWith opts f = OnWith opts "mouseleave" (f . MouseLeave . toMouseEvent)

mouseLeave :: View -> (Producer MouseLeave => View)
mouseLeave = mouseLeaveWith def yield

mouseLeaves :: (Exists MouseLeave => IO ()) -> View -> View
mouseLeaves f = events @MouseLeave f mouseLeave

newtype ContextMenu = ContextMenu MouseEvent

contextMenuWith :: Options -> (ContextMenu -> IO ()) -> View -> View
contextMenuWith opts f = OnWith opts "contextmenu" (f . ContextMenu . toMouseEvent)

contextMenu :: View -> (Producer ContextMenu => View)
contextMenu = contextMenuWith def yield

contextMenus :: (Exists ContextMenu => IO ()) -> View -> View
contextMenus f = events @ContextMenu f contextMenu


