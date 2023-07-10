{-# language PatternSynonyms, RankNTypes, FlexibleContexts, OverloadedStrings, ViewPatterns, TypeApplications #-}
module Web.Events.Keyboard where

import Control.Arrow
import Control.Producer
import Control.Monad
import Data.Coerce
import Data.Char (isDigit,isPunctuation)
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
import Text.Read hiding (Symbol)
import Prelude hiding (repeat)

data KeyboardEvent = KeyboardEvent
  { eventObject :: JSV
  , location :: Int -- Not worth the overhead to abstract.
  , altKey :: Bool
  , ctrlKey :: Bool
  , metaKey :: Bool
  , shiftKey :: Bool
  , code :: Txt
  , key :: Txt
  , isComposing :: Bool
  , repeat :: Bool
  }

toKeyboardEvent :: Evt -> KeyboardEvent
toKeyboardEvent (evtObj -> o) = let err = error "Invalid Keyboard Event." in
  KeyboardEvent
    { eventObject = o
    , location = fromMaybe def (o .# "location")
    , altKey = fromMaybe def (o .# "altKey")
    , ctrlKey = fromMaybe def (o .# "ctrlKey")
    , metaKey = fromMaybe def (o .# "metaKey")
    , shiftKey = fromMaybe def (o .# "shiftKey")
    , code = fromMaybe def (o .# "code")
    , key = fromMaybe def (o .# "key")
    , isComposing = fromMaybe def (o .# "isComposing")
    , repeat = fromMaybe def (o .# "repeat")
    }   

newtype KeyUp = KeyUp KeyboardEvent

keyUpWith :: Options -> (KeyUp -> IO ()) -> View -> View
keyUpWith opts f = OnWith opts "keyup" (f . KeyUp . toKeyboardEvent)

keyUp :: View -> (Producer KeyUp => View)
keyUp = keyUpWith def yield

keyUps :: (Exists KeyUp => IO ()) -> View -> View
keyUps f = events @KeyUp f keyUp

newtype KeyPress = KeyPress KeyboardEvent

keyPressWith :: Options -> (KeyPress -> IO ()) -> View -> View
keyPressWith opts f = OnWith opts "keypress" (f . KeyPress . toKeyboardEvent)

keyPress :: View -> (Producer KeyPress => View)
keyPress = keyPressWith def yield

keyPresses :: (Exists KeyPress => IO ()) -> View -> View
keyPresses f = events @KeyPress f keyPress

newtype KeyDown = KeyDown KeyboardEvent

keyDownWith :: Options -> (KeyDown -> IO ()) -> View -> View
keyDownWith opts f = OnWith opts "keydown" (f . KeyDown . toKeyboardEvent)

keyDown :: View -> (Producer KeyDown => View)
keyDown = keyDownWith def yield

keyDowns :: (Exists KeyDown => IO ()) -> View -> View
keyDowns f = events @KeyDown f keyDown

-- Modifiers

pattern Modifier k <- (id &&& modifierKey -> (k,True))

modifierKey :: Txt -> Bool
modifierKey = (`elem` keys)
  where
    keys = 
      [Alt,AltGraph,CapsLock,Control
      ,Fn,FnLock,Hyper,Meta,NumLock
      ,ScrollLock,Shift,Super,Symbol
      ,SymbolLock
      ]

pattern Alt = "Alt"
pattern AltGraph = "AltGraph"
pattern CapsLock = "CapsLock"
pattern Control = "Control"
pattern Fn = "Fn"
pattern FnLock = "FnLock"
pattern Hyper = "Hyper"
pattern Meta = "Meta"
pattern OS = "OS"
pattern NumLock = "NumLock"
pattern ScrollLock = "ScrollLock"
pattern Shift = "Shift"
pattern Super = "Super"
pattern Symbol = "Symbol"
pattern SymbolLock = "SymbolLock"

-- Whitespace

pattern Whitespace k <- (id &&& whitespaceKey -> (k,True))

whitespaceKey :: Txt -> Bool
whitespaceKey = (`elem` keys)
  where
    keys = [Enter,Tab,Space]

pattern Enter = "Enter"
pattern Tab = "Tab"
pattern Space = " "

-- Navigation

pattern Arrow k <- (id &&& arrowKey -> (k,True))
pattern Navigation k <- (id &&& navigationKey -> (k,True))

navigationKey :: Txt -> Bool
navigationKey = (`elem` keys)
  where
    keys =
      [ArrowLeft,ArrowRight,ArrowDown,ArrowUp
      ,End,Home,PageDown,PageUp
      ]

arrowKey :: Txt -> Bool
arrowKey = (`elem` keys)
  where
    keys =
      [ArrowLeft,ArrowRight,ArrowDown,ArrowUp]

pattern ArrowDown = "ArrowDown"
pattern ArrowLeft = "ArrowLeft"
pattern ArrowRight = "ArrowRight"
pattern ArrowUp = "ArrowUp"
pattern End = "End"
pattern Home = "Home"
pattern PageDown = "PageDown"
pattern PageUp = "PageUp"

-- Editing

pattern Editing k <- (id &&& editingKey -> (k,True))

editingKey :: Txt -> Bool
editingKey = (`elem` keys)
  where
    keys = 
      [Backspace,Clear,Copy,CrSel
      ,Cut,Delete,EraseEOF,ExSel
      ,Insert,Paste,Redo,Undo
      ]

pattern Backspace = "Backspace"
pattern Clear = "Clear"
pattern Copy = "Copy"
pattern CrSel = "CrSel"
pattern Cut = "Cut"
pattern Delete = "Delete"
pattern EraseEOF = "EraseEof"
pattern ExSel = "ExSel"
pattern Insert = "Insert"
pattern Paste = "Paste"
pattern Redo = "Redo"
pattern Undo = "Undo"

-- UI Keys

pattern UI k <- (id &&& uiKey -> (k,True))

uiKey :: Txt -> Bool
uiKey = (`elem` keys)
  where
    keys = 
      [Accept,Again,Attn,Cancel,ContextMenu
      ,Escape,Execute,Find,Finish,Help,Pause
      ,Play,Props,Select,ZoomIn,ZoomOut
      ]

pattern Accept = "Accept"
pattern Again = "Again"
pattern Attn = "Attn"
pattern Cancel = "Cancel"
pattern ContextMenu = "ContextMenu"
pattern Escape = "Escape"
pattern Execute = "Execute"
pattern Find = "Find"
pattern Finish = "Finish"
pattern Help = "Help"
pattern Pause = "Pause"
pattern Play = "Play"
pattern Props = "Props"
pattern Select = "Select"
pattern ZoomIn = "ZoomIn"
pattern ZoomOut = "ZoomOut"

-- Number

pattern Number n <- (numberKey &&& (readMaybe @Int . fromTxt) -> (True,Just n))

numberKey :: Txt -> Bool
numberKey k = Txt.length k == 1 && Txt.all isDigit k 

-- Letter

pattern Letter l <- (letterKey &&& (Txt.head . Txt.drop 3) -> (True,l))

letterKey :: Txt -> Bool
letterKey k = Txt.length k == 4 && "Key" `Txt.isPrefixOf` k

-- Function

pattern Function n <- (functionKey &&& (readMaybe @Int . fromTxt . Txt.tail) -> (True,Just n))

functionKey :: Txt -> Bool
functionKey k = "F" `Txt.isPrefixOf` k && Txt.all isDigit (Txt.tail k)

-- Punctuation

pattern Punctuation p <- (punctuationKey &&& id -> (True,p))

punctuationKey :: Txt -> Bool
punctuationKey = isPunctuation . Txt.head

-- Misc

pattern Dead = "Dead"
pattern Unidentified = "Unidentified"

pattern Other :: Txt -> Txt
pattern Other k = k
