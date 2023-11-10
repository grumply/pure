{-# language PatternSynonyms, RankNTypes, FlexibleContexts, OverloadedStrings, ViewPatterns, TypeApplications, CPP, DuplicateRecordFields, LambdaCase, BlockArguments, RecordWildCards, MagicHash, NamedFieldPuns #-}
{-# OPTIONS_GHC -O2 #-}
module Web.Events where

import Control.Arrow ((&&&))
import Control.Concurrent hiding (yield)
import Control.Monad
import Data.Bits
import Data.Char (isDigit,isPunctuation)
import Data.Coerce
import Data.Default
import Data.DOM
import Data.Events (pattern OnWith)
import Data.File
import Data.Foldable
import Data.Function ((&),fix)
import Data.HTML ()
import Data.IORef
import Data.JSON
import Data.Maybe
import Data.Time
import Data.Traversable
import Data.Txt as Txt
import Data.View
import GHC.Exts
import GHC.Generics as G
import System.IO.Unsafe
import Text.Read (readMaybe)
import Web.DataTransfer
import Web.Range

#ifdef __GHCJS__
import GHCJS.Marshal.Internal
import JavaScript.Object.Internal as JS (Object(..),create,setProp)
#endif

data AnimationEvent = AnimationEvent
  { eventObject :: JSV
  , animationName :: Txt
  , elapsedTime :: Time
  , pseudoElement :: Txt
  }

toAnimationEvent :: Evt -> AnimationEvent
toAnimationEvent (evtObj -> o) = let err = error "Invalid Animation Event." in
  AnimationEvent
    { eventObject = o
    , animationName = fromMaybe err (o .# "animationName")
    , elapsedTime = maybe err (`Seconds` 0) (o .# "elapsedTime")
    , pseudoElement = fromMaybe err (o .# "pseudoElement")
    }

newtype AnimationStart = AnimationStart AnimationEvent

{-# INLINE animationStartWith #-}
animationStartWith :: Options -> (AnimationStart -> IO ()) -> View -> View
animationStartWith opts f = OnWith opts "animationstart" (f . AnimationStart . toAnimationEvent)

{-# INLINE animationStart #-}
animationStart :: View -> (Producer AnimationStart => View)
animationStart = animationStartWith def yield

{-# INLINE animationStarts #-}
animationStarts :: (Exists AnimationStart => IO ()) -> View -> View
animationStarts f = events @AnimationStart f animationStart

{-# INLINE onAnimationStart #-}
onAnimationStart :: (Exists AnimationStart => a) -> View -> (Producer a => View)
onAnimationStart a = animationStarts (yield a)

newtype AnimationIteration = AnimationIteration AnimationEvent

{-# INLINE animationIterationWith #-}
animationIterationWith :: Options -> (AnimationIteration -> IO ()) -> View -> View
animationIterationWith opts f = OnWith opts "animationiteration" (f . AnimationIteration . toAnimationEvent)

{-# INLINE animationIteration #-}
animationIteration :: View -> (Producer AnimationIteration => View)
animationIteration = animationIterationWith def yield

{-# INLINE animationIterations #-}
animationIterations :: (Exists AnimationIteration => IO ()) -> View -> View
animationIterations f = events @AnimationIteration f animationIteration

{-# INLINE onAnimationIteration #-}
onAnimationIteration :: (Exists AnimationIteration => a) -> View -> (Producer a => View)
onAnimationIteration a = animationIterations (yield a)

newtype AnimationCancel = AnimationCancel AnimationEvent

{-# INLINE animationCancelWith #-}
animationCancelWith :: Options -> (AnimationCancel -> IO ()) -> View -> View
animationCancelWith opts f = OnWith opts "animationcancel" (f . AnimationCancel . toAnimationEvent)

{-# INLINE animationCancel #-}
animationCancel :: View -> (Producer AnimationCancel => View)
animationCancel = animationCancelWith def yield

{-# INLINE animationCancels #-}
animationCancels :: (Exists AnimationCancel => IO ()) -> View -> View
animationCancels f = events @AnimationCancel f animationCancel

{-# INLINE onAnimationCancel #-}
onAnimationCancel :: (Exists AnimationCancel => a) -> View -> (Producer a => View)
onAnimationCancel a = animationCancels (yield a)

newtype AnimationEnd = AnimationEnd AnimationEvent

{-# INLINE animationEndWith #-}
animationEndWith :: Options -> (AnimationEnd -> IO ()) -> View -> View
animationEndWith opts f = OnWith opts "animationend" (f . AnimationEnd . toAnimationEvent)

{-# INLINE animationEnd #-}
animationEnd :: View -> (Producer AnimationEnd => View)
animationEnd = animationEndWith def yield

{-# INLINE animationEnds #-}
animationEnds :: (Exists AnimationEnd => IO ()) -> View -> View
animationEnds f = events @AnimationEnd f animationEnd

{-# INLINE onAnimationEnd #-}
onAnimationEnd :: (Exists AnimationEnd => a) -> View -> (Producer a => View)
onAnimationEnd a = animationEnds (yield a)

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

{-# INLINE cutWith #-}
cutWith :: Options -> (Cut -> IO ()) -> View -> View
cutWith opts f = OnWith opts "cut" (f . Cut . toClipboardEvent)

{-# INLINE cut #-}
cut :: View -> (Producer Cut => View)
cut = cutWith def yield

{-# INLINE cuts #-}
cuts :: (Exists Cut => IO ()) -> View -> View
cuts f = events @Cut f cut

{-# INLINE onCut #-}
onCut :: (Exists Cut => a) -> View -> (Producer a => View)
onCut a = cuts (yield a)

newtype Copy = Copy ClipboardEvent

{-# INLINE copyWith #-}
copyWith :: Options -> (Copy -> IO ()) -> View -> View
copyWith opts f = OnWith opts "copy" (f . Copy . toClipboardEvent)

{-# INLINE copy #-}
copy :: View -> (Producer Copy => View)
copy = copyWith def yield

{-# INLINE copies #-}
copies :: (Exists Copy => IO ()) -> View -> View
copies f = events @Copy f Web.Events.copy

{-# INLINE onCopy #-}
onCopy :: (Exists Copy => a) -> View -> (Producer a => View)
onCopy a = copies (yield a)

newtype Paste = Paste ClipboardEvent

{-# INLINE pasteWith #-}
pasteWith :: Options -> (Paste -> IO ()) -> View -> View
pasteWith opts f = OnWith opts "paste" (f . Paste . toClipboardEvent)

{-# INLINE paste #-}
paste :: View -> (Producer Paste => View)
paste = pasteWith def yield

{-# INLINE pastes #-}
pastes :: (Exists Paste => IO ()) -> View -> View
pastes f = events @Paste f paste

{-# INLINE onPaste #-}
onPaste :: (Exists Paste => a) -> View -> (Producer a => View)
onPaste a = pastes (yield a)

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

{-# INLINE compositionStartWith #-}
compositionStartWith :: Options -> (CompositionStart -> IO ()) -> View -> View
compositionStartWith opts f = OnWith opts "compositionstart" (f . CompositionStart . toCompositionEvent)

{-# INLINE compositionStart #-}
compositionStart :: View -> (Producer CompositionStart => View)
compositionStart = compositionStartWith def yield

{-# INLINE compositionStarts #-}
compositionStarts :: (Exists CompositionStart => IO ()) -> View -> View
compositionStarts f = events @CompositionStart f compositionStart

{-# INLINE onCompositionStart #-}
onCompositionStart :: (Exists CompositionStart => a) -> View -> (Producer a => View)
onCompositionStart a = compositionStarts (yield a)

newtype CompositionUpdate = CompositionUpdate CompositionEvent

{-# INLINE compositionUpdateWith #-}
compositionUpdateWith :: Options -> (CompositionUpdate -> IO ()) -> View -> View
compositionUpdateWith opts f = OnWith opts "compositionupdate" (f . CompositionUpdate . toCompositionEvent)

{-# INLINE compositionUpdate #-}
compositionUpdate :: View -> (Producer CompositionUpdate => View)
compositionUpdate = compositionUpdateWith def yield

{-# INLINE compositionUpdates #-}
compositionUpdates :: (Exists CompositionUpdate => IO ()) -> View -> View
compositionUpdates f = events @CompositionUpdate f compositionUpdate

{-# INLINE onCompositionUpdate #-}
onCompositionUpdate :: (Exists CompositionUpdate => a) -> View -> (Producer a => View)
onCompositionUpdate a = compositionUpdates (yield a)

newtype CompositionEnd = CompositionEnd CompositionEvent

{-# INLINE compositionEndWith #-}
compositionEndWith :: Options -> (CompositionEnd -> IO ()) -> View -> View
compositionEndWith opts f = OnWith opts "compositionend" (f . CompositionEnd . toCompositionEvent)

{-# INLINE compositionEnd #-}
compositionEnd :: View -> (Producer CompositionEnd => View)
compositionEnd = compositionEndWith def yield

{-# INLINE compositionEnds #-}
compositionEnds :: (Exists CompositionEnd => IO ()) -> View -> View
compositionEnds f = events @CompositionEnd f compositionEnd

{-# INLINE onCompositionEnd #-}
onCompositionEnd :: (Exists CompositionEnd => a) -> View -> (Producer a => View)
onCompositionEnd a = compositionEnds (yield a)

data DragEvent = DragEvent
  { eventObject :: JSV
  , dataTransfer :: DataTransfer
  }

toDragEvent :: Evt -> DragEvent
toDragEvent (evtObj -> o) = let err = error "Invalid DragEvent." in
  DragEvent
    { eventObject = o 
    , dataTransfer = maybe err toDataTransfer (o .# "dataTransfer")
    }

newtype Drag = Drag DragEvent

{-# INLINE dragWith #-}
dragWith :: Options -> (Drag -> IO ()) -> View -> View
dragWith opts f = OnWith opts "drag" (f . Drag . toDragEvent)

{-# INLINE drag #-}
drag :: View -> (Producer Drag => View)
drag = dragWith def yield

{-# INLINE drags #-}
drags :: (Exists Drag => IO ()) -> View -> View
drags f = events @Drag f drag

{-# INLINE onDrag #-}
onDrag :: (Exists Drag => a) -> View -> (Producer a => View)
onDrag a = drags (yield a)

newtype DragEnd = DragEnd DragEvent

{-# INLINE dragEndWith #-}
dragEndWith :: Options -> (DragEnd -> IO ()) -> View -> View
dragEndWith opts f = OnWith opts "dragend" (f . DragEnd . toDragEvent)

{-# INLINE dragEnd #-}
dragEnd :: View -> (Producer DragEnd => View)
dragEnd = dragEndWith def yield

{-# INLINE dragEnds #-}
dragEnds :: (Exists DragEnd => IO ()) -> View -> View
dragEnds f = events @DragEnd f dragEnd

{-# INLINE onDragEnd #-}
onDragEnd :: (Exists DragEnd => a) -> View -> (Producer a => View)
onDragEnd a = dragEnds (yield a)

newtype DragEnter = DragEnter DragEvent

{-# INLINE dragEnterWith #-}
dragEnterWith :: Options -> (DragEnter -> IO ()) -> View -> View
dragEnterWith opts f = OnWith opts "dragenter" (f . DragEnter . toDragEvent)

{-# INLINE dragEnter #-}
dragEnter :: View -> (Producer DragEnter => View)
dragEnter = dragEnterWith def yield

{-# INLINE dragEnters #-}
dragEnters :: (Exists DragEnter => IO ()) -> View -> View
dragEnters f = events @DragEnter f dragEnter

{-# INLINE onDragEnter #-}
onDragEnter :: (Exists DragEnter => a) -> View -> (Producer a => View)
onDragEnter a = dragEnters (yield a)

newtype DragLeave = DragLeave DragEvent

{-# INLINE dragLeaveWith #-}
dragLeaveWith :: Options -> (DragLeave -> IO ()) -> View -> View
dragLeaveWith opts f = OnWith opts "dragleave" (f . DragLeave . toDragEvent)

{-# INLINE dragLeave #-}
dragLeave :: View -> (Producer DragLeave => View)
dragLeave = dragLeaveWith def yield

{-# INLINE dragLeaves #-}
dragLeaves :: (Exists DragLeave => IO ()) -> View -> View
dragLeaves f = events @DragLeave f dragLeave

{-# INLINE onDragLeave #-}
onDragLeave :: (Exists DragLeave => a) -> View -> (Producer a => View)
onDragLeave a = dragLeaves (yield a)

newtype DragOver = DragOver DragEvent

{-# INLINE dragOverWith #-}
dragOverWith :: Options -> (DragOver -> IO ()) -> View -> View
dragOverWith opts f = OnWith opts "dragover" (f . DragOver . toDragEvent)

{-# INLINE dragOver #-}
dragOver :: View -> (Producer DragOver => View)
dragOver = dragOverWith def yield

{-# INLINE dragOvers #-}
dragOvers :: (Exists DragOver => IO ()) -> View -> View
dragOvers f = events @DragOver f dragOver

{-# INLINE onDragOver #-}
onDragOver :: (Exists DragOver => a) -> View -> (Producer a => View)
onDragOver a = dragOvers (yield a)

newtype DragStart = DragStart DragEvent

{-# INLINE dragStartWith #-}
dragStartWith :: Options -> (DragStart -> IO ()) -> View -> View
dragStartWith opts f = OnWith opts "dragstart" (f . DragStart . toDragEvent)

{-# INLINE dragStart #-}
dragStart :: View -> (Producer DragStart => View)
dragStart = dragStartWith def yield

{-# INLINE dragStarts #-}
dragStarts :: (Exists DragStart => IO ()) -> View -> View
dragStarts f = events @DragStart f dragStart

{-# INLINE onDragStart #-}
onDragStart :: (Exists DragStart => a) -> View -> (Producer a => View)
onDragStart a = dragStarts (yield a)

newtype Drop = Drop DragEvent

{-# INLINE dropWith #-}
dropWith :: Options -> (Drop -> IO ()) -> View -> View
dropWith opts f = OnWith opts "drop" (f . Drop . toDragEvent)

{-# INLINE drop #-}
drop :: View -> (Producer Drop => View)
drop = dropWith def yield

{-# INLINE drops #-}
drops :: (Exists Drop => IO ()) -> View -> View
drops f = events @Drop f Web.Events.drop

{-# INLINE onDrop #-}
onDrop :: (Exists Drop => a) -> View -> (Producer a => View)
onDrop a = drops (yield a)

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

{-# INLINE focusWith #-}
focusWith :: Options -> (Focus -> IO ()) -> View -> View
focusWith opts f = OnWith opts "focus" (f . Focus . toFocusEvent)

{-# INLINE focus #-}
focus :: View -> (Producer Focus => View)
focus = focusWith def yield

{-# INLINE focuses #-}
focuses :: (Exists Focus => IO ()) -> View -> View
focuses f = events @Focus f focus

{-# INLINE onFocus #-}
onFocus :: (Exists Focus => a) -> View -> (Producer a => View)
onFocus a = focuses (yield a)

newtype Blur = Blur FocusEvent

{-# INLINE blurWith #-}
blurWith :: Options -> (Blur -> IO ()) -> View -> View
blurWith opts f = OnWith opts "blur" (f . Blur . toFocusEvent)

{-# INLINE blur #-}
blur :: View -> (Producer Blur => View)
blur = blurWith def yield

{-# INLINE blurs #-}
blurs :: (Exists Blur => IO ()) -> View -> View
blurs f = events @Blur f blur

{-# INLINE onBlur #-}
onBlur :: (Exists Blur => a) -> View -> (Producer a => View)
onBlur a = blurs (yield a)

newtype FocusIn = FocusIn FocusEvent

{-# INLINE focusInWith #-}
focusInWith :: Options -> (FocusIn -> IO ()) -> View -> View
focusInWith opts f = OnWith opts "focusin" (f . FocusIn . toFocusEvent)

{-# INLINE focusIn #-}
focusIn :: View -> (Producer FocusIn => View)
focusIn = focusInWith def yield

{-# INLINE focusIns #-}
focusIns :: (Exists FocusIn => IO ()) -> View -> View
focusIns f = events @FocusIn f focusIn

{-# INLINE onFocusIn #-}
onFocusIn :: (Exists FocusIn => a) -> View -> (Producer a => View)
onFocusIn a = focusIns (yield a)

newtype FocusOut = FocusOut FocusEvent

{-# INLINE focusOutWith #-}
focusOutWith :: Options -> (FocusOut -> IO ()) -> View -> View
focusOutWith opts f = OnWith opts "focusout" (f . FocusOut . toFocusEvent)

{-# INLINE focusOut #-}
focusOut :: View -> (Producer FocusOut => View)
focusOut = focusOutWith def yield

{-# INLINE focusOuts #-}
focusOuts :: (Exists FocusOut => IO ()) -> View -> View
focusOuts f = events @FocusOut f focusOut

{-# INLINE onFocusOut #-}
onFocusOut :: (Exists FocusOut => a) -> View -> (Producer a => View)
onFocusOut a = focusOuts (yield a)

data ChangeEvent = ChangeEvent
  { eventObject :: JSV
  , value :: Txt
  }

toChangeEvent :: Evt -> ChangeEvent
toChangeEvent (evtObj -> o) = let err = error "Invalid ChangeEvent" in
  ChangeEvent
    { eventObject = o
    , value = fromMaybe err (o .# "target" >>= (.# "value"))
    }

newtype Change = Change ChangeEvent

{-# INLINE changeWith #-}
changeWith :: Options -> (Change -> IO ()) -> View -> View
changeWith opts f = OnWith opts "change" (f . Change . toChangeEvent)

{-# INLINE change #-}
change :: View -> (Producer Change => View)
change = changeWith def yield

{-# INLINE changes #-}
changes :: (Exists Change => IO ()) -> View -> View
changes f = events @Change f change

{-# INLINE onChange #-}
onChange :: (Exists Change => a) -> View -> (Producer a => View)
onChange a = changes (yield a)

data InputEvent = InputEvent
  { eventObject :: JSV
  , value :: Txt
  , inputType :: Txt
  , files :: [(Txt,ByteTxt)]
  }

toInputEvent :: Evt -> InputEvent
toInputEvent (evtObj -> o) = let err = error "Invalid InputEvent" in
  InputEvent
    { eventObject = o 
    , value = fromMaybe err (o .# "target" >>= (.# "value"))
    , inputType = fromMaybe err (o .# "inputType")
    , files = maybe err (unsafePerformIO . getFiles . (coerce :: JSV -> Node)) (o .# "target")
    }

newtype BeforeInput = BeforeInput InputEvent

{-# INLINE beforeInputWith #-}
beforeInputWith :: Options -> (BeforeInput -> IO ()) -> View -> View
beforeInputWith opts f = OnWith opts "beforeinput" (f . BeforeInput . toInputEvent)

{-# INLINE beforeInput #-}
beforeInput :: View -> (Producer BeforeInput => View)
beforeInput = beforeInputWith def yield

{-# INLINE beforeInputs #-}
beforeInputs :: (Exists BeforeInput => IO ()) -> View -> View
beforeInputs f = events @BeforeInput f beforeInput

{-# INLINE onBeforeInput #-}
onBeforeInput :: (Exists BeforeInput => a) -> View -> (Producer a => View)
onBeforeInput a = beforeInputs (yield a)

newtype Input = In InputEvent

{-# INLINE inputWith #-}
inputWith :: Options -> (Input -> IO ()) -> View -> View
inputWith opts f = OnWith opts "input" (f . In . toInputEvent)

{-# INLINE input #-}
input :: View -> (Producer Input => View)
input = inputWith def yield

{-# INLINE inputs #-}
inputs :: (Exists Input => IO ()) -> View -> View
inputs f = events @Input f input

{-# INLINE onInput #-}
onInput :: (Exists Input => a) -> View -> (Producer a => View)
onInput a = inputs (yield a)

#ifdef __GHCJS__
foreign import javascript unsafe
  "var file = $1.files[$2]; var reader = new FileReader(); reader.readAsBinaryString(file); $r = reader;" get_file_reader_js :: Node -> Int -> IO JSV

foreign import javascript unsafe
  "$r = $1.files[$2].name" get_file_name_js :: Node -> Int -> IO Txt

foreign import javascript unsafe
  "$r = $1.result" get_result_js :: JSV -> IO Txt

foreign import javascript unsafe
  "$r = $1.files.length" get_file_count_js :: Node -> IO Int
#endif

getFiles :: Node -> IO [(Txt,ByteTxt)]
getFiles node = do
#ifdef __GHCJS__
  files <- get_file_count_js node
  if files > 0 then 
    unsafeInterleaveIO do
      catMaybes <$> for [0..files - 1] \n -> do
        rdr <- get_file_reader_js node n
        path <- get_file_name_js node n
        mv <- newEmptyMVar
        onRaw rdr "load" def $ \stop _ -> do
          result <- rdr ..# "result"
          putMVar mv result
          stop
        fmap (\x -> (path,unsafeTxtToByteTxt x)) <$> takeMVar mv
  else
    pure []
#else
  pure []
#endif

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

{-# INLINE keyUpWith #-}
keyUpWith :: Options -> (KeyUp -> IO ()) -> View -> View
keyUpWith opts f = OnWith opts "keyup" (f . KeyUp . toKeyboardEvent)

{-# INLINE keyUp #-}
keyUp :: View -> (Producer KeyUp => View)
keyUp = keyUpWith def yield

{-# INLINE keyUps #-}
keyUps :: (Exists KeyUp => IO ()) -> View -> View
keyUps f = events @KeyUp f keyUp

{-# INLINE onKeyUps #-}
onKeyUps :: (Exists KeyUp => a) -> View -> (Producer a => View)
onKeyUps a = keyUps (yield a)

newtype KeyPress = KeyPress KeyboardEvent

{-# INLINE keyPressWith #-}
keyPressWith :: Options -> (KeyPress -> IO ()) -> View -> View
keyPressWith opts f = OnWith opts "keypress" (f . KeyPress . toKeyboardEvent)

{-# INLINE keyPress #-}
keyPress :: View -> (Producer KeyPress => View)
keyPress = keyPressWith def yield

{-# INLINE keyPresses #-}
keyPresses :: (Exists KeyPress => IO ()) -> View -> View
keyPresses f = events @KeyPress f keyPress

{-# INLINE onKeyPress #-}
onKeyPress :: (Exists KeyPress => a) -> View -> (Producer a => View)
onKeyPress a = keyPresses (yield a)

newtype KeyDown = KeyDown KeyboardEvent

{-# INLINE keyDownWith #-}
keyDownWith :: Options -> (KeyDown -> IO ()) -> View -> View
keyDownWith opts f = OnWith opts "keydown" (f . KeyDown . toKeyboardEvent)

{-# INLINE keyDown #-}
keyDown :: View -> (Producer KeyDown => View)
keyDown = keyDownWith def yield

{-# INLINE keyDowns #-}
keyDowns :: (Exists KeyDown => IO ()) -> View -> View
keyDowns f = events @KeyDown f keyDown

{-# INLINE onKeyDown #-}
onKeyDown :: (Exists KeyDown => a) -> View -> (Producer a => View)
onKeyDown a = keyDowns (yield a)

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
      ["ArrowLeft","ArrowRight","ArrowDown","ArrowUp"
      ,"End","Home","PageDown","PageUp"
      ]

arrowKey :: Txt -> Bool
arrowKey = (`elem` keys)
  where
    keys =
      ["ArrowLeft","ArrowRight","ArrowDown","ArrowUp"]

-- Editing

pattern Editing k <- (id &&& editingKey -> (k,True))

editingKey :: Txt -> Bool
editingKey = (`elem` keys)
  where
    keys = 
      ["Backspace","Clear","Copy","CrSel"
      ,"Cut","Delete","EraseEOF","ExSel"
      ,"Insert","Paste","Redo","Undo"
      ]

-- UI Keys

pattern UI k <- (id &&& uiKey -> (k,True))

uiKey :: Txt -> Bool
uiKey = (`elem` keys)
  where
    keys = 
      ["Accept","Again","Attn","Cancel","ContextMenu"
      ,"Escape","Execute","Find","Finish","Help","Pause"
      ,"Play","Props","Select","ZoomIn","ZoomOut"
      ]

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

data Button = Primary | Auxiliary | Secondary | Back | Forward | OtherButton Int
  deriving (Eq,Ord)

data MouseEvent = MouseEvent
  { eventObject :: JSV
  , target :: Node
  , currentTarget :: Node
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
toMouseEvent e@(evtObj -> o) = let err = error "Invalid MouseEvent." in
  MouseEvent 
    { eventObject = o
    , target = maybe err (coerce :: JSV -> Node) (o .# "target")
    , currentTarget = coerce (evtTarget e)
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
          x -> OtherButton x
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

data Click = Click MouseEvent

{-# INLINE clickWith #-}
clickWith :: Options -> (Click -> IO ()) -> View -> View
clickWith opts f = OnWith opts "click" (f . Click . toMouseEvent)

{-# INLINE click #-}
click :: View -> (Producer Click => View)
click = clickWith def yield

{-# INLINE clicks #-}
clicks :: (Exists Click => IO ()) -> View -> View
clicks f = events @Click f click

{-# INLINE onClick #-}
onClick :: (Exists Click => a) -> View -> (Producer a => View)
onClick a = clicks (yield a) 

newtype DoubleClick = DoubleClick MouseEvent

{-# INLINE doubleClickWith #-}
doubleClickWith :: Options -> (DoubleClick -> IO ()) -> View -> View
doubleClickWith opts f = OnWith opts "dblclick" (f . DoubleClick . toMouseEvent)

{-# INLINE doubleClick #-}
doubleClick :: View -> (Producer DoubleClick => View)
doubleClick = doubleClickWith def yield

{-# INLINE doubleClicks #-}
doubleClicks :: (Exists DoubleClick => IO ()) -> View -> View
doubleClicks f = events @DoubleClick f doubleClick

{-# INLINE onDoubleClick #-}
onDoubleClick :: (Exists DoubleClick => a) -> View -> (Producer a => View)
onDoubleClick a = doubleClicks (yield a)

newtype MouseOver = MouseOver MouseEvent

{-# INLINE mouseOverWith #-}
mouseOverWith :: Options -> (MouseOver -> IO ()) -> View -> View
mouseOverWith opts f = OnWith opts "mouseover" (f . MouseOver . toMouseEvent)

{-# INLINE mouseOver #-}
mouseOver :: View -> (Producer MouseOver => View)
mouseOver = mouseOverWith def yield

{-# INLINE mouseOvers #-}
mouseOvers :: (Exists MouseOver => IO ()) -> View -> View
mouseOvers f = events @MouseOver f mouseOver

{-# INLINE onMouseOver #-}
onMouseOver :: (Exists MouseOver => a) -> View -> (Producer a => View)
onMouseOver a = mouseOvers (yield a)

newtype MouseEnter = MouseEnter MouseEvent

{-# INLINE mouseEnterWith #-}
mouseEnterWith :: Options -> (MouseEnter -> IO ()) -> View -> View
mouseEnterWith opts f = OnWith opts "mouseenter" (f . MouseEnter . toMouseEvent)

{-# INLINE mouseEnter #-}
mouseEnter :: View -> (Producer MouseEnter => View)
mouseEnter = mouseEnterWith def yield

{-# INLINE mouseEnters #-}
mouseEnters :: (Exists MouseEnter => IO ()) -> View -> View
mouseEnters f = events @MouseEnter f mouseEnter

{-# INLINE onMouseEnter #-}
onMouseEnter :: (Exists MouseEnter => a) -> View -> (Producer a => View)
onMouseEnter a = mouseEnters (yield a)

newtype MouseMove = MouseMove MouseEvent

{-# INLINE mouseMoveWith #-}
mouseMoveWith :: Options -> (MouseMove -> IO ()) -> View -> View
mouseMoveWith opts f = OnWith opts "mousemove" (f . MouseMove . toMouseEvent)

{-# INLINE mouseMove #-}
mouseMove :: View -> (Producer MouseMove => View)
mouseMove = mouseMoveWith def yield

{-# INLINE mouseMoves #-}
mouseMoves :: (Exists MouseMove => IO ()) -> View -> View
mouseMoves f = events @MouseMove f mouseMove

{-# INLINE onMouseMove #-}
onMouseMove :: (Exists MouseMove => a) -> View -> (Producer a => View)
onMouseMove a = mouseMoves (yield a)

newtype MouseOut = MouseOut MouseEvent

{-# INLINE mouseOutWith #-}
mouseOutWith :: Options -> (MouseOut -> IO ()) -> View -> View
mouseOutWith opts f = OnWith opts "mouseout" (f . MouseOut . toMouseEvent)

{-# INLINE mouseOut #-}
mouseOut :: View -> (Producer MouseOut => View)
mouseOut = mouseOutWith def yield

{-# INLINE mouseOuts #-}
mouseOuts :: (Exists MouseOut => IO ()) -> View -> View
mouseOuts f = events @MouseOut f mouseOut

{-# INLINE onMouseOut #-}
onMouseOut :: (Exists MouseOut => a) -> View -> (Producer a => View)
onMouseOut a = mouseOuts (yield a)

newtype MouseLeave = MouseLeave MouseEvent

{-# INLINE mouseLeaveWith #-}
mouseLeaveWith :: Options -> (MouseLeave -> IO ()) -> View -> View
mouseLeaveWith opts f = OnWith opts "mouseleave" (f . MouseLeave . toMouseEvent)

{-# INLINE mouseLeave #-}
mouseLeave :: View -> (Producer MouseLeave => View)
mouseLeave = mouseLeaveWith def yield

{-# INLINE mouseLeaves #-}
mouseLeaves :: (Exists MouseLeave => IO ()) -> View -> View
mouseLeaves f = events @MouseLeave f mouseLeave

{-# INLINE onMouseLeave #-}
onMouseLeave :: (Exists MouseLeave => a) -> View -> (Producer a => View)
onMouseLeave a = mouseLeaves (yield a)

newtype MouseDown = MouseDown MouseEvent

{-# INLINE mouseDownWith #-}
mouseDownWith :: Options -> (MouseDown -> IO ()) -> View -> View
mouseDownWith opts f = OnWith opts "mousedown" (f . MouseDown . toMouseEvent)

{-# INLINE mouseDown #-}
mouseDown :: View -> (Producer MouseDown => View)
mouseDown = mouseDownWith def yield

{-# INLINE mouseDowns #-}
mouseDowns :: (Exists MouseDown => IO ()) -> View -> View
mouseDowns f = events @MouseDown f mouseDown

{-# INLINE onMouseDown #-}
onMouseDown :: (Exists MouseDown => a) -> View -> (Producer a => View)
onMouseDown a = mouseDowns (yield a)

newtype MouseUp = MouseUp MouseEvent

{-# INLINE mouseUpWith #-}
mouseUpWith :: Options -> (MouseUp -> IO ()) -> View -> View
mouseUpWith opts f = OnWith opts "mouseup" (f . MouseUp . toMouseEvent)

{-# INLINE mouseUp #-}
mouseUp :: View -> (Producer MouseUp => View)
mouseUp = mouseUpWith def yield

{-# INLINE mouseUps #-}
mouseUps :: (Exists MouseUp => IO ()) -> View -> View
mouseUps f = events @MouseUp f mouseUp

{-# INLINE onMouseUp #-}
onMouseUp :: (Exists MouseUp => a) -> View -> (Producer a => View)
onMouseUp a = mouseUps (yield a)

newtype ContextMenu = ContextMenu MouseEvent

{-# INLINE contextMenuWith #-}
contextMenuWith :: Options -> (ContextMenu -> IO ()) -> View -> View
contextMenuWith opts f = OnWith opts "contextmenu" (f . ContextMenu . toMouseEvent)

{-# INLINE contextMenu #-}
contextMenu :: View -> (Producer ContextMenu => View)
contextMenu = contextMenuWith def yield

{-# INLINE contextMenus #-}
contextMenus :: (Exists ContextMenu => IO ()) -> View -> View
contextMenus f = events @ContextMenu f contextMenu

{-# INLINE onContextMenu #-}
onContextMenu :: (Exists ContextMenu => a) -> View -> (Producer a => View)
onContextMenu a = contextMenus (yield a)

data Mutation 
  = AttributeMutation
    { target :: Node
    , attributeName :: Txt
    , attributeNamespace :: Txt
    , oldAttribute :: Maybe Txt
    , newAttribute :: Maybe JSV
    }
  | ContentMutation
    { target :: Node
    , oldContent :: Maybe Txt
    , newContent :: Maybe Txt
    }
  | ChildrenMutation
    { target :: Node
    , addedNodes :: [Node]
    , removedNodes :: [Node]
    , previousSibling :: Maybe Node
    , nextSibling :: Maybe Node
    }

data MOOptions = MOOptions
  { subtree :: Bool
  , childList :: Bool
  , attributes :: Bool
  , attributeFilter :: [Txt]
  , attributeOldValue :: Bool
  , characterData :: Bool
  , characterDataOldValue :: Bool
  }

instance Default MOOptions where
  def = MOOptions True False False [] False False False

{-# INLINE onMutation #-}
onMutation :: MOOptions -> (Exists Mutation => a) -> View -> (Producer a => View)
onMutation options a = mutations options (yield a)

{-# INLINE mutations #-}
mutations :: MOOptions -> (Exists Mutation => IO ()) -> View -> View
mutations options f = events @Mutation f (mutation options)

{-# INLINE mutation #-}
mutation :: MOOptions -> View -> (Producer Mutation => View)
mutation options v = 
  unsafePerformIO (do
    writeIORef ref yield
    join $ atomicModifyIORef' opts $ \os -> 
      (options,unless (isTrue# (reallyUnsafePtrEquality# options os)) $ do
        n <- readIORef node
        if isNull n then pure () else do 
          writeIORef opts options
          join (readIORef shutdown)
          sd <- observeWith n options (\ms -> readIORef ref >>= \f -> traverse_ f ms)
          writeIORef shutdown sd
      )
    ) `seq` OnMounted go v
  where
    {-# NOINLINE opts #-}
    opts :: IORef MOOptions
    opts = unsafePerformIO (newIORef undefined)

    {-# NOINLINE ref #-}
    ref :: IORef (Mutation -> IO ())
    ref = unsafePerformIO (newIORef undefined)

    {-# NOINLINE node #-}
    node :: IORef Node
    node = unsafePerformIO (newIORef (coerce nullJSV :: Node))

    {-# NOINLINE shutdown #-}
    shutdown :: IORef (IO ())
    shutdown = unsafePerformIO (newIORef def)

    {-# NOINLINE go #-}
    go n = do
      writeIORef node n
      writeIORef opts options
      join (readIORef shutdown)
      sd <- observeWith n options (\ms -> readIORef ref >>= \f -> traverse_ f ms)
      writeIORef shutdown sd
      pure (join (readIORef shutdown))

observeWith :: Node -> MOOptions -> ([Mutation] -> IO ()) -> IO (IO ())
observeWith n o f = do
#ifdef __GHCJS__
  obj <- JS.create
  when (subtree o) (JS.setProp "subtree" (pToJSVal True) obj)
  when (childList o) (JS.setProp "childList" (pToJSVal True) obj)
  when (Web.Events.attributes o) (JS.setProp "attributes" (pToJSVal True) obj)
  mas <- if Prelude.null (attributeFilter o) then pure Nothing else Just <$> toJSValListOf (attributeFilter o)
  for_ mas $ \as -> JS.setProp "attributeFilter" as obj 
  when (attributeOldValue o) (JS.setProp "attributeOldValue" (pToJSVal True) obj)
  when (characterData o) (JS.setProp "characterData" (pToJSVal True) obj)
  when (characterDataOldValue o) (JS.setProp "characterDataOldValue" (pToJSVal True) obj)
  cb <- syncCallback1 ContinueAsync $ \arr -> do
    Just mutations <- fromJSValListOf arr 
    ms <- traverse mkMutation mutations
    f ms
  obs <- observer_js cb
  observe_js obs n (coerce obj)
  pure (disconnect_js obs >> releaseCallback cb)
#else
  pure (pure ())
#endif

mkMutation :: JSV -> IO Mutation
mkMutation jsv =
#ifdef __GHCJS__
    let
      Just s = jsv .# "type"
      Just t = jsv .# "target"
    in case s of
        "attributes"    -> mkAttributeMutation t jsv
        "characterData" -> mkContentMutation t jsv
        "childList"     -> mkChildrenMutation t jsv
        _               -> error ("Web.Events..Mutation.mkMutation: unknown mutation event type " ++ fromTxt s)
  where
    mkAttributeMutation :: JSV -> JSV -> IO Mutation
    mkAttributeMutation t jsv = do
      let 
        target = coerce t
        attributeName = fromMaybe "" (jsv .# "attributeName")
        attributeNamespace = fromMaybe "" (jsv .# "attributeNamespace")
        oldAttribute = jsv .# "oldValue"
        newAttribute = t .# attributeName
      pure AttributeMutation {..} 

    mkContentMutation :: JSV -> JSV -> IO Mutation
    mkContentMutation t jsv = do
      let 
        target = coerce t
        oldContent = jsv .# "oldValue"
        newContent = t .# "textContent"
      pure ContentMutation {..}

    mkChildrenMutation :: JSV -> JSV -> IO Mutation
    mkChildrenMutation t jsv = do
      let 
        target = coerce t
        previousSibling = (coerce :: Maybe JSV -> Maybe Node) (jsv .# "previousSibling")
        nextSibling = (coerce :: Maybe JSV -> Maybe Node) (jsv .# "nextSibling")
        Just added = jsv .# "addedNodes"
        Just removed = jsv .# "removedNodes"
      (maybe [] (coerce :: [JSV] -> [Node]) -> addedNodes) <- fromJSValListOf added
      (maybe [] (coerce :: [JSV] -> [Node]) -> removedNodes) <- fromJSValListOf removed
      pure ChildrenMutation {..}
#else
  pure (AttributeMutation (coerce ()) def def def def)
#endif

#ifdef __GHCJS__
foreign import javascript safe
  "$r = new MutationObserver($1)" observer_js :: Callback (JSV -> IO ()) -> IO JSV

foreign import javascript safe
  "$1.observe($2,$3)" observe_js :: JSV -> Node -> JSV -> IO ()

foreign import javascript safe
  "$1.disconnect()" disconnect_js :: JSV -> IO ()
#endif

data PointerType = Mouse | Pen | Finger

data PointerEvent = PointerEvent
  { eventObject :: JSV
  , currentTarget :: Node
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
  , mouseEvent :: MouseEvent
  }

toPointerEvent :: Evt -> PointerEvent
toPointerEvent evt@(evtObj -> o) = let err = error "Invalid Pointer Event." in
  PointerEvent
    { eventObject = o
    , currentTarget = coerce (evtTarget evt)
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
          "touch" -> Finger
          _ -> err
    , mouseEvent = toMouseEvent evt
    }

pointerCapture :: Node -> PointerEvent -> IO ()
pointerCapture n PointerEvent { pointerId } = 
#ifdef __GHCJS__
  pointer_capture_js n pointerId 
#else
  pure ()
#endif

pointerRelease :: Node -> PointerEvent -> IO ()
pointerRelease n PointerEvent { pointerId } =
#ifdef __GHCJS__
  pointer_release_js n pointerId 
#else
  pure ()
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "$1.setPointerCapture($2)" pointer_capture_js :: Node -> Int -> IO ()

foreign import javascript unsafe
  "$1.releasePointerCapture($2)" pointer_release_js :: Node -> Int -> IO ()
#endif


newtype PointerOver = PointerOver PointerEvent

{-# INLINE pointerOverWith #-}
pointerOverWith :: Options -> (PointerOver -> IO ()) -> View -> View
pointerOverWith opts f = OnWith opts "pointerover" (f . PointerOver . toPointerEvent)

{-# INLINE pointerOver #-}
pointerOver :: View -> (Producer PointerOver => View)
pointerOver = pointerOverWith def yield

{-# INLINE pointerOvers #-}
pointerOvers :: (Exists PointerOver => IO ()) -> View -> View
pointerOvers f = events @PointerOver f pointerOver

{-# INLINE onPointerOver #-}
onPointerOver :: (Exists PointerOver => a) -> View -> (Producer a => View)
onPointerOver a = pointerOvers (yield a)

newtype PointerEnter = PointerEnter PointerEvent

{-# INLINE pointerEnterWith #-}
pointerEnterWith :: Options -> (PointerEnter -> IO ()) -> View -> View
pointerEnterWith opts f = OnWith opts "pointerenter" (f . PointerEnter . toPointerEvent)

{-# INLINE pointerEnter #-}
pointerEnter :: View -> (Producer PointerEnter => View)
pointerEnter = pointerEnterWith def yield

{-# INLINE pointerEnters #-}
pointerEnters :: (Exists PointerEnter => IO ()) -> View -> View
pointerEnters f = events @PointerEnter f pointerEnter

{-# INLINE onPointerEnter #-}
onPointerEnter :: (Exists PointerEnter => a) -> View -> (Producer a => View)
onPointerEnter a = pointerEnters (yield a)

newtype PointerDown = PointerDown PointerEvent

{-# INLINE pointerDownWith #-}
pointerDownWith :: Options -> (PointerDown -> IO ()) -> View -> View
pointerDownWith opts f = OnWith opts "pointerdown" (f . PointerDown . toPointerEvent)

{-# INLINE pointerDown #-}
pointerDown :: View -> (Producer PointerDown => View)
pointerDown = pointerDownWith def yield

{-# INLINE pointerDowns #-}
pointerDowns :: (Exists PointerDown => IO ()) -> View -> View
pointerDowns f = events @PointerDown f pointerDown

{-# INLINE onPointerDown #-}
onPointerDown :: (Exists PointerDown => a) -> View -> (Producer a => View)
onPointerDown a = pointerDowns (yield a)

newtype PointerMove = PointerMove PointerEvent

{-# INLINE pointerMoveWith #-}
pointerMoveWith :: Options -> (PointerMove -> IO ()) -> View -> View
pointerMoveWith opts f = OnWith opts "pointermove" (f . PointerMove . toPointerEvent)

{-# INLINE pointerMove #-}
pointerMove :: View -> (Producer PointerMove => View)
pointerMove = pointerMoveWith def yield

{-# INLINE pointerMoves #-}
pointerMoves :: (Exists PointerMove => IO ()) -> View -> View
pointerMoves f = events @PointerMove f pointerMove

{-# INLINE onPointerMove #-}
onPointerMove :: (Exists PointerMove => a) -> View -> (Producer a => View)
onPointerMove a = pointerMoves (yield a)

newtype PointerRawUpdate = PointerRawUpdate PointerEvent

{-# INLINE pointerRawUpdateWith #-}
pointerRawUpdateWith :: Options -> (PointerRawUpdate -> IO ()) -> View -> View
pointerRawUpdateWith opts f = OnWith opts "pointerrawupdate" (f . PointerRawUpdate . toPointerEvent)

{-# INLINE pointerRawUpdate #-}
pointerRawUpdate :: View -> (Producer PointerRawUpdate => View)
pointerRawUpdate = pointerRawUpdateWith def yield

{-# INLINE pointerRawUpdates #-}
pointerRawUpdates :: (Exists PointerRawUpdate => IO ()) -> View -> View
pointerRawUpdates f = events @PointerRawUpdate f pointerRawUpdate

{-# INLINE onPointerRawUpdate #-}
onPointerRawUpdate :: (Exists PointerRawUpdate => a) -> View -> (Producer a => View)
onPointerRawUpdate a = pointerRawUpdates (yield a)

newtype PointerUp = PointerUp PointerEvent

{-# INLINE pointerUpWith #-}
pointerUpWith :: Options -> (PointerUp -> IO ()) -> View -> View
pointerUpWith opts f = OnWith opts "pointerup" (f . PointerUp . toPointerEvent)

{-# INLINE pointerUp #-}
pointerUp :: View -> (Producer PointerUp => View)
pointerUp = pointerUpWith def yield

{-# INLINE pointerUps #-}
pointerUps :: (Exists PointerUp => IO ()) -> View -> View
pointerUps f = events @PointerUp f pointerUp

{-# INLINE onPointerUp #-}
onPointerUp :: (Exists PointerUp => a) -> View -> (Producer a => View)
onPointerUp a = pointerUps (yield a)

newtype PointerCancel = PointerCancel PointerEvent

{-# INLINE pointerCancelWith #-}
pointerCancelWith :: Options -> (PointerCancel -> IO ()) -> View -> View
pointerCancelWith opts f = OnWith opts "pointercancel" (f . PointerCancel . toPointerEvent)

{-# INLINE pointerCancel #-}
pointerCancel :: View -> (Producer PointerCancel => View)
pointerCancel = pointerCancelWith def yield

{-# INLINE pointerCancels #-}
pointerCancels :: (Exists PointerCancel => IO ()) -> View -> View
pointerCancels f = events @PointerCancel f pointerCancel

{-# INLINE onPointerCancel #-}
onPointerCancel :: (Exists PointerCancel => a) -> View -> (Producer a => View)
onPointerCancel a = pointerCancels (yield a)

newtype PointerOut = PointerOut PointerEvent

{-# INLINE pointerOutWith #-}
pointerOutWith :: Options -> (PointerOut -> IO ()) -> View -> View
pointerOutWith opts f = OnWith opts "pointerout" (f . PointerOut . toPointerEvent)

{-# INLINE pointerOut #-}
pointerOut :: View -> (Producer PointerOut => View)
pointerOut = pointerOutWith def yield

{-# INLINE pointerOuts #-}
pointerOuts :: (Exists PointerOut => IO ()) -> View -> View
pointerOuts f = events @PointerOut f pointerOut

{-# INLINE onPointerOut #-}
onPointerOut :: (Exists PointerOut => a) -> View -> (Producer a => View)
onPointerOut a = pointerOuts (yield a)

newtype PointerLeave = PointerLeave PointerEvent

{-# INLINE pointerLeaveWith #-}
pointerLeaveWith :: Options -> (PointerLeave -> IO ()) -> View -> View
pointerLeaveWith opts f = OnWith opts "pointerleave" (f . PointerLeave . toPointerEvent)

{-# INLINE pointerLeave #-}
pointerLeave :: View -> (Producer PointerLeave => View)
pointerLeave = pointerLeaveWith def yield

{-# INLINE pointerLeaves #-}
pointerLeaves :: (Exists PointerLeave => IO ()) -> View -> View
pointerLeaves f = events @PointerLeave f pointerLeave

{-# INLINE onPointerLeave #-}
onPointerLeave :: (Exists PointerLeave => a) -> View -> (Producer a => View)
onPointerLeave a = pointerLeaves (yield a)

newtype GotPointerCapture = GotPointerCapture PointerEvent

{-# INLINE gotPointerCaptureWith #-}
gotPointerCaptureWith :: Options -> (GotPointerCapture -> IO ()) -> View -> View
gotPointerCaptureWith opts f = OnWith opts "gotpointercapture" (f . GotPointerCapture . toPointerEvent)

{-# INLINE gotPointerCapture #-}
gotPointerCapture :: View -> (Producer GotPointerCapture => View)
gotPointerCapture = gotPointerCaptureWith def yield

{-# INLINE gotPointerCaptures #-}
gotPointerCaptures :: (Exists GotPointerCapture => IO ()) -> View -> View
gotPointerCaptures f = events @GotPointerCapture f gotPointerCapture

{-# INLINE onGotPointerCapture #-}
onGotPointerCapture :: (Exists GotPointerCapture => a) -> View -> (Producer a => View)
onGotPointerCapture a = gotPointerCaptures (yield a)

newtype LostPointerCapture = LostPointerCapture PointerEvent

{-# INLINE lostPointerCaptureWith #-}
lostPointerCaptureWith :: Options -> (LostPointerCapture -> IO ()) -> View -> View
lostPointerCaptureWith opts f = OnWith opts "lostpointercapture" (f . LostPointerCapture . toPointerEvent)

{-# INLINE lostPointerCapture #-}
lostPointerCapture :: View -> (Producer LostPointerCapture => View)
lostPointerCapture = lostPointerCaptureWith def yield

{-# INLINE lostPointerCaptures #-}
lostPointerCaptures :: (Exists LostPointerCapture => IO ()) -> View -> View
lostPointerCaptures f = events @LostPointerCapture f lostPointerCapture

{-# INLINE onLostPointerCapture #-}
onLostPointerCapture :: (Exists LostPointerCapture => a) -> View -> (Producer a => View)
onLostPointerCapture a = lostPointerCaptures (yield a)

data Resize = Resize
  { eventObject :: JSV
  , target :: JSV
  , borderBlockSize :: Double
  , contentBlockSize :: Double
  , devicePixelContentBlockSize :: Double
  , borderInlineSize :: Double
  , contentInlineSize :: Double
  , devicePixelContentInlineSize :: Double
  , width :: Double
  , height :: Double
  , x :: Double
  , y :: Double
  , top :: Double
  , right :: Double
  , bottom :: Double
  , left :: Double
  }

toResize :: JSV -> Resize
toResize o = let err = error "Invalid Resize Event." in
  fix $ \re -> do
    let r = fromMaybe err (o .# "contentRect")
        w = width :: Resize -> Double
        h = height :: Resize -> Double
    Resize
      { eventObject = o
      , target = fromMaybe err (o .# "target")
      , devicePixelContentBlockSize = fromMaybe (w re) (o .# "devicePixelContentBoxSize[0]" >>= (.# "blockSize"))
      , devicePixelContentInlineSize = fromMaybe (h re) (o .# "devicePixelContentBoxSize[0]" >>= (.# "inlineSize"))
      , borderBlockSize = fromMaybe (w re) (o .# "borderBoxSize[0]" >>= (.# "blockSize"))
      , borderInlineSize = fromMaybe (h re) (o .# "borderBoxSize[0]" >>= (.# "inlineSize"))
      , contentBlockSize = fromMaybe (w re) (o .# "contentBoxSize[0]" >>= (.# "blockSize"))
      , contentInlineSize = fromMaybe (h re) (o .# "contentSize[0]" >>= (.# "inlineSize"))
      , width = fromMaybe err (r .# "width")
      , height = fromMaybe err (r .# "height")
      , x = fromMaybe err (r .# "x")
      , y = fromMaybe err (r .# "y")
      , top = fromMaybe err (r .# "top")
      , right = fromMaybe err (r .# "right")
      , bottom = fromMaybe err (r .# "bottom")
      , left = fromMaybe err (r .# "left")
      }

{-# INLINE resize #-}
resize :: View -> (Producer Resize => View)
resize v = unsafePerformIO (writeIORef ref yield) `seq` OnMounted go v
  where
    {-# NOINLINE ref #-}
    ref :: IORef (Resize -> IO ())
    ref = unsafePerformIO (newIORef undefined)

    {-# NOINLINE go #-}
    go :: Producer Resize => Node -> IO (IO ())
    go n = do
#ifdef __GHCJS__
      cb <- asyncCallback1 (\re -> let es = fmap toResize (fromMaybe [] (unsafePerformIO (fromJSValListOf re))) in readIORef ref >>= \f -> for_ es f)
      ro <- create_js cb
      connect_js ro n
      pure (unobserve_js ro n >> releaseCallback cb)
#else
      pure (pure ())
#endif

#ifdef __GHCJS__
newtype ResizeObserver = ResizeObserver JSV
foreign import javascript unsafe
  "new ResizeObserver($1)" create_js :: Callback (JSV -> IO ()) -> IO ResizeObserver
foreign import javascript unsafe
  "$1.observe($2)" connect_js :: ResizeObserver -> Node -> IO ()
foreign import javascript unsafe
  "$1.unobserver($2)" unobserve_js :: ResizeObserver -> Node -> IO ()
#endif

{-# INLINE resizes #-}
resizes :: (Exists Resize => IO ()) -> View -> View
resizes f = events @Resize f resize

{-# INLINE onResize #-}
onResize :: (Exists Resize => a) -> View -> (Producer a => View)
onResize a = resizes (yield a)

data ScrollEvent = ScrollEvent
  { eventObject :: JSV 
  }

toScrollEvent :: Evt -> ScrollEvent
toScrollEvent (evtObj -> o) = ScrollEvent { eventObject = o }

newtype Scroll = Scroll ScrollEvent

{-# INLINE scrollWith #-}
scrollWith :: Options -> (Scroll -> IO ()) -> View -> View
scrollWith opts f = OnWith opts "scroll" (f . Scroll . toScrollEvent)

{-# INLINE scroll #-}
scroll :: View -> (Producer Scroll => View)
scroll = scrollWith def yield

{-# INLINE scrolls #-}
scrolls :: (Exists Scroll => IO ()) -> View -> View
scrolls f = events @Scroll f scroll

{-# INLINE onScroll #-}
onScroll :: (Exists Scroll => a) -> View -> (Producer a => View)
onScroll a = scrolls (yield a)

newtype ScrollEnd = ScrollEnd ScrollEvent

{-# INLINE scrollEndWith #-}
scrollEndWith :: Options -> (ScrollEnd -> IO ()) -> View -> View
scrollEndWith opts f = OnWith opts "scrollend" (f . ScrollEnd . toScrollEvent)

{-# INLINE scrollEnd #-}
scrollEnd :: View -> (Producer ScrollEnd => View)
scrollEnd = scrollEndWith def yield

{-# INLINE scrollEnds #-}
scrollEnds :: (Exists ScrollEnd => IO ()) -> View -> View
scrollEnds f = events @ScrollEnd f scrollEnd

{-# INLINE onScrollEnd #-}
onScrollEnd :: (Exists ScrollEnd => a) -> View -> (Producer a => View)
onScrollEnd a = scrollEnds (yield a)

data SelectionEvent = SelectionEvent
  { eventObject :: JSV
  }

toSelectionEvent :: Evt -> SelectionEvent
toSelectionEvent (evtObj -> o) =
  SelectionEvent
    { eventObject = o }

newtype Selected = Selected SelectionEvent

{-# INLINE selectedWith #-}
selectedWith :: Options -> (Selected -> IO ()) -> View -> View
selectedWith opts f = OnWith opts "select" (f . Selected . toSelectionEvent)

{-# INLINE selected #-}
selected :: View -> (Producer Selected => View)
selected = selectedWith def yield

{-# INLINE selecteds #-}
selecteds :: (Exists Selected => IO ()) -> View -> View
selecteds f = events @Selected f selected

{-# INLINE onSelected #-}
onSelected :: (Exists Selected => a) -> View -> (Producer a => View)
onSelected a = selecteds (yield a)

data TouchType = Direct | Stylus
instance FromJSON TouchType where
  parseJSON v = do
    str <- parseJSON v
    case str :: Txt of
      "stylus" -> pure Stylus
      _        -> pure Direct

data Touch = Touch
  { eventObject :: JSV
  , identifier :: Int
  , target :: Node
  , screenX :: Double
  , screenY :: Double
  , offsetX :: Double
  , offsetY :: Double
  , clientX :: Double
  , clientY :: Double
  , radiusX :: Double
  , radiusY :: Double
  , rotationAngle :: Double
  , force :: Double
  , altitudeAngle :: Double
  , azimuthAngle :: Double
  , touchType :: TouchType
  } 

toTouch :: JSV -> Touch
toTouch o = let err = error "Invalid Touch Object." in
  Touch
    { eventObject = o
    , identifier = fromMaybe err (o .# "identifier")
    , target = maybe err (coerce :: JSV -> Node) (o .# "identifier")
    , screenX = fromMaybe err (o .# "screenX")
    , screenY = fromMaybe err (o .# "screenX")
    , offsetX = fromMaybe err (o .# "offsetX")
    , offsetY = fromMaybe err (o .# "offsetY")
    , clientX = fromMaybe err (o .# "clientX")
    , clientY = fromMaybe err (o .# "clientY")
    , radiusX = fromMaybe err (o .# "radiusX")
    , radiusY = fromMaybe err (o .# "radiusY")
    , force = fromMaybe err (o .# "force")
    , rotationAngle = fromMaybe err (o .# "rotationAngle")
    , altitudeAngle = fromMaybe err (o .# "altitudeAngle")
    , azimuthAngle = fromMaybe err (o .# "azimuthAngle")
    , touchType = 
      case fromMaybe err (o .# "rotationAngle") :: Txt of
        "stylus" -> Stylus
        _        -> Direct
    }

data TouchEvent = TouchEvent
  { eventObject :: JSV
  , touches :: [Touch]
  , targetTouches :: [Touch]
  , changedTouches :: [Touch]
  , currentTarget :: Node
  , altKey :: Bool
  , metaKey :: Bool
  , ctrlKey :: Bool
  , shiftKey :: Bool
  }

toTouchEvent :: Evt -> TouchEvent
toTouchEvent e@(evtObj -> o) = let err = error "Invalid TouchEvent Object." in
  TouchEvent 
    { eventObject = o
#ifdef __GHCJS__
    , touches        = fmap toTouch (fromMaybe err (join (fmap (unsafePerformIO . fromJSValListOf) (o .# "touches"))))
    , targetTouches  = fmap toTouch (fromMaybe err (join (fmap (unsafePerformIO . fromJSValListOf) (o .# "targetTouches"))))
    , changedTouches = fmap toTouch (fromMaybe err (join (fmap (unsafePerformIO . fromJSValListOf) (o .# "changedTouches"))))
#else
    , touches = []
    , targetTouches = []
    , changedTouches = []
#endif
    , currentTarget = coerce (evtTarget e)
    , altKey = fromMaybe err (o .# "altKey")
    , metaKey = fromMaybe err (o .# "metaKey")
    , ctrlKey = fromMaybe err (o .# "ctrlKey")
    , shiftKey = fromMaybe err (o .# "shiftKey")
    }

newtype TouchStart = TouchStart TouchEvent

{-# INLINE touchStartWith #-}
touchStartWith :: Options -> (TouchStart -> IO ()) -> View -> View
touchStartWith opts f = OnWith opts "touchstart" (f . TouchStart . toTouchEvent)

{-# INLINE touchStart #-}
touchStart :: View -> (Producer TouchStart => View)
touchStart = touchStartWith def yield

{-# INLINE touchStarts #-}
touchStarts :: (Exists TouchStart => IO ()) -> View -> View
touchStarts f = events @TouchStart f touchStart

{-# INLINE onTouchStart #-}
onTouchStart :: (Exists TouchStart => a) -> View -> (Producer a => View)
onTouchStart a = touchStarts (yield a)

newtype TouchEnd = TouchEnd TouchEvent

{-# INLINE touchEndWith #-}
touchEndWith :: Options -> (TouchEnd -> IO ()) -> View -> View
touchEndWith opts f = OnWith opts "touchend" (f . TouchEnd . toTouchEvent)

{-# INLINE touchEnd #-}
touchEnd :: View -> (Producer TouchEnd => View)
touchEnd = touchEndWith def yield

{-# INLINE touchEnds #-}
touchEnds :: (Exists TouchEnd => IO ()) -> View -> View
touchEnds f = events @TouchEnd f touchEnd

{-# INLINE onTouchEnd #-}
onTouchEnd :: (Exists TouchEnd => a) -> View -> (Producer a => View)
onTouchEnd a = touchEnds (yield a)

newtype TouchMove = TouchMove TouchEvent

{-# INLINE touchMoveWith #-}
touchMoveWith :: Options -> (TouchMove -> IO ()) -> View -> View
touchMoveWith opts f = OnWith opts "touchmove" (f . TouchMove . toTouchEvent)

{-# INLINE touchMove #-}
touchMove :: View -> (Producer TouchMove => View)
touchMove = touchMoveWith def yield

{-# INLINE touchMoves #-}
touchMoves :: (Exists TouchMove => IO ()) -> View -> View
touchMoves f = events @TouchMove f touchMove

{-# INLINE onTouchMove #-}
onTouchMove :: (Exists TouchMove => a) -> View -> (Producer a => View)
onTouchMove a = touchMoves (yield a)

newtype TouchCancel = TouchCancel TouchEvent

{-# INLINE touchCancelWith #-}
touchCancelWith :: Options -> (TouchCancel -> IO ()) -> View -> View
touchCancelWith opts f = OnWith opts "touchcancel" (f . TouchCancel . toTouchEvent)

{-# INLINE touchCancel #-}
touchCancel :: View -> (Producer TouchCancel => View)
touchCancel = touchCancelWith def yield

{-# INLINE touchCancels #-}
touchCancels :: (Exists TouchCancel => IO ()) -> View -> View
touchCancels f = events @TouchCancel f touchCancel

{-# INLINE onTouchCancel #-}
onTouchCancel :: (Exists TouchCancel => a) -> View -> (Producer a => View)
onTouchCancel a = touchCancels (yield a)

data TransitionEvent = TransitionEvent
  { eventObject :: JSV
  , propertyName :: Txt
  , elapsedTime :: Time
  , pseudoElement :: Txt
  }

toTransitionEvent :: Evt -> TransitionEvent
toTransitionEvent (evtObj -> o) = let err = error "Invalid Transition Event." in
  TransitionEvent
    { eventObject = o
    , propertyName = fromMaybe err (o .# "propertyName")
    , elapsedTime = maybe err (`Seconds` 0) (o .# "elapsedTime")
    , pseudoElement = fromMaybe err (o .# "pseudoElement")
    }

newtype TransitionStart = TransitionStart TransitionEvent

{-# INLINE transitionStartWith #-}
transitionStartWith :: Options -> (TransitionStart -> IO ()) -> View -> View
transitionStartWith opts f = OnWith opts "transitionstart" (f . TransitionStart . toTransitionEvent)

{-# INLINE transitionStart #-}
transitionStart :: View -> (Producer TransitionStart => View)
transitionStart = transitionStartWith def yield

{-# INLINE transitionStarts #-}
transitionStarts :: (Exists TransitionStart => IO ()) -> View -> View
transitionStarts f = events @TransitionStart f transitionStart

{-# INLINE onTransitionStart #-}
onTransitionStart :: (Exists TransitionStart => a) -> View -> (Producer a => View)
onTransitionStart a = transitionStarts (yield a)

newtype TransitionEnd = TransitionEnd TransitionEvent

{-# INLINE transitionEndWith #-}
transitionEndWith :: Options -> (TransitionEnd -> IO ()) -> View -> View
transitionEndWith opts f = OnWith opts "transitionend" (f . TransitionEnd . toTransitionEvent)

{-# INLINE transitionEnd #-}
transitionEnd :: View -> (Producer TransitionEnd => View)
transitionEnd = transitionEndWith def yield

{-# INLINE transitionEnds #-}
transitionEnds :: (Exists TransitionEnd => IO ()) -> View -> View
transitionEnds f = events @TransitionEnd f transitionEnd

{-# INLINE onTransitionEnd #-}
onTransitionEnd :: (Exists TransitionEnd => a) -> View -> (Producer a => View)
onTransitionEnd a = transitionEnds (yield a)

newtype TransitionRun = TransitionRun TransitionEvent

{-# INLINE transitionRunWith #-}
transitionRunWith :: Options -> (TransitionRun -> IO ()) -> View -> View
transitionRunWith opts f = OnWith opts "transitionrun" (f . TransitionRun . toTransitionEvent)

{-# INLINE transitionRun #-}
transitionRun :: View -> (Producer TransitionRun => View)
transitionRun = transitionRunWith def yield

{-# INLINE transitionRuns #-}
transitionRuns :: (Exists TransitionRun => IO ()) -> View -> View
transitionRuns f = events @TransitionRun f transitionRun

{-# INLINE onTransitionRun #-}
onTransitionRun :: (Exists TransitionRun => a) -> View -> (Producer a => View)
onTransitionRun a = transitionRuns (yield a)

newtype TransitionCancel = TransitionCancel TransitionEvent

{-# INLINE transitionCancelWith #-}
transitionCancelWith :: Options -> (TransitionCancel -> IO ()) -> View -> View
transitionCancelWith opts f = OnWith opts "transitioncancel" (f . TransitionCancel . toTransitionEvent)

{-# INLINE transitionCancel #-}
transitionCancel :: View -> (Producer TransitionCancel => View)
transitionCancel = transitionCancelWith def yield

{-# INLINE transitionCancels #-}
transitionCancels :: (Exists TransitionCancel => IO ()) -> View -> View
transitionCancels f = events @TransitionCancel f transitionCancel

{-# INLINE onTransitionCancel #-}
onTransitionCancel :: (Exists TransitionCancel => a) -> View -> (Producer a => View)
onTransitionCancel a = transitionCancels (yield a)

data DeltaMode = Pixel | Line | Page

data WheelEvent = WheelEvent
  { eventObject :: JSV 
  , currentTarget :: Node
  , deltaX :: Double
  , deltaY :: Double
  , deltaZ :: Double
  , deltaMode :: DeltaMode
  , mouseEvent :: MouseEvent
  }

toWheelEvent :: Evt -> WheelEvent
toWheelEvent evt@(evtObj -> o) = let err = error "Invalid Wheel Event." in
  WheelEvent
    { eventObject = o
    , currentTarget = coerce (evtTarget evt)
    , deltaX = fromMaybe err (o .# "deltaX")
    , deltaY = fromMaybe err (o .# "deltaY")
    , deltaZ = fromMaybe err (o .# "deltaZ")
    , deltaMode =
      case fromMaybe err (o .# "deltaMode") :: Int of
        0 -> Pixel
        1 -> Line
        2 -> Page
        _ -> err
    , mouseEvent = toMouseEvent evt
    }

newtype Wheel = Wheel WheelEvent

{-# INLINE wheelWith #-}
wheelWith :: Options -> (Wheel -> IO ()) -> View -> View
wheelWith opts f = OnWith opts "wheel" (f . Wheel . toWheelEvent)

{-# INLINE wheel #-}
wheel :: View -> (Producer Wheel => View)
wheel = wheelWith def yield

{-# INLINE wheels #-}
wheels :: (Exists Wheel => IO ()) -> View -> View
wheels f = events @Wheel f wheel

{-# INLINE onWheel #-}
onWheel :: (Exists Wheel => a) -> View -> (Producer a => View)
onWheel a = wheels (yield a)