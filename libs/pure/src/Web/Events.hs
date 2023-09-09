{-# language PatternSynonyms, RankNTypes, FlexibleContexts, OverloadedStrings, ViewPatterns, TypeApplications, CPP, DuplicateRecordFields, LambdaCase, BlockArguments, RecordWildCards, MagicHash #-}
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

animationStartWith :: Options -> (AnimationStart -> IO ()) -> View -> View
animationStartWith opts f = OnWith opts "animationstart" (f . AnimationStart . toAnimationEvent)

animationStart :: View -> (Producer AnimationStart => View)
animationStart = animationStartWith def yield

animationStarts :: (Exists AnimationStart => IO ()) -> View -> View
animationStarts f = events @AnimationStart f animationStart

newtype AnimationIteration = AnimationIteration AnimationEvent

animationIterationWith :: Options -> (AnimationIteration -> IO ()) -> View -> View
animationIterationWith opts f = OnWith opts "animationiteration" (f . AnimationIteration . toAnimationEvent)

animationIteration :: View -> (Producer AnimationIteration => View)
animationIteration = animationIterationWith def yield

animationIterations :: (Exists AnimationIteration => IO ()) -> View -> View
animationIterations f = events @AnimationIteration f animationIteration

newtype AnimationCancel = AnimationCancel AnimationEvent

animationCancelWith :: Options -> (AnimationCancel -> IO ()) -> View -> View
animationCancelWith opts f = OnWith opts "animationcancel" (f . AnimationCancel . toAnimationEvent)

animationCancel :: View -> (Producer AnimationCancel => View)
animationCancel = animationCancelWith def yield

animationCancels :: (Exists AnimationCancel => IO ()) -> View -> View
animationCancels f = events @AnimationCancel f animationCancel

newtype AnimationEnd = AnimationEnd AnimationEvent

animationEndWith :: Options -> (AnimationEnd -> IO ()) -> View -> View
animationEndWith opts f = OnWith opts "animationend" (f . AnimationEnd . toAnimationEvent)

animationEnd :: View -> (Producer AnimationEnd => View)
animationEnd = animationEndWith def yield

animationEnds :: (Exists AnimationEnd => IO ()) -> View -> View
animationEnds f = events @AnimationEnd f animationEnd

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

cutWith :: Options -> (Cut -> IO ()) -> View -> View
cutWith opts f = OnWith opts "cut" (f . Cut . toClipboardEvent)

cut :: View -> (Producer Cut => View)
cut = cutWith def yield

cuts :: (Exists Cut => IO ()) -> View -> View
cuts f = events @Cut f cut

newtype Copy = Copy ClipboardEvent

copyWith :: Options -> (Copy -> IO ()) -> View -> View
copyWith opts f = OnWith opts "copy" (f . Copy . toClipboardEvent)

copy :: View -> (Producer Copy => View)
copy = copyWith def yield

copies :: (Exists Copy => IO ()) -> View -> View
copies f = events @Copy f Web.Events.copy

newtype Paste = Paste ClipboardEvent

pasteWith :: Options -> (Paste -> IO ()) -> View -> View
pasteWith opts f = OnWith opts "paste" (f . Paste . toClipboardEvent)

paste :: View -> (Producer Paste => View)
paste = pasteWith def yield

pastes :: (Exists Paste => IO ()) -> View -> View
pastes f = events @Paste f paste

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

dragWith :: Options -> (Drag -> IO ()) -> View -> View
dragWith opts f = OnWith opts "drag" (f . Drag . toDragEvent)

drag :: View -> (Producer Drag => View)
drag = dragWith def yield

drags :: (Exists Drag => IO ()) -> View -> View
drags f = events @Drag f drag

newtype DragEnd = DragEnd DragEvent

dragEndWith :: Options -> (DragEnd -> IO ()) -> View -> View
dragEndWith opts f = OnWith opts "dragend" (f . DragEnd . toDragEvent)

dragEnd :: View -> (Producer DragEnd => View)
dragEnd = dragEndWith def yield

dragEnds :: (Exists DragEnd => IO ()) -> View -> View
dragEnds f = events @DragEnd f dragEnd

newtype DragEnter = DragEnter DragEvent

dragEnterWith :: Options -> (DragEnter -> IO ()) -> View -> View
dragEnterWith opts f = OnWith opts "dragenter" (f . DragEnter . toDragEvent)

dragEnter :: View -> (Producer DragEnter => View)
dragEnter = dragEnterWith def yield

dragEnters :: (Exists DragEnter => IO ()) -> View -> View
dragEnters f = events @DragEnter f dragEnter

newtype DragLeave = DragLeave DragEvent

dragLeaveWith :: Options -> (DragLeave -> IO ()) -> View -> View
dragLeaveWith opts f = OnWith opts "dragleave" (f . DragLeave . toDragEvent)

dragLeave :: View -> (Producer DragLeave => View)
dragLeave = dragLeaveWith def yield

dragLeaves :: (Exists DragLeave => IO ()) -> View -> View
dragLeaves f = events @DragLeave f dragLeave

newtype DragOver = DragOver DragEvent

dragOverWith :: Options -> (DragOver -> IO ()) -> View -> View
dragOverWith opts f = OnWith opts "dragover" (f . DragOver . toDragEvent)

dragOver :: View -> (Producer DragOver => View)
dragOver = dragOverWith def yield

dragOvers :: (Exists DragOver => IO ()) -> View -> View
dragOvers f = events @DragOver f dragOver

newtype DragStart = DragStart DragEvent

dragStartWith :: Options -> (DragStart -> IO ()) -> View -> View
dragStartWith opts f = OnWith opts "dragstart" (f . DragStart . toDragEvent)

dragStart :: View -> (Producer DragStart => View)
dragStart = dragStartWith def yield

dragStarts :: (Exists DragStart => IO ()) -> View -> View
dragStarts f = events @DragStart f dragStart

newtype Drop = Drop DragEvent

dropWith :: Options -> (Drop -> IO ()) -> View -> View
dropWith opts f = OnWith opts "drop" (f . Drop . toDragEvent)

drop :: View -> (Producer Drop => View)
drop = dropWith def yield

drops :: (Exists Drop => IO ()) -> View -> View
drops f = events @Drop f Web.Events.drop

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

changeWith :: Options -> (Change -> IO ()) -> View -> View
changeWith opts f = OnWith opts "change" (f . Change . toChangeEvent)

change :: View -> (Producer Change => View)
change = changeWith def yield

changes :: (Exists Change => IO ()) -> View -> View
changes f = events @Change f change

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

mutations :: MOOptions -> (Exists Mutation => IO ()) -> View -> View
mutations options f = events @Mutation f (mutation options)

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
          "touch" -> Finger
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

resizes :: (Exists Resize => IO ()) -> View -> View
resizes f = events @Resize f resize

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

data SelectionEvent = SelectionEvent
  { eventObject :: JSV
  }

toSelectionEvent :: Evt -> SelectionEvent
toSelectionEvent (evtObj -> o) =
  SelectionEvent
    { eventObject = o }

newtype Selected = Selected SelectionEvent

selectedWith :: Options -> (Selected -> IO ()) -> View -> View
selectedWith opts f = OnWith opts "select" (f . Selected . toSelectionEvent)

selected :: View -> (Producer Selected => View)
selected = selectedWith def yield

selecteds :: (Exists Selected => IO ()) -> View -> View
selecteds f = events @Selected f selected

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
  , altKey :: Bool
  , metaKey :: Bool
  , ctrlKey :: Bool
  , shiftKey :: Bool
  }

toTouchEvent :: Evt -> TouchEvent
toTouchEvent (evtObj -> o) = let err = error "Invalid TouchEvent Object." in
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
    , altKey = fromMaybe err (o .# "altKey")
    , metaKey = fromMaybe err (o .# "metaKey")
    , ctrlKey = fromMaybe err (o .# "ctrlKey")
    , shiftKey = fromMaybe err (o .# "shiftKey")
    }

newtype TouchStart = TouchStart TouchEvent

touchStartWith :: Options -> (TouchStart -> IO ()) -> View -> View
touchStartWith opts f = OnWith opts "touchstart" (f . TouchStart . toTouchEvent)

touchStart :: View -> (Producer TouchStart => View)
touchStart = touchStartWith def yield

touchStarts :: (Exists TouchStart => IO ()) -> View -> View
touchStarts f = events @TouchStart f touchStart

newtype TouchEnd = TouchEnd TouchEvent

touchEndWith :: Options -> (TouchEnd -> IO ()) -> View -> View
touchEndWith opts f = OnWith opts "touchend" (f . TouchEnd . toTouchEvent)

touchEnd :: View -> (Producer TouchEnd => View)
touchEnd = touchEndWith def yield

touchEnds :: (Exists TouchEnd => IO ()) -> View -> View
touchEnds f = events @TouchEnd f touchEnd

newtype TouchMove = TouchMove TouchEvent

touchMoveWith :: Options -> (TouchMove -> IO ()) -> View -> View
touchMoveWith opts f = OnWith opts "touchmove" (f . TouchMove . toTouchEvent)

touchMove :: View -> (Producer TouchMove => View)
touchMove = touchMoveWith def yield

touchMoves :: (Exists TouchMove => IO ()) -> View -> View
touchMoves f = events @TouchMove f touchMove

newtype TouchCancel = TouchCancel TouchEvent

touchCancelWith :: Options -> (TouchCancel -> IO ()) -> View -> View
touchCancelWith opts f = OnWith opts "touchcancel" (f . TouchCancel . toTouchEvent)

touchCancel :: View -> (Producer TouchCancel => View)
touchCancel = touchCancelWith def yield

touchCancels :: (Exists TouchCancel => IO ()) -> View -> View
touchCancels f = events @TouchCancel f touchCancel

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

transitionStartWith :: Options -> (TransitionStart -> IO ()) -> View -> View
transitionStartWith opts f = OnWith opts "transitionstart" (f . TransitionStart . toTransitionEvent)

transitionStart :: View -> (Producer TransitionStart => View)
transitionStart = transitionStartWith def yield

transitionStarts :: (Exists TransitionStart => IO ()) -> View -> View
transitionStarts f = events @TransitionStart f transitionStart

newtype TransitionEnd = TransitionEnd TransitionEvent

transitionEndWith :: Options -> (TransitionEnd -> IO ()) -> View -> View
transitionEndWith opts f = OnWith opts "transitionend" (f . TransitionEnd . toTransitionEvent)

transitionEnd :: View -> (Producer TransitionEnd => View)
transitionEnd = transitionEndWith def yield

transitionEnds :: (Exists TransitionEnd => IO ()) -> View -> View
transitionEnds f = events @TransitionEnd f transitionEnd

newtype TransitionRun = TransitionRun TransitionEvent

transitionRunWith :: Options -> (TransitionRun -> IO ()) -> View -> View
transitionRunWith opts f = OnWith opts "transitionrun" (f . TransitionRun . toTransitionEvent)

transitionRun :: View -> (Producer TransitionRun => View)
transitionRun = transitionRunWith def yield

transitionRuns :: (Exists TransitionRun => IO ()) -> View -> View
transitionRuns f = events @TransitionRun f transitionRun

newtype TransitionCancel = TransitionCancel TransitionEvent

transitionCancelWith :: Options -> (TransitionCancel -> IO ()) -> View -> View
transitionCancelWith opts f = OnWith opts "transitioncancel" (f . TransitionCancel . toTransitionEvent)

transitionCancel :: View -> (Producer TransitionCancel => View)
transitionCancel = transitionCancelWith def yield

transitionCancels :: (Exists TransitionCancel => IO ()) -> View -> View
transitionCancels f = events @TransitionCancel f transitionCancel

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


