{-# language CPP #-}
{-# language OverloadedStrings #-}
module Atomic.Attribute where

import Ef.Base

import Data.Txt hiding (Options)

import Atomic.FromTxt
import Atomic.ToTxt
import Atomic.Cond

import Data.String
import Data.Maybe

#ifdef LENS
import Control.Lens (makePrisms,makeLenses)
#endif

#ifdef __GHCJS__
import qualified Data.JSString as JSS
#else
import qualified Data.Text as JSS
#endif

data Options = Options
  { _preventDefault :: Bool
  , _stopPropagation :: Bool
  } deriving Eq

defaultOptions :: Options
defaultOptions = Options False False

interceptOptions :: Options
interceptOptions = Options True True

data Feature e
  = NullFeature
  | Attribute
    { _attr :: Txt
    , _value :: Either Bool Txt
    }
  | Style
    { _stylePairs :: [(Txt,Txt)] }
  | CurrentValue
    { _currentValue :: Txt }
  | On
    { _event :: Txt
    , _eventE :: e
    , _eventListener :: Maybe (IO ())
    }
  | On'
    { _event :: Txt
    , _eventOptions :: Options
    , _eventECreate :: Data.Txt.Object -> IO (Maybe e)
    , _eventListener :: Maybe (IO ())
    }
  | Link
    { _event :: Txt
    , _eventListener :: Maybe (IO ())
    }
  deriving (Functor)

type Attribute ms = Feature (Code ms IO ())

instance Cond (Feature e) where
  nil = NullFeature

instance IsString (Feature e) where
  fromString = Attribute "class" . Right . fromString

instance {-# OVERLAPS #-} IsString [Feature e] where
  fromString s = [fromString s]

instance FromTxt (Feature e) where
  fromTxt = Attribute "class" . Right . fromTxt

instance FromTxt [Feature e] where
  fromTxt t = [fromTxt t]

nullF :: Feature e
nullF = NullFeature

attribute :: Txt -> Txt -> Feature e
attribute nm = Attribute nm . Right

attr :: Txt -> Txt -> Feature e
attr = attribute

boolAttribute :: Txt -> Bool -> Feature e
boolAttribute nm = Attribute nm . Left

boolattr :: Txt -> Bool -> Feature e
boolattr = boolAttribute

on' :: Txt -> Options -> (Data.Txt.Object -> IO (Maybe e)) -> Feature e
on' ev os f = On' ev os f Nothing

on :: Txt -> e -> Feature e
on ev e = On ev e Nothing

styleList :: [(Txt,Txt)] -> Feature e
styleList = Style

link :: Txt -> Feature e
link = flip Link Nothing

value :: Txt -> Feature e
value jst = CurrentValue jst

#ifdef LENS
makePrisms ''Feature
makeLenses ''Feature
makePrisms ''Options
makeLenses ''Options
#endif

class_ :: Txt -> Feature e
class_ = attr "class"

classes :: [(Txt,Bool)] -> Feature e
classes = class_
  . JSS.intercalate " "
  . mapMaybe (\(s,b) -> if b then Just s else Nothing)

id_ :: Txt -> Feature e
id_ = attr "id"

title_ :: Txt -> Feature e
title_ = attr "title"

hidden :: Bool -> Feature e
hidden = boolattr "hidden"

type_ :: Txt -> Feature e
type_ = attr "type"

initialValue :: Txt -> Feature e
initialValue = attr "value"

defaultValue :: Txt -> Feature e
defaultValue = attr "default-value"

checked :: Bool -> Feature e
checked = boolattr "checked"

placeholder :: Txt -> Feature e
placeholder = attr "placeholder"

selected :: Bool -> Feature e
selected = boolattr "selected"

accept :: Txt -> Feature e
accept = attr "accept"

acceptCharset :: Txt -> Feature e
acceptCharset = attr "accept-charset"

autocomplete :: Bool -> Feature e
autocomplete b = attr "autocomplete" (if b then "on" else "off")

autofocus :: Bool -> Feature e
autofocus = boolattr "autofocus"

disabled :: Bool -> Feature e
disabled = boolattr "disabled"

enctype :: Txt -> Feature e
enctype = attr "enctyp"

formaction :: Txt -> Feature e
formaction = attr "formaction"

list :: Txt -> Feature e
list = attr "list"

maxlength :: Int -> Feature e
maxlength = attr "maxlength" . toTxt

minlength :: Int -> Feature e
minlength = attr "minlength" . toTxt

method_ :: Txt -> Feature e
method_ = attr "method"

multiple :: Bool -> Feature e
multiple = boolattr "multiple"

name :: Txt -> Feature e
name = attr "name"

novalidate :: Bool -> Feature e
novalidate = boolattr "novalidate"

pattern_ :: Txt -> Feature e
pattern_ = attr "pattern"

readonly :: Bool -> Feature e
readonly = boolattr "readonly"

required :: Bool -> Feature e
required = boolattr "required"

size :: Int -> Feature e
size = attr "size" . toTxt

for_ :: Txt -> Feature e
for_ = attr "for"

form_ :: Txt -> Feature e
form_ = attr "form"

max_ :: Txt -> Feature e
max_ = attr "max"

min_ :: Txt -> Feature e
min_ = attr "min"

step :: Txt -> Feature e
step = attr "step"

cols :: Int -> Feature e
cols = attr "cols" . toTxt

rows :: Int -> Feature e
rows = attr "rows" . toTxt

wrap_ :: Txt -> Feature e
wrap_ = attr "wrap"

href :: Txt -> Feature e
href = attr "href"

target :: Txt -> Feature e
target = attr "target"

hreflang :: Txt -> Feature e
hreflang = attr "hreflang"

media :: Txt -> Feature e
media = attr "media"

rel :: Txt -> Feature e
rel = attr "rel"

ismap :: Bool -> Feature e
ismap = boolattr "ismap"

usemap :: Txt -> Feature e
usemap = attr "usemap"

shape :: Txt -> Feature e
shape = attr "shape"

src :: Txt -> Feature e
src = attr "src"

height_ :: Int -> Feature e
height_ = attr "height" . toTxt

width_ :: Int -> Feature e
width_ = attr "width" . toTxt

alt :: Txt -> Feature e
alt = attr "alt"

autoplay :: Bool -> Feature e
autoplay = boolattr "autoplay"

controls :: Bool -> Feature e
controls = boolattr "controls"

loop :: Bool -> Feature e
loop = boolattr "loop"

preload :: Txt -> Feature e
preload = attr "preload"

poster :: Txt -> Feature e
poster = attr "poster"

default_ :: Bool -> Feature e
default_ = boolattr "default"

kind :: Txt -> Feature e
kind = attr "kind"

srclang :: Txt -> Feature e
srclang = attr "srclang"

sandbox :: Txt -> Feature e
sandbox = attr "sandbox"

seamless :: Bool -> Feature e
seamless = boolattr "seamless"

srcdoc :: Txt -> Feature e
srcdoc = attr "srcdoc"

reversed :: Bool -> Feature e
reversed = boolattr "reversed"

start :: Int -> Feature e
start = attr "start" . toTxt

align :: Txt -> Feature e
align = attr "align"

colspan :: Int -> Feature e
colspan = attr "colspan" . toTxt

rowspan :: Int -> Feature e
rowspan = attr "rowspan" . toTxt

headers :: [Txt] -> Feature e
headers = attr "headers" . JSS.intercalate " "

scope :: Txt -> Feature e
scope = attr "scope"

async :: Bool -> Feature e
async = boolattr "async"

charset :: Txt -> Feature e
charset = attr "charset"

content :: Txt -> Feature e
content = attr "content"

defer :: Bool -> Feature e
defer = boolattr "defer"

httpEquiv :: Txt -> Feature e
httpEquiv = attr "http-equiv"

language :: Txt -> Feature e
language = attr "language"

scoped :: Bool -> Feature e
scoped = boolattr "scoped"

accesskey :: Char -> Feature e
accesskey = attr "accesskey" . JSS.singleton

contenteditable :: Bool -> Feature e
contenteditable = boolattr "contenteditable"

contextmenu :: Txt -> Feature e
contextmenu = attr "contextmenu"

dir :: Txt -> Feature e
dir = attr "dir"

draggable :: Bool -> Feature e
draggable b = attr "draggable" (if b then "true" else "false")

itemprop :: Txt -> Feature e
itemprop = attr "itemprop"

lang :: Txt -> Feature e
lang = attr "lang"

spellcheck :: Bool -> Feature e
spellcheck = boolattr "spellcheck"

tabindex :: Int -> Feature e
tabindex = attr "tabindex" . toTxt

cite_ :: Txt -> Feature e
cite_ = attr "cite"

datetime :: Txt -> Feature e
datetime = attr "datetime"

manifest :: Txt -> Feature e
manifest = attr "manifest"


--------------------------------------------------------------------------------
-- SVG Attributes

xlinkhref :: Txt -> Feature e
xlinkhref = attr "xlink:href"

clipPathUrl :: Txt -> Feature e
clipPathUrl = attr "clip-path" . (\x -> "url(#" <> x <> ")")

x_ :: Int -> Feature e
x_ = attr "x" . toTxt

y_ :: Int -> Feature e
y_ = attr "y" . toTxt

x1_ :: Int -> Feature e
x1_ = attr "x1" . toTxt

x2_ :: Int -> Feature e
x2_ = attr "x2" . toTxt

y1_ :: Int -> Feature e
y1_ = attr "y1" . toTxt

y2_ :: Int -> Feature e
y2_ = attr "y2" . toTxt

r_ :: Int -> Feature e
r_ = attr "r" . toTxt

cx_ :: Int -> Feature e
cx_ = attr "cx" . toTxt

cy_ :: Int -> Feature e
cy_ = attr "cy" . toTxt

--------------------------------------------------------------------------------
-- Event listener 'Attribute's

onClick :: e -> Feature e
onClick = on "click"

onDoubleClick :: e -> Feature e
onDoubleClick = on "dblclick"

onMouseDown :: e -> Feature e
onMouseDown = on "mousedown"

onMouseUp :: e -> Feature e
onMouseUp = on "mouseup"

onMouseEnter :: e -> Feature e
onMouseEnter = on "mouseenter"

onMouseLeave :: e -> Feature e
onMouseLeave = on "mouseleave"

onMouseOver :: e -> Feature e
onMouseOver = on "mouseover"

onMouseOut :: e -> Feature e
onMouseOut = on "mouseout"

onInput :: (Txt -> e) -> Feature e
onInput f = on' "input" Atomic.Attribute.defaultOptions $ fmap return $ parseMaybe $ \o -> do
  target <- o .: "target"
  value <- target .: "value"
  pure $ f value

onInputChange :: (Txt -> e) -> Feature e
onInputChange f = on' "change" Atomic.Attribute.defaultOptions $ fmap return $ parseMaybe $ \o -> do
  target <- o .: "target"
  value <- target .: "value"
  pure $ f value

onCheck :: (Bool -> e) -> Feature e
onCheck f = on' "change" Atomic.Attribute.defaultOptions $ fmap return $ parseMaybe $ \o -> do
  target <- o .: "target"
  checked <- target .: "checked"
  pure $ f checked

onSubmit :: e -> Feature e
onSubmit e = on' "submit" interceptOptions $ \_ ->
  return $ Just e

onBlur :: e -> Feature e
onBlur = on "blur"

onFocus :: e -> Feature e
onFocus = on "focus"

onKeyUp :: (Int -> e) -> Feature e
onKeyUp f = on' "keyup" Atomic.Attribute.defaultOptions $ fmap return $ parseMaybe $ \o -> do
  key <- o .: "key"
  pure $ f key

onKeyDown :: (Int -> e) -> Feature e
onKeyDown f = on' "keydown" Atomic.Attribute.defaultOptions $ fmap return $ parseMaybe $ \o -> do
  key <- o .: "key"
  pure $ f key

onKeyPress :: (Int -> e) -> Feature e
onKeyPress f = on' "keypress" Atomic.Attribute.defaultOptions $ fmap return $ parseMaybe $ \o -> do
  key <- o .: "key"
  pure $ f key

ignoreClick :: Feature e
ignoreClick = on' "click" interceptOptions $ const $ return Nothing
