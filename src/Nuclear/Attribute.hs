{-# language CPP #-}
{-# language OverloadedStrings #-}
module Nuclear.Attribute where

import Ef.Base

import Data.JSText hiding (Options)

import Nuclear.FromText
import Nuclear.ToText
import Nuclear.Cond

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
    { _attr :: JSText
    , _value :: Either Bool JSText
    }
  | Style
    { _stylePairs :: [(JSText,JSText)] }
  | CurrentValue
    { _currentValue :: JSText }
  | On
    { _event :: JSText
    , _eventE :: e
    , _eventListener :: Maybe (IO ())
    }
  | On'
    { _event :: JSText
    , _eventOptions :: Options
    , _eventECreate :: Data.JSText.Object -> IO (Maybe e)
    , _eventListener :: Maybe (IO ())
    }
  | Link
    { _event :: JSText
    , _eventListener :: Maybe (IO ())
    }
  deriving (Functor)

type Attribute ms = Feature (Code ms IO ())

instance Cond (Feature e) where
  nil = NullFeature

instance IsString (Feature e) where
  fromString = Attribute "class" . Right . fromString

instance IsString [Feature e] where
  fromString s = [fromString s]

instance FromText (Feature e) where
  fromText = Attribute "class" . Right . fromText

instance FromText [Feature e] where
  fromText t = [fromText t]

nullF :: Feature e
nullF = NullFeature

attribute :: JSText -> JSText -> Feature e
attribute nm = Attribute nm . Right

attr :: JSText -> JSText -> Feature e
attr = attribute

boolAttribute :: JSText -> Bool -> Feature e
boolAttribute nm = Attribute nm . Left

boolattr :: JSText -> Bool -> Feature e
boolattr = boolAttribute

on' :: JSText -> Options -> (Data.JSText.Object -> IO (Maybe e)) -> Feature e
on' ev os f = On' ev os f Nothing

on :: JSText -> e -> Feature e
on ev e = On ev e Nothing

styleList :: [(JSText,JSText)] -> Feature e
styleList = Style

link :: JSText -> Feature e
link = flip Link Nothing

value :: JSText -> Feature e
value jst = CurrentValue jst

#ifdef LENS
makePrisms ''Feature
makeLenses ''Feature
makePrisms ''Options
makeLenses ''Options
#endif

class_ :: JSText -> Feature e
class_ = attr "class"

classes :: [(JSText,Bool)] -> Feature e
classes = class_
  . JSS.intercalate " "
  . mapMaybe (\(s,b) -> if b then Just s else Nothing)

id_ :: JSText -> Feature e
id_ = attr "id"

title_ :: JSText -> Feature e
title_ = attr "title"

hidden :: Bool -> Feature e
hidden = boolattr "hidden"

type_ :: JSText -> Feature e
type_ = attr "type"

initialValue :: JSText -> Feature e
initialValue = attr "value"

defaultValue :: JSText -> Feature e
defaultValue = attr "default-value"

checked :: Bool -> Feature e
checked = boolattr "checked"

placeholder :: JSText -> Feature e
placeholder = attr "placeholder"

selected :: Bool -> Feature e
selected = boolattr "selected"

accept :: JSText -> Feature e
accept = attr "accept"

acceptCharset :: JSText -> Feature e
acceptCharset = attr "accept-charset"

autocomplete :: Bool -> Feature e
autocomplete b = attr "autocomplete" (if b then "on" else "off")

autofocus :: Bool -> Feature e
autofocus = boolattr "autofocus"

disabled :: Bool -> Feature e
disabled = boolattr "disabled"

enctype :: JSText -> Feature e
enctype = attr "enctyp"

formaction :: JSText -> Feature e
formaction = attr "formaction"

list :: JSText -> Feature e
list = attr "list"

maxlength :: Int -> Feature e
maxlength = attr "maxlength" . toText

minlength :: Int -> Feature e
minlength = attr "minlength" . toText

method_ :: JSText -> Feature e
method_ = attr "method"

multiple :: Bool -> Feature e
multiple = boolattr "multiple"

name :: JSText -> Feature e
name = attr "name"

novalidate :: Bool -> Feature e
novalidate = boolattr "novalidate"

pattern_ :: JSText -> Feature e
pattern_ = attr "pattern"

readonly :: Bool -> Feature e
readonly = boolattr "readonly"

required :: Bool -> Feature e
required = boolattr "required"

size :: Int -> Feature e
size = attr "size" . toText

for_ :: JSText -> Feature e
for_ = attr "for"

form_ :: JSText -> Feature e
form_ = attr "form"

max_ :: JSText -> Feature e
max_ = attr "max"

min_ :: JSText -> Feature e
min_ = attr "min"

step :: JSText -> Feature e
step = attr "step"

cols :: Int -> Feature e
cols = attr "cols" . toText

rows :: Int -> Feature e
rows = attr "rows" . toText

wrap_ :: JSText -> Feature e
wrap_ = attr "wrap"

href :: JSText -> Feature e
href = attr "href"

target :: JSText -> Feature e
target = attr "target"

hreflang :: JSText -> Feature e
hreflang = attr "hreflang"

media :: JSText -> Feature e
media = attr "media"

rel :: JSText -> Feature e
rel = attr "rel"

ismap :: Bool -> Feature e
ismap = boolattr "ismap"

usemap :: JSText -> Feature e
usemap = attr "usemap"

shape :: JSText -> Feature e
shape = attr "shape"

src :: JSText -> Feature e
src = attr "src"

height_ :: Int -> Feature e
height_ = attr "height" . toText

width_ :: Int -> Feature e
width_ = attr "width" . toText

alt :: JSText -> Feature e
alt = attr "alt"

autoplay :: Bool -> Feature e
autoplay = boolattr "autoplay"

controls :: Bool -> Feature e
controls = boolattr "controls"

loop :: Bool -> Feature e
loop = boolattr "loop"

preload :: JSText -> Feature e
preload = attr "preload"

poster :: JSText -> Feature e
poster = attr "poster"

default_ :: Bool -> Feature e
default_ = boolattr "default"

kind :: JSText -> Feature e
kind = attr "kind"

srclang :: JSText -> Feature e
srclang = attr "srclang"

sandbox :: JSText -> Feature e
sandbox = attr "sandbox"

seamless :: Bool -> Feature e
seamless = boolattr "seamless"

srcdoc :: JSText -> Feature e
srcdoc = attr "srcdoc"

reversed :: Bool -> Feature e
reversed = boolattr "reversed"

start :: Int -> Feature e
start = attr "start" . toText

align :: JSText -> Feature e
align = attr "align"

colspan :: Int -> Feature e
colspan = attr "colspan" . toText

rowspan :: Int -> Feature e
rowspan = attr "rowspan" . toText

headers :: [JSText] -> Feature e
headers = attr "headers" . JSS.intercalate " "

scope :: JSText -> Feature e
scope = attr "scope"

async :: Bool -> Feature e
async = boolattr "async"

charset :: JSText -> Feature e
charset = attr "charset"

content :: JSText -> Feature e
content = attr "content"

defer :: Bool -> Feature e
defer = boolattr "defer"

httpEquiv :: JSText -> Feature e
httpEquiv = attr "http-equiv"

language :: JSText -> Feature e
language = attr "language"

scoped :: Bool -> Feature e
scoped = boolattr "scoped"

accesskey :: Char -> Feature e
accesskey = attr "accesskey" . JSS.singleton

contenteditable :: Bool -> Feature e
contenteditable = boolattr "contenteditable"

contextmenu :: JSText -> Feature e
contextmenu = attr "contextmenu"

dir :: JSText -> Feature e
dir = attr "dir"

draggable :: Bool -> Feature e
draggable b = attr "draggable" (if b then "true" else "false")

itemprop :: JSText -> Feature e
itemprop = attr "itemprop"

lang :: JSText -> Feature e
lang = attr "lang"

spellcheck :: Bool -> Feature e
spellcheck = boolattr "spellcheck"

tabindex :: Int -> Feature e
tabindex = attr "tabindex" . toText

cite_ :: JSText -> Feature e
cite_ = attr "cite"

datetime :: JSText -> Feature e
datetime = attr "datetime"

manifest :: JSText -> Feature e
manifest = attr "manifest"


--------------------------------------------------------------------------------
-- SVG Attributes

xlinkhref :: JSText -> Feature e
xlinkhref = attr "xlink:href"

clipPathUrl :: JSText -> Feature e
clipPathUrl = attr "clip-path" . (\x -> "url(#" <> x <> ")")

x_ :: Int -> Feature e
x_ = attr "x" . toText

y_ :: Int -> Feature e
y_ = attr "y" . toText

x1_ :: Int -> Feature e
x1_ = attr "x1" . toText

x2_ :: Int -> Feature e
x2_ = attr "x2" . toText

y1_ :: Int -> Feature e
y1_ = attr "y1" . toText

y2_ :: Int -> Feature e
y2_ = attr "y2" . toText

r_ :: Int -> Feature e
r_ = attr "r" . toText

cx_ :: Int -> Feature e
cx_ = attr "cx" . toText

cy_ :: Int -> Feature e
cy_ = attr "cy" . toText

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

instance FromJSON Data.JSText.Object where
  parseJSON = withObject "object" pure

onInput :: (JSText -> e) -> Feature e
onInput f = on' "input" Nuclear.Attribute.defaultOptions $ fmap return $ parseMaybe $ \o -> do
  target <- o .: "target"
  value <- target .: "value"
  pure $ f value

onInputChange :: (JSText -> e) -> Feature e
onInputChange f = on' "change" Nuclear.Attribute.defaultOptions $ fmap return $ parseMaybe $ \o -> do
  target <- o .: "target"
  value <- target .: "value"
  pure $ f value

onCheck :: (Bool -> e) -> Feature e
onCheck f = on' "change" Nuclear.Attribute.defaultOptions $ fmap return $ parseMaybe $ \o -> do
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
onKeyUp f = on' "keyup" Nuclear.Attribute.defaultOptions $ fmap return $ parseMaybe $ \o -> do
  key <- o .: "key"
  pure $ f key

onKeyDown :: (Int -> e) -> Feature e
onKeyDown f = on' "keydown" Nuclear.Attribute.defaultOptions $ fmap return $ parseMaybe $ \o -> do
  key <- o .: "key"
  pure $ f key

onKeyPress :: (Int -> e) -> Feature e
onKeyPress f = on' "keypress" Nuclear.Attribute.defaultOptions $ fmap return $ parseMaybe $ \o -> do
  key <- o .: "key"
  pure $ f key

ignoreClick :: Feature e
ignoreClick = on' "click" interceptOptions $ const $ return Nothing
