{-# language DeriveDataTypeable #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language StandaloneDeriving #-}
{-# language CPP #-}
module Atomic.Attribute where

import Ef.Base hiding (Object,object)

import Data.Txt as T
import Data.JSON hiding (Options)

import Atomic.Default
import Atomic.FromTxt
import Atomic.ToTxt
import Atomic.Cond
import Atomic.UnsafeEq

import Data.Data
import Data.Maybe
import Data.String
import Data.Typeable
import Data.Void

import GHC.Exts

import qualified Data.Function as F
import Data.List (sortBy)

import Control.Lens (makePrisms,makeLenses)

import Prelude

#ifdef __GHCJS__
import qualified Data.JSString as JSS
import qualified GHCJS.DOM.Types as T
import qualified GHCJS.DOM.Window as W
import qualified GHCJS.DOM.Document as D
import qualified GHCJS.DOM.Location as L
#else
import qualified Data.Text as JSS
import Data.Aeson (Value(..))
#endif

type Win =
#ifdef __GHCJS__
  W.Window
#else
  ()
#endif

type Doc =
#ifdef __GHCJS__
  D.Document
#else
  ()
#endif

type ENode =
#ifdef __GHCJS__
  T.Element
#else
  ()
#endif

type TNode =
#ifdef __GHCJS__
  T.Text
#else
  ()
#endif

type NNode =
#ifdef __GHCJS__
  T.Node
#else
  ()
#endif

type Loc =
#ifdef __GHCJS__
  L.Location
#else
  ()
#endif

data Options = Options
  { _preventDef :: Bool
  , _stopProp   :: Bool
  } deriving (Eq)

instance Default Options where
  def = Options False False

data Feature e
  = NullFeature
  | Attribute
    { _attr :: Txt
    , _value :: Txt
    }
  | Property
    { _prop :: Txt
    , _value :: Txt
    }
  | Style
    { _stylePairs :: [(Txt,Txt)] }
  | On
    { _eventId :: Txt
    , _eventName :: Txt
    , _eventOptions :: Options
    , _eventCreate :: IO () -> ENode -> Obj -> IO (Maybe e)
    , _eventListener :: Maybe (IO ())
    }
  | OnWin
    { _eventId :: Txt
    , _eventName :: Txt
    , _eventOptions :: Options
    , _eventWinCreate :: IO () -> ENode -> Win -> Obj -> IO (Maybe e)
    , _eventListener :: Maybe (IO ())
    }
  | OnDoc
    { _eventId :: Txt
    , _eventName :: Txt
    , _eventOptions :: Options
    , _eventDocCreate :: IO () -> ENode -> Doc -> Obj -> IO (Maybe e)
    , _eventListener :: Maybe (IO ())
    }
  | OnBuild
    { _eventId :: Txt
    , _buildEvent :: e
    }
  | OnDestroy
    { _eventId :: Txt
    , _destroyEvent :: e
    }
  | OnWillMount
    { _eventId :: Txt
    , _willMountEvent :: ENode -> IO ()
    }
  | OnDidMount
    { _eventId :: Txt
    , _didMountEvent :: ENode -> IO ()
    }
  | forall model. OnUpdate
    { _eventId :: Txt
    , _updateModel :: model
    , _updateEvent :: model -> ENode -> IO ()
    }
  | forall model. OnModel
    { _eventId :: Txt
    , _watchModel :: model
    , _modelEvent :: model -> ENode -> e
    }
  | OnWillUnmount
    { _eventId :: Txt
    , _willUnmountEvent :: ENode -> IO ()
    }
  | OnDidUnmount
    { _eventId :: Txt
    , _didUnmountEvent :: ENode -> IO ()
    }
  | Link
    { _link :: Txt
    , _eventListener :: Maybe (IO ())
    }
  | SVGLink
    { _link :: Txt
    , _eventListener :: Maybe (IO ())
    }
  | XLink
    { _attr :: Txt
    , _value :: Txt
    }

deriving instance Functor Feature

instance ToJSON (Feature e) where
  toJSON f =
#ifdef __GHCJS__
    objectValue $
#endif
      go f
    where
      go NullFeature = object [ "type" .= ("null" :: Txt)]
      go (Property k v) = object [ "type" .= ("prop" :: Txt), "prop" .= k, "val" .= v]
      go (Attribute k v) = object [ "type" .= ("attr" :: Txt), "attr" .= k, "val" .= v]
      go (Style ss) = object [ "type" .= ("style" :: Txt), "styles" .= ss ]
      go (Link e _) = object [ "type" .= ("link" :: Txt), "link" .= e]
      go (SVGLink e _) = object [ "type" .= ("svglink" :: Txt), "link" .= e ]
      go (XLink k v) = object [ "type" .= ("xlink" :: Txt), "key" .= k, "val" .= v]
      go _ = object []

instance FromJSON (Feature e) where
  parseJSON o0 = do
#ifdef __GHCJS__
    flip (withObject "obj") o0 $ \o -> do
#else
      let (Object o) = o0
#endif
      t <- o .: "type"
      case t :: Txt of
        "null" ->
          pure NullFeature
        "attr" -> do
          k <- o .: "attr"
          v <- o .: "val"
          pure $ Attribute k v
        "prop" -> do
          k <- o .: "prop"
          v <- o .: "val"
          pure $ Property k v
        "style" -> do
          ss <- o .: "styles"
          pure $ Style ss
        "link" -> do
          l <- o .: "link"
          pure $ Link l Nothing
        "svglink" -> do
          l <- o .: "link"
          pure $ SVGLink l Nothing
        "xlink" -> do
          k <- o .: "key"
          v <- o .: "val"
          pure $ XLink k v
        _ -> Ef.Base.empty

instance Eq (Feature e) where
  (==) NullFeature NullFeature = True
  (==) (Property p v) (Property p' v') =
    prettyUnsafeEq p p' && prettyUnsafeEq v v'
  (==) (Attribute a v) (Attribute a' v') =
    prettyUnsafeEq a a' && prettyUnsafeEq v v'
  (==) (Style ss) (Style ss') =
    reallyUnsafeEq ss ss' || (==) (sortBy (compare `F.on` fst) ss) (sortBy (compare `F.on` fst) ss')
  (==) (On en e os ev _) (On en' e' os' ev' _) =
    prettyUnsafeEq en en' && prettyUnsafeEq e e' && prettyUnsafeEq os os'
  (==) (OnWin en e os ev _) (OnWin en' e' os' ev' _) =
    prettyUnsafeEq en en' && prettyUnsafeEq e e' && prettyUnsafeEq os os'
  (==) (OnDoc en e os ev _) (OnDoc en' e' os' ev' _) =
    prettyUnsafeEq en en' && prettyUnsafeEq e e' && prettyUnsafeEq os os'
  (==) (OnBuild en e) (OnBuild en' e') =
    prettyUnsafeEq en en'
  (==) (OnDestroy en e) (OnDestroy en' e') =
    prettyUnsafeEq en en'
  (==) (OnWillMount en e) (OnWillMount en' e') =
    prettyUnsafeEq en en'
  (==) (OnDidMount en e) (OnDidMount en' e') =
    prettyUnsafeEq en en'
  (==) (OnUpdate en m f) (OnUpdate en' m' f') =
    prettyUnsafeEq en en' && reallyVeryUnsafeEq m m'
  (==) (OnModel en m f) (OnModel en' m' f') =
    prettyUnsafeEq en en' && reallyVeryUnsafeEq m m'
  (==) (OnWillUnmount en e) (OnWillUnmount en' e') =
    prettyUnsafeEq en en'
  (==) (OnDidUnmount en e) (OnDidUnmount en' e') =
    prettyUnsafeEq en en'
  (==) (Link t _) (Link t' _) =
    prettyUnsafeEq t t'
  (==) (SVGLink t _) (SVGLink t' _) =
    prettyUnsafeEq t t'
  (==) (XLink t _) (XLink t' _) =
    prettyUnsafeEq t t'
  (==) _ _ = False

instance Cond (Feature e) where
  nil = NullFeature

instance IsString (Feature e) where
  fromString = Attribute "class" . fromString

instance GHC.Exts.IsList (Feature e) where
  type Item (Feature e) = Txt
  fromList = fromTxt . T.intercalate " "
  toList (Attribute "class" cs) = T.words cs
  toList _ = []

-- is this a terrible idea?
instance GHC.Exts.IsList ([a] -> [a]) where
  type Item ([a] -> [a]) = a
  fromList = go
    where
      go [] xs' = xs'
      go (x:xs) xs' = x:go xs xs'
  toList f = f []

instance {-# OVERLAPS #-} IsString [Feature e] where
  fromString s = [fromString s]

instance FromTxt (Feature e) where
  fromTxt = Attribute "class" . fromTxt

instance FromTxt [Feature e] where
  fromTxt t = [fromTxt t]

nullA :: Feature e
nullA = NullFeature

attribute :: Txt -> Txt -> Feature e
attribute = Attribute

boolAttribute :: Txt -> Feature e
boolAttribute nm = Attribute nm ""

property :: Txt -> Txt -> Feature e
property = Property

boolProperty :: Txt -> Bool -> Feature e
boolProperty nm b = property nm (if b then "true" else "") -- exploit the true/false nature of JS non-empty and empty strings, respectively

on :: Txt -> e -> Feature e
on ev e = On "" ev def (\_ _ _ -> return (Just e)) Nothing

on' :: Txt -> (IO () -> ENode -> Obj -> IO (Maybe e)) -> Feature e
on' = on_ ""

on_ :: Txt -> Txt -> (IO () -> ENode -> Obj -> IO (Maybe e)) -> Feature e
on_ en ev f = On en ev def f Nothing

onDoc :: Txt -> e -> Feature e
onDoc ev e = OnDoc "" ev def (\_ _ _ _ -> return (Just e)) Nothing

onDoc' :: Txt -> (IO () -> ENode -> Doc -> Obj -> IO (Maybe e)) -> Feature e
onDoc' = onDoc_ ""

onDoc_ :: Txt -> Txt -> (IO () -> ENode -> Doc -> Obj -> IO (Maybe e)) -> Feature e
onDoc_ en ev f = OnDoc en ev def f Nothing

onWin :: Txt -> e -> Feature e
onWin ev e = OnWin "" ev def (\_ _ _ _ -> return (Just e)) Nothing

onWin' :: Txt -> (IO () -> ENode -> Win -> Obj -> IO (Maybe e)) -> Feature e
onWin' = onWin_ ""

onWin_ :: Txt -> Txt -> (IO () -> ENode -> Win -> Obj -> IO (Maybe e)) -> Feature e
onWin_ en ev f = OnWin en ev def f Nothing

-- runs when the feature is created; top-down
-- onBuild is guaranteed to run before onDestroy, but ordering w.r.t. mount/update/unmount is undetermined
onBuild :: e -> Feature e
onBuild = onBuild' ""

onBuild' :: Txt -> e -> Feature e
onBuild' = OnBuild

-- runs when the feature is destroyed; top-down
-- onDestroy is guaranteed to run after onBuild, but ordering w.r.t. mount/update/unmount is undetermined
onDestroy :: e -> Feature e
onDestroy = onDestroy' ""

onDestroy' :: Txt -> e -> Feature e
onDestroy' = OnDestroy

-- runs when the attribute is being set; top-down
onWillMount :: (ENode -> IO ()) -> Feature e
onWillMount = onWillMount' ""

onWillMount' :: Txt -> (ENode -> IO ()) -> Feature e
onWillMount' = OnWillMount

-- runs when the element is inserted into its parent, not after it is inserted into the DOM; bottom-up
-- not guaranteed to run
onDidMount :: (ENode -> IO ()) -> Feature e
onDidMount = onDidMount' ""

onDidMount' :: Txt -> (ENode -> IO ()) -> Feature e
onDidMount' = OnDidMount

-- watches a model, as supplied, and calls the callback during feature diffing; top-down
-- make sure the body of the function will not change; must be totally static modulo the model/enode!
onModel :: model -> (model -> ENode -> e) -> Feature e
onModel = onModel' ""

onModel' :: Txt -> model -> (model -> ENode -> e) -> Feature e
onModel' = OnModel

-- watches a model, as supplied, and calls the callback during feature diffing; top-down
-- make sure the body of the function will not change; must be totally static modulo the model/enode!
onModelIO :: model -> (model -> ENode -> IO ()) -> Feature e
onModelIO = onModelIO' ""

onModelIO' :: Txt -> model -> (model -> ENode -> IO ()) -> Feature e
onModelIO' = OnUpdate

-- runs when the attribute is being cleaned up; top-down
onWillUnmount :: (ENode -> IO ()) -> Feature e
onWillUnmount = onWillUnmount' ""

onWillUnmount' :: Txt -> (ENode -> IO ()) -> Feature e
onWillUnmount' = OnWillUnmount

-- runs when the element is remove()'d, not when it is removed from the DOM; bottom-up
onDidUnmount :: (ENode -> IO ()) -> Feature e
onDidUnmount = onDidUnmount' ""

onDidUnmount' :: Txt -> (ENode -> IO ()) -> Feature e
onDidUnmount' = OnDidUnmount

preventDefault :: Feature e -> Feature e
preventDefault (On en ev os f m) = On en ev (os { _preventDef = True }) f m
preventDefault (OnDoc en ev os f m) = OnDoc en ev (os { _preventDef = True }) f m
preventDefault (OnWin en ev os f m) = OnWin en ev (os { _preventDef = True }) f m
preventDefault f = f

stopPropagation :: Feature e -> Feature e
stopPropagation (On en ev os f m) = On en ev (os { _stopProp = True }) f m
stopPropagation (OnDoc en ev os f m) = OnDoc en ev (os { _stopProp = True }) f m
stopPropagation (OnWin en ev os f m) = OnWin en ev (os { _stopProp = True }) f m
stopPropagation f = f

intercept :: Feature e -> Feature e
intercept (On en ev os f m) = On en ev (os { _preventDef = True, _stopProp = True }) f m
intercept (OnDoc en ev os f m) = OnDoc en ev (os { _preventDef = True, _stopProp = True }) f m
intercept (OnWin en ev os f m) = OnWin en ev (os { _preventDef = True, _stopProp = True }) f m
intercept f = f

styleList :: [(Txt,Txt)] -> Feature e
styleList = Style

linkA :: Txt -> Feature e
linkA = flip Link Nothing

href :: Txt -> Feature e
href = property "href"

val :: Txt -> Feature e
val jst = property "value" jst

xlink :: Txt -> Txt -> Feature e
xlink xl v = XLink xl v

svgLink :: Txt -> Feature e
svgLink = flip SVGLink Nothing

makePrisms ''Feature
makeLenses ''Feature
makePrisms ''Options
makeLenses ''Options

classA :: Txt -> Feature e
classA = attribute "class"

classes :: [(Txt,Bool)] -> Feature e
classes = classA
  . JSS.intercalate " "
  . mapMaybe (\(s,b) -> if b then Just s else Nothing)

-- not a fan of the inefficiency
-- addClass :: Txt -> [Feature e] -> [Feature e]
-- addClass c = go False
--   where
--     go False [] = [Attribute "class" c]
--     go True [] = []
--     go _ ((Attribute "class" cs):fs) = (Attribute "class" (c <> " " <> cs)) : go True fs
--     go b (f:fs) = f:go b fs

idA :: Txt -> Feature e
idA = property "id"

titleA :: Txt -> Feature e
titleA = property "title"

hiddenA :: Bool -> Feature e
hiddenA = boolProperty "hidden"

typeA :: Txt -> Feature e
typeA = property "type"

roleA :: Txt -> Feature e
roleA = attribute "role"

defaultValue :: Txt -> Feature e
defaultValue = attribute "default-value"

checked :: Bool -> Feature e
checked = boolProperty "checked"

defaultChecked :: Feature e
defaultChecked = attribute "checked" "checked"

placeholder :: Txt -> Feature e
placeholder = property "placeholder"

selected :: Bool -> Feature e
selected = boolProperty "selected"

accept :: Txt -> Feature e
accept = property "accept"

acceptCharset :: Txt -> Feature e
acceptCharset = property "accept-charset"

autocomplete :: Bool -> Feature e
autocomplete = boolProperty "autocomplete"

autofocus :: Bool -> Feature e
autofocus = boolProperty "autofocus"

disabled :: Bool -> Feature e
disabled = boolProperty "disabled"

enctype :: Txt -> Feature e
enctype = property "enctyp"

formaction :: Txt -> Feature e
formaction = attribute "formaction"

listA :: Txt -> Feature e
listA = attribute "list"

maxlength :: Int -> Feature e
maxlength = attribute "maxlength" . toTxt

minlength :: Int -> Feature e
minlength = attribute "minlength" . toTxt

methodA :: Txt -> Feature e
methodA = property "method"

multiple :: Bool -> Feature e
multiple b = property "multiple" (if b then "multiple" else "")

muted :: Bool -> Feature e
muted b = property "muted" (if b then "muted" else "")

name :: Txt -> Feature e
name = property "name"

novalidate :: Bool -> Feature e
novalidate = boolProperty "novalidate"

patternA :: Txt -> Feature e
patternA = property "pattern"

readonly :: Bool -> Feature e
readonly = boolProperty "readonly"

required :: Bool -> Feature e
required = boolProperty "required"

size :: Int -> Feature e
size = attribute "size" . toTxt

forA :: Txt -> Feature e
forA = property "htmlFor"

formA :: Txt -> Feature e
formA = attribute "form"

maxA :: Txt -> Feature e
maxA = property "max"

minA :: Txt -> Feature e
minA = property "min"

step :: Txt -> Feature e
step = property "step"

cols :: Int -> Feature e
cols = attribute "cols" . toTxt

rows :: Int -> Feature e
rows = attribute "rows" . toTxt

wrapA :: Txt -> Feature e
wrapA = property "wrap"

-- href :: Txt -> Feature e
-- href = attribute "href"

target :: Txt -> Feature e
target = property "target"

download :: Bool -> Feature e
download = boolProperty "download"

downloadAs :: Txt -> Feature e
downloadAs = property "download"

hreflang :: Txt -> Feature e
hreflang = property "hreflang"

media :: Txt -> Feature e
media = attribute "media"

rel :: Txt -> Feature e
rel = attribute "rel"

ismap :: Bool -> Feature e
ismap = boolProperty "ismap"

usemap :: Txt -> Feature e
usemap = property "usemap"

shape :: Txt -> Feature e
shape = property "shape"

coords :: Txt -> Feature e
coords = property "coords"

keytype :: Txt -> Feature e
keytype = property "keytype"

src :: Txt -> Feature e
src = property "src"

heightA :: ToTxt a => a -> Feature e
heightA = attribute "height" . toTxt

widthA :: ToTxt a => a -> Feature e
widthA = attribute "width" . toTxt

alt :: Txt -> Feature e
alt = property "alt"

autoplay :: Bool -> Feature e
autoplay = boolProperty "autoplay"

controls :: Bool -> Feature e
controls = boolProperty "controls"

loop :: Bool -> Feature e
loop = boolProperty "loop"

preload :: Txt -> Feature e
preload = property "preload"

poster :: Txt -> Feature e
poster = property "poster"

defaultA :: Bool -> Feature e
defaultA = boolProperty "default"

kind :: Txt -> Feature e
kind = property "kind"

srclang :: Txt -> Feature e
srclang = property "srclang"

sandbox :: Txt -> Feature e
sandbox = property "sandbox"

seamless :: Bool -> Feature e
seamless = boolProperty "seamless"

srcdoc :: Txt -> Feature e
srcdoc = property "srcdoc"

reversedA :: Bool -> Feature e
reversedA = boolProperty "reversed"

start :: Int -> Feature e
start = property "start" . toTxt

align :: Txt -> Feature e
align = property "align"

colspan :: Int -> Feature e
colspan = attribute "colspan" . toTxt

rowspan :: Int -> Feature e
rowspan = attribute "rowspan" . toTxt

headers :: [Txt] -> Feature e
headers = property "headers" . JSS.intercalate " "

scope :: Txt -> Feature e
scope = property "scope"

asyncA :: Bool -> Feature e
asyncA = boolProperty "async"

charset :: Txt -> Feature e
charset = attribute "charset"

contentA :: Txt -> Feature e
contentA = property "content"

defer :: Bool -> Feature e
defer = boolProperty "defer"

httpEquiv :: Txt -> Feature e
httpEquiv = property "http-equiv"

languageA :: Txt -> Feature e
languageA = property "language"

scopedA :: Bool -> Feature e
scopedA = boolProperty "scoped"

accesskey :: Char -> Feature e
accesskey = property "accesskey" . JSS.singleton

contenteditable :: Bool -> Feature e
contenteditable = boolProperty "contenteditable"

contextmenu :: Txt -> Feature e
contextmenu = attribute "contextmenu"

dir :: Txt -> Feature e
dir = property "dir"

draggable :: Bool -> Feature e
draggable b = attribute "draggable" (if b then "true" else "false")

dropzone :: Txt -> Feature e
dropzone = property "dropzone"

itemprop :: Txt -> Feature e
itemprop = attribute "itemprop"

lang :: Txt -> Feature e
lang = property "lang"

spellcheck :: Bool -> Feature e
spellcheck = boolProperty "spellcheck"

tabindex :: Int -> Feature e
tabindex = attribute "tabindex" . toTxt

citeA :: Txt -> Feature e
citeA = property "cite"

datetime :: Txt -> Feature e
datetime = attribute "datetime"

manifest :: Txt -> Feature e
manifest = attribute "manifest"

--------------------------------------------------------------------------------
-- SVG Attributes

svgAccentHeight :: Txt -> Feature e
svgAccentHeight = attribute "accent-height"

svgAccumulate :: Txt -> Feature e
svgAccumulate = attribute "accumulate"

svgAdditive :: Txt -> Feature e
svgAdditive = attribute "additive"

svgAlignmentBaseline :: Txt -> Feature e
svgAlignmentBaseline = attribute "alignment-baseline"

svgAllowReorder :: Txt -> Feature e
svgAllowReorder = attribute "allowReorder"

svgAlphabetic :: Txt -> Feature e
svgAlphabetic = attribute "alphabetic"

svgArabicForm :: Txt -> Feature e
svgArabicForm = attribute "arabic-form"

svgAscent :: Txt -> Feature e
svgAscent = attribute "ascent"

svgAttributeName :: Txt -> Feature e
svgAttributeName = attribute "attributeName"

svgAttributeType :: Txt -> Feature e
svgAttributeType = attribute "attributeType"

svgAutoReverse :: Txt -> Feature e
svgAutoReverse = attribute "autoReverse"

svgAzimuth :: Txt -> Feature e
svgAzimuth = attribute "azimuth"

svgBaseFrequency :: Txt -> Feature e
svgBaseFrequency = attribute "baseFrequency"

svgBaslineShift :: Txt -> Feature e
svgBaslineShift = attribute "basline-shift"

svgBaseProfile :: Txt -> Feature e
svgBaseProfile = attribute "baseProfile"

svgBbox :: Txt -> Feature e
svgBbox = attribute "bbox"

svgBegin :: Txt -> Feature e
svgBegin = attribute "begin"

svgBias :: Txt -> Feature e
svgBias = attribute "bias"

svgBy :: Txt -> Feature e
svgBy = attribute "by"

svgCalcMode :: Txt -> Feature e
svgCalcMode = attribute "calcMode"

svgCapHeight :: Txt -> Feature e
svgCapHeight = attribute "cap-height"

svgClass :: Txt -> Feature e
svgClass = attribute "class"

svgClip :: Txt -> Feature e
svgClip = attribute "clip"

svgClipPathUnits :: Txt -> Feature e
svgClipPathUnits = attribute "clipPathUnits"

svgClipPathA :: Txt -> Feature e
svgClipPathA = attribute "clip-path"

svgClipRule :: Txt -> Feature e
svgClipRule = attribute "clip-rule"

svgColor :: Txt -> Feature e
svgColor = attribute "color"

svgColorInterpolation :: Txt -> Feature e
svgColorInterpolation = attribute "color-interpolation"

svgColorInterpolationFilters :: Txt -> Feature e
svgColorInterpolationFilters = attribute "color-interpolation-filters"

svgColorProfileA :: Txt -> Feature e
svgColorProfileA = attribute "color-profile"

svgColorRendering :: Txt -> Feature e
svgColorRendering = attribute "color-rendering"

svgContentScriptType :: Txt -> Feature e
svgContentScriptType = attribute "contentScriptType"

svgContentStyleType :: Txt -> Feature e
svgContentStyleType = attribute "contentStyleType"

svgCursorA :: Txt -> Feature e
svgCursorA = attribute "cursor"

svgCx :: Txt -> Feature e
svgCx = attribute "cx"

svgCy :: Txt -> Feature e
svgCy = attribute "cy"

svgD :: Txt -> Feature e
svgD = attribute "d"

svgDecelerate :: Txt -> Feature e
svgDecelerate = attribute "decelerate"

svgDescent :: Txt -> Feature e
svgDescent = attribute "descent"

svgDiffuseConstant :: Txt -> Feature e
svgDiffuseConstant = attribute "diffuseConstant"

svgDirection :: Txt -> Feature e
svgDirection = attribute "direction"

svgDisplay :: Txt -> Feature e
svgDisplay = attribute "display"

svgDivisor :: Txt -> Feature e
svgDivisor = attribute "divisor"

svgDominantBaseline :: Txt -> Feature e
svgDominantBaseline = attribute "dominant-baseline"

svgDur :: Txt -> Feature e
svgDur = attribute "dur"

svgDx :: Txt -> Feature e
svgDx = attribute "dx"

svgDy :: Txt -> Feature e
svgDy = attribute "dy"

svgEdgeMode :: Txt -> Feature e
svgEdgeMode = attribute "edgeMode"

svgElevation :: Txt -> Feature e
svgElevation = attribute "elevation"

svgEnableBackground :: Txt -> Feature e
svgEnableBackground = attribute "enable-background"

svgEnd :: Txt -> Feature e
svgEnd = attribute "end"

svgExponent :: Txt -> Feature e
svgExponent = attribute "exponent"

svgExternalResourcesRequired :: Txt -> Feature e
svgExternalResourcesRequired = attribute "externalResourcesRequired"

svgFill :: Txt -> Feature e
svgFill = attribute "fill"

svgFillOpacity :: Txt -> Feature e
svgFillOpacity = attribute "fill-opacity"

svgFillRule :: Txt -> Feature e
svgFillRule = attribute "fill-rule"

svgFilterA :: Txt -> Feature e
svgFilterA = attribute "filter"

svgFilterRes :: Txt -> Feature e
svgFilterRes = attribute "filterRes"

svgFilterUnits :: Txt -> Feature e
svgFilterUnits = attribute "filterUnits"

svgFloodColor :: Txt -> Feature e
svgFloodColor = attribute "flood-color"

svgFontFamily :: Txt -> Feature e
svgFontFamily = attribute "font-family"

svgFontSize :: Txt -> Feature e
svgFontSize = attribute "font-size"

svgFontSizeAdjust :: Txt -> Feature e
svgFontSizeAdjust = attribute "font-size-adjust"

svgFontStretch :: Txt -> Feature e
svgFontStretch = attribute "font-stretch"

svgFontStyle :: Txt -> Feature e
svgFontStyle = attribute "font-style"

svgFontVariant :: Txt -> Feature e
svgFontVariant = attribute "font-variant"

svgFontWeight :: Txt -> Feature e
svgFontWeight = attribute "font-weight"

svgFormat :: Txt -> Feature e
svgFormat = attribute "format"

svgFrom :: Txt -> Feature e
svgFrom = attribute "from"

svgFx :: Txt -> Feature e
svgFx = attribute "fx"

svgFy :: Txt -> Feature e
svgFy = attribute "fy"

svgG1 :: Txt -> Feature e
svgG1 = attribute "g1"

svgG2 :: Txt -> Feature e
svgG2 = attribute "g2"

svgGlyphName :: Txt -> Feature e
svgGlyphName = attribute "glyph-name"

svgGlyphOrientationHorizontal :: Txt -> Feature e
svgGlyphOrientationHorizontal = attribute "glyph-orientation-horizontal"

svgGlyphOrientationVertical :: Txt -> Feature e
svgGlyphOrientationVertical = attribute "glyph-orientation-vertical"

svgGlyphRefA :: Txt -> Feature e
svgGlyphRefA = attribute "glyphRef"

svgGradientTransform :: Txt -> Feature e
svgGradientTransform = attribute "gradientTransform"

svgGradientUnits :: Txt -> Feature e
svgGradientUnits = attribute "gradientUnits"

svgHanging :: Txt -> Feature e
svgHanging = attribute "hanging"

svgHeight :: Txt -> Feature e
svgHeight = attribute "height"

-- prefer svgLink for inter-organism links
svgHref :: Txt -> Feature e
svgHref = attribute "href"

svgHorizAdvX :: Txt -> Feature e
svgHorizAdvX = attribute "horiz-adv-x"

svgHorizOriginX :: Txt -> Feature e
svgHorizOriginX = attribute "horiz-origin-x"

svgId :: Txt -> Feature e
svgId = attribute "id"

svgIdeographic :: Txt -> Feature e
svgIdeographic = attribute "ideographic"

svgImageRendering :: Txt -> Feature e
svgImageRendering = attribute "image-rendering"

svgIn :: Txt -> Feature e
svgIn = attribute "in"

svgIn2 :: Txt -> Feature e
svgIn2 = attribute "in2"

svgIntercept :: Txt -> Feature e
svgIntercept = attribute "intercept"

svgK :: Txt -> Feature e
svgK = attribute "k"

svgK1 :: Txt -> Feature e
svgK1 = attribute "k1"

svgK2 :: Txt -> Feature e
svgK2 = attribute "k2"

svgK3 :: Txt -> Feature e
svgK3 = attribute "k3"

svgK4 :: Txt -> Feature e
svgK4 = attribute "k4"

svgKernelMatrix :: Txt -> Feature e
svgKernelMatrix = attribute "kernelMatrix"

svgKernelUnitLength :: Txt -> Feature e
svgKernelUnitLength = attribute "kernelUnitLength"

svgKerning :: Txt -> Feature e
svgKerning = attribute "kerning"

svgKeyPoints :: Txt -> Feature e
svgKeyPoints = attribute "keyPoints"

svgKeySplines :: Txt -> Feature e
svgKeySplines = attribute "keySplines"

svgKeyTimes :: Txt -> Feature e
svgKeyTimes = attribute "keyTimes"

svgLang :: Txt -> Feature e
svgLang = attribute "lang"

svgLengthAdjust :: Txt -> Feature e
svgLengthAdjust = attribute "lengthAdjust"

svgLetterSpacing :: Txt -> Feature e
svgLetterSpacing = attribute "letter-spacing"

svgLightingColor :: Txt -> Feature e
svgLightingColor = attribute "lighting-color"

svgLimitingConeAngle :: Txt -> Feature e
svgLimitingConeAngle = attribute "limitingConeAngle"

svgLocal :: Txt -> Feature e
svgLocal = attribute "local"

svgMarkerEnd :: Txt -> Feature e
svgMarkerEnd = attribute "marker-end"

svgMarkerMid :: Txt -> Feature e
svgMarkerMid = attribute "marker-mid"

svgMarkerStart :: Txt -> Feature e
svgMarkerStart = attribute "marker-start"

svgMarkerHeight :: Txt -> Feature e
svgMarkerHeight = attribute "markerHeight"

svgMarkerUnits :: Txt -> Feature e
svgMarkerUnits = attribute "markerUnits"

svgMarkerWidth :: Txt -> Feature e
svgMarkerWidth = attribute "markerWidth"

svgMaskA :: Txt -> Feature e
svgMaskA = attribute "maskA"

svgMaskContentUnits :: Txt -> Feature e
svgMaskContentUnits = attribute "maskContentUnits"

svgMaskUnits :: Txt -> Feature e
svgMaskUnits = attribute "maskUnits"

svgMathematical :: Txt -> Feature e
svgMathematical = attribute "mathematical"

svgMax :: Txt -> Feature e
svgMax = attribute "max"

svgMedia :: Txt -> Feature e
svgMedia = attribute "media"

svgMethod :: Txt -> Feature e
svgMethod = attribute "method"

svgMin :: Txt -> Feature e
svgMin = attribute "min"

svgMode :: Txt -> Feature e
svgMode = attribute "mode"

svgName :: Txt -> Feature e
svgName = attribute "name"

svgNumOctaves :: Txt -> Feature e
svgNumOctaves = attribute "numOctaves"

svgOffset :: Txt -> Feature e
svgOffset = attribute "offset"

svgOnabort :: Txt -> Feature e
svgOnabort = attribute "onabort"

svgOnactivate :: Txt -> Feature e
svgOnactivate = attribute "onactivate"

svgOnbegin :: Txt -> Feature e
svgOnbegin = attribute "onbegin"

svgOnclick :: Txt -> Feature e
svgOnclick = attribute "onclick"

svgOnend :: Txt -> Feature e
svgOnend = attribute "onend"

svgOnerror :: Txt -> Feature e
svgOnerror = attribute "onerror"

svgOnfocusin :: Txt -> Feature e
svgOnfocusin = attribute "onfocusin"

svgOnfocusout :: Txt -> Feature e
svgOnfocusout = attribute "onfocusout"

svgOnload :: Txt -> Feature e
svgOnload = attribute "onload"

svgOnmousedown :: Txt -> Feature e
svgOnmousedown = attribute "onmousedown"

svgOnmousemove :: Txt -> Feature e
svgOnmousemove = attribute "onmousemove"

svgOnmouseout :: Txt -> Feature e
svgOnmouseout = attribute "onmouseout"

svgOnmouseover :: Txt -> Feature e
svgOnmouseover = attribute "onmouseover"

svgOnmouseup :: Txt -> Feature e
svgOnmouseup = attribute "onmouseup"

svgOnrepeat :: Txt -> Feature e
svgOnrepeat = attribute "onrepeat"

svgOnresize :: Txt -> Feature e
svgOnresize = attribute "onresize"

svgOnscroll :: Txt -> Feature e
svgOnscroll = attribute "onscroll"

svgOnunload :: Txt -> Feature e
svgOnunload = attribute "onunload"

svgOnzoom :: Txt -> Feature e
svgOnzoom = attribute "onzoom"

svgOpacity :: Txt -> Feature e
svgOpacity = attribute "opacity"

svgOperator :: Txt -> Feature e
svgOperator = attribute "operator"

svgOrder :: Txt -> Feature e
svgOrder = attribute "order"

svgOrient :: Txt -> Feature e
svgOrient = attribute "orient"

svgOrientation :: Txt -> Feature e
svgOrientation = attribute "orientation"

svgOrigin :: Txt -> Feature e
svgOrigin = attribute "origin"

svgOverflow :: Txt -> Feature e
svgOverflow = attribute "overflow"

svgOverlinePosition :: Txt -> Feature e
svgOverlinePosition = attribute "overline-position"

svgOverlineThickness :: Txt -> Feature e
svgOverlineThickness = attribute "overline-thickness"

svgPanose1 :: Txt -> Feature e
svgPanose1 = attribute "panose-1"

svgPaintOrder :: Txt -> Feature e
svgPaintOrder = attribute "paint-order"

svgPathLength :: Txt -> Feature e
svgPathLength = attribute "pathLength"

svgPatternContentUnits :: Txt -> Feature e
svgPatternContentUnits = attribute "patternContentUnits"

svgPatternTransform :: Txt -> Feature e
svgPatternTransform = attribute "patternTransform"

svgPatternUnits :: Txt -> Feature e
svgPatternUnits = attribute "patternUnits"

svgPointerEvents :: Txt -> Feature e
svgPointerEvents = attribute "pointer-events"

svgPoints :: Txt -> Feature e
svgPoints = attribute "points"

svgPointsAtX :: Txt -> Feature e
svgPointsAtX = attribute "pointsAtX"

svgPointsAtY :: Txt -> Feature e
svgPointsAtY = attribute "pointsAtY"

svgPointsAtZ :: Txt -> Feature e
svgPointsAtZ = attribute "pointsAtZ"

svgPreserveAlpha :: Txt -> Feature e
svgPreserveAlpha = attribute "preserveAlpha"

svgPreserveAspectRatio :: Txt -> Feature e
svgPreserveAspectRatio = attribute "preserveAspectRatio"

svgPrimitiveUnits :: Txt -> Feature e
svgPrimitiveUnits = attribute "primitiveUnits"

svgR :: Txt -> Feature e
svgR = attribute "r"

svgRadius :: Txt -> Feature e
svgRadius = attribute "radius"

svgRefX :: Txt -> Feature e
svgRefX = attribute "refX"

svgRefY :: Txt -> Feature e
svgRefY = attribute "refY"

svgRenderingIntent :: Txt -> Feature e
svgRenderingIntent = attribute "rendering-intent"

svgRepeatCount :: Txt -> Feature e
svgRepeatCount = attribute "repeatCount"

svgRepeatDur :: Txt -> Feature e
svgRepeatDur = attribute "repeatDur"

svgRequiredExtensions :: Txt -> Feature e
svgRequiredExtensions = attribute "requiredExtensions"

svgRequiredFeatures :: Txt -> Feature e
svgRequiredFeatures = attribute "requiredFeatures"

svgRestart :: Txt -> Feature e
svgRestart = attribute "restart"

svgResult :: Txt -> Feature e
svgResult = attribute "result"

svgRotate :: Txt -> Feature e
svgRotate = attribute "rotate"

svgRx :: Txt -> Feature e
svgRx = attribute "rx"

svgRy :: Txt -> Feature e
svgRy = attribute "ry"

svgScale :: Txt -> Feature e
svgScale = attribute "scale"

svgSeed :: Txt -> Feature e
svgSeed = attribute "seed"

svgShapeRendering :: Txt -> Feature e
svgShapeRendering = attribute "shape-rendering"

svgSlope :: Txt -> Feature e
svgSlope = attribute "slope"

svgSpacing :: Txt -> Feature e
svgSpacing = attribute "spacing"

svgSpecularConstant :: Txt -> Feature e
svgSpecularConstant = attribute "specularConstant"

svgSpecularExponent :: Txt -> Feature e
svgSpecularExponent = attribute "specularExponent"

svgSpeed :: Txt -> Feature e
svgSpeed = attribute "speed"

svgSpreadMethod :: Txt -> Feature e
svgSpreadMethod = attribute "spreadMethod"

svgStartOffset :: Txt -> Feature e
svgStartOffset = attribute "startOffset"

svgStdDeviationA :: Txt -> Feature e
svgStdDeviationA = attribute "stdDeviationA"

svgStemh :: Txt -> Feature e
svgStemh = attribute "stemh"

svgStemv :: Txt -> Feature e
svgStemv = attribute "stemv"

svgStitchTiles :: Txt -> Feature e
svgStitchTiles = attribute "stitchTiles"

svgStopColor :: Txt -> Feature e
svgStopColor = attribute "stop-color"

svgStopOpacity :: Txt -> Feature e
svgStopOpacity = attribute "stop-opacity"

svgStrikethroughPosition :: Txt -> Feature e
svgStrikethroughPosition = attribute "strikethrough-position"

svgStrikethroughThickness :: Txt -> Feature e
svgStrikethroughThickness = attribute "strikethrough-thickness"

svgString :: Txt -> Feature e
svgString = attribute "string"

svgStroke :: Txt -> Feature e
svgStroke = attribute "stroke"

svgStrokeDasharray :: Txt -> Feature e
svgStrokeDasharray = attribute "stroke-dasharray"

svgStrokeDashoffset :: Txt -> Feature e
svgStrokeDashoffset = attribute "stroke-dashoffset"

svgStrokeLinecap :: Txt -> Feature e
svgStrokeLinecap = attribute "stroke-linecap"

svgStrokeLinejoin :: Txt -> Feature e
svgStrokeLinejoin = attribute "stroke-linejoin"

svgStrokeMiterlimit :: Txt -> Feature e
svgStrokeMiterlimit = attribute "stroke-miterlimit"

svgStrokeOpacity :: Txt -> Feature e
svgStrokeOpacity = attribute "stroke-opacity"

svgStrokeWidth :: Txt -> Feature e
svgStrokeWidth = attribute "stroke-width"

svgStyleA :: Txt -> Feature e
svgStyleA = attribute "style"

svgSurfaceScale :: Txt -> Feature e
svgSurfaceScale = attribute "surfaceScale"

svgSystemLanguage :: Txt -> Feature e
svgSystemLanguage = attribute "systemLanguage"

svgTabindex :: Txt -> Feature e
svgTabindex = attribute "tabindex"

svgTableValues :: Txt -> Feature e
svgTableValues = attribute "tableValues"

svgTarget :: Txt -> Feature e
svgTarget = attribute "target"

svgTargetX :: Txt -> Feature e
svgTargetX = attribute "targetX"

svgTargetY :: Txt -> Feature e
svgTargetY = attribute "targetY"

svgTextAnchor :: Txt -> Feature e
svgTextAnchor = attribute "text-anchor"

svgTextDecoration :: Txt -> Feature e
svgTextDecoration = attribute "text-decoration"

svgTextRendering :: Txt -> Feature e
svgTextRendering = attribute "text-rendering"

svgTextLength :: Txt -> Feature e
svgTextLength = attribute "textLength"

svgTo :: Txt -> Feature e
svgTo = attribute "to"

svgTransform :: Txt -> Feature e
svgTransform = attribute "transform"

svgType :: Txt -> Feature e
svgType = attribute "type"

svgU1 :: Txt -> Feature e
svgU1 = attribute "u1"

svgU2 :: Txt -> Feature e
svgU2 = attribute "u2"

svgUnerlinePosition :: Txt -> Feature e
svgUnerlinePosition = attribute "unerline-position"

svgUnderlineThickness :: Txt -> Feature e
svgUnderlineThickness = attribute "underline-thickness"

svgUnicode :: Txt -> Feature e
svgUnicode = attribute "unicode"

svgUnicodeBidi :: Txt -> Feature e
svgUnicodeBidi = attribute "unicode-bidi"

svgUnicodeRange :: Txt -> Feature e
svgUnicodeRange = attribute "unicode-range"

svgUnitsPerEm :: Txt -> Feature e
svgUnitsPerEm = attribute "units-per-em"

svgVAlphabetic :: Txt -> Feature e
svgVAlphabetic = attribute "v-alphabetic"

svgVHanging :: Txt -> Feature e
svgVHanging = attribute "v-hanging"

svgVIdeographic :: Txt -> Feature e
svgVIdeographic = attribute "v-ideographic"

svgVMathematical :: Txt -> Feature e
svgVMathematical = attribute "v-mathematical"

svgValues :: Txt -> Feature e
svgValues = attribute "values"

svgVersion :: Txt -> Feature e
svgVersion = attribute "version"

svgVertAdvY :: Txt -> Feature e
svgVertAdvY = attribute "vert-adv-y"

svgVertOriginX :: Txt -> Feature e
svgVertOriginX = attribute "vert-origin-x"

svgVerOriginY :: Txt -> Feature e
svgVerOriginY = attribute "ver-origin-y"

svgViewBox :: Txt -> Feature e
svgViewBox = attribute "viewBox"

svgViewTarget :: Txt -> Feature e
svgViewTarget = attribute "viewTarget"

svgVisibility :: Txt -> Feature e
svgVisibility = attribute "visibility"

svgWidth :: Txt -> Feature e
svgWidth = attribute "width"

svgWidths :: Txt -> Feature e
svgWidths = attribute "widths"

svgWordSpacing :: Txt -> Feature e
svgWordSpacing = attribute "word-spacing"

svgWritingMode :: Txt -> Feature e
svgWritingMode = attribute "writing-mode"

svgX :: Txt -> Feature e
svgX = attribute "x"

svgXHeight :: Txt -> Feature e
svgXHeight = attribute "xHeight"

svgX1 :: Txt -> Feature e
svgX1 = attribute "x1"

svgX2 :: Txt -> Feature e
svgX2 = attribute "x2"

svgXChannelSelector :: Txt -> Feature e
svgXChannelSelector = attribute "xChannelSelector"

svgXlinkactuate :: Txt -> Feature e
svgXlinkactuate = xlink "xlink:actuate"

svgXlinkarcrole :: Txt -> Feature e
svgXlinkarcrole = xlink "xlink:arcrole"

svgXlinkhref :: Txt -> Feature e
svgXlinkhref = xlink "xlink:href"

svgXlinkrole :: Txt -> Feature e
svgXlinkrole = xlink "xlink:role"

svgXlinkshow :: Txt -> Feature e
svgXlinkshow = xlink "xlink:show"

svgXlinktitle :: Txt -> Feature e
svgXlinktitle = xlink "xlink:title"

svgXlinktype :: Txt -> Feature e
svgXlinktype = xlink "xlink:type"

svgXmlbase :: Txt -> Feature e
svgXmlbase = attribute "xml:base"

svgXmllang :: Txt -> Feature e
svgXmllang = attribute "xml:lang"

svgXmlspace :: Txt -> Feature e
svgXmlspace = attribute "xml:space"

svgY :: Txt -> Feature e
svgY = attribute "y"

svgY1 :: Txt -> Feature e
svgY1 = attribute "y1"

svgY2 :: Txt -> Feature e
svgY2 = attribute "y2"

svgYChannelSelector :: Txt -> Feature e
svgYChannelSelector = attribute "yChannelSelector"

svgZ :: Txt -> Feature e
svgZ = attribute "z"

svgZoomAndPan :: Txt -> Feature e
svgZoomAndPan = attribute "zoomAndPan"

clipPathUrl :: Txt -> Feature e
clipPathUrl = attribute "clip-path" . (\x -> "url(#" <> x <> ")")

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

onTouchStart :: e -> Feature e
onTouchStart = on "touchstart"

onTouchEnd :: e -> Feature e
onTouchEnd = on "touchend"

onMouseEnter :: e -> Feature e
onMouseEnter = on "mouseenter"

onMouseLeave :: e -> Feature e
onMouseLeave = on "mouseleave"

onMouseOver :: e -> Feature e
onMouseOver = on "mouseover"

onMouseOut :: e -> Feature e
onMouseOut = on "mouseout"

onMouseMove :: e -> Feature e
onMouseMove = on "mousemove"

onTouchMove :: e -> Feature e
onTouchMove = on "touchmove"

onTouchCancel :: e -> Feature e
onTouchCancel = on "touchcancel"

onInput :: (Txt -> e) -> Feature e
onInput f = on' "input" $ \_ _ -> fmap return $ flip parse $ \o -> do
  target <- o .: "target"
  value <- target .: "value"
  pure $ f value

onInputChange :: (Txt -> e) -> Feature e
onInputChange f = on' "change" $ \_ _ -> fmap return $ flip parse $ \o -> do
  target <- o .: "target"
  value <- target .: "value"
  pure $ f value

onCheck :: (Bool -> e) -> Feature e
onCheck f = on' "change" $ \_ _ -> fmap return $ flip parse $ \o -> do
  target <- o .: "target"
  checked <- target .: "checked"
  pure $ f checked

onSubmit :: e -> Feature e
onSubmit e = intercept $ on' "submit" $ \_ _ _ -> return $ Just e

onBlur :: e -> Feature e
onBlur = on "blur"

onFocus :: e -> Feature e
onFocus = on "focus"

onKeyUp :: (Int -> e) -> Feature e
onKeyUp f = on' "keyup" $ \_ _ -> fmap return $ flip parse $ \o -> do
  key <- o .: "keyCode"
  pure $ f key


onKeyDown :: (Int -> e) -> Feature e
onKeyDown f = on' "keydown" $ \_ _ -> fmap return $ flip parse $ \o -> do
  key <- o .: "keyCode"
  pure $ f key

onKeyPress :: (Int -> e) -> Feature e
onKeyPress f = on' "keypress" $ \_ _ -> fmap return $ flip parse $ \o -> do
  key <- o .: "keyCode"
  pure $ f key

ignoreClick :: Feature e
ignoreClick = intercept $ on' "click" $ \_ _ _ -> return Nothing
