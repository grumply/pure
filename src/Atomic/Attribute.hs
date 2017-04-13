{-# language DeriveDataTypeable #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language CPP #-}
module Atomic.Attribute where

import Ef.Base hiding (Object,object)

import Data.Txt as T
import Data.JSON hiding (Options)

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

#ifdef __GHCJS__
import qualified Data.JSString as JSS
#else
import qualified Data.Text as JSS
import Data.Aeson (Value(..))
#endif

data Options = Options
  { _preventDef :: Bool
  , _stopProp   :: Bool
  } deriving (Eq)

defaultOptions :: Options
defaultOptions = Options False False

noDefaultOptions :: Options
noDefaultOptions = Options True False

interceptOptions :: Options
interceptOptions = Options True True

data Feature e
  = NullFeature
  | Attribute
    { _attr :: Txt
    , _value :: Txt
    }
  | Style
    { _stylePairs :: [(Txt,Txt)] }
  | CurrentValue
    { _value :: Txt }
  | On
    { _eventName :: Txt
    , _event :: e
    , _eventListener :: Maybe (IO ())
    }
  | On'
    { _eventName :: Txt
    , _eventOptions :: Options
    , _eventCreate :: Obj -> IO (Maybe e)
    , _eventListener :: Maybe (IO ())
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
  deriving (Functor)

instance ToJSON (Feature e) where
  toJSON f =
#ifdef __GHCJS__
    objectValue $
#endif
      go f
    where
      go NullFeature = object [ "type" .= ("null" :: Txt)]
      go (Attribute k v) = object [ "type" .= ("attr" :: Txt), "attr" .= k, "val" .= v]
      go (Style ss) = object [ "type" .= ("style" :: Txt), "styles" .= ss ]
      go (CurrentValue cv) = object [ "type" .= ("value" :: Txt), "val" .= cv ]
      go (Link e _) = object [ "type" .= ("link" :: Txt), "link" .= e]
      go (SVGLink e _) = object [ "type" .= ("svglink" :: Txt), "link" .= e ]
      go (XLink k v) = object [ "type" .= ("xlink" :: Txt), "key" .= k, "val" .= v]

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
        "style" -> do
          ss <- o .: "styles"
          pure $ Style ss
        "value" -> do
          v <- o .: "val"
          pure $ CurrentValue v
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
  (==) (Attribute a v) (Attribute a' v') =
    prettyUnsafeEq a a' && prettyUnsafeEq v v'
  (==) (Style ss) (Style ss') =
    reallyUnsafeEq ss ss' || (==) (sortBy (compare `F.on` fst) ss) (sortBy (compare `F.on` fst) ss')
  (==) (CurrentValue v) (CurrentValue v') =
    prettyUnsafeEq v v'
  (==) (On e ev _) (On e' ev' _) =
    prettyUnsafeEq e e' && reallyUnsafeEq ev ev'
  (==) (On' e os ev _) (On' e' os' ev' _) =
    prettyUnsafeEq e e' && prettyUnsafeEq os os' && reallyUnsafeEq ev ev'
  (==) (Link t _) (Link t' _) =
    prettyUnsafeEq t t'
  (==) (SVGLink t _) (SVGLink t' _) =
    prettyUnsafeEq t t'
  (==) (XLink t _) (XLink t' _) =
    prettyUnsafeEq t t'

instance Cond (Feature e) where
  nil = NullFeature

instance IsString (Feature e) where
  fromString = Attribute "class" . fromString

instance IsList (Feature e) where
  type Item (Feature e) = Txt
  fromList = fromTxt . T.intercalate " "
  toList (Attribute "class" cs) = T.words cs
  toList _ = []

-- is this a terrible idea?
instance IsList ([a] -> [a]) where
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
attribute nm = Attribute nm

boolAttribute :: Txt -> Feature e
boolAttribute nm = Attribute nm ""

on' :: Txt -> Options -> (Obj -> IO (Maybe e)) -> Feature e
on' ev os f = On' ev os f Nothing

on :: Txt -> e -> Feature e
on ev e = On ev e Nothing

preventDefault :: Feature e -> Feature e
preventDefault (On ev e m) = On' ev (Options True False) (\_ -> return (Just e)) Nothing
preventDefault (On' ev os f m) = On' ev (os { _preventDef = True }) f m
preventDefault f = f

stopPropagation :: Feature e -> Feature e
stopPropagation (On ev e m) = On' ev (Options False True) (\_ -> return (Just e)) Nothing
stopPropagation (On' ev os f m) = On' ev (os { _stopProp = True }) f m
stopPropagation f = f

onPreventDefault :: Txt -> e -> Feature e
onPreventDefault ev e = on' ev noDefaultOptions (\_ -> return (Just e))

onPreventDefault' :: Txt -> (Obj -> IO (Maybe e)) -> Feature e
onPreventDefault' ev f = on' ev noDefaultOptions f

onIntercept :: Txt -> e -> Feature e
onIntercept ev e = on' ev interceptOptions (\_ -> return (Just e))

onIntercept' :: Txt -> (Obj -> IO (Maybe e)) -> Feature e
onIntercept' ev f = on' ev interceptOptions f

styleList :: [(Txt,Txt)] -> Feature e
styleList = Style

linkA :: Txt -> Feature e
linkA = flip Link Nothing

href :: Txt -> Feature e
href = attribute "href"

val :: Txt -> Feature e
val jst = CurrentValue jst

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

idA :: Txt -> Feature e
idA = attribute "id"

titleA :: Txt -> Feature e
titleA = attribute "title"

hiddenA :: Feature e
hiddenA = boolAttribute "hidden"

typeA :: Txt -> Feature e
typeA = attribute "type"

initialValue :: Txt -> Feature e
initialValue = attribute "value"

defaultValue :: Txt -> Feature e
defaultValue = attribute "default-value"

checked :: Feature e
checked = boolAttribute "checked"

placeholder :: Txt -> Feature e
placeholder = attribute "placeholder"

selected :: Feature e
selected = boolAttribute "selected"

accept :: Txt -> Feature e
accept = attribute "accept"

acceptCharset :: Txt -> Feature e
acceptCharset = attribute "accept-charset"

autocomplete :: Bool -> Feature e
autocomplete b = attribute "autocomplete" (if b then "on" else "off")

autofocus :: Feature e
autofocus = boolAttribute "autofocus"

disabled :: Feature e
disabled = boolAttribute "disabled"

enctype :: Txt -> Feature e
enctype = attribute "enctyp"

formaction :: Txt -> Feature e
formaction = attribute "formaction"

listA :: Txt -> Feature e
listA = attribute "list"

maxlength :: Int -> Feature e
maxlength = attribute "maxlength" . toTxt

minlength :: Int -> Feature e
minlength = attribute "minlength" . toTxt

methodA :: Txt -> Feature e
methodA = attribute "method"

multiple :: Feature e
multiple = boolAttribute "multiple"

name :: Txt -> Feature e
name = attribute "name"

novalidate :: Feature e
novalidate = boolAttribute "novalidate"

patternA :: Txt -> Feature e
patternA = attribute "pattern"

readonly :: Feature e
readonly = boolAttribute "readonly"

required :: Feature e
required = boolAttribute "required"

size :: Int -> Feature e
size = attribute "size" . toTxt

forA :: Txt -> Feature e
forA = attribute "for"

formA :: Txt -> Feature e
formA = attribute "form"

maxA :: Txt -> Feature e
maxA = attribute "max"

minA :: Txt -> Feature e
minA = attribute "min"

step :: Txt -> Feature e
step = attribute "step"

cols :: Int -> Feature e
cols = attribute "cols" . toTxt

rows :: Int -> Feature e
rows = attribute "rows" . toTxt

wrapA :: Txt -> Feature e
wrapA = attribute "wrap"

-- href :: Txt -> Feature e
-- href = attribute "href"

target :: Txt -> Feature e
target = attribute "target"

hreflang :: Txt -> Feature e
hreflang = attribute "hreflang"

media :: Txt -> Feature e
media = attribute "media"

rel :: Txt -> Feature e
rel = attribute "rel"

ismap :: Feature e
ismap = boolAttribute "ismap"

usemap :: Txt -> Feature e
usemap = attribute "usemap"

shape :: Txt -> Feature e
shape = attribute "shape"

src :: Txt -> Feature e
src = attribute "src"

heightA :: ToTxt a => a -> Feature e
heightA = attribute "height" . toTxt

widthA :: ToTxt a => a -> Feature e
widthA = attribute "width" . toTxt

alt :: Txt -> Feature e
alt = attribute "alt"

autoplay :: Feature e
autoplay = boolAttribute "autoplay"

controls :: Feature e
controls = boolAttribute "controls"

loop :: Feature e
loop = boolAttribute "loop"

preload :: Txt -> Feature e
preload = attribute "preload"

poster :: Txt -> Feature e
poster = attribute "poster"

defaultA :: Feature e
defaultA = boolAttribute "default"

kind :: Txt -> Feature e
kind = attribute "kind"

srclang :: Txt -> Feature e
srclang = attribute "srclang"

sandbox :: Txt -> Feature e
sandbox = attribute "sandbox"

seamless :: Feature e
seamless = boolAttribute "seamless"

srcdoc :: Txt -> Feature e
srcdoc = attribute "srcdoc"

reversedA :: Feature e
reversedA = boolAttribute "reversed"

start :: Int -> Feature e
start = attribute "start" . toTxt

align :: Txt -> Feature e
align = attribute "align"

colspan :: Int -> Feature e
colspan = attribute "colspan" . toTxt

rowspan :: Int -> Feature e
rowspan = attribute "rowspan" . toTxt

headers :: [Txt] -> Feature e
headers = attribute "headers" . JSS.intercalate " "

scope :: Txt -> Feature e
scope = attribute "scope"

asyncA :: Txt -> Feature e
asyncA = attribute "async"

charset :: Txt -> Feature e
charset = attribute "charset"

contentA :: Txt -> Feature e
contentA = attribute "content"

defer :: Txt -> Feature e
defer = attribute "defer"

httpEquiv :: Txt -> Feature e
httpEquiv = attribute "http-equiv"

languageA :: Txt -> Feature e
languageA = attribute "language"

scopedA :: Feature e
scopedA = boolAttribute "scoped"

accesskey :: Char -> Feature e
accesskey = attribute "accesskey" . JSS.singleton

contenteditable :: Feature e
contenteditable = boolAttribute "contenteditable"

contextmenu :: Txt -> Feature e
contextmenu = attribute "contextmenu"

dir :: Txt -> Feature e
dir = attribute "dir"

draggable :: Bool -> Feature e
draggable b = attribute "draggable" (if b then "true" else "false")

itemprop :: Txt -> Feature e
itemprop = attribute "itemprop"

lang :: Txt -> Feature e
lang = attribute "lang"

spellcheck :: Feature e
spellcheck = boolAttribute "spellcheck"

tabindex :: Int -> Feature e
tabindex = attribute "tabindex" . toTxt

citeA :: Txt -> Feature e
citeA = attribute "cite"

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
