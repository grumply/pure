{-# language CPP #-}
{-# language OverloadedStrings #-}
module Atomic.Attribute where

import Ef.Base

import Data.Txt as T
import Data.JSON hiding (Options)

import Atomic.FromTxt
import Atomic.ToTxt
import Atomic.Cond

import Data.String
import Data.Maybe

import GHC.Exts

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

noDefaultOptions :: Options
noDefaultOptions = Options True False

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
    , _eventECreate :: Obj -> IO (Maybe e)
    , _eventListener :: Maybe (IO ())
    }
  | Link
    { _event :: Txt
    , _eventListener :: Maybe (IO ())
    }
  | SVGLink
    { _event :: Txt
    , _eventListener :: Maybe (IO ())
    }
  | XLink
    { _attr :: Txt
    , _xlinkValue :: Txt
    }
  deriving (Functor)

instance Cond (Feature e) where
  nil = NullFeature

instance IsString (Feature e) where
  fromString = Attribute "class" . Right . fromString

instance IsList (Feature e) where
  type Item (Feature e) = Txt
  fromList = fromTxt . T.intercalate " "
  toList (Attribute "class" (Right cs)) = T.words cs
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
  fromTxt = Attribute "class" . Right . fromTxt

instance FromTxt [Feature e] where
  fromTxt t = [fromTxt t]

nullA :: Feature e
nullA = NullFeature

attribute :: Txt -> Txt -> Feature e
attribute nm = Attribute nm . Right

attr :: Txt -> Txt -> Feature e
attr = attribute

boolAttribute :: Txt -> Bool -> Feature e
boolAttribute nm = Attribute nm . Left

boolattr :: Txt -> Bool -> Feature e
boolattr = boolAttribute

on' :: Txt -> Options -> (Obj -> IO (Maybe e)) -> Feature e
on' ev os f = On' ev os f Nothing

on :: Txt -> e -> Feature e
on ev e = On ev e Nothing

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

link :: Txt -> Feature e
link = flip Link Nothing

value :: Txt -> Feature e
value jst = CurrentValue jst

xlink :: Txt -> Txt -> Feature e
xlink xl v = XLink xl v

svgLink :: Txt -> Feature e
svgLink = flip SVGLink Nothing

#ifdef LENS
makePrisms ''Feature
makeLenses ''Feature
makePrisms ''Options
makeLenses ''Options
#endif

classA :: Txt -> Feature e
classA = attr "class"

classes :: [(Txt,Bool)] -> Feature e
classes = classA
  . JSS.intercalate " "
  . mapMaybe (\(s,b) -> if b then Just s else Nothing)

idA :: Txt -> Feature e
idA = attr "id"

titleA :: Txt -> Feature e
titleA = attr "title"

hidden :: Bool -> Feature e
hidden = boolattr "hidden"

typeA :: Txt -> Feature e
typeA = attr "type"

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

methodA :: Txt -> Feature e
methodA = attr "method"

multiple :: Bool -> Feature e
multiple = boolattr "multiple"

name :: Txt -> Feature e
name = attr "name"

novalidate :: Bool -> Feature e
novalidate = boolattr "novalidate"

patternA :: Txt -> Feature e
patternA = attr "pattern"

readonly :: Bool -> Feature e
readonly = boolattr "readonly"

required :: Bool -> Feature e
required = boolattr "required"

size :: Int -> Feature e
size = attr "size" . toTxt

forA :: Txt -> Feature e
forA = attr "for"

formA :: Txt -> Feature e
formA = attr "form"

maxA :: Txt -> Feature e
maxA = attr "max"

minA :: Txt -> Feature e
minA = attr "min"

step :: Txt -> Feature e
step = attr "step"

cols :: Int -> Feature e
cols = attr "cols" . toTxt

rows :: Int -> Feature e
rows = attr "rows" . toTxt

wrapA :: Txt -> Feature e
wrapA = attr "wrap"

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

heightA :: ToTxt a => a -> Feature e
heightA = attr "height" . toTxt

widthA :: ToTxt a => a -> Feature e
widthA = attr "width" . toTxt

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

defaultA :: Bool -> Feature e
defaultA = boolattr "default"

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

async :: Txt -> Feature e
async = attr "async"

charset :: Txt -> Feature e
charset = attr "charset"

content :: Txt -> Feature e
content = attr "content"

defer :: Txt -> Feature e
defer = attr "defer"

httpEquiv :: Txt -> Feature e
httpEquiv = attr "http-equiv"

languageA :: Txt -> Feature e
languageA = attr "language"

scopedA :: Bool -> Feature e
scopedA = boolattr "scoped"

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

citeA :: Txt -> Feature e
citeA = attr "cite"

datetime :: Txt -> Feature e
datetime = attr "datetime"

manifest :: Txt -> Feature e
manifest = attr "manifest"

--------------------------------------------------------------------------------
-- SVG Attributes

svgAccentHeight :: Txt -> Feature e
svgAccentHeight = attr "accent-height"

svgAccumulate :: Txt -> Feature e
svgAccumulate = attr "accumulate"

svgAdditive :: Txt -> Feature e
svgAdditive = attr "additive"

svgAlignmentBaseline :: Txt -> Feature e
svgAlignmentBaseline = attr "alignment-baseline"

svgAllowReorder :: Txt -> Feature e
svgAllowReorder = attr "allowReorder"

svgAlphabetic :: Txt -> Feature e
svgAlphabetic = attr "alphabetic"

svgArabicForm :: Txt -> Feature e
svgArabicForm = attr "arabic-form"

svgAscent :: Txt -> Feature e
svgAscent = attr "ascent"

svgAttributeName :: Txt -> Feature e
svgAttributeName = attr "attributeName"

svgAttributeType :: Txt -> Feature e
svgAttributeType = attr "attributeType"

svgAutoReverse :: Txt -> Feature e
svgAutoReverse = attr "autoReverse"

svgAzimuth :: Txt -> Feature e
svgAzimuth = attr "azimuth"

svgBaseFrequency :: Txt -> Feature e
svgBaseFrequency = attr "baseFrequency"

svgBaslineShift :: Txt -> Feature e
svgBaslineShift = attr "basline-shift"

svgBaseProfile :: Txt -> Feature e
svgBaseProfile = attr "baseProfile"

svgBbox :: Txt -> Feature e
svgBbox = attr "bbox"

svgBegin :: Txt -> Feature e
svgBegin = attr "begin"

svgBias :: Txt -> Feature e
svgBias = attr "bias"

svgBy :: Txt -> Feature e
svgBy = attr "by"

svgCalcMode :: Txt -> Feature e
svgCalcMode = attr "calcMode"

svgCapHeight :: Txt -> Feature e
svgCapHeight = attr "cap-height"

svgClass :: Txt -> Feature e
svgClass = attr "class"

svgClip :: Txt -> Feature e
svgClip = attr "clip"

svgClipPathUnits :: Txt -> Feature e
svgClipPathUnits = attr "clipPathUnits"

svgClipPathA :: Txt -> Feature e
svgClipPathA = attr "clip-path"

svgClipRule :: Txt -> Feature e
svgClipRule = attr "clip-rule"

svgColor :: Txt -> Feature e
svgColor = attr "color"

svgColorInterpolation :: Txt -> Feature e
svgColorInterpolation = attr "color-interpolation"

svgColorInterpolationFilters :: Txt -> Feature e
svgColorInterpolationFilters = attr "color-interpolation-filters"

svgColorProfileA :: Txt -> Feature e
svgColorProfileA = attr "color-profile"

svgColorRendering :: Txt -> Feature e
svgColorRendering = attr "color-rendering"

svgContentScriptType :: Txt -> Feature e
svgContentScriptType = attr "contentScriptType"

svgContentStyleType :: Txt -> Feature e
svgContentStyleType = attr "contentStyleType"

svgCursorA :: Txt -> Feature e
svgCursorA = attr "cursor"

svgCx :: Txt -> Feature e
svgCx = attr "cx"

svgCy :: Txt -> Feature e
svgCy = attr "cy"

svgD :: Txt -> Feature e
svgD = attr "d"

svgDecelerate :: Txt -> Feature e
svgDecelerate = attr "decelerate"

svgDescent :: Txt -> Feature e
svgDescent = attr "descent"

svgDiffuseConstant :: Txt -> Feature e
svgDiffuseConstant = attr "diffuseConstant"

svgDirection :: Txt -> Feature e
svgDirection = attr "direction"

svgDisplay :: Txt -> Feature e
svgDisplay = attr "display"

svgDivisor :: Txt -> Feature e
svgDivisor = attr "divisor"

svgDominantBaseline :: Txt -> Feature e
svgDominantBaseline = attr "dominant-baseline"

svgDur :: Txt -> Feature e
svgDur = attr "dur"

svgDx :: Txt -> Feature e
svgDx = attr "dx"

svgDy :: Txt -> Feature e
svgDy = attr "dy"

svgEdgeMode :: Txt -> Feature e
svgEdgeMode = attr "edgeMode"

svgElevation :: Txt -> Feature e
svgElevation = attr "elevation"

svgEnableBackground :: Txt -> Feature e
svgEnableBackground = attr "enable-background"

svgEnd :: Txt -> Feature e
svgEnd = attr "end"

svgExponent :: Txt -> Feature e
svgExponent = attr "exponent"

svgExternalResourcesRequired :: Txt -> Feature e
svgExternalResourcesRequired = attr "externalResourcesRequired"

svgFill :: Txt -> Feature e
svgFill = attr "fill"

svgFillOpacity :: Txt -> Feature e
svgFillOpacity = attr "fill-opacity"

svgFillRule :: Txt -> Feature e
svgFillRule = attr "fill-rule"

svgFilterA :: Txt -> Feature e
svgFilterA = attr "filter"

svgFilterRes :: Txt -> Feature e
svgFilterRes = attr "filterRes"

svgFilterUnits :: Txt -> Feature e
svgFilterUnits = attr "filterUnits"

svgFloodColor :: Txt -> Feature e
svgFloodColor = attr "flood-color"

svgFontFamily :: Txt -> Feature e
svgFontFamily = attr "font-family"

svgFontSize :: Txt -> Feature e
svgFontSize = attr "font-size"

svgFontSizeAdjust :: Txt -> Feature e
svgFontSizeAdjust = attr "font-size-adjust"

svgFontStretch :: Txt -> Feature e
svgFontStretch = attr "font-stretch"

svgFontStyle :: Txt -> Feature e
svgFontStyle = attr "font-style"

svgFontVariant :: Txt -> Feature e
svgFontVariant = attr "font-variant"

svgFontWeight :: Txt -> Feature e
svgFontWeight = attr "font-weight"

svgFormat :: Txt -> Feature e
svgFormat = attr "format"

svgFrom :: Txt -> Feature e
svgFrom = attr "from"

svgFx :: Txt -> Feature e
svgFx = attr "fx"

svgFy :: Txt -> Feature e
svgFy = attr "fy"

svgG1 :: Txt -> Feature e
svgG1 = attr "g1"

svgG2 :: Txt -> Feature e
svgG2 = attr "g2"

svgGlyphName :: Txt -> Feature e
svgGlyphName = attr "glyph-name"

svgGlyphOrientationHorizontal :: Txt -> Feature e
svgGlyphOrientationHorizontal = attr "glyph-orientation-horizontal"

svgGlyphOrientationVertical :: Txt -> Feature e
svgGlyphOrientationVertical = attr "glyph-orientation-vertical"

svgGlyphRefA :: Txt -> Feature e
svgGlyphRefA = attr "glyphRef"

svgGradientTransform :: Txt -> Feature e
svgGradientTransform = attr "gradientTransform"

svgGradientUnits :: Txt -> Feature e
svgGradientUnits = attr "gradientUnits"

svgHanging :: Txt -> Feature e
svgHanging = attr "hanging"

svgHeight :: Txt -> Feature e
svgHeight = attr "height"

-- prefer svgLink for inter-organism links
svgHref :: Txt -> Feature e
svgHref = attr "href"

svgHorizAdvX :: Txt -> Feature e
svgHorizAdvX = attr "horiz-adv-x"

svgHorizOriginX :: Txt -> Feature e
svgHorizOriginX = attr "horiz-origin-x"

svgId :: Txt -> Feature e
svgId = attr "id"

svgIdeographic :: Txt -> Feature e
svgIdeographic = attr "ideographic"

svgImageRendering :: Txt -> Feature e
svgImageRendering = attr "image-rendering"

svgIn :: Txt -> Feature e
svgIn = attr "in"

svgIn2 :: Txt -> Feature e
svgIn2 = attr "in2"

svgIntercept :: Txt -> Feature e
svgIntercept = attr "intercept"

svgK :: Txt -> Feature e
svgK = attr "k"

svgK1 :: Txt -> Feature e
svgK1 = attr "k1"

svgK2 :: Txt -> Feature e
svgK2 = attr "k2"

svgK3 :: Txt -> Feature e
svgK3 = attr "k3"

svgK4 :: Txt -> Feature e
svgK4 = attr "k4"

svgKernelMatrix :: Txt -> Feature e
svgKernelMatrix = attr "kernelMatrix"

svgKernelUnitLength :: Txt -> Feature e
svgKernelUnitLength = attr "kernelUnitLength"

svgKerning :: Txt -> Feature e
svgKerning = attr "kerning"

svgKeyPoints :: Txt -> Feature e
svgKeyPoints = attr "keyPoints"

svgKeySplines :: Txt -> Feature e
svgKeySplines = attr "keySplines"

svgKeyTimes :: Txt -> Feature e
svgKeyTimes = attr "keyTimes"

svgLang :: Txt -> Feature e
svgLang = attr "lang"

svgLengthAdjust :: Txt -> Feature e
svgLengthAdjust = attr "lengthAdjust"

svgLetterSpacing :: Txt -> Feature e
svgLetterSpacing = attr "letter-spacing"

svgLightingColor :: Txt -> Feature e
svgLightingColor = attr "lighting-color"

svgLimitingConeAngle :: Txt -> Feature e
svgLimitingConeAngle = attr "limitingConeAngle"

svgLocal :: Txt -> Feature e
svgLocal = attr "local"

svgMarkerEnd :: Txt -> Feature e
svgMarkerEnd = attr "marker-end"

svgMarkerMid :: Txt -> Feature e
svgMarkerMid = attr "marker-mid"

svgMarkerStart :: Txt -> Feature e
svgMarkerStart = attr "marker-start"

svgMarkerHeight :: Txt -> Feature e
svgMarkerHeight = attr "markerHeight"

svgMarkerUnits :: Txt -> Feature e
svgMarkerUnits = attr "markerUnits"

svgMarkerWidth :: Txt -> Feature e
svgMarkerWidth = attr "markerWidth"

svgMaskA :: Txt -> Feature e
svgMaskA = attr "maskA"

svgMaskContentUnits :: Txt -> Feature e
svgMaskContentUnits = attr "maskContentUnits"

svgMaskUnits :: Txt -> Feature e
svgMaskUnits = attr "maskUnits"

svgMathematical :: Txt -> Feature e
svgMathematical = attr "mathematical"

svgMax :: Txt -> Feature e
svgMax = attr "max"

svgMedia :: Txt -> Feature e
svgMedia = attr "media"

svgMethod :: Txt -> Feature e
svgMethod = attr "method"

svgMin :: Txt -> Feature e
svgMin = attr "min"

svgMode :: Txt -> Feature e
svgMode = attr "mode"

svgName :: Txt -> Feature e
svgName = attr "name"

svgNumOctaves :: Txt -> Feature e
svgNumOctaves = attr "numOctaves"

svgOffset :: Txt -> Feature e
svgOffset = attr "offset"

svgOnabort :: Txt -> Feature e
svgOnabort = attr "onabort"

svgOnactivate :: Txt -> Feature e
svgOnactivate = attr "onactivate"

svgOnbegin :: Txt -> Feature e
svgOnbegin = attr "onbegin"

svgOnclick :: Txt -> Feature e
svgOnclick = attr "onclick"

svgOnend :: Txt -> Feature e
svgOnend = attr "onend"

svgOnerror :: Txt -> Feature e
svgOnerror = attr "onerror"

svgOnfocusin :: Txt -> Feature e
svgOnfocusin = attr "onfocusin"

svgOnfocusout :: Txt -> Feature e
svgOnfocusout = attr "onfocusout"

svgOnload :: Txt -> Feature e
svgOnload = attr "onload"

svgOnmousedown :: Txt -> Feature e
svgOnmousedown = attr "onmousedown"

svgOnmousemove :: Txt -> Feature e
svgOnmousemove = attr "onmousemove"

svgOnmouseout :: Txt -> Feature e
svgOnmouseout = attr "onmouseout"

svgOnmouseover :: Txt -> Feature e
svgOnmouseover = attr "onmouseover"

svgOnmouseup :: Txt -> Feature e
svgOnmouseup = attr "onmouseup"

svgOnrepeat :: Txt -> Feature e
svgOnrepeat = attr "onrepeat"

svgOnresize :: Txt -> Feature e
svgOnresize = attr "onresize"

svgOnscroll :: Txt -> Feature e
svgOnscroll = attr "onscroll"

svgOnunload :: Txt -> Feature e
svgOnunload = attr "onunload"

svgOnzoom :: Txt -> Feature e
svgOnzoom = attr "onzoom"

svgOpacity :: Txt -> Feature e
svgOpacity = attr "opacity"

svgOperator :: Txt -> Feature e
svgOperator = attr "operator"

svgOrder :: Txt -> Feature e
svgOrder = attr "order"

svgOrient :: Txt -> Feature e
svgOrient = attr "orient"

svgOrientation :: Txt -> Feature e
svgOrientation = attr "orientation"

svgOrigin :: Txt -> Feature e
svgOrigin = attr "origin"

svgOverflow :: Txt -> Feature e
svgOverflow = attr "overflow"

svgOverlinePosition :: Txt -> Feature e
svgOverlinePosition = attr "overline-position"

svgOverlineThickness :: Txt -> Feature e
svgOverlineThickness = attr "overline-thickness"

svgPanose1 :: Txt -> Feature e
svgPanose1 = attr "panose-1"

svgPaintOrder :: Txt -> Feature e
svgPaintOrder = attr "paint-order"

svgPathLength :: Txt -> Feature e
svgPathLength = attr "pathLength"

svgPatternContentUnits :: Txt -> Feature e
svgPatternContentUnits = attr "patternContentUnits"

svgPatternTransform :: Txt -> Feature e
svgPatternTransform = attr "patternTransform"

svgPatternUnits :: Txt -> Feature e
svgPatternUnits = attr "patternUnits"

svgPointerEvents :: Txt -> Feature e
svgPointerEvents = attr "pointer-events"

svgPoints :: Txt -> Feature e
svgPoints = attr "points"

svgPointsAtX :: Txt -> Feature e
svgPointsAtX = attr "pointsAtX"

svgPointsAtY :: Txt -> Feature e
svgPointsAtY = attr "pointsAtY"

svgPointsAtZ :: Txt -> Feature e
svgPointsAtZ = attr "pointsAtZ"

svgPreserveAlpha :: Txt -> Feature e
svgPreserveAlpha = attr "preserveAlpha"

svgPreserveAspectRatio :: Txt -> Feature e
svgPreserveAspectRatio = attr "preserveAspectRatio"

svgPrimitiveUnits :: Txt -> Feature e
svgPrimitiveUnits = attr "primitiveUnits"

svgR :: Txt -> Feature e
svgR = attr "r"

svgRadius :: Txt -> Feature e
svgRadius = attr "radius"

svgRefX :: Txt -> Feature e
svgRefX = attr "refX"

svgRefY :: Txt -> Feature e
svgRefY = attr "refY"

svgRenderingIntent :: Txt -> Feature e
svgRenderingIntent = attr "rendering-intent"

svgRepeatCount :: Txt -> Feature e
svgRepeatCount = attr "repeatCount"

svgRepeatDur :: Txt -> Feature e
svgRepeatDur = attr "repeatDur"

svgRequiredExtensions :: Txt -> Feature e
svgRequiredExtensions = attr "requiredExtensions"

svgRequiredFeatures :: Txt -> Feature e
svgRequiredFeatures = attr "requiredFeatures"

svgRestart :: Txt -> Feature e
svgRestart = attr "restart"

svgResult :: Txt -> Feature e
svgResult = attr "result"

svgRotate :: Txt -> Feature e
svgRotate = attr "rotate"

svgRx :: Txt -> Feature e
svgRx = attr "rx"

svgRy :: Txt -> Feature e
svgRy = attr "ry"

svgScale :: Txt -> Feature e
svgScale = attr "scale"

svgSeed :: Txt -> Feature e
svgSeed = attr "seed"

svgShapeRendering :: Txt -> Feature e
svgShapeRendering = attr "shape-rendering"

svgSlope :: Txt -> Feature e
svgSlope = attr "slope"

svgSpacing :: Txt -> Feature e
svgSpacing = attr "spacing"

svgSpecularConstant :: Txt -> Feature e
svgSpecularConstant = attr "specularConstant"

svgSpecularExponent :: Txt -> Feature e
svgSpecularExponent = attr "specularExponent"

svgSpeed :: Txt -> Feature e
svgSpeed = attr "speed"

svgSpreadMethod :: Txt -> Feature e
svgSpreadMethod = attr "spreadMethod"

svgStartOffset :: Txt -> Feature e
svgStartOffset = attr "startOffset"

svgStdDeviationA :: Txt -> Feature e
svgStdDeviationA = attr "stdDeviationA"

svgStemh :: Txt -> Feature e
svgStemh = attr "stemh"

svgStemv :: Txt -> Feature e
svgStemv = attr "stemv"

svgStitchTiles :: Txt -> Feature e
svgStitchTiles = attr "stitchTiles"

svgStopColor :: Txt -> Feature e
svgStopColor = attr "stop-color"

svgStopOpacity :: Txt -> Feature e
svgStopOpacity = attr "stop-opacity"

svgStrikethroughPosition :: Txt -> Feature e
svgStrikethroughPosition = attr "strikethrough-position"

svgStrikethroughThickness :: Txt -> Feature e
svgStrikethroughThickness = attr "strikethrough-thickness"

svgString :: Txt -> Feature e
svgString = attr "string"

svgStroke :: Txt -> Feature e
svgStroke = attr "stroke"

svgStrokeDasharray :: Txt -> Feature e
svgStrokeDasharray = attr "stroke-dasharray"

svgStrokeDashoffset :: Txt -> Feature e
svgStrokeDashoffset = attr "stroke-dashoffset"

svgStrokeLinecap :: Txt -> Feature e
svgStrokeLinecap = attr "stroke-linecap"

svgStrokeLinejoin :: Txt -> Feature e
svgStrokeLinejoin = attr "stroke-linejoin"

svgStrokeMiterlimit :: Txt -> Feature e
svgStrokeMiterlimit = attr "stroke-miterlimit"

svgStrokeOpacity :: Txt -> Feature e
svgStrokeOpacity = attr "stroke-opacity"

svgStrokeWidth :: Txt -> Feature e
svgStrokeWidth = attr "stroke-width"

svgStyleA :: Txt -> Feature e
svgStyleA = attr "style"

svgSurfaceScale :: Txt -> Feature e
svgSurfaceScale = attr "surfaceScale"

svgSystemLanguage :: Txt -> Feature e
svgSystemLanguage = attr "systemLanguage"

svgTabindex :: Txt -> Feature e
svgTabindex = attr "tabindex"

svgTableValues :: Txt -> Feature e
svgTableValues = attr "tableValues"

svgTarget :: Txt -> Feature e
svgTarget = attr "target"

svgTargetX :: Txt -> Feature e
svgTargetX = attr "targetX"

svgTargetY :: Txt -> Feature e
svgTargetY = attr "targetY"

svgTextAnchor :: Txt -> Feature e
svgTextAnchor = attr "text-anchor"

svgTextDecoration :: Txt -> Feature e
svgTextDecoration = attr "text-decoration"

svgTextRendering :: Txt -> Feature e
svgTextRendering = attr "text-rendering"

svgTextLength :: Txt -> Feature e
svgTextLength = attr "textLength"

svgTo :: Txt -> Feature e
svgTo = attr "to"

svgTransform :: Txt -> Feature e
svgTransform = attr "transform"

svgType :: Txt -> Feature e
svgType = attr "type"

svgU1 :: Txt -> Feature e
svgU1 = attr "u1"

svgU2 :: Txt -> Feature e
svgU2 = attr "u2"

svgUnerlinePosition :: Txt -> Feature e
svgUnerlinePosition = attr "unerline-position"

svgUnderlineThickness :: Txt -> Feature e
svgUnderlineThickness = attr "underline-thickness"

svgUnicode :: Txt -> Feature e
svgUnicode = attr "unicode"

svgUnicodeBidi :: Txt -> Feature e
svgUnicodeBidi = attr "unicode-bidi"

svgUnicodeRange :: Txt -> Feature e
svgUnicodeRange = attr "unicode-range"

svgUnitsPerEm :: Txt -> Feature e
svgUnitsPerEm = attr "units-per-em"

svgVAlphabetic :: Txt -> Feature e
svgVAlphabetic = attr "v-alphabetic"

svgVHanging :: Txt -> Feature e
svgVHanging = attr "v-hanging"

svgVIdeographic :: Txt -> Feature e
svgVIdeographic = attr "v-ideographic"

svgVMathematical :: Txt -> Feature e
svgVMathematical = attr "v-mathematical"

svgValues :: Txt -> Feature e
svgValues = attr "values"

svgVersion :: Txt -> Feature e
svgVersion = attr "version"

svgVertAdvY :: Txt -> Feature e
svgVertAdvY = attr "vert-adv-y"

svgVertOriginX :: Txt -> Feature e
svgVertOriginX = attr "vert-origin-x"

svgVerOriginY :: Txt -> Feature e
svgVerOriginY = attr "ver-origin-y"

svgViewBox :: Txt -> Feature e
svgViewBox = attr "viewBox"

svgViewTarget :: Txt -> Feature e
svgViewTarget = attr "viewTarget"

svgVisibility :: Txt -> Feature e
svgVisibility = attr "visibility"

svgWidth :: Txt -> Feature e
svgWidth = attr "width"

svgWidths :: Txt -> Feature e
svgWidths = attr "widths"

svgWordSpacing :: Txt -> Feature e
svgWordSpacing = attr "word-spacing"

svgWritingMode :: Txt -> Feature e
svgWritingMode = attr "writing-mode"

svgX :: Txt -> Feature e
svgX = attr "x"

svgXHeight :: Txt -> Feature e
svgXHeight = attr "xHeight"

svgX1 :: Txt -> Feature e
svgX1 = attr "x1"

svgX2 :: Txt -> Feature e
svgX2 = attr "x2"

svgXChannelSelector :: Txt -> Feature e
svgXChannelSelector = attr "xChannelSelector"

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
svgXmlbase = attr "xml:base"

svgXmllang :: Txt -> Feature e
svgXmllang = attr "xml:lang"

svgXmlspace :: Txt -> Feature e
svgXmlspace = attr "xml:space"

svgY :: Txt -> Feature e
svgY = attr "y"

svgY1 :: Txt -> Feature e
svgY1 = attr "y1"

svgY2 :: Txt -> Feature e
svgY2 = attr "y2"

svgYChannelSelector :: Txt -> Feature e
svgYChannelSelector = attr "yChannelSelector"

svgZ :: Txt -> Feature e
svgZ = attr "z"

svgZoomAndPan :: Txt -> Feature e
svgZoomAndPan = attr "zoomAndPan"

clipPathUrl :: Txt -> Feature e
clipPathUrl = attr "clip-path" . (\x -> "url(#" <> x <> ")")

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
