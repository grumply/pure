{-# language OverloadedStrings #-}
module Atomic.HTML where

import Ef.Base

import Data.Txt (Txt)

import Atomic.Attribute
import Atomic.Component

import Control.Lens

abbr :: [Feature e] -> [Atom e] -> Atom e
abbr = mkAtom "abbr"

address :: [Feature e] -> [Atom e] -> Atom e
address = mkAtom "address"

area :: [Feature e] -> Atom e
area fs = mkAtom "area" fs []

a :: [Feature e] -> [Atom e] -> Atom e
a = mkAtom "a"

article :: [Feature e] -> [Atom e] -> Atom e
article = mkAtom "article"

asideE :: [Feature e] -> [Atom e] -> Atom e
asideE = mkAtom "aside"

audio :: [Feature e] -> [Atom e] -> Atom e
audio = mkAtom "audio"

base :: [Feature e] -> Atom e
base fs = mkAtom "base" fs []

bdi :: [Feature e] -> [Atom e] -> Atom e
bdi = mkAtom "bdi"

bdo :: [Feature e] -> [Atom e] -> Atom e
bdo = mkAtom "bdo"

big :: [Feature e] -> [Atom e] -> Atom e
big = mkAtom "big"

blockquote :: [Feature e] -> [Atom e] -> Atom e
blockquote = mkAtom "blockquote"

body :: [Feature e] -> [Atom e] -> Atom e
body = mkAtom "body"

b :: [Feature e] -> [Atom e] -> Atom e
b = mkAtom "b"

br :: Atom e
br = mkAtom "br" [] []

button :: [Feature e] -> [Atom e] -> Atom e
button = mkAtom "button"

canvas :: [Feature e] -> [Atom e] -> Atom e
canvas = mkAtom "canvas"

caption :: [Feature e] -> [Atom e] -> Atom e
caption = mkAtom "caption"

cite :: [Feature e] -> [Atom e] -> Atom e
cite = mkAtom "cite"

code :: [Feature e] -> [Atom e] -> Atom e
code = mkAtom "code"

col :: [Feature e] -> Atom e
col fs = mkAtom "col" fs []

colgroup :: [Feature e] -> [Atom e] -> Atom e
colgroup = mkAtom "colgroup"

dataE :: [Feature e] -> [Atom e] -> Atom e
dataE = mkAtom "data"

datalist :: [Feature e] -> [Atom e] -> Atom e
datalist = mkAtom "datalist"

dd :: [Feature e] -> [Atom e] -> Atom e
dd = mkAtom "dd"

description :: Txt -> Atom e
description d = meta [ name "description", contentA d ]

dl :: [Feature e] -> [Atom e] -> Atom e
dl = mkAtom "dl"

dt :: [Feature e] -> [Atom e] -> Atom e
dt = mkAtom "dt"

del :: [Feature e] -> [Atom e] -> Atom e
del = mkAtom "del"

details :: [Feature e] -> [Atom e] -> Atom e
details = mkAtom "details"

dfn :: [Feature e] -> [Atom e] -> Atom e
dfn = mkAtom "dfn"

dialog :: [Feature e] -> [Atom e] -> Atom e
dialog = mkAtom "dialog"

div :: [Feature e] -> [Atom e] -> Atom e
div = mkAtom "div"

em :: [Feature e] -> [Atom e] -> Atom e
em = mkAtom "em"

embed :: [Feature e] -> [Atom e] -> Atom e
embed = mkAtom "embed"

fieldset :: [Feature e] -> [Atom e] -> Atom e
fieldset = mkAtom "fieldset"

figcaption :: [Feature e] -> [Atom e] -> Atom e
figcaption = mkAtom "figcaption"

figure :: [Feature e] -> [Atom e] -> Atom e
figure = mkAtom "figure"

footer :: [Feature e] -> [Atom e] -> Atom e
footer = mkAtom "footer"

form :: [Feature e] -> [Atom e] -> Atom e
form = mkAtom "form"

frame :: [Feature e] ->Atom e
frame fs = mkAtom "frame" fs []

head :: [Atom e] -> Atom e
head = mkAtom "head" []

header :: [Feature e] -> [Atom e] -> Atom e
header = mkAtom "header"

h1 :: [Feature e] -> [Atom e] -> Atom e
h1 = mkAtom "h1"

h2 :: [Feature e] -> [Atom e] -> Atom e
h2 = mkAtom "h2"

h3 :: [Feature e] -> [Atom e] -> Atom e
h3 = mkAtom "h3"

h4 :: [Feature e] -> [Atom e] -> Atom e
h4 = mkAtom "h4"

h5 :: [Feature e] -> [Atom e] -> Atom e
h5 = mkAtom "h5"

h6 :: [Feature e] -> [Atom e] -> Atom e
h6 = mkAtom "h6"

hgroup :: [Feature e] -> [Atom e] -> Atom e
hgroup = mkAtom "hgroup"

hr :: [Feature e] -> Atom e
hr fs = mkAtom "hr" fs []

htmlE :: [Feature e] -> [Atom e] -> Atom e
htmlE = mkAtom "html"

iframe :: [Feature e] -> [Atom e] -> Atom e
iframe = mkAtom "iframe"

img :: [Feature e] -> Atom e
img fs = mkAtom "img" fs []

input :: [Feature e] -> Atom e
input fs = mkAtom "input" fs []

textInput :: [Feature e] -> [Atom e] -> Atom e
textInput fs = mkAtom "input" (typeA "text":fs)

ins :: [Feature e] -> [Atom e] -> Atom e
ins = mkAtom "ins"

iE :: [Feature e] -> [Atom e] -> Atom e
iE = mkAtom "i"

kbd :: [Feature e] -> [Atom e] -> Atom e
kbd = mkAtom "kbd"

keygen :: [Feature e] -> [Atom e] -> Atom e
keygen = mkAtom "keygen"

label :: [Feature e] -> [Atom e] -> Atom e
label = mkAtom "label"

legend :: [Feature e] -> [Atom e] -> Atom e
legend = mkAtom "legend"

li :: [Feature e] -> [Atom e] -> Atom e
li = mkAtom "li"

linkE :: [Feature e] -> Atom e
linkE fs = mkAtom "link" fs []

mainE :: [Feature e] -> [Atom e] -> Atom e
mainE = mkAtom "main"

mapE :: [Feature e] -> [Atom e] -> Atom e
mapE = mkAtom "map"

mark :: [Feature e] -> [Atom e] -> Atom e
mark = mkAtom "mark"

menu :: [Feature e] -> [Atom e] -> Atom e
menu = mkAtom "menu"

menuitem :: [Feature e] -> [Atom e] -> Atom e
menuitem = mkAtom "menuitem"

meta :: [Feature e] -> Atom e
meta fs = mkAtom "meta" fs []

meter :: [Feature e] -> [Atom e] -> Atom e
meter = mkAtom "meter"

nav :: [Feature e] -> [Atom e] -> Atom e
nav = mkAtom "nav"

noscript :: [Feature e] -> [Atom e] -> Atom e
noscript = mkAtom "noscript"

objectE :: [Feature e] -> [Atom e] -> Atom e
objectE = mkAtom "object"

optgroup :: [Feature e] -> [Atom e] -> Atom e
optgroup = mkAtom "optgroup"

option :: [Feature e] -> [Atom e] -> Atom e
option = mkAtom "option"

ol :: [Feature e] -> [Atom e] -> Atom e
ol = mkAtom "ol"

output :: [Feature e] -> [Atom e] -> Atom e
output = mkAtom "output"

p :: [Feature e] -> [Atom e] -> Atom e
p = mkAtom "p"

param :: [Feature e] -> Atom e
param fs = mkAtom "param" fs []

picture :: [Feature e] -> [Atom e] -> Atom e
picture = mkAtom "picture"

preE :: [Feature e] -> [Atom e] -> Atom e
preE = mkAtom "pre"

progress :: [Feature e] -> [Atom e] -> Atom e
progress = mkAtom "progress"

q :: [Feature e] -> [Atom e] -> Atom e
q = mkAtom "q"

rp :: [Feature e] -> [Atom e] -> Atom e
rp = mkAtom "rp"

rt :: [Feature e] -> [Atom e] -> Atom e
rt = mkAtom "rt"

ruby :: [Feature e] -> [Atom e] -> Atom e
ruby = mkAtom "ruby"

samp :: [Feature e] -> [Atom e] -> Atom e
samp = mkAtom "samp"

script :: [Feature e] -> [Atom e] -> Atom e
script = mkAtom "script"

s :: [Feature e] -> [Atom e] -> Atom e
s = mkAtom "s"

section :: [Feature e] -> [Atom e] -> Atom e
section = mkAtom "section"

selectE :: [Feature e] -> [Atom e] -> Atom e
selectE = mkAtom "select"

small :: [Feature e] -> [Atom e] -> Atom e
small = mkAtom "small"

source :: [Feature e] -> [Atom e] -> Atom e
source = mkAtom "source"

span :: [Feature e] -> [Atom e] -> Atom e
span = mkAtom "span"

strong :: [Feature e] -> [Atom e] -> Atom e
strong = mkAtom "strong"

style :: [Feature e] -> [Atom e] -> Atom e
style = mkAtom "style"

sub :: [Feature e] -> [Atom e] -> Atom e
sub = mkAtom "sub"

summary :: [Feature e] -> [Atom e] -> Atom e
summary = mkAtom "summary"

sup :: [Feature e] -> [Atom e] -> Atom e
sup = mkAtom "sup"

table :: [Feature e] -> [Atom e] -> Atom e
table = mkAtom "table"

tbody :: [Feature e] -> [Atom e] -> Atom e
tbody = mkAtom "tbody"

td :: [Feature e] -> [Atom e] -> Atom e
td = mkAtom "td"

textarea :: [Feature e] -> [Atom e] -> Atom e
textarea = mkAtom "textarea"

tfoot :: [Feature e] -> [Atom e] -> Atom e
tfoot = mkAtom "tfoot"

th :: [Feature e] -> [Atom e] -> Atom e
th = mkAtom "th"

thead :: [Feature e] -> [Atom e] -> Atom e
thead = mkAtom "thead"

time :: [Feature e] -> [Atom e] -> Atom e
time = mkAtom "time"

title :: Txt -> Atom e
title jst = mkAtom "title" [] [ text jst ]

tr :: [Feature e] -> [Atom e] -> Atom e
tr = mkAtom "tr"

track :: [Feature e] -> [Atom e] -> Atom e
track = mkAtom "track"

u :: [Feature e] -> [Atom e] -> Atom e
u = mkAtom "u"

ul :: [Feature e] -> [Atom e] -> Atom e
ul = mkAtom "ul"

varE :: [Feature e] -> [Atom e] -> Atom e
varE = mkAtom "var"

video :: [Feature e] -> [Atom e] -> Atom e
video = mkAtom "video"

viewport :: Txt -> Atom e
viewport jst = mkAtom "meta" [ name "viewport", contentA jst ] []

wbr :: [Feature e] -> [Atom e] -> Atom e
wbr = mkAtom "wbr"

--------------------------------------------------------------------------------
-- SVG

svgA :: [Feature e] -> [Atom e] -> Atom e
svgA = mkSVGAtom "a"

svgAudio :: [Feature e] -> [Atom e] -> Atom e
svgAudio = mkSVGAtom "audio"

svgAltGlyph :: [Feature e] -> [Atom e] -> Atom e
svgAltGlyph = mkSVGAtom "altGlyph"

svgAltGlyphDef :: [Feature e] -> [Atom e] -> Atom e
svgAltGlyphDef = mkSVGAtom "altGlyphDef"

svgAltGlyphItem :: [Feature e] -> [Atom e] -> Atom e
svgAltGlyphItem = mkSVGAtom "altGlyphItem"

svgAnimate :: [Feature e] -> [Atom e] -> Atom e
svgAnimate = mkSVGAtom "animate"

svgAnimateColor :: [Feature e] -> [Atom e] -> Atom e
svgAnimateColor = mkSVGAtom "animateColor"

svgAnimateMotion :: [Feature e] -> [Atom e] -> Atom e
svgAnimateMotion = mkSVGAtom "animateMotion"

svgAnimateTransform :: [Feature e] -> [Atom e] -> Atom e
svgAnimateTransform = mkSVGAtom "animateTransform"

svgCanvas :: [Feature e] -> [Atom e] -> Atom e
svgCanvas = mkSVGAtom "canvas"

svgCircle :: [Feature e] -> [Atom e] -> Atom e
svgCircle = mkSVGAtom "circle"

svgClipPath :: [Feature e] -> [Atom e] -> Atom e
svgClipPath = mkSVGAtom "clipPath"

svgColorProfile :: [Feature e] -> [Atom e] -> Atom e
svgColorProfile = mkSVGAtom "color-profile"

svgCursor :: [Feature e] -> [Atom e] -> Atom e
svgCursor = mkSVGAtom "cursor"

svgDefs :: [Feature e] -> [Atom e] -> Atom e
svgDefs = mkSVGAtom "defs"

svgDesc :: [Feature e] -> [Atom e] -> Atom e
svgDesc = mkSVGAtom "desc"

svgDiscard :: [Feature e] -> [Atom e] -> Atom e
svgDiscard = mkSVGAtom "discard"

svgEllipse :: [Feature e] -> [Atom e] -> Atom e
svgEllipse = mkSVGAtom "ellipse"

svgFeBlend :: [Feature e] -> [Atom e] -> Atom e
svgFeBlend = mkSVGAtom "feBlend"

svgFeColorMatrix :: [Feature e] -> [Atom e] -> Atom e
svgFeColorMatrix = mkSVGAtom "feColorMatrix"

svgFeComponentTransfer :: [Feature e] -> [Atom e] -> Atom e
svgFeComponentTransfer = mkSVGAtom "feComponentTransfer"

svgFeComposite :: [Feature e] -> [Atom e] -> Atom e
svgFeComposite = mkSVGAtom "feComposite"

svgFeConvolveMatrix :: [Feature e] -> [Atom e] -> Atom e
svgFeConvolveMatrix = mkSVGAtom "feConvolveMatrix"

svgFeDiffuseLighting :: [Feature e] -> [Atom e] -> Atom e
svgFeDiffuseLighting = mkSVGAtom "feDiffuseLighting"

svgFeDisplacementMap :: [Feature e] -> [Atom e] -> Atom e
svgFeDisplacementMap = mkSVGAtom "feDisplacementMap"

svgFeDistantLight :: [Feature e] -> [Atom e] -> Atom e
svgFeDistantLight = mkSVGAtom "feDistantLight"

svgFeDropShadow :: [Feature e] -> [Atom e] -> Atom e
svgFeDropShadow = mkSVGAtom "feDropShadow"

svgFeFlood :: [Feature e] -> [Atom e] -> Atom e
svgFeFlood = mkSVGAtom "feFlood"

svgFeFuncA :: [Feature e] -> [Atom e] -> Atom e
svgFeFuncA = mkSVGAtom "feFuncA"

svgFeFuncB :: [Feature e] -> [Atom e] -> Atom e
svgFeFuncB = mkSVGAtom "feFuncB"

svgFeFuncG :: [Feature e] -> [Atom e] -> Atom e
svgFeFuncG = mkSVGAtom "feFuncG"

svgFeFuncR :: [Feature e] -> [Atom e] -> Atom e
svgFeFuncR = mkSVGAtom "feFuncR"

svgFeGaussianBlur :: [Feature e] -> [Atom e] -> Atom e
svgFeGaussianBlur = mkSVGAtom "feGaussianBlur"

svgFeImage :: [Feature e] -> [Atom e] -> Atom e
svgFeImage = mkSVGAtom "feImage"

svgFeMerge :: [Feature e] -> [Atom e] -> Atom e
svgFeMerge = mkSVGAtom "feMerge"

svgFeMergeNode :: [Feature e] -> [Atom e] -> Atom e
svgFeMergeNode = mkSVGAtom "feMergeNode"

svgFeMorphology :: [Feature e] -> [Atom e] -> Atom e
svgFeMorphology = mkSVGAtom "feMorphology"

svgFeOffset :: [Feature e] -> [Atom e] -> Atom e
svgFeOffset = mkSVGAtom "feOffset"

svgFePointLight :: [Feature e] -> [Atom e] -> Atom e
svgFePointLight = mkSVGAtom "fePointLight"

svgFeSpecularLighting :: [Feature e] -> [Atom e] -> Atom e
svgFeSpecularLighting = mkSVGAtom "feSpecularLighting"

svgFeSpotLight :: [Feature e] -> [Atom e] -> Atom e
svgFeSpotLight = mkSVGAtom "feSpotLight"

svgFeTile :: [Feature e] -> [Atom e] -> Atom e
svgFeTile = mkSVGAtom "feTile"

svgFeTurbulence :: [Feature e] -> [Atom e] -> Atom e
svgFeTurbulence = mkSVGAtom "feTurbulence"

svgFilter :: [Feature e] -> [Atom e] -> Atom e
svgFilter = mkSVGAtom "filter"

svgFont :: [Feature e] -> [Atom e] -> Atom e
svgFont = mkSVGAtom "font"

svgFontFace :: [Feature e] -> [Atom e] -> Atom e
svgFontFace = mkSVGAtom "font-face"

svgFontFaceFormat :: [Feature e] -> [Atom e] -> Atom e
svgFontFaceFormat = mkSVGAtom "font-face-format"

svgFontFaceName :: [Feature e] -> [Atom e] -> Atom e
svgFontFaceName = mkSVGAtom "font-face-name"

svgFontFaceSrc :: [Feature e] -> [Atom e] -> Atom e
svgFontFaceSrc = mkSVGAtom "font-face-src"

svgFontFaceUri :: [Feature e] -> [Atom e] -> Atom e
svgFontFaceUri = mkSVGAtom "font-face-uri"

svgForeignObject :: [Feature e] -> [Atom e] -> Atom e
svgForeignObject = mkSVGAtom "foreignObject"

svgG :: [Feature e] -> [Atom e] -> Atom e
svgG = mkSVGAtom "g"

svgGlyph :: [Feature e] -> [Atom e] -> Atom e
svgGlyph = mkSVGAtom "glyph"

svgGlyphRef :: [Feature e] -> [Atom e] -> Atom e
svgGlyphRef = mkSVGAtom "glyphRef"

svgHatch :: [Feature e] -> [Atom e] -> Atom e
svgHatch = mkSVGAtom "hatch"

svgHatchpath :: [Feature e] -> [Atom e] -> Atom e
svgHatchpath = mkSVGAtom "hatchpath"

svgHkern :: [Feature e] -> [Atom e] -> Atom e
svgHkern = mkSVGAtom "hkern"

svgIframe :: [Feature e] -> [Atom e] -> Atom e
svgIframe = mkSVGAtom "iframe"

svgImage :: [Feature e] -> [Atom e] -> Atom e
svgImage = mkSVGAtom "image"

svgLine :: [Feature e] -> [Atom e] -> Atom e
svgLine = mkSVGAtom "line"

svgLinearGradient :: [Feature e] -> [Atom e] -> Atom e
svgLinearGradient = mkSVGAtom "linearGradient"

svgMarker :: [Feature e] -> [Atom e] -> Atom e
svgMarker = mkSVGAtom "marker"

svgMask :: [Feature e] -> [Atom e] -> Atom e
svgMask = mkSVGAtom "mask"

svgMesh :: [Feature e] -> [Atom e] -> Atom e
svgMesh = mkSVGAtom "mesh"

svgMeshgradient :: [Feature e] -> [Atom e] -> Atom e
svgMeshgradient = mkSVGAtom "meshgradient"

svgMeshpatch :: [Feature e] -> [Atom e] -> Atom e
svgMeshpatch = mkSVGAtom "meshpatch"

svgMeshrow :: [Feature e] -> [Atom e] -> Atom e
svgMeshrow = mkSVGAtom "meshrow"

svgMetadata :: [Feature e] -> [Atom e] -> Atom e
svgMetadata = mkSVGAtom "metadata"

svgMissingGlyph :: [Feature e] -> [Atom e] -> Atom e
svgMissingGlyph = mkSVGAtom "missing-glyph"

svgMpath :: [Feature e] -> [Atom e] -> Atom e
svgMpath = mkSVGAtom "mpath"

svgPath :: [Feature e] -> [Atom e] -> Atom e
svgPath = mkSVGAtom "path"

svgPattern :: [Feature e] -> [Atom e] -> Atom e
svgPattern = mkSVGAtom "pattern"

svgPolygon :: [Feature e] -> [Atom e] -> Atom e
svgPolygon = mkSVGAtom "polygon"

svgPolyline :: [Feature e] -> [Atom e] -> Atom e
svgPolyline = mkSVGAtom "polyline"

svgRadialGradient :: [Feature e] -> [Atom e] -> Atom e
svgRadialGradient = mkSVGAtom "radialGradient"

svgRect :: [Feature e] -> [Atom e] -> Atom e
svgRect = mkSVGAtom "rect"

svgScript :: [Feature e] -> [Atom e] -> Atom e
svgScript = mkSVGAtom "script"

svgSet :: [Feature e] -> [Atom e] -> Atom e
svgSet = mkSVGAtom "set"

svgSolidcolor :: [Feature e] -> [Atom e] -> Atom e
svgSolidcolor = mkSVGAtom "solidcolor"

svgStop :: [Feature e] -> [Atom e] -> Atom e
svgStop = mkSVGAtom "stop"

svgStyle :: [Feature e] -> [Atom e] -> Atom e
svgStyle = mkSVGAtom "style"

svgSvg :: [Feature e] -> [Atom e] -> Atom e
svgSvg = mkSVGAtom "svg"

svgSwitch :: [Feature e] -> [Atom e] -> Atom e
svgSwitch = mkSVGAtom "switch"

svgSymbol :: [Feature e] -> [Atom e] -> Atom e
svgSymbol = mkSVGAtom "symbol"

svgText :: [Feature e] -> [Atom e] -> Atom e
svgText = mkSVGAtom "text"

svgTextPath :: [Feature e] -> [Atom e] -> Atom e
svgTextPath = mkSVGAtom "textPath"

svgTitle :: [Feature e] -> [Atom e] -> Atom e
svgTitle = mkSVGAtom "title"

svgTref :: [Feature e] -> [Atom e] -> Atom e
svgTref = mkSVGAtom "tref"

svgTspan :: [Feature e] -> [Atom e] -> Atom e
svgTspan = mkSVGAtom "tspan"

svgUnknown :: [Feature e] -> [Atom e] -> Atom e
svgUnknown = mkSVGAtom "unknown"

svgUse :: [Feature e] -> [Atom e] -> Atom e
svgUse = mkSVGAtom "use"

svgVideo :: [Feature e] -> [Atom e] -> Atom e
svgVideo = mkSVGAtom "video"

svgView :: [Feature e] -> [Atom e] -> Atom e
svgView = mkSVGAtom "view"

svgVkern :: [Feature e] -> [Atom e] -> Atom e
svgVkern = mkSVGAtom "vkern"
