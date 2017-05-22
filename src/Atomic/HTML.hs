{-# language OverloadedStrings #-}
module Atomic.HTML where

import Ef.Base

import Data.Txt (Txt)

import Data.Typeable

import Atomic.Attribute
import Atomic.Component

abbr :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
abbr = mkAtom "abbr"

address :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
address = mkAtom "address"

area :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
area = mkAtom "area"

a :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
a = mkAtom "a"

article :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
article = mkAtom "article"

asideE :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
asideE = mkAtom "aside"

audio :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
audio = mkAtom "audio"

base :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
base = mkAtom "base"

bdi :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
bdi = mkAtom "bdi"

bdo :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
bdo = mkAtom "bdo"

big :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
big = mkAtom "big"

blockquote :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
blockquote = mkAtom "blockquote"

body :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
body = mkAtom "body"

b :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
b = mkAtom "b"

br :: Typeable e => SomeAtom e
br = mkAtom "br" [] []

button :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
button = mkAtom "button"

canvas :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
canvas = mkAtom "canvas"

caption :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
caption = mkAtom "caption"

cite :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
cite = mkAtom "cite"

code :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
code = mkAtom "code"

col :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
col = mkAtom "col"

colgroup :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
colgroup = mkAtom "colgroup"

dataE :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
dataE = mkAtom "data"

datalist :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
datalist = mkAtom "datalist"

dd :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
dd = mkAtom "dd"

description :: Typeable e => Txt -> SomeAtom e
description d = meta [ name "description", contentA d ] []

dl :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
dl = mkAtom "dl"

dt :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
dt = mkAtom "dt"

del :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
del = mkAtom "del"

details :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
details = mkAtom "details"

dfn :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
dfn = mkAtom "dfn"

dialog :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
dialog = mkAtom "dialog"

div :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
div = mkAtom "div"

em :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
em = mkAtom "em"

embed :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
embed = mkAtom "embed"

fieldset :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
fieldset = mkAtom "fieldset"

figcaption :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
figcaption = mkAtom "figcaption"

figure :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
figure = mkAtom "figure"

footer :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
footer = mkAtom "footer"

form :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
form = mkAtom "form"

frame :: Typeable e => [Feature e] -> SomeAtom e
frame fs = mkAtom "frame" fs []

head :: Typeable e => [SomeAtom e] -> SomeAtom e
head = mkAtom "head" []

header :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
header = mkAtom "header"

h1 :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
h1 = mkAtom "h1"

h2 :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
h2 = mkAtom "h2"

h3 :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
h3 = mkAtom "h3"

h4 :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
h4 = mkAtom "h4"

h5 :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
h5 = mkAtom "h5"

h6 :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
h6 = mkAtom "h6"

hgroup :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
hgroup = mkAtom "hgroup"

hr :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
hr = mkAtom "hr"

htmlE :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
htmlE = mkAtom "html"

iframe :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
iframe = mkAtom "iframe"

img :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
img = mkAtom "img"

input :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
input = mkAtom "input"

textInput :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
textInput fs = mkAtom "input" (typeA "text":fs)

ins :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
ins = mkAtom "ins"

iE :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
iE = mkAtom "i"

kbd :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
kbd = mkAtom "kbd"

keygen :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
keygen = mkAtom "keygen"

label :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
label = mkAtom "label"

legend :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
legend = mkAtom "legend"

li :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
li = mkAtom "li"

linkE :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
linkE = mkAtom "link"

mainE :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
mainE = mkAtom "main"

mapE :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
mapE = mkAtom "map"

mark :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
mark = mkAtom "mark"

menu :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
menu = mkAtom "menu"

menuitem :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
menuitem = mkAtom "menuitem"

meta :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
meta = mkAtom "meta"

meter :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
meter = mkAtom "meter"

nav :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
nav = mkAtom "nav"

noscript :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
noscript = mkAtom "noscript"

objectE :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
objectE = mkAtom "object"

optgroup :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
optgroup = mkAtom "optgroup"

option :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
option = mkAtom "option"

ol :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
ol = mkAtom "ol"

output :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
output = mkAtom "output"

p :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
p = mkAtom "p"

param :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
param = mkAtom "param"

picture :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
picture = mkAtom "picture"

preE :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
preE = mkAtom "pre"

progress :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
progress = mkAtom "progress"

q :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
q = mkAtom "q"

rp :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
rp = mkAtom "rp"

rt :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
rt = mkAtom "rt"

ruby :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
ruby = mkAtom "ruby"

samp :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
samp = mkAtom "samp"

script :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
script = mkAtom "script"

s :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
s = mkAtom "s"

section :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
section = mkAtom "section"

selectE :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
selectE = mkAtom "select"

small :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
small = mkAtom "small"

source :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
source = mkAtom "source"

span :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
span = mkAtom "span"

strong :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
strong = mkAtom "strong"

style :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
style = mkAtom "style"

sub :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
sub = mkAtom "sub"

summary :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
summary = mkAtom "summary"

sup :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
sup = mkAtom "sup"

table :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
table = mkAtom "table"

tbody :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
tbody = mkAtom "tbody"

td :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
td = mkAtom "td"

textarea :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
textarea = mkAtom "textarea"

tfoot :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
tfoot = mkAtom "tfoot"

th :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
th = mkAtom "th"

thead :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
thead = mkAtom "thead"

time :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
time = mkAtom "time"

title :: Typeable e => Txt -> SomeAtom e
title jst = mkAtom "title" [] [ text jst ]

tr :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
tr = mkAtom "tr"

track :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
track = mkAtom "track"

u :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
u = mkAtom "u"

ul :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
ul = mkAtom "ul"

varE :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
varE = mkAtom "var"

video :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
video = mkAtom "video"

viewport :: Typeable e => Txt -> SomeAtom e
viewport jst = mkAtom "meta" [ name "viewport", contentA jst ] []

wbr :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
wbr = mkAtom "wbr"

--------------------------------------------------------------------------------
-- SVG

svgA :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgA = mkSVGAtom "a"

svgAudio :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgAudio = mkSVGAtom "audio"

svgAltGlyph :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgAltGlyph = mkSVGAtom "altGlyph"

svgAltGlyphDef :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgAltGlyphDef = mkSVGAtom "altGlyphDef"

svgAltGlyphItem :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgAltGlyphItem = mkSVGAtom "altGlyphItem"

svgAnimate :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgAnimate = mkSVGAtom "animate"

svgAnimateColor :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgAnimateColor = mkSVGAtom "animateColor"

svgAnimateMotion :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgAnimateMotion = mkSVGAtom "animateMotion"

svgAnimateTransform :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgAnimateTransform = mkSVGAtom "animateTransform"

svgCanvas :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgCanvas = mkSVGAtom "canvas"

svgCircle :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgCircle = mkSVGAtom "circle"

svgClipPath :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgClipPath = mkSVGAtom "clipPath"

svgColorProfile :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgColorProfile = mkSVGAtom "color-profile"

svgCursor :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgCursor = mkSVGAtom "cursor"

svgDefs :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgDefs = mkSVGAtom "defs"

svgDesc :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgDesc = mkSVGAtom "desc"

svgDiscard :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgDiscard = mkSVGAtom "discard"

svgEllipse :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgEllipse = mkSVGAtom "ellipse"

svgFeBlend :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeBlend = mkSVGAtom "feBlend"

svgFeColorMatrix :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeColorMatrix = mkSVGAtom "feColorMatrix"

svgFeComponentTransfer :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeComponentTransfer = mkSVGAtom "feComponentTransfer"

svgFeComposite :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeComposite = mkSVGAtom "feComposite"

svgFeConvolveMatrix :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeConvolveMatrix = mkSVGAtom "feConvolveMatrix"

svgFeDiffuseLighting :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeDiffuseLighting = mkSVGAtom "feDiffuseLighting"

svgFeDisplacementMap :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeDisplacementMap = mkSVGAtom "feDisplacementMap"

svgFeDistantLight :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeDistantLight = mkSVGAtom "feDistantLight"

svgFeDropShadow :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeDropShadow = mkSVGAtom "feDropShadow"

svgFeFlood :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeFlood = mkSVGAtom "feFlood"

svgFeFuncA :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeFuncA = mkSVGAtom "feFuncA"

svgFeFuncB :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeFuncB = mkSVGAtom "feFuncB"

svgFeFuncG :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeFuncG = mkSVGAtom "feFuncG"

svgFeFuncR :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeFuncR = mkSVGAtom "feFuncR"

svgFeGaussianBlur :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeGaussianBlur = mkSVGAtom "feGaussianBlur"

svgFeImage :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeImage = mkSVGAtom "feImage"

svgFeMerge :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeMerge = mkSVGAtom "feMerge"

svgFeMergeNode :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeMergeNode = mkSVGAtom "feMergeNode"

svgFeMorphology :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeMorphology = mkSVGAtom "feMorphology"

svgFeOffset :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeOffset = mkSVGAtom "feOffset"

svgFePointLight :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFePointLight = mkSVGAtom "fePointLight"

svgFeSpecularLighting :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeSpecularLighting = mkSVGAtom "feSpecularLighting"

svgFeSpotLight :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeSpotLight = mkSVGAtom "feSpotLight"

svgFeTile :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeTile = mkSVGAtom "feTile"

svgFeTurbulence :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFeTurbulence = mkSVGAtom "feTurbulence"

svgFilter :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFilter = mkSVGAtom "filter"

svgFont :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFont = mkSVGAtom "font"

svgFontFace :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFontFace = mkSVGAtom "font-face"

svgFontFaceFormat :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFontFaceFormat = mkSVGAtom "font-face-format"

svgFontFaceName :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFontFaceName = mkSVGAtom "font-face-name"

svgFontFaceSrc :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFontFaceSrc = mkSVGAtom "font-face-src"

svgFontFaceUri :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgFontFaceUri = mkSVGAtom "font-face-uri"

svgForeignObject :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgForeignObject = mkSVGAtom "foreignObject"

svgG :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgG = mkSVGAtom "g"

svgGlyph :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgGlyph = mkSVGAtom "glyph"

svgGlyphRef :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgGlyphRef = mkSVGAtom "glyphRef"

svgHatch :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgHatch = mkSVGAtom "hatch"

svgHatchpath :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgHatchpath = mkSVGAtom "hatchpath"

svgHkern :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgHkern = mkSVGAtom "hkern"

svgIframe :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgIframe = mkSVGAtom "iframe"

svgImage :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgImage = mkSVGAtom "image"

svgLine :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgLine = mkSVGAtom "line"

svgLinearGradient :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgLinearGradient = mkSVGAtom "linearGradient"

svgMarker :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgMarker = mkSVGAtom "marker"

svgMask :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgMask = mkSVGAtom "mask"

svgMesh :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgMesh = mkSVGAtom "mesh"

svgMeshgradient :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgMeshgradient = mkSVGAtom "meshgradient"

svgMeshpatch :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgMeshpatch = mkSVGAtom "meshpatch"

svgMeshrow :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgMeshrow = mkSVGAtom "meshrow"

svgMetadata :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgMetadata = mkSVGAtom "metadata"

svgMissingGlyph :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgMissingGlyph = mkSVGAtom "missing-glyph"

svgMpath :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgMpath = mkSVGAtom "mpath"

svgPath :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgPath = mkSVGAtom "path"

svgPattern :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgPattern = mkSVGAtom "pattern"

svgPolygon :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgPolygon = mkSVGAtom "polygon"

svgPolyline :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgPolyline = mkSVGAtom "polyline"

svgRadialGradient :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgRadialGradient = mkSVGAtom "radialGradient"

svgRect :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgRect = mkSVGAtom "rect"

svgScript :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgScript = mkSVGAtom "script"

svgSet :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgSet = mkSVGAtom "set"

svgSolidcolor :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgSolidcolor = mkSVGAtom "solidcolor"

svgStop :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgStop = mkSVGAtom "stop"

svgStyle :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgStyle = mkSVGAtom "style"

svgSvg :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgSvg = mkSVGAtom "svg"

svgSwitch :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgSwitch = mkSVGAtom "switch"

svgSymbol :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgSymbol = mkSVGAtom "symbol"

svgText :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgText = mkSVGAtom "text"

svgTextPath :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgTextPath = mkSVGAtom "textPath"

svgTitle :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgTitle = mkSVGAtom "title"

svgTref :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgTref = mkSVGAtom "tref"

svgTspan :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgTspan = mkSVGAtom "tspan"

svgUnknown :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgUnknown = mkSVGAtom "unknown"

svgUse :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgUse = mkSVGAtom "use"

svgVideo :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgVideo = mkSVGAtom "video"

svgView :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgView = mkSVGAtom "view"

svgVkern :: Typeable e => [Feature e] -> [SomeAtom e] -> SomeAtom e
svgVkern = mkSVGAtom "vkern"
