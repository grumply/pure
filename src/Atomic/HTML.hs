{-# language OverloadedStrings #-}
module Atomic.HTML where

import Ef.Base

import Data.Txt (Txt)

import Atomic.Attribute
import Atomic.Construct

abbr :: [Feature e] -> [Atom e] -> Atom e
abbr = html "abbr"

address :: [Feature e] -> [Atom e] -> Atom e
address = html "address"

area :: [Feature e] -> [Atom e] -> Atom e
area = html "area"

a :: [Feature e] -> [Atom e] -> Atom e
a = html "a"

article :: [Feature e] -> [Atom e] -> Atom e
article = html "article"

aside :: [Feature e] -> [Atom e] -> Atom e
aside = html "aside"

audio :: [Feature e] -> [Atom e] -> Atom e
audio = html "audio"

base :: [Feature e] -> [Atom e] -> Atom e
base = html "base"

bdi :: [Feature e] -> [Atom e] -> Atom e
bdi = html "bdi"

bdo :: [Feature e] -> [Atom e] -> Atom e
bdo = html "bdo"

big :: [Feature e] -> [Atom e] -> Atom e
big = html "big"

blockquote :: [Feature e] -> [Atom e] -> Atom e
blockquote = html "blockquote"

body :: [Feature e] -> [Atom e] -> Atom e
body = html "body"

b :: [Feature e] -> [Atom e] -> Atom e
b = html "b"

br :: Atom e
br = html "br" [] []

button :: [Feature e] -> [Atom e] -> Atom e
button = html "button"

canvas :: [Feature e] -> [Atom e] -> Atom e
canvas = html "canvas"

caption :: [Feature e] -> [Atom e] -> Atom e
caption = html "caption"

cite :: [Feature e] -> [Atom e] -> Atom e
cite = html "cite"

code :: [Feature e] -> [Atom e] -> Atom e
code = html "code"

col :: [Feature e] -> [Atom e] -> Atom e
col = html "col"

colgroup :: [Feature e] -> [Atom e] -> Atom e
colgroup = html "colgroup"

data_ :: [Feature e] -> [Atom e] -> Atom e
data_ = html "data"

datalist :: [Feature e] -> [Atom e] -> Atom e
datalist = html "datalist"

dd :: [Feature e] -> [Atom e] -> Atom e
dd = html "dd"

description :: Txt -> Atom e
description d = meta [ name "description", content d ] []

dl :: [Feature e] -> [Atom e] -> Atom e
dl = html "dl"

dt :: [Feature e] -> [Atom e] -> Atom e
dt = html "dt"

del :: [Feature e] -> [Atom e] -> Atom e
del = html "del"

details :: [Feature e] -> [Atom e] -> Atom e
details = html "details"

dfn :: [Feature e] -> [Atom e] -> Atom e
dfn = html "dfn"

dialog :: [Feature e] -> [Atom e] -> Atom e
dialog = html "dialog"

div :: [Feature e] -> [Atom e] -> Atom e
div = html "div"

em :: [Feature e] -> [Atom e] -> Atom e
em = html "em"

embed :: [Feature e] -> [Atom e] -> Atom e
embed = html "embed"

fieldset :: [Feature e] -> [Atom e] -> Atom e
fieldset = html "fieldset"

figcaption :: [Feature e] -> [Atom e] -> Atom e
figcaption = html "figcaption"

figure :: [Feature e] -> [Atom e] -> Atom e
figure = html "figure"

footer :: [Feature e] -> [Atom e] -> Atom e
footer = html "footer"

form :: [Feature e] -> [Atom e] -> Atom e
form = html "form"

head :: [Atom e] -> Atom e
head = html "head" []

header :: [Feature e] -> [Atom e] -> Atom e
header = html "header"

h1 :: [Feature e] -> [Atom e] -> Atom e
h1 = html "h1"

h2 :: [Feature e] -> [Atom e] -> Atom e
h2 = html "h2"

h3 :: [Feature e] -> [Atom e] -> Atom e
h3 = html "h3"

h4 :: [Feature e] -> [Atom e] -> Atom e
h4 = html "h4"

h5 :: [Feature e] -> [Atom e] -> Atom e
h5 = html "h5"

h6 :: [Feature e] -> [Atom e] -> Atom e
h6 = html "h6"

hgroup :: [Feature e] -> [Atom e] -> Atom e
hgroup = html "hgroup"

hr :: [Feature e] -> [Atom e] -> Atom e
hr = html "hr"

html_ :: [Feature e] -> [Atom e] -> Atom e
html_ = html "html"

iframe :: [Feature e] -> [Atom e] -> Atom e
iframe = html "iframe"

img :: [Feature e] -> [Atom e] -> Atom e
img = html "img"

input :: [Feature e] -> [Atom e] -> Atom e
input = html "input"

textInput :: [Feature e] -> [Atom e] -> Atom e
textInput fs = html "input" (typeA "text":fs)

ins :: [Feature e] -> [Atom e] -> Atom e
ins = html "ins"

i_ :: [Feature e] -> [Atom e] -> Atom e
i_ = html "i"

kbd :: [Feature e] -> [Atom e] -> Atom e
kbd = html "kbd"

keygen :: [Feature e] -> [Atom e] -> Atom e
keygen = html "keygen"

label :: [Feature e] -> [Atom e] -> Atom e
label = html "label"

legend :: [Feature e] -> [Atom e] -> Atom e
legend = html "legend"

li :: [Feature e] -> [Atom e] -> Atom e
li = html "li"

link_ :: [Feature e] -> [Atom e] -> Atom e
link_ = html "link"

main_ :: [Feature e] -> [Atom e] -> Atom e
main_ = html "main"

map_ :: [Feature e] -> [Atom e] -> Atom e
map_ = html "map"

mark :: [Feature e] -> [Atom e] -> Atom e
mark = html "mark"

menu :: [Feature e] -> [Atom e] -> Atom e
menu = html "menu"

menuitem :: [Feature e] -> [Atom e] -> Atom e
menuitem = html "menuitem"

meta :: [Feature e] -> [Atom e] -> Atom e
meta = html "meta"

meter :: [Feature e] -> [Atom e] -> Atom e
meter = html "meter"

nav :: [Feature e] -> [Atom e] -> Atom e
nav = html "nav"

noscript :: [Feature e] -> [Atom e] -> Atom e
noscript = html "noscript"

object_ :: [Feature e] -> [Atom e] -> Atom e
object_ = html "object"

optgroup :: [Feature e] -> [Atom e] -> Atom e
optgroup = html "optgroup"

option :: [Feature e] -> [Atom e] -> Atom e
option = html "option"

ol :: [Feature e] -> [Atom e] -> Atom e
ol = html "ol"

output :: [Feature e] -> [Atom e] -> Atom e
output = html "output"

p :: [Feature e] -> [Atom e] -> Atom e
p = html "p"

param :: [Feature e] -> [Atom e] -> Atom e
param = html "param"

picture :: [Feature e] -> [Atom e] -> Atom e
picture = html "picture"

pre :: [Feature e] -> [Atom e] -> Atom e
pre = html "pre"

progress :: [Feature e] -> [Atom e] -> Atom e
progress = html "progress"

q :: [Feature e] -> [Atom e] -> Atom e
q = html "q"

rp :: [Feature e] -> [Atom e] -> Atom e
rp = html "rp"

rt :: [Feature e] -> [Atom e] -> Atom e
rt = html "rt"

ruby :: [Feature e] -> [Atom e] -> Atom e
ruby = html "ruby"

samp :: [Feature e] -> [Atom e] -> Atom e
samp = html "samp"

script :: [Feature e] -> [Atom e] -> Atom e
script = html "script"

s :: [Feature e] -> [Atom e] -> Atom e
s = html "s"

section :: [Feature e] -> [Atom e] -> Atom e
section = html "section"

select_ :: [Feature e] -> [Atom e] -> Atom e
select_ = html "select"

small :: [Feature e] -> [Atom e] -> Atom e
small = html "small"

source :: [Feature e] -> [Atom e] -> Atom e
source = html "source"

span :: [Feature e] -> [Atom e] -> Atom e
span = html "span"

strong :: [Feature e] -> [Atom e] -> Atom e
strong = html "strong"

style :: [Feature e] -> [Atom e] -> Atom e
style = html "style"

sub :: [Feature e] -> [Atom e] -> Atom e
sub = html "sub"

summary :: [Feature e] -> [Atom e] -> Atom e
summary = html "summary"

sup :: [Feature e] -> [Atom e] -> Atom e
sup = html "sup"

table :: [Feature e] -> [Atom e] -> Atom e
table = html "table"

tbody :: [Feature e] -> [Atom e] -> Atom e
tbody = html "tbody"

td :: [Feature e] -> [Atom e] -> Atom e
td = html "td"

textarea :: [Feature e] -> [Atom e] -> Atom e
textarea = html "textarea"

tfoot :: [Feature e] -> [Atom e] -> Atom e
tfoot = html "tfoot"

th :: [Feature e] -> [Atom e] -> Atom e
th = html "th"

thead :: [Feature e] -> [Atom e] -> Atom e
thead = html "thead"

time :: [Feature e] -> [Atom e] -> Atom e
time = html "time"

title :: Txt -> Atom e
title jst = html "title" [] [ jss jst ]

tr :: [Feature e] -> [Atom e] -> Atom e
tr = html "tr"

track :: [Feature e] -> [Atom e] -> Atom e
track = html "track"

u :: [Feature e] -> [Atom e] -> Atom e
u = html "u"

ul :: [Feature e] -> [Atom e] -> Atom e
ul = html "ul"

var_ :: [Feature e] -> [Atom e] -> Atom e
var_ = html "var"

video :: [Feature e] -> [Atom e] -> Atom e
video = html "video"

viewport :: Txt -> Atom e
viewport jst = html "meta" [ name "viewport", content jst ] []

wbr :: [Feature e] -> [Atom e] -> Atom e
wbr = html "wbr"

--------------------------------------------------------------------------------
-- SVG

svgA :: [Feature e] -> [Atom e] -> Atom e
svgA = svgHTML "a"

svgAudio :: [Feature e] -> [Atom e] -> Atom e
svgAudio = svgHTML "audio"

svgAltGlyph :: [Feature e] -> [Atom e] -> Atom e
svgAltGlyph = svgHTML "altGlyph"

svgAltGlyphDef :: [Feature e] -> [Atom e] -> Atom e
svgAltGlyphDef = svgHTML "altGlyphDef"

svgAltGlyphItem :: [Feature e] -> [Atom e] -> Atom e
svgAltGlyphItem = svgHTML "altGlyphItem"

svgAnimate :: [Feature e] -> [Atom e] -> Atom e
svgAnimate = svgHTML "animate"

svgAnimateColor :: [Feature e] -> [Atom e] -> Atom e
svgAnimateColor = svgHTML "animateColor"

svgAnimateMotion :: [Feature e] -> [Atom e] -> Atom e
svgAnimateMotion = svgHTML "animateMotion"

svgAnimateTransform :: [Feature e] -> [Atom e] -> Atom e
svgAnimateTransform = svgHTML "animateTransform"

svgCanvas :: [Feature e] -> [Atom e] -> Atom e
svgCanvas = svgHTML "canvas"

svgCircle :: [Feature e] -> [Atom e] -> Atom e
svgCircle = svgHTML "circle"

svgClipPath :: [Feature e] -> [Atom e] -> Atom e
svgClipPath = svgHTML "clipPath"

svgColorProfile :: [Feature e] -> [Atom e] -> Atom e
svgColorProfile = svgHTML "color-profile"

svgCursor :: [Feature e] -> [Atom e] -> Atom e
svgCursor = svgHTML "cursor"

svgDefs :: [Feature e] -> [Atom e] -> Atom e
svgDefs = svgHTML "defs"

svgDesc :: [Feature e] -> [Atom e] -> Atom e
svgDesc = svgHTML "desc"

svgDiscard :: [Feature e] -> [Atom e] -> Atom e
svgDiscard = svgHTML "discard"

svgEllipse :: [Feature e] -> [Atom e] -> Atom e
svgEllipse = svgHTML "ellipse"

svgFeBlend :: [Feature e] -> [Atom e] -> Atom e
svgFeBlend = svgHTML "feBlend"

svgFeColorMatrix :: [Feature e] -> [Atom e] -> Atom e
svgFeColorMatrix = svgHTML "feColorMatrix"

svgFeComponentTransfer :: [Feature e] -> [Atom e] -> Atom e
svgFeComponentTransfer = svgHTML "feComponentTransfer"

svgFeComposite :: [Feature e] -> [Atom e] -> Atom e
svgFeComposite = svgHTML "feComposite"

svgFeConvolveMatrix :: [Feature e] -> [Atom e] -> Atom e
svgFeConvolveMatrix = svgHTML "feConvolveMatrix"

svgFeDiffuseLighting :: [Feature e] -> [Atom e] -> Atom e
svgFeDiffuseLighting = svgHTML "feDiffuseLighting"

svgFeDisplacementMap :: [Feature e] -> [Atom e] -> Atom e
svgFeDisplacementMap = svgHTML "feDisplacementMap"

svgFeDistantLight :: [Feature e] -> [Atom e] -> Atom e
svgFeDistantLight = svgHTML "feDistantLight"

svgFeDropShadow :: [Feature e] -> [Atom e] -> Atom e
svgFeDropShadow = svgHTML "feDropShadow"

svgFeFlood :: [Feature e] -> [Atom e] -> Atom e
svgFeFlood = svgHTML "feFlood"

svgFeFuncA :: [Feature e] -> [Atom e] -> Atom e
svgFeFuncA = svgHTML "feFuncA"

svgFeFuncB :: [Feature e] -> [Atom e] -> Atom e
svgFeFuncB = svgHTML "feFuncB"

svgFeFuncG :: [Feature e] -> [Atom e] -> Atom e
svgFeFuncG = svgHTML "feFuncG"

svgFeFuncR :: [Feature e] -> [Atom e] -> Atom e
svgFeFuncR = svgHTML "feFuncR"

svgFeGaussianBlur :: [Feature e] -> [Atom e] -> Atom e
svgFeGaussianBlur = svgHTML "feGaussianBlur"

svgFeImage :: [Feature e] -> [Atom e] -> Atom e
svgFeImage = svgHTML "feImage"

svgFeMerge :: [Feature e] -> [Atom e] -> Atom e
svgFeMerge = svgHTML "feMerge"

svgFeMergeNode :: [Feature e] -> [Atom e] -> Atom e
svgFeMergeNode = svgHTML "feMergeNode"

svgFeMorphology :: [Feature e] -> [Atom e] -> Atom e
svgFeMorphology = svgHTML "feMorphology"

svgFeOffset :: [Feature e] -> [Atom e] -> Atom e
svgFeOffset = svgHTML "feOffset"

svgFePointLight :: [Feature e] -> [Atom e] -> Atom e
svgFePointLight = svgHTML "fePointLight"

svgFeSpecularLighting :: [Feature e] -> [Atom e] -> Atom e
svgFeSpecularLighting = svgHTML "feSpecularLighting"

svgFeSpotLight :: [Feature e] -> [Atom e] -> Atom e
svgFeSpotLight = svgHTML "feSpotLight"

svgFeTile :: [Feature e] -> [Atom e] -> Atom e
svgFeTile = svgHTML "feTile"

svgFeTurbulence :: [Feature e] -> [Atom e] -> Atom e
svgFeTurbulence = svgHTML "feTurbulence"

svgFilter :: [Feature e] -> [Atom e] -> Atom e
svgFilter = svgHTML "filter"

svgFont :: [Feature e] -> [Atom e] -> Atom e
svgFont = svgHTML "font"

svgFontFace :: [Feature e] -> [Atom e] -> Atom e
svgFontFace = svgHTML "font-face"

svgFontFaceFormat :: [Feature e] -> [Atom e] -> Atom e
svgFontFaceFormat = svgHTML "font-face-format"

svgFontFaceName :: [Feature e] -> [Atom e] -> Atom e
svgFontFaceName = svgHTML "font-face-name"

svgFontFaceSrc :: [Feature e] -> [Atom e] -> Atom e
svgFontFaceSrc = svgHTML "font-face-src"

svgFontFaceUri :: [Feature e] -> [Atom e] -> Atom e
svgFontFaceUri = svgHTML "font-face-uri"

svgForeignObject :: [Feature e] -> [Atom e] -> Atom e
svgForeignObject = svgHTML "foreignObject"

svgG :: [Feature e] -> [Atom e] -> Atom e
svgG = svgHTML "g"

svgGlyph :: [Feature e] -> [Atom e] -> Atom e
svgGlyph = svgHTML "glyph"

svgGlyphRef :: [Feature e] -> [Atom e] -> Atom e
svgGlyphRef = svgHTML "glyphRef"

svgHatch :: [Feature e] -> [Atom e] -> Atom e
svgHatch = svgHTML "hatch"

svgHatchpath :: [Feature e] -> [Atom e] -> Atom e
svgHatchpath = svgHTML "hatchpath"

svgHkern :: [Feature e] -> [Atom e] -> Atom e
svgHkern = svgHTML "hkern"

svgIframe :: [Feature e] -> [Atom e] -> Atom e
svgIframe = svgHTML "iframe"

svgImage :: [Feature e] -> [Atom e] -> Atom e
svgImage = svgHTML "image"

svgLine :: [Feature e] -> [Atom e] -> Atom e
svgLine = svgHTML "line"

svgLinearGradient :: [Feature e] -> [Atom e] -> Atom e
svgLinearGradient = svgHTML "linearGradient"

svgMarker :: [Feature e] -> [Atom e] -> Atom e
svgMarker = svgHTML "marker"

svgMask :: [Feature e] -> [Atom e] -> Atom e
svgMask = svgHTML "mask"

svgMesh :: [Feature e] -> [Atom e] -> Atom e
svgMesh = svgHTML "mesh"

svgMeshgradient :: [Feature e] -> [Atom e] -> Atom e
svgMeshgradient = svgHTML "meshgradient"

svgMeshpatch :: [Feature e] -> [Atom e] -> Atom e
svgMeshpatch = svgHTML "meshpatch"

svgMeshrow :: [Feature e] -> [Atom e] -> Atom e
svgMeshrow = svgHTML "meshrow"

svgMetadata :: [Feature e] -> [Atom e] -> Atom e
svgMetadata = svgHTML "metadata"

svgMissingGlyph :: [Feature e] -> [Atom e] -> Atom e
svgMissingGlyph = svgHTML "missing-glyph"

svgMpath :: [Feature e] -> [Atom e] -> Atom e
svgMpath = svgHTML "mpath"

svgPath :: [Feature e] -> [Atom e] -> Atom e
svgPath = svgHTML "path"

svgPattern :: [Feature e] -> [Atom e] -> Atom e
svgPattern = svgHTML "pattern"

svgPolygon :: [Feature e] -> [Atom e] -> Atom e
svgPolygon = svgHTML "polygon"

svgPolyline :: [Feature e] -> [Atom e] -> Atom e
svgPolyline = svgHTML "polyline"

svgRadialGradient :: [Feature e] -> [Atom e] -> Atom e
svgRadialGradient = svgHTML "radialGradient"

svgRect :: [Feature e] -> [Atom e] -> Atom e
svgRect = svgHTML "rect"

svgScript :: [Feature e] -> [Atom e] -> Atom e
svgScript = svgHTML "script"

svgSet :: [Feature e] -> [Atom e] -> Atom e
svgSet = svgHTML "set"

svgSolidcolor :: [Feature e] -> [Atom e] -> Atom e
svgSolidcolor = svgHTML "solidcolor"

svgStop :: [Feature e] -> [Atom e] -> Atom e
svgStop = svgHTML "stop"

svgStyle :: [Feature e] -> [Atom e] -> Atom e
svgStyle = svgHTML "style"

svgSvg :: [Feature e] -> [Atom e] -> Atom e
svgSvg = svgHTML "svg"

svgSwitch :: [Feature e] -> [Atom e] -> Atom e
svgSwitch = svgHTML "switch"

svgSymbol :: [Feature e] -> [Atom e] -> Atom e
svgSymbol = svgHTML "symbol"

svgText :: [Feature e] -> [Atom e] -> Atom e
svgText = svgHTML "text"

svgTextPath :: [Feature e] -> [Atom e] -> Atom e
svgTextPath = svgHTML "textPath"

svgTitle :: [Feature e] -> [Atom e] -> Atom e
svgTitle = svgHTML "title"

svgTref :: [Feature e] -> [Atom e] -> Atom e
svgTref = svgHTML "tref"

svgTspan :: [Feature e] -> [Atom e] -> Atom e
svgTspan = svgHTML "tspan"

svgUnknown :: [Feature e] -> [Atom e] -> Atom e
svgUnknown = svgHTML "unknown"

svgUse :: [Feature e] -> [Atom e] -> Atom e
svgUse = svgHTML "use"

svgVideo :: [Feature e] -> [Atom e] -> Atom e
svgVideo = svgHTML "video"

svgView :: [Feature e] -> [Atom e] -> Atom e
svgView = svgHTML "view"

svgVkern :: [Feature e] -> [Atom e] -> Atom e
svgVkern = svgHTML "vkern"
