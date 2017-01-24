{-# language OverloadedStrings #-}
{-# language CPP #-}
module Nuclear.CSS.Styles where

import Data.JSText

import Nuclear.ToText
import Nuclear.CSS.Helpers

#ifdef __GHCJS__
import Data.JSString as JSText (intercalate)
#else
import Data.Text as JSText (intercalate)
#endif

import Data.Monoid

screenMinWidth :: Int -> JSText
screenMinWidth w = "only screen and (min-width:" <> px w <> ")"

allMinWidth :: Int -> JSText
allMinWidth w = "all and (min-width" <> px w <> ")"

screenMaxWidth :: Int -> JSText
screenMaxWidth w = "only screen and (max-width:" <> px w <> ")"

allMaxWidth :: Int -> JSText
allMaxWidth w = "all and (max-width:" <> px w <> ")"

true :: JSText
true = "true"

false :: JSText
false = "false"

color :: JSText
color = "color"

bgcolor :: JSText
bgcolor = "bgcolor"

border :: JSText
border = "border"

sizes :: JSText
sizes = "sizes"

srcset :: JSText
srcset = "srcset"

subject :: JSText
subject = "subject"

valign :: JSText
valign = "valign"

important :: JSText
important = "!important"

none_ :: JSText
none_ = "none"

zero :: JSText
zero = "0"

one :: JSText
one = "1"

two :: JSText
two = "2"

negativeOne :: JSText
negativeOne = "-1"

initial :: JSText
initial = "initial"

inherit :: JSText
inherit = "inherit"

auto :: JSText
auto = "auto"

center :: JSText
center = "center"

normal :: JSText
normal = "normal"

int :: Int -> JSText
int = toText

dec :: Double -> JSText
dec = toText

sec :: Double -> JSText
sec n = (toText n) <> "s"

ms :: Int -> JSText
ms n = (toText n) <> "ms"

per :: Double -> JSText
per n = (toText n) <> "%"

deg :: Double -> JSText
deg n = (toText n) <> "deg"

per2 :: Double -> Double -> JSText
per2 n1 n2 = per n1 <+> per n2

per3 :: Double -> Double -> Double -> JSText
per3 n1 n2 n3 = per n1 <+> per n2 <+> per n3

per4 :: Double -> Double -> Double -> Double -> JSText
per4 n1 n2 n3 n4 = per n1 <+> per n2 <+> per n3 <+> per n4

px :: Int -> JSText
px n = int n <> "px"

px2 :: Int -> Int -> JSText
px2 n1 n2 = px n1 <+> px n2

px3 :: Int -> Int -> Int -> JSText
px3 n1 n2 n3 = px n1 <+> px n2 <+> px n3

px4 :: Int -> Int -> Int -> Int -> JSText
px4 n1 n2 n3 n4 = px n1 <+> px n2 <+> px n3 <+> px n4

ems :: Double -> JSText
ems n = dec n <> "em"

ems2 :: Double -> Double -> JSText
ems2 n1 n2 = ems n1 <+> ems n2

ems3 :: Double -> Double -> Double -> JSText
ems3 n1 n2 n3 = ems n1 <+> ems n2 <+> ems n3

ems4 :: Double -> Double -> Double -> Double -> JSText
ems4 n1 n2 n3 n4 = ems n1 <+> ems n2 <+> ems n3 <+> ems n4

rems :: Double -> JSText
rems n = dec n <> "rem"

rems2 :: Double -> Double -> JSText
rems2 n1 n2 = rems n1 <+> rems n2

rems3 :: Double -> Double -> Double -> JSText
rems3 n1 n2 n3 = rems n1 <+> rems n2 <+> rems n3

rems4 :: Double -> Double -> Double -> Double -> JSText
rems4 n1 n2 n3 n4 = rems n1 <+> rems n2 <+> rems n3 <+> rems n4

individual :: JSText -> JSText
individual = ("#" <>)

classified :: JSText -> JSText
classified = ("." <>)

contentS :: JSText
contentS = "content"

touchAction :: JSText
touchAction = "touch-action"

pointerEvents :: JSText
pointerEvents = "pointer-events"

margin :: JSText
margin = "margin"

marginLeft :: JSText
marginLeft = "margin-left"

marginRight :: JSText
marginRight = "margin-right"

marginTop :: JSText
marginTop = "margin-top"

marginBottom :: JSText
marginBottom = "margin-bottom"

padding :: JSText
padding = "padding"

paddingLeft :: JSText
paddingLeft = "padding-left"

paddingRight :: JSText
paddingRight = "padding-right"

paddingTop :: JSText
paddingTop = "padding-top"

paddingBottom :: JSText
paddingBottom = "padding-bottom"

borderImage :: JSText
borderImage = "border-image"

borderImageSource :: JSText
borderImageSource = "border-image-source"

borderImageSlice :: JSText
borderImageSlice = "border-image-slice"

borderImageWidth :: JSText
borderImageWidth = "border-image-width"

borderImageOutset :: JSText
borderImageOutset = "border-image-outset"

borderImageRepeat :: JSText
borderImageRepeat = "border-image-repeat"

outline :: JSText
outline = "outline"

outlineStyle :: JSText
outlineStyle = "outline-style"

outlineWidth :: JSText
outlineWidth = "outline-width"

outlineColor :: JSText
outlineColor = "outline-color"

outlineOffset :: JSText
outlineOffset = "outline-offset"

borderS :: JSText
borderS = "border"

borderTop :: JSText
borderTop = "border-top"

borderTopImage :: JSText
borderTopImage = "border-top-image"

borderRight :: JSText
borderRight = "border-right"

borderRightImage :: JSText
borderRightImage = "border-right-image"

borderBottom :: JSText
borderBottom = "border-bottom"

borderBottomImage :: JSText
borderBottomImage = "border-bottom-image"

borderLeft :: JSText
borderLeft = "border-left"

borderLeftImage :: JSText
borderLeftImage = "border-left-image"

borderRadius :: JSText
borderRadius = "border-radius"

borderTopRightRadius :: JSText
borderTopRightRadius = "border-top-right-radius"

borderBottomRightRadius :: JSText
borderBottomRightRadius = "border-bottom-right-radius"

borderBottomLeftRadius :: JSText
borderBottomLeftRadius = "border-bottom-left-radius"

borderTopLeftRadius :: JSText
borderTopLeftRadius = "border-top-left-radius"

borderStyle :: JSText
borderStyle = "border-style"

borderTopStyle :: JSText
borderTopStyle = "border-top-style"

borderRightStyle :: JSText
borderRightStyle = "border-right-style"

borderBottomStyle :: JSText
borderBottomStyle = "border-bottom-style"

borderLeftStyle :: JSText
borderLeftStyle = "border-left-style"

borderWidth :: JSText
borderWidth = "border-width"

borderTopWidth :: JSText
borderTopWidth = "border-top-width"

borderRightWidth :: JSText
borderRightWidth = "border-right-width"

borderBottomWidth :: JSText
borderBottomWidth = "border-bottom-width"

borderLeftWidth :: JSText
borderLeftWidth = "border-left-width"

borderColor :: JSText
borderColor = "border-color"

borderTopColor :: JSText
borderTopColor = "border-top-color"

borderRightColor :: JSText
borderRightColor = "border-right-color"

borderBottomColor :: JSText
borderBottomColor = "border-bottom-color"

borderLeftColor :: JSText
borderLeftColor = "border-left-color"

collapse :: JSText
collapse = "collapse"

borderCollapse :: JSText
borderCollapse = "border-collapse"

borderSpacing :: JSText
borderSpacing = "border-spacing"

dotted :: JSText
dotted = "dotted"

dashed :: JSText
dashed = "dashed"

solid :: JSText
solid = "solid"

double :: JSText
double = "double"

groove :: JSText
groove = "groove"

ridge :: JSText
ridge = "ridge"

inset :: JSText
inset = "inset"

outset :: JSText
outset = "outset"

listStyle :: JSText
listStyle = "list-style"

listStyleType :: JSText
listStyleType = "list-style-type"

listStyleImage :: JSText
listStyleImage = "list-style-image"

listStylePosition :: JSText
listStylePosition = "list-style-position"

inside :: JSText
inside = "inside"

outside :: JSText
outside = "outside"

disc :: JSText
disc = "disc"

armenian :: JSText
armenian = "armenian"

cjkIdeographic :: JSText
cjkIdeographic = "cjk-ideographic"

decimal :: JSText
decimal = "decimal"

decimalLeadingZero :: JSText
decimalLeadingZero = "decimal-leading-zero"

georgian :: JSText
georgian = "georgian"

hebrew :: JSText
hebrew = "hebrew"

hiragana :: JSText
hiragana = "hiragana"

katakana :: JSText
katakana = "katakana"

katakanaIroha :: JSText
katakanaIroha = "katakana-iroha"

lowerAlpha :: JSText
lowerAlpha = "lower-alpha"

lowerGreek :: JSText
lowerGreek = "lower-greek"

lowerLatin :: JSText
lowerLatin = "lower-latin"

lowerRoman :: JSText
lowerRoman = "lower-roman"

square :: JSText
square = "square"

upperAlpha :: JSText
upperAlpha = "upper-alpha"

upperLatin :: JSText
upperLatin = "upper-latin"

upperRoman :: JSText
upperRoman = "upper-roman"

zIndex :: JSText
zIndex = "z-index"

display :: JSText
display = "display"

inline :: JSText
inline = "inline"

block :: JSText
block = "block"

inlineBlock :: JSText
inlineBlock = "inline-block"

inlineTable :: JSText
inlineTable = "inline-table"

listItem :: JSText
listItem = "list-item"

runIn :: JSText
runIn = "run-in"

tableCaption :: JSText
tableCaption = "table-caption"

captionSide :: JSText
captionSide = "caption-side"

emptyCells :: JSText
emptyCells = "empty-cells"

tableCell :: JSText
tableCell = "table-cell"

tableLayout :: JSText
tableLayout = "table-layout"

tableColumn :: JSText
tableColumn = "table-column"

tableColumnGroup :: JSText
tableColumnGroup = "table-column-group"

tableFooterGroup :: JSText
tableFooterGroup = "table-footer-group"

tableHeaderGroup :: JSText
tableHeaderGroup = "table-header-group"

tableRow :: JSText
tableRow = "table-row"

tableRowGroup :: JSText
tableRowGroup = "table-row-group"

both :: JSText
both = "both"

float :: JSText
float = "float"

clear :: JSText
clear = "clear"

position :: JSText
position = "position"

staticS :: JSText
staticS = "static"

relative :: JSText
relative = "relative"

fixed :: JSText
fixed = "fixed"

absolute :: JSText
absolute = "absolute"

top :: JSText
top = "top"

right :: JSText
right = "right"

bottom :: JSText
bottom = "bottom"

left :: JSText
left = "left"

overflow :: JSText
overflow = "overflow"

visible :: JSText
visible = "visible"

scroll :: JSText
scroll = "scroll"

overflowX :: JSText
overflowX = "overflow-x"

overflowY :: JSText
overflowY = "overflow-y"

clip :: JSText
clip = "clip"

rectS :: (Num n, ToText n) => (n,n,n,n) -> JSText
rectS (t,r,b,l) = "rect(" <> (JSText.intercalate "," $ map toText [t,r,b,l]) <> ")"

lineHeight :: JSText
lineHeight = "line-height"

height :: JSText
height = "height"

width :: JSText
width = "width"

maxWidth :: JSText
maxWidth = "max-width"

minWidth :: JSText
minWidth = "min-width"

maxHeight :: JSText
maxHeight = "max-height"

minHeight :: JSText
minHeight = "min-height"

boxSizing :: JSText
boxSizing = "box-sizing"

borderBox :: JSText
borderBox = "border-box"

contentBox :: JSText
contentBox = "content-box"

font :: JSText
font = "font"

fontFamily :: JSText
fontFamily = "font-family"

fontWeight :: JSText
fontWeight = "font-weight"

fontSize :: JSText
fontSize = "font-size"

xxSmall :: JSText
xxSmall = "xx-small"

xSmall :: JSText
xSmall = "x-small"

medium :: JSText
medium = "medium"

large :: JSText
large = "large"

xLarge :: JSText
xLarge = "x-large"

xxLarge :: JSText
xxLarge = "xx-large"

smaller :: JSText
smaller = "smaller"

larger :: JSText
larger = "larger"

italic :: JSText
italic = "italic"

regular :: JSText
regular = "regular"

weight :: Int -> JSText
weight = toText

bold :: JSText
bold = "bold"

bolder :: JSText
bolder = "bolder"

lighter :: JSText
lighter = "lighter"

boxShadow :: JSText
boxShadow = "box-shadow"

textDecoration :: JSText
textDecoration = "text-decoration"

textDecorationColor :: JSText
textDecorationColor = "text-decoration-color"

textDecorationLine :: JSText
textDecorationLine = "text-decoration-line"

textDecorationStyle :: JSText
textDecorationStyle = "text-decoration-style"

textAlign :: JSText
textAlign = "text-align"

verticalAlign :: JSText
verticalAlign = "vertical-align"

middle :: JSText
middle = "middle"

textIndent :: JSText
textIndent = "text-indent"

textJustify :: JSText
textJustify = "text-justify"

textOverflow :: JSText
textOverflow = "text-overflow"

underline :: JSText
underline = "underline"

textShadow :: JSText
textShadow = "text-shadow"

textTransform :: JSText
textTransform = "text-transform"

capitalize :: JSText
capitalize = "capitalize"

uppercase :: JSText
uppercase = "uppercase"

lowercase :: JSText
lowercase = "lowercase"

unicodeBidi :: JSText
unicodeBidi = "unicode-bidi"

bidiOverride :: JSText
bidiOverride = "bidir-override"

whiteSpace :: JSText
whiteSpace = "white-space"

wrapS :: JSText
wrapS = "wrap"

nowrap :: JSText
nowrap = "nowrap"

wrapreverse :: JSText
wrapreverse = "wrap-reverse"

preLine :: JSText
preLine = "pre-line"

preWrap :: JSText
preWrap = "pre-wrap"

wordWrap :: JSText
wordWrap = "word-wrap"

wordBreak :: JSText
wordBreak = "word-break"

wordSpacing :: JSText
wordSpacing = "word-spacing"

breakWord :: JSText
breakWord = "break-word"

to :: JSText -> JSText
to dir = "to " <> dir

from :: JSText -> JSText
from dir = "from " <> dir

closestSide :: JSText
closestSide = "closest-side"

farthestSide :: JSText
farthestSide = "farthest-side"

closestCorner :: JSText
closestCorner = "closest-corner"

farthestCorner :: JSText
farthestCorner = "farthest-corner"

background :: JSText
background = "background"

bgColor :: JSText
bgColor = backgroundColor

backgroundColor :: JSText
backgroundColor = "background-color"

bgImage :: JSText
bgImage = backgroundImage

backgroundImage :: JSText
backgroundImage = "background-image"

url :: JSText -> JSText
url u = "url(" <> u <> ")"

backgroundRepeat :: JSText
backgroundRepeat = "background-repeat"

noRepeat :: JSText
noRepeat = "no-repeat"

bgRepeat :: JSText
bgRepeat = backgroundRepeat

backgroundAttachment :: JSText
backgroundAttachment = "background-attachment"

bgAttachment :: JSText
bgAttachment = backgroundAttachment

backgroundPosition :: JSText
backgroundPosition = "background-position"

bgPosition :: JSText
bgPosition = bgPosition

backgroundSize :: JSText
backgroundSize = "background-size"

cover :: JSText
cover = "cover"

bgSize :: JSText
bgSize = backgroundSize

backgroundOrigin :: JSText
backgroundOrigin = "background-origin"

bgOrigin :: JSText
bgOrigin = backgroundOrigin

backgroundClip :: JSText
backgroundClip = "background-clip"

bgClip :: JSText
bgClip = backgroundClip

imageFilter :: JSText
imageFilter = "filter"

cursor :: JSText
cursor = "cursor"

pointer :: JSText
pointer = "pointer"

alias :: JSText
alias = "alias"

allScroll :: JSText
allScroll = "all-scroll"

cell :: JSText
cell = "cell"

contextMenu :: JSText
contextMenu = "context-menu"

colResize :: JSText
colResize = "col-resize"

copy :: JSText
copy = "copy"

crosshair :: JSText
crosshair = "crosshair"

eResize :: JSText
eResize = "e-resize"

ewResize :: JSText
ewResize = "ew-resize"

grab :: JSText
grab = "grab"

grabbing :: JSText
grabbing = "grabbing"

help :: JSText
help = "help"

move :: JSText
move = "move"

nResize :: JSText
nResize = "n-resize"

neResize :: JSText
neResize = "ne-resize"

neswResize :: JSText
neswResize = "nesw-resize"

nsResize :: JSText
nsResize = "ns-resize"

nwResize :: JSText
nwResize = "nw-resize"

nwseResize :: JSText
nwseResize = "nwse-resize"

noDrop :: JSText
noDrop = "no-drop"

notAllowed :: JSText
notAllowed = "not-allowed"

rowResize :: JSText
rowResize = "row-resize"

sResize :: JSText
sResize = "s-resize"

seResize :: JSText
seResize = "se-resize"

swResize :: JSText
swResize = "sw-resize"

vText :: JSText
vText = "vertical-text"

wResize :: JSText
wResize = "w-resize"

wait_ :: JSText
wait_ = "wait"

zoomIn :: JSText
zoomIn = "zoom-in"

zoomOut :: JSText
zoomOut = "zoom-out"

navDown :: JSText
navDown = "nav-down"

navIndex :: JSText
navIndex = "nav-index"

navLeft :: JSText
navLeft = "nav-left"

navRight :: JSText
navRight = "nav-right"

navUp :: JSText
navUp = "nav-up"

matrix :: Double -> Double -> Double -> Double -> Int -> Int -> JSText
matrix scX skY skX scY tX tY =
  "matrix(" <> dec scX <> ","
            <> dec skY <> ","
            <> dec skX <> ","
            <> dec scY <> ","
            <> int tX  <> ","
            <> int tY  <> ")"

scale :: Double -> Double -> JSText
scale opts1 opts2 = "scale(" <> dec opts1 <> "," <> dec opts2 <> ")"

scaleX :: Double -> JSText
scaleX opts1 = "scaleX(" <> dec opts1 <> ")"

scaleY :: Double -> JSText
scaleY opts1 = "scaleY(" <> dec opts1 <> ")"

scaleZ :: Double -> JSText
scaleZ opts1 = "scaleZ(" <> dec opts1 <> ")"

scale3d :: Double -> Double -> JSText
scale3d opts1 opts2 = "scale3d(" <> dec opts1 <> "," <> dec opts2 <> ")"

rotate :: Double -> JSText
rotate opts = "rotate(" <> deg opts <> ")"

translate :: Double -> Double -> JSText
translate opts1 opts2 = "translate(" <> per opts1 <> "," <> per opts2 <> ")"

translateX :: Double -> JSText
translateX opts1 = "translateX(" <> per opts1 <> ")"

translateY :: Double -> JSText
translateY opts1 = "translateY(" <> per opts1 <> ")"

translateZ :: Double -> JSText
translateZ opts1 = "translateZ(" <> per opts1 <> ")"

translate3d :: Double -> Double -> Double -> JSText
translate3d opts1 opts2 opts3 =
  "translate3d(" <> per opts1 <> ","
                 <> per opts2 <> ","
                 <> per opts3 <> ")"

matrix3d :: Double -> Double -> Double -> Double
         -> Double -> Double -> Double -> Double
         -> Double -> Double -> Double -> Double
         -> Double -> Double -> Double -> Double
         -> JSText
matrix3d a1 b1 c1 d1 a2 b2 c2 d2 a3 b3 c3 d3 a4 b4 c4 d4 =
  "matrix3d(" <> dec a1 <> ","
              <> dec b1 <> ","
              <> dec c1 <> ","
              <> dec d1 <> ","
              <> dec a2 <> ","
              <> dec b2 <> ","
              <> dec c2 <> ","
              <> dec d2 <> ","
              <> dec a3 <> ","
              <> dec b3 <> ","
              <> dec c3 <> ","
              <> dec d3 <> ","
              <> dec a4 <> ","
              <> dec b4 <> ","
              <> dec c4 <> ","
              <> dec d4 <> ")"

skew :: Double -> Double -> JSText
skew opts1 opts2 = "skew(" <> deg opts1 <> "," <> deg opts2 <> ")"

skewX :: Double -> JSText
skewX opts1 = "skewX(" <> deg opts1 <> ")"

skewY :: Double -> JSText
skewY opts1 = "skewY(" <> deg opts1 <> ")"

rotateX :: Double -> JSText
rotateX opts = "rotateX(" <> deg opts <> ")"

rotateY :: Double -> JSText
rotateY opts = "rotateY(" <> deg opts <> ")"

rotateZ :: Double -> JSText
rotateZ opts = "rotateZ(" <> deg opts <> ")"

rotate3d :: Double -> Double -> JSText
rotate3d opts1 opts2 = "rotate3d(" <> deg opts1 <> "," <> deg opts2 <> ")"

perspective :: JSText
perspective = "perspective"

perspectiveOrigin :: JSText
perspectiveOrigin = "perspective-origin"

backfaceVisibility :: JSText
backfaceVisibility = "backface-visibility"

transition :: JSText
transition = "transition"

transitionDelay :: JSText
transitionDelay = "transition-delay"

transitionDuration :: JSText
transitionDuration = "transition-duration"

transitionProperty :: JSText
transitionProperty = "transition-property"

transitionTimingFunction :: JSText
transitionTimingFunction = "transition-timing-function"

linear :: JSText
linear = "linear"

linearGradientS :: JSText -> JSText
linearGradientS lg = "linear-gradient(" <> lg <> ")"

transform :: JSText
transform = "transform"

transformStyle :: JSText
transformStyle = "transform-style"

transformOrigin :: JSText
transformOrigin = "transform-origin"

ease_ :: JSText
ease_ = "ease"

easeIn :: JSText
easeIn = "ease-in"

easeOut :: JSText
easeOut = "ease-out"

easeInOut :: JSText
easeInOut = "ease-in-out"

cubicBezier :: (Double,Double,Double,Double) -> JSText
cubicBezier (n1,n2,n3,n4) =
  "cubic-bezier(" <> (JSText.intercalate "," $ map toText [n1,n2,n3,n4]) <> ")"

visibility :: JSText
visibility = "visibility"

all :: JSText
all = "all"

direction :: JSText
direction = "direction"

rtl :: JSText
rtl = "rtl"

ltr :: JSText
ltr = "ltr"

alpha :: JSText -> JSText
alpha alph = "alpha(" <> alph <> ")"

opacity :: JSText
opacity = "opacity"

transparent :: JSText
transparent = "transparent"

hex :: JSText -> JSText
hex rgb = "#" <> rgb
  -- "#" <> (pack $ showHex rgb []) -- doesn't work with leading zeros

rgb :: (Int,Int,Int) -> JSText
rgb (r,g,b) = "rgb(" <> (JSText.intercalate "," [int r,int g,int b]) <> ")"

rgba :: (Int,Int,Int,Double) -> JSText
rgba (r,g,b,a) = "rgba(" <> (JSText.intercalate "," $ [int r,int g,int b,dec a]) <> ")"

hsl :: (Int,Double,Double) -> JSText
hsl (h,s,l) = "hsl(" <> (JSText.intercalate "," [int h,per s,per l]) <> ")"

hsla :: (Int,Double,Double,Double) -> JSText
hsla (h,s,l,a) = "hsla(" <> (JSText.intercalate "," [int h,per s, per l,dec a]) <> ")"

aqua :: JSText
aqua = "aqua"

aquamarine :: JSText
aquamarine = "aquamarine"

azure :: JSText
azure = "azure"

beige :: JSText
beige = "beige"

bisque :: JSText
bisque = "bisque"

black :: JSText
black = "black"

blanchedalmond :: JSText
blanchedalmond = "blanchedalmond"

blue :: JSText
blue = "blue"

blueviolet :: JSText
blueviolet = "blueviolet"

brown :: JSText
brown = "brown"

burlywood :: JSText
burlywood = "burlywood"

cadetblue :: JSText
cadetblue = "cadetblue"

chartreuse :: JSText
chartreuse = "chartreuse"

chocolate :: JSText
chocolate = "chocolate"

coral :: JSText
coral = "coral"

cornflowerblue :: JSText
cornflowerblue = "cornflowerblue"

cornsilk :: JSText
cornsilk = "cornsilk"

crimson :: JSText
crimson = "crimson"

cyan :: JSText
cyan = "cyan"

darkblue :: JSText
darkblue = "darkblue"

darkcyan :: JSText
darkcyan = "darkcyan"

darkgoldenrod :: JSText
darkgoldenrod = "darkgoldenrod"

darkgray :: JSText
darkgray = "darkgray"

darkgreen :: JSText
darkgreen = "darkgreen"

darkgrey :: JSText
darkgrey = "darkgrey"

darkkhaki :: JSText
darkkhaki = "darkkhaki"

darkmagenta :: JSText
darkmagenta = "darkmagenta"

darkolivegreen :: JSText
darkolivegreen = "darkolivegreen"

darkorange :: JSText
darkorange = "darkorange"

darkorchid :: JSText
darkorchid = "darkorchid"

darkred :: JSText
darkred = "darkred"

darksalmon :: JSText
darksalmon = "darksalmon"

darkseagreen :: JSText
darkseagreen = "darkseagreen"

darkslateblue :: JSText
darkslateblue = "darkslateblue"

darkslategray :: JSText
darkslategray = "darkslategray"

darkslategrey :: JSText
darkslategrey = "darkslategrey"

darkturquoise :: JSText
darkturquoise = "darkturquoise"

darkviolet :: JSText
darkviolet = "darkviolet"

deeppink :: JSText
deeppink = "deeppink"

deepskyblue :: JSText
deepskyblue = "deepskyblue"

dimgray :: JSText
dimgray = "dimgray"

dimgrey :: JSText
dimgrey = "dimgrey"

dodgerblue :: JSText
dodgerblue = "dodgerblue"

firebrick :: JSText
firebrick = "firebrick"

floralwhite :: JSText
floralwhite = "floralwhite"

forestgreen :: JSText
forestgreen = "forestgreen"

fuchsia :: JSText
fuchsia = "fuchsia"

gainsboro :: JSText
gainsboro = "gainsboro"

ghostwhite :: JSText
ghostwhite = "ghostwhite"

gold :: JSText
gold = "gold"

goldenrod :: JSText
goldenrod = "goldenrod"

gray :: JSText
gray = "gray"

green :: JSText
green = "green"

greenyellow :: JSText
greenyellow = "greenyellow"

grey :: JSText
grey = "grey"

honeydew :: JSText
honeydew = "honeydew"

hotpink :: JSText
hotpink = "hotpink"

indianred :: JSText
indianred = "indianred"

indigo :: JSText
indigo = "indigo"

ivory :: JSText
ivory = "ivory"

khaki :: JSText
khaki = "khaki"

lavender :: JSText
lavender = "lavender"

lavenderblush :: JSText
lavenderblush = "lavenderblush"

lawngreen :: JSText
lawngreen = "lawngreen"

lemonchiffon :: JSText
lemonchiffon = "lemonchiffon"

lightblue :: JSText
lightblue = "lightblue"

lightcoral :: JSText
lightcoral = "lightcoral"

lightcyan :: JSText
lightcyan = "lightcyan"

lightgoldenrodyellow :: JSText
lightgoldenrodyellow = "lightgoldenrodyellow"

lightgray :: JSText
lightgray = "lightgray"

lightgreen :: JSText
lightgreen = "lightgreen"

lightgrey :: JSText
lightgrey = "lightgrey"

lightpink :: JSText
lightpink = "lightpink"

lightsalmon :: JSText
lightsalmon = "lightsalmon"

lightseagreen :: JSText
lightseagreen = "lightseagreen"

lightskyblue :: JSText
lightskyblue = "lightskyblue"

lightslategray :: JSText
lightslategray = "lightslategray"

lightslategrey :: JSText
lightslategrey = "lightslategrey"

lightsteelblue :: JSText
lightsteelblue = "lightsteelblue"

lightyellow :: JSText
lightyellow = "lightyellow"

lime :: JSText
lime = "lime"

limegreen :: JSText
limegreen = "limegreen"

linen :: JSText
linen = "linen"

magenta :: JSText
magenta = "magenta"

maroon :: JSText
maroon = "maroon"

mediumaquamarine :: JSText
mediumaquamarine = "mediumaquamarine"

mediumblue :: JSText
mediumblue = "mediumblue"

mediumorchid :: JSText
mediumorchid = "mediumorchid"

mediumpurple :: JSText
mediumpurple = "mediumpurple"

mediumseagreen :: JSText
mediumseagreen = "mediumseagreen"

mediumslateblue :: JSText
mediumslateblue = "mediumslateblue"

mediumspringgreen :: JSText
mediumspringgreen = "mediumspringgreen"

mediumturquoise :: JSText
mediumturquoise = "mediumturquoise"

mediumvioletred :: JSText
mediumvioletred = "mediumvioletred"

midnightblue :: JSText
midnightblue = "midnightblue"

mintcream :: JSText
mintcream = "mintcream"

mistyrose :: JSText
mistyrose = "mistyrose"

moccasin :: JSText
moccasin = "moccasin"

navajowhite :: JSText
navajowhite = "navajowhite"

navy :: JSText
navy = "navy"

oldlace :: JSText
oldlace = "oldlace"

olive :: JSText
olive = "olive"

olivedrab :: JSText
olivedrab = "olivedrab"

orange :: JSText
orange = "orange"

orangered :: JSText
orangered = "orangered"

orchid :: JSText
orchid = "orchid"

palegoldenrod :: JSText
palegoldenrod = "palegoldenrod"

palegreen :: JSText
palegreen = "palegreen"

paleturquoise :: JSText
paleturquoise = "paleturquoise"

palevioletred :: JSText
palevioletred = "palevioletred"

papayawhip :: JSText
papayawhip = "papayawhip"

peachpuff :: JSText
peachpuff = "peachpuff"

peru :: JSText
peru = "peru"

pink :: JSText
pink = "pink"

plum :: JSText
plum = "plum"

powderblue :: JSText
powderblue = "powderblue"

purple :: JSText
purple = "purple"

red :: JSText
red = "red"

rosybrown :: JSText
rosybrown = "rosybrown"

royalblue :: JSText
royalblue = "royalblue"

saddlebrown :: JSText
saddlebrown = "saddlebrown"

salmon :: JSText
salmon = "salmon"

sandybrown :: JSText
sandybrown = "sandybrown"

seagreen :: JSText
seagreen = "seagreen"

seashell :: JSText
seashell = "seashell"

sienna :: JSText
sienna = "sienna"

silver :: JSText
silver = "silver"

skyblue :: JSText
skyblue = "skyblue"

slateblue :: JSText
slateblue = "slateblue"

slategray :: JSText
slategray = "slategray"

slategrey :: JSText
slategrey = "slategrey"

snow :: JSText
snow = "snow"

springgreen :: JSText
springgreen = "springgreen"

steelblue :: JSText
steelblue = "steelblue"

tan :: JSText
tan = "tan"

teal :: JSText
teal = "teal"

thistle :: JSText
thistle = "thistle"

tomato :: JSText
tomato = "tomato"

turquoise :: JSText
turquoise = "turquoise"

violet :: JSText
violet = "violet"

wheat :: JSText
wheat = "wheat"

white :: JSText
white = "white"

whitesmoke :: JSText
whitesmoke = "whitesmoke"

yellow :: JSText
yellow = "yellow"

yellowgree :: JSText
yellowgree = "yellowgree"

alignSelf :: JSText
alignSelf = "align-self"

webkitJustifyContent :: JSText
webkitJustifyContent = "-webkit-justify-content"

msFlexPack :: JSText
msFlexPack = "-ms-flex-pack"

webkitBoxPack :: JSText
webkitBoxPack = "-webkit-box-pack"

justifyContent :: JSText
justifyContent = "justify-content"

webkitAlignItems :: JSText
webkitAlignItems = "-webkit-align-items"

msFlexAlign :: JSText
msFlexAlign = "-ms-flex-align"

webkitBoxAlign :: JSText
webkitBoxAlign = "-webkit-box-aign"

alignItems :: JSText
alignItems = "align-items"

flexStart :: JSText
flexStart = "flex-start"

flexEnd :: JSText
flexEnd = "flex-end"

endS :: JSText
endS = "end"

spaceAround :: JSText
spaceAround = "space-around"

spaceBetween :: JSText
spaceBetween = "space-between"

distribute :: JSText
distribute = "distribute"

justify :: JSText
justify = "justify"

webkitOrder :: JSText
webkitOrder = "-webkit-order"

msFlexOrder :: JSText
msFlexOrder = "-ms-flex-order"

webkitBoxOrdinalGroup :: JSText
webkitBoxOrdinalGroup = "-webkit-box-ordinal-group"

order :: JSText
order = "order"

msFlexPreferredSize :: JSText
msFlexPreferredSize = "-ms-flex-preferred-size"

webkitFlexBasis :: JSText
webkitFlexBasis = "-webkit-flex-basis"

flexBasis :: JSText
flexBasis = "flex-basis"

webkitFlexGrow :: JSText
webkitFlexGrow = "-webkit-flex-grow"

msFlexPositive :: JSText
msFlexPositive = "-ms-flex-positive"

webkitBoxFlex :: JSText
webkitBoxFlex = "-webkit-box-flex"

flexGrow :: JSText
flexGrow = "flex-grow"

webkitFlex :: JSText
webkitFlex = "-webkit-flex"

msFlex :: JSText
msFlex = "-ms-flex"

flex :: JSText
flex = "flex"

flexFlow :: JSText
flexFlow = "flex-flow"

webkitFlexDirection :: JSText
webkitFlexDirection = "-webkit-flex-direction"

msFlexDirection :: JSText
msFlexDirection = "-ms-flex-direction"

webkitBoxOrient :: JSText
webkitBoxOrient = "-webkit-box-orient"

webkitBoxDirection :: JSText
webkitBoxDirection = "-webkit-box-direction"

flexDirection :: JSText
flexDirection = "flex-direction"

columnReverse :: JSText
columnReverse = "column-reverse"

vertical :: JSText
vertical = "vertical"

reverse :: JSText
reverse = "reverse"

rowReverse :: JSText
rowReverse = "row-reverse"

horizontal :: JSText
horizontal = "horizontal"

webkitFlexWrap :: JSText
webkitFlexWrap = "-webkit-flex-wrap"

msFlexWrap :: JSText
msFlexWrap = "-ms-flex-wrap"

flexWrap :: JSText
flexWrap = "flex-wrap"

msFlexBox :: JSText
msFlexBox = "-ms-flexbox"

webkitBox :: JSText
webkitBox = "-webkit-box"

rowS :: JSText
rowS = "row"

column :: JSText
column = "column"

xmlns :: JSText
xmlns = "xmlns"

accentHeight :: JSText
accentHeight = "accentHeight"

accumulate :: JSText
accumulate = "accumulate"


additive :: JSText
additive = "additive"

alignmentBaseline :: JSText
alignmentBaseline = "alignment-baseline"

allowReorder :: JSText
allowReorder = "allowReorder"

alphabetic :: JSText
alphabetic = "alphabetic"

amplitude :: JSText
amplitude = "amplitude"

arabicForm :: JSText
arabicForm = "arabic-form"

ascent :: JSText
ascent = "ascent"

attributeName :: JSText
attributeName = "attributeName"

attributeType :: JSText
attributeType = "attributeType"

autoReverse :: JSText
autoReverse = "autoReverse"

azimuth :: JSText
azimuth = "azimuth"

baseFrequency :: JSText
baseFrequency = "baseFrequency"

baselineShift :: JSText
baselineShift = "baseline-shift"

baseProfile :: JSText
baseProfile = "baseProfile"

bbox :: JSText
bbox = "bbox"

begin :: JSText
begin = "begin"

bias :: JSText
bias = "bias"

by :: JSText
by = "by"

calcMode :: JSText
calcMode = "calcMode"

capHeight :: JSText
capHeight = "cap-height"

clipPathUnits :: JSText
clipPathUnits = "clipPathUnits"

clipRule :: JSText
clipRule = "clip-rule"

colorInterpolation :: JSText
colorInterpolation = "color-interpolation"

colorInterpolationFilters :: JSText
colorInterpolationFilters = "color-interpolation-filters"

colorRendering :: JSText
colorRendering = "color-rendering"

contentScriptType :: JSText
contentScriptType = "contentScriptType"

contentStyleType :: JSText
contentStyleType = "contentStyleType"

cx :: JSText
cx = "cx"

cy :: JSText
cy = "cy"

dS :: JSText
dS = "d"

decelerate :: JSText
decelerate = "decelerate"

descent :: JSText
descent = "descent"

diffuseConstant :: JSText
diffuseConstant = "diffuseConstant"

divisor :: JSText
divisor = "divisor"

dominantBaseline :: JSText
dominantBaseline = "dominant-baseline"

dur :: JSText
dur = "dur"

dx :: JSText
dx = "dx"

dy :: JSText
dy = "dy"

edgeMode :: JSText
edgeMode = "edgeMode"

elevation :: JSText
elevation = "elevation"

enableBackground :: JSText
enableBackground = "enable-background"

exponent :: JSText
exponent = "exponent"

externalResourcesRequired :: JSText
externalResourcesRequired = "externalResourcesRequired"

fill :: JSText
fill = "fill"

fillOpacity :: JSText
fillOpacity = "fill-opacity"

fillRule :: JSText
fillRule = "fill-rule"

filterRes :: JSText
filterRes = "filterRes"

filterUnits :: JSText
filterUnits = "filterUnits"

floodColor :: JSText
floodColor = "flood-color"

floodOpacity :: JSText
floodOpacity = "flood-opacity"

fontSizeAdjust :: JSText
fontSizeAdjust = "font-size-adjust"

fontStretch :: JSText
fontStretch = "font-stretch"

fontStyle :: JSText
fontStyle = "font-style"

fontVariant :: JSText
fontVariant = "font-variant"

format :: JSText -> JSText
format f = "format('" <> f <> "')"

fx :: JSText
fx = "fx"

fy :: JSText
fy = "fy"

g1 :: JSText
g1 = "g1"

g2 :: JSText
g2 = "g2"

glyphName :: JSText
glyphName = "glyph-name"

glyphOrientationHorizontal :: JSText
glyphOrientationHorizontal = "glyph-orientation-horizontal"

glyphOrientationVertical :: JSText
glyphOrientationVertical = "glyph-orientation-vertical"

gradientTransform :: JSText
gradientTransform = "gradientTransform"

gradientUnits :: JSText
gradientUnits = "gradientUnits"

hanging :: JSText
hanging = "hanging"

horizAdvX :: JSText
horizAdvX = "horiz-adv-x"

horizOriginX :: JSText
horizOriginX = "horiz-origin-x"

ideographic :: JSText
ideographic = "ideographic"

imageRendering :: JSText
imageRendering = "image-rendering"

inS :: JSText
inS = "in"

in2 :: JSText
in2 = "in2"

intercept :: JSText
intercept = "intercept"

k :: JSText
k = "k"

k1 :: JSText
k1 = "k1"

k2 :: JSText
k2 = "k2"

k3 :: JSText
k3 = "k3"

k4 :: JSText
k4 = "k4"

kernelMatrix :: JSText
kernelMatrix = "kernelMatrix"

kernelUnitLength :: JSText
kernelUnitLength = "kernelUnitLength"

kerning :: JSText
kerning = "kerning"

keyPoints :: JSText
keyPoints = "keyPoints"

keySplines :: JSText
keySplines = "keySplines"

keyTimes :: JSText
keyTimes = "keyTimes"

lengthAdjust :: JSText
lengthAdjust = "lengthAdjust"

letterSpacing :: JSText
letterSpacing = "letter-spacing"

lightingColor :: JSText
lightingColor = "lighting-color"

limitingConeAngle :: JSText
limitingConeAngle = "limitingConeAngle"

markerEnd :: JSText
markerEnd = "marker-end"

markerMid :: JSText
markerMid = "marker-mid"

markerStart :: JSText
markerStart = "marker-start"

markerHeight :: JSText
markerHeight = "markerHeight"

markerUnits :: JSText
markerUnits = "markerUnits"

markerWidth :: JSText
markerWidth = "markerWidth"

maskContentUnits :: JSText
maskContentUnits = "maskContentUnits"

maskUnits :: JSText
maskUnits = "maskUnits"

mathematical :: JSText
mathematical = "mathematical"

mode :: JSText
mode = "mode"

nameS :: JSText
nameS = "name"

numOctaves :: JSText
numOctaves = "numOctaves"

offset :: JSText
offset = "offset"

onabort :: JSText
onabort = "onabort"

onactivate :: JSText
onactivate = "onactivate"

onbegin :: JSText
onbegin = "onbegin"

onclick :: JSText
onclick = "onclick"

onend :: JSText
onend = "onend"

onerror :: JSText
onerror = "onerror"

onfocusin :: JSText
onfocusin = "onfocusin"

onfocusout :: JSText
onfocusout = "onfocusout"

onload :: JSText
onload = "onload"

onmousedown :: JSText
onmousedown = "onmousedown"

onmousemove :: JSText
onmousemove = "onmousemove"

onmouseout :: JSText
onmouseout = "onmouseout"

onmouseover :: JSText
onmouseover = "onmouseover"

onmouseup :: JSText
onmouseup = "onmouseup"

onrepeat :: JSText
onrepeat = "onrepeat"

onresize :: JSText
onresize = "onresize"

onscroll :: JSText
onscroll = "onscroll"

onunload :: JSText
onunload = "onunload"

onzoom :: JSText
onzoom = "onzoom"

operator :: JSText
operator = "operator"

orient :: JSText
orient = "orient"

orientation :: JSText
orientation = "orientation"

origin :: JSText
origin = "origin"

overlinePosition :: JSText
overlinePosition = "overline-position"

overlineThickness :: JSText
overlineThickness = "overline-thickness"

panose1 :: JSText
panose1 = "panose-1"

paintOrder :: JSText
paintOrder = "paint-order"

pathLength :: JSText
pathLength = "pathLength"

patternContentUnits :: JSText
patternContentUnits = "patternContentUnits"

patternTransform :: JSText
patternTransform = "patternTransform"

patternUnits :: JSText
patternUnits = "patternUnits"

points :: JSText
points = "points"

pointsAtX :: JSText
pointsAtX = "pointsAtX"

pointsAtY :: JSText
pointsAtY = "pointsAtY"

pointsAtZ :: JSText
pointsAtZ = "pointsAtZ"

preserveAlpha :: JSText
preserveAlpha = "preserveAlpha"

preserveAspectRatio :: JSText
preserveAspectRatio = "preserveAspectRatio"

primitiveUnits :: JSText
primitiveUnits = "primitiveUnits"

r :: JSText
r = "r"

radius :: JSText
radius = "radius"

refX :: JSText
refX = "refX"

refY :: JSText
refY = "refY"

renderingIntent :: JSText
renderingIntent = "rendering-intent"

counterIncrement :: JSText
counterIncrement = "counter-increment"

counterReset :: JSText
counterReset = "counter-reset"

repeatCount :: JSText
repeatCount = "repeatCount"

repeatDur :: JSText
repeatDur = "repeatDur"

requiredExtensions :: JSText
requiredExtensions = "requiredExtensions"

requiredFeatures :: JSText
requiredFeatures = "requiredFeatures"

restart :: JSText
restart = "restart"

result :: JSText
result = "result"

rx :: JSText
rx = "rx"

ry :: JSText
ry = "ry"

rS :: JSText
rS = "r"

seed :: JSText
seed = "seed"

shapeRendering :: JSText
shapeRendering = "shape-rendering"

slope :: JSText
slope = "slope"

spacing :: JSText
spacing = "spacing"

specularConstant :: JSText
specularConstant = "specularConstant"

specularExponent :: JSText
specularExponent = "specularExponent"

speed :: JSText
speed = "speed"

spreadMethod :: JSText
spreadMethod = "spreadMethod"

startOffset :: JSText
startOffset = "startOffset"

stdDeviation :: JSText
stdDeviation = "stdDeviation"

stemh :: JSText
stemh = "stemh"

stemv :: JSText
stemv = "stemv"

stitchTiles :: JSText
stitchTiles = "stitchTiles"

stopColor :: JSText
stopColor = "stop-color"

stopOpacity :: JSText
stopOpacity = "stop-opacity"

strikethroughPosition :: JSText
strikethroughPosition = "strikethrough-position"

strikethroughThickness :: JSText
strikethroughThickness = "strikethrough-thickness"

stringS :: JSText
stringS = "string"

stroke :: JSText
stroke = "stroke"

strokeDasharray :: JSText
strokeDasharray = "stroke-dasharray"

strokeDashoffset :: JSText
strokeDashoffset = "stroke-dashoffset"

strokeLinecap :: JSText

strokeLinecap = "stroke-linecap"

roundS :: JSText
roundS = "round"

butt :: JSText
butt = "butt"

strokeLinejoin :: JSText
strokeLinejoin = "stroke-linejoin"

strokeMiterlimit :: JSText
strokeMiterlimit = "stroke-miterlimit"

strokeOpacity :: JSText
strokeOpacity = "stroke-opacity"

strokeWidth :: JSText
strokeWidth = "stroke-width"

surfaceScale :: JSText
surfaceScale = "surfaceScale"

systemLanguage :: JSText
systemLanguage = "systemLanguage"

tableValues :: JSText
tableValues = "tableValues"

targetX :: JSText
targetX = "targetX"

targetY :: JSText
targetY = "targetY"

textAnchor :: JSText
textAnchor = "text-anchor"

textLength :: JSText
textLength = "textLength"

u1 :: JSText
u1 = "u1"

u2 :: JSText
u2 = "u2"

underlinePosition :: JSText
underlinePosition = "underline-position"

underlineThickness :: JSText
underlineThickness = "underline-thickness"

unicode :: JSText
unicode = "unicode"

unicodeRange :: JSText
unicodeRange = "unicode-range"

unitsPerEm :: JSText
unitsPerEm = "units-per-em"

vAlphabetic :: JSText
vAlphabetic = "v-alphabetic"

vHanging :: JSText
vHanging = "v-hanging"

vIdeographic :: JSText
vIdeographic = "v-ideographic"

vMathematical :: JSText
vMathematical = "v-mathematical"

values :: JSText
values = "values"

version :: JSText
version = "version"

vertAdvY :: JSText
vertAdvY = "vert-adv-y"

vertOriginX :: JSText
vertOriginX = "vert-origin-x"

vertOriginY :: JSText
vertOriginY = "vert-origin-y"

viewBox :: JSText
viewBox = "viewBox"

viewTarget :: JSText
viewTarget = "viewTarget"

widths :: JSText
widths = "widths"

writingMode :: JSText
writingMode = "writing-mode"

xS :: JSText
xS = "x"

xHeight :: JSText
xHeight = "x-height"

x1 :: JSText
x1 = "x1"

x2 :: JSText
x2 = "x2"

xChannelSelector :: JSText
xChannelSelector = "xChannelSelector"

yS :: JSText
yS = "y"

y1 :: JSText
y1 = "y1"

y2 :: JSText
y2 = "y2"

yChannelSelector :: JSText
yChannelSelector = "yChannelSelector"

zS :: JSText
zS = "z"

zoomAndPan :: JSText
zoomAndPan = "zoomAndPan"
