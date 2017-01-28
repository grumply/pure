{-# language OverloadedStrings #-}
{-# language CPP #-}
module Atomic.CSS.Styles where

import Data.Txt as Txt (Txt,intercalate)


import Atomic.ToTxt
import Atomic.CSS.Helpers

import Data.Monoid

screenMinWidth :: Int -> Txt
screenMinWidth w = "only screen and (min-width:" <> px w <> ")"

allMinWidth :: Int -> Txt
allMinWidth w = "all and (min-width" <> px w <> ")"

screenMaxWidth :: Int -> Txt
screenMaxWidth w = "only screen and (max-width:" <> px w <> ")"

allMaxWidth :: Int -> Txt
allMaxWidth w = "all and (max-width:" <> px w <> ")"

true :: Txt
true = "true"

false :: Txt
false = "false"

color :: Txt
color = "color"

bgcolor :: Txt
bgcolor = "bgcolor"

border :: Txt
border = "border"

sizes :: Txt
sizes = "sizes"

srcset :: Txt
srcset = "srcset"

subject :: Txt
subject = "subject"

valign :: Txt
valign = "valign"

important :: Txt
important = "!important"

noneS :: Txt
noneS = "none"

zero :: Txt
zero = "0"

one :: Txt
one = "1"

two :: Txt
two = "2"

negativeOne :: Txt
negativeOne = "-1"

initial :: Txt
initial = "initial"

inherit :: Txt
inherit = "inherit"

auto :: Txt
auto = "auto"

center :: Txt
center = "center"

normal :: Txt
normal = "normal"

int :: Int -> Txt
int = toTxt

dec :: Double -> Txt
dec = toTxt

sec :: Double -> Txt
sec n = (toTxt n) <> "s"

ms :: Int -> Txt
ms n = (toTxt n) <> "ms"

per :: Double -> Txt
per n = (toTxt n) <> "%"

deg :: Double -> Txt
deg n = (toTxt n) <> "deg"

per2 :: Double -> Double -> Txt
per2 n1 n2 = per n1 <+> per n2

per3 :: Double -> Double -> Double -> Txt
per3 n1 n2 n3 = per n1 <+> per n2 <+> per n3

per4 :: Double -> Double -> Double -> Double -> Txt
per4 n1 n2 n3 n4 = per n1 <+> per n2 <+> per n3 <+> per n4

px :: Int -> Txt
px n = int n <> "px"

px2 :: Int -> Int -> Txt
px2 n1 n2 = px n1 <+> px n2

px3 :: Int -> Int -> Int -> Txt
px3 n1 n2 n3 = px n1 <+> px n2 <+> px n3

px4 :: Int -> Int -> Int -> Int -> Txt
px4 n1 n2 n3 n4 = px n1 <+> px n2 <+> px n3 <+> px n4

ems :: Double -> Txt
ems n = dec n <> "em"

ems2 :: Double -> Double -> Txt
ems2 n1 n2 = ems n1 <+> ems n2

ems3 :: Double -> Double -> Double -> Txt
ems3 n1 n2 n3 = ems n1 <+> ems n2 <+> ems n3

ems4 :: Double -> Double -> Double -> Double -> Txt
ems4 n1 n2 n3 n4 = ems n1 <+> ems n2 <+> ems n3 <+> ems n4

rems :: Double -> Txt
rems n = dec n <> "rem"

rems2 :: Double -> Double -> Txt
rems2 n1 n2 = rems n1 <+> rems n2

rems3 :: Double -> Double -> Double -> Txt
rems3 n1 n2 n3 = rems n1 <+> rems n2 <+> rems n3

rems4 :: Double -> Double -> Double -> Double -> Txt
rems4 n1 n2 n3 n4 = rems n1 <+> rems n2 <+> rems n3 <+> rems n4

individual :: Txt -> Txt
individual = ("#" <>)

classified :: Txt -> Txt
classified = ("." <>)

contentS :: Txt
contentS = "content"

touchAction :: Txt
touchAction = "touch-action"

pointerEvents :: Txt
pointerEvents = "pointer-events"

margin :: Txt
margin = "margin"

marginLeft :: Txt
marginLeft = "margin-left"

marginRight :: Txt
marginRight = "margin-right"

marginTop :: Txt
marginTop = "margin-top"

marginBottom :: Txt
marginBottom = "margin-bottom"

padding :: Txt
padding = "padding"

paddingLeft :: Txt
paddingLeft = "padding-left"

paddingRight :: Txt
paddingRight = "padding-right"

paddingTop :: Txt
paddingTop = "padding-top"

paddingBottom :: Txt
paddingBottom = "padding-bottom"

borderImage :: Txt
borderImage = "border-image"

borderImageSource :: Txt
borderImageSource = "border-image-source"

borderImageSlice :: Txt
borderImageSlice = "border-image-slice"

borderImageWidth :: Txt
borderImageWidth = "border-image-width"

borderImageOutset :: Txt
borderImageOutset = "border-image-outset"

borderImageRepeat :: Txt
borderImageRepeat = "border-image-repeat"

outline :: Txt
outline = "outline"

outlineStyle :: Txt
outlineStyle = "outline-style"

outlineWidth :: Txt
outlineWidth = "outline-width"

outlineColor :: Txt
outlineColor = "outline-color"

outlineOffset :: Txt
outlineOffset = "outline-offset"

borderS :: Txt
borderS = "border"

borderTop :: Txt
borderTop = "border-top"

borderTopImage :: Txt
borderTopImage = "border-top-image"

borderRight :: Txt
borderRight = "border-right"

borderRightImage :: Txt
borderRightImage = "border-right-image"

borderBottom :: Txt
borderBottom = "border-bottom"

borderBottomImage :: Txt
borderBottomImage = "border-bottom-image"

borderLeft :: Txt
borderLeft = "border-left"

borderLeftImage :: Txt
borderLeftImage = "border-left-image"

borderRadius :: Txt
borderRadius = "border-radius"

borderTopRightRadius :: Txt
borderTopRightRadius = "border-top-right-radius"

borderBottomRightRadius :: Txt
borderBottomRightRadius = "border-bottom-right-radius"

borderBottomLeftRadius :: Txt
borderBottomLeftRadius = "border-bottom-left-radius"

borderTopLeftRadius :: Txt
borderTopLeftRadius = "border-top-left-radius"

borderStyle :: Txt
borderStyle = "border-style"

borderTopStyle :: Txt
borderTopStyle = "border-top-style"

borderRightStyle :: Txt
borderRightStyle = "border-right-style"

borderBottomStyle :: Txt
borderBottomStyle = "border-bottom-style"

borderLeftStyle :: Txt
borderLeftStyle = "border-left-style"

borderWidth :: Txt
borderWidth = "border-width"

borderTopWidth :: Txt
borderTopWidth = "border-top-width"

borderRightWidth :: Txt
borderRightWidth = "border-right-width"

borderBottomWidth :: Txt
borderBottomWidth = "border-bottom-width"

borderLeftWidth :: Txt
borderLeftWidth = "border-left-width"

borderColor :: Txt
borderColor = "border-color"

borderTopColor :: Txt
borderTopColor = "border-top-color"

borderRightColor :: Txt
borderRightColor = "border-right-color"

borderBottomColor :: Txt
borderBottomColor = "border-bottom-color"

borderLeftColor :: Txt
borderLeftColor = "border-left-color"

collapse :: Txt
collapse = "collapse"

borderCollapse :: Txt
borderCollapse = "border-collapse"

borderSpacing :: Txt
borderSpacing = "border-spacing"

dotted :: Txt
dotted = "dotted"

dashed :: Txt
dashed = "dashed"

solid :: Txt
solid = "solid"

double :: Txt
double = "double"

groove :: Txt
groove = "groove"

ridge :: Txt
ridge = "ridge"

inset :: Txt
inset = "inset"

outset :: Txt
outset = "outset"

listStyle :: Txt
listStyle = "list-style"

listStyleType :: Txt
listStyleType = "list-style-type"

listStyleImage :: Txt
listStyleImage = "list-style-image"

listStylePosition :: Txt
listStylePosition = "list-style-position"

inside :: Txt
inside = "inside"

outside :: Txt
outside = "outside"

disc :: Txt
disc = "disc"

armenian :: Txt
armenian = "armenian"

cjkIdeographic :: Txt
cjkIdeographic = "cjk-ideographic"

decimal :: Txt
decimal = "decimal"

decimalLeadingZero :: Txt
decimalLeadingZero = "decimal-leading-zero"

georgian :: Txt
georgian = "georgian"

hebrew :: Txt
hebrew = "hebrew"

hiragana :: Txt
hiragana = "hiragana"

katakana :: Txt
katakana = "katakana"

katakanaIroha :: Txt
katakanaIroha = "katakana-iroha"

lowerAlpha :: Txt
lowerAlpha = "lower-alpha"

lowerGreek :: Txt
lowerGreek = "lower-greek"

lowerLatin :: Txt
lowerLatin = "lower-latin"

lowerRoman :: Txt
lowerRoman = "lower-roman"

square :: Txt
square = "square"

upperAlpha :: Txt
upperAlpha = "upper-alpha"

upperLatin :: Txt
upperLatin = "upper-latin"

upperRoman :: Txt
upperRoman = "upper-roman"

zIndex :: Txt
zIndex = "z-index"

display :: Txt
display = "display"

inline :: Txt
inline = "inline"

block :: Txt
block = "block"

inlineBlock :: Txt
inlineBlock = "inline-block"

inlineTable :: Txt
inlineTable = "inline-table"

listItem :: Txt
listItem = "list-item"

runIn :: Txt
runIn = "run-in"

tableCaption :: Txt
tableCaption = "table-caption"

captionSide :: Txt
captionSide = "caption-side"

emptyCells :: Txt
emptyCells = "empty-cells"

tableCell :: Txt
tableCell = "table-cell"

tableLayout :: Txt
tableLayout = "table-layout"

tableColumn :: Txt
tableColumn = "table-column"

tableColumnGroup :: Txt
tableColumnGroup = "table-column-group"

tableFooterGroup :: Txt
tableFooterGroup = "table-footer-group"

tableHeaderGroup :: Txt
tableHeaderGroup = "table-header-group"

tableRow :: Txt
tableRow = "table-row"

tableRowGroup :: Txt
tableRowGroup = "table-row-group"

both :: Txt
both = "both"

float :: Txt
float = "float"

clear :: Txt
clear = "clear"

position :: Txt
position = "position"

staticS :: Txt
staticS = "static"

relative :: Txt
relative = "relative"

fixed :: Txt
fixed = "fixed"

absolute :: Txt
absolute = "absolute"

top :: Txt
top = "top"

right :: Txt
right = "right"

bottom :: Txt
bottom = "bottom"

left :: Txt
left = "left"

overflow :: Txt
overflow = "overflow"

visible :: Txt
visible = "visible"

scroll :: Txt
scroll = "scroll"

overflowX :: Txt
overflowX = "overflow-x"

overflowY :: Txt
overflowY = "overflow-y"

clip :: Txt
clip = "clip"

rectS :: (Num n, ToTxt n) => (n,n,n,n) -> Txt
rectS (t,r,b,l) = "rect(" <> (Txt.intercalate "," $ map toTxt [t,r,b,l]) <> ")"

lineHeight :: Txt
lineHeight = "line-height"

height :: Txt
height = "height"

width :: Txt
width = "width"

maxWidth :: Txt
maxWidth = "max-width"

minWidth :: Txt
minWidth = "min-width"

maxHeight :: Txt
maxHeight = "max-height"

minHeight :: Txt
minHeight = "min-height"

boxSizing :: Txt
boxSizing = "box-sizing"

borderBox :: Txt
borderBox = "border-box"

contentBox :: Txt
contentBox = "content-box"

font :: Txt
font = "font"

fontFamily :: Txt
fontFamily = "font-family"

fontWeight :: Txt
fontWeight = "font-weight"

fontSize :: Txt
fontSize = "font-size"

xxSmall :: Txt
xxSmall = "xx-small"

xSmall :: Txt
xSmall = "x-small"

medium :: Txt
medium = "medium"

large :: Txt
large = "large"

xLarge :: Txt
xLarge = "x-large"

xxLarge :: Txt
xxLarge = "xx-large"

smaller :: Txt
smaller = "smaller"

larger :: Txt
larger = "larger"

italic :: Txt
italic = "italic"

regular :: Txt
regular = "regular"

weight :: Int -> Txt
weight = toTxt

bold :: Txt
bold = "bold"

bolder :: Txt
bolder = "bolder"

lighter :: Txt
lighter = "lighter"

boxShadow :: Txt
boxShadow = "box-shadow"

textDecoration :: Txt
textDecoration = "text-decoration"

textDecorationColor :: Txt
textDecorationColor = "text-decoration-color"

textDecorationLine :: Txt
textDecorationLine = "text-decoration-line"

textDecorationStyle :: Txt
textDecorationStyle = "text-decoration-style"

textAlign :: Txt
textAlign = "text-align"

verticalAlign :: Txt
verticalAlign = "vertical-align"

middle :: Txt
middle = "middle"

textIndent :: Txt
textIndent = "text-indent"

textJustify :: Txt
textJustify = "text-justify"

textOverflow :: Txt
textOverflow = "text-overflow"

underline :: Txt
underline = "underline"

textShadow :: Txt
textShadow = "text-shadow"

textTransform :: Txt
textTransform = "text-transform"

capitalize :: Txt
capitalize = "capitalize"

uppercase :: Txt
uppercase = "uppercase"

lowercase :: Txt
lowercase = "lowercase"

unicodeBidi :: Txt
unicodeBidi = "unicode-bidi"

bidiOverride :: Txt
bidiOverride = "bidir-override"

whiteSpace :: Txt
whiteSpace = "white-space"

wrapS :: Txt
wrapS = "wrap"

nowrap :: Txt
nowrap = "nowrap"

wrapreverse :: Txt
wrapreverse = "wrap-reverse"

preLine :: Txt
preLine = "pre-line"

preWrap :: Txt
preWrap = "pre-wrap"

wordWrap :: Txt
wordWrap = "word-wrap"

wordBreak :: Txt
wordBreak = "word-break"

wordSpacing :: Txt
wordSpacing = "word-spacing"

breakWord :: Txt
breakWord = "break-word"

to :: Txt -> Txt
to dir = "to " <> dir

from :: Txt -> Txt
from dir = "from " <> dir

closestSide :: Txt
closestSide = "closest-side"

farthestSide :: Txt
farthestSide = "farthest-side"

closestCorner :: Txt
closestCorner = "closest-corner"

farthestCorner :: Txt
farthestCorner = "farthest-corner"

background :: Txt
background = "background"

bgColor :: Txt
bgColor = backgroundColor

backgroundColor :: Txt
backgroundColor = "background-color"

bgImage :: Txt
bgImage = backgroundImage

backgroundImage :: Txt
backgroundImage = "background-image"

url :: Txt -> Txt
url u = "url(" <> u <> ")"

backgroundRepeat :: Txt
backgroundRepeat = "background-repeat"

noRepeat :: Txt
noRepeat = "no-repeat"

bgRepeat :: Txt
bgRepeat = backgroundRepeat

backgroundAttachment :: Txt
backgroundAttachment = "background-attachment"

bgAttachment :: Txt
bgAttachment = backgroundAttachment

backgroundPosition :: Txt
backgroundPosition = "background-position"

bgPosition :: Txt
bgPosition = bgPosition

backgroundSize :: Txt
backgroundSize = "background-size"

cover :: Txt
cover = "cover"

bgSize :: Txt
bgSize = backgroundSize

backgroundOrigin :: Txt
backgroundOrigin = "background-origin"

bgOrigin :: Txt
bgOrigin = backgroundOrigin

backgroundClip :: Txt
backgroundClip = "background-clip"

bgClip :: Txt
bgClip = backgroundClip

imageFilter :: Txt
imageFilter = "filter"

cursor :: Txt
cursor = "cursor"

pointer :: Txt
pointer = "pointer"

alias :: Txt
alias = "alias"

allScroll :: Txt
allScroll = "all-scroll"

cell :: Txt
cell = "cell"

contextMenu :: Txt
contextMenu = "context-menu"

colResize :: Txt
colResize = "col-resize"

copy :: Txt
copy = "copy"

crosshair :: Txt
crosshair = "crosshair"

eResize :: Txt
eResize = "e-resize"

ewResize :: Txt
ewResize = "ew-resize"

grab :: Txt
grab = "grab"

grabbing :: Txt
grabbing = "grabbing"

help :: Txt
help = "help"

move :: Txt
move = "move"

nResize :: Txt
nResize = "n-resize"

neResize :: Txt
neResize = "ne-resize"

neswResize :: Txt
neswResize = "nesw-resize"

nsResize :: Txt
nsResize = "ns-resize"

nwResize :: Txt
nwResize = "nw-resize"

nwseResize :: Txt
nwseResize = "nwse-resize"

noDrop :: Txt
noDrop = "no-drop"

notAllowed :: Txt
notAllowed = "not-allowed"

rowResize :: Txt
rowResize = "row-resize"

sResize :: Txt
sResize = "s-resize"

seResize :: Txt
seResize = "se-resize"

swResize :: Txt
swResize = "sw-resize"

vText :: Txt
vText = "vertical-text"

wResize :: Txt
wResize = "w-resize"

waitS :: Txt
waitS = "wait"

zoomIn :: Txt
zoomIn = "zoom-in"

zoomOut :: Txt
zoomOut = "zoom-out"

navDown :: Txt
navDown = "nav-down"

navIndex :: Txt
navIndex = "nav-index"

navLeft :: Txt
navLeft = "nav-left"

navRight :: Txt
navRight = "nav-right"

navUp :: Txt
navUp = "nav-up"

matrix :: Double -> Double -> Double -> Double -> Int -> Int -> Txt
matrix scX skY skX scY tX tY =
  "matrix(" <> dec scX <> ","
            <> dec skY <> ","
            <> dec skX <> ","
            <> dec scY <> ","
            <> int tX  <> ","
            <> int tY  <> ")"

scale :: Double -> Double -> Txt
scale opts1 opts2 = "scale(" <> dec opts1 <> "," <> dec opts2 <> ")"

scaleX :: Double -> Txt
scaleX opts1 = "scaleX(" <> dec opts1 <> ")"

scaleY :: Double -> Txt
scaleY opts1 = "scaleY(" <> dec opts1 <> ")"

scaleZ :: Double -> Txt
scaleZ opts1 = "scaleZ(" <> dec opts1 <> ")"

scale3d :: Double -> Double -> Txt
scale3d opts1 opts2 = "scale3d(" <> dec opts1 <> "," <> dec opts2 <> ")"

rotate :: Double -> Txt
rotate opts = "rotate(" <> deg opts <> ")"

translate :: Double -> Double -> Txt
translate opts1 opts2 = "translate(" <> per opts1 <> "," <> per opts2 <> ")"

translateX :: Double -> Txt
translateX opts1 = "translateX(" <> per opts1 <> ")"

translateY :: Double -> Txt
translateY opts1 = "translateY(" <> per opts1 <> ")"

translateZ :: Double -> Txt
translateZ opts1 = "translateZ(" <> per opts1 <> ")"

translate3d :: Double -> Double -> Double -> Txt
translate3d opts1 opts2 opts3 =
  "translate3d(" <> per opts1 <> ","
                 <> per opts2 <> ","
                 <> per opts3 <> ")"

matrix3d :: Double -> Double -> Double -> Double
         -> Double -> Double -> Double -> Double
         -> Double -> Double -> Double -> Double
         -> Double -> Double -> Double -> Double
         -> Txt
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

skew :: Double -> Double -> Txt
skew opts1 opts2 = "skew(" <> deg opts1 <> "," <> deg opts2 <> ")"

skewX :: Double -> Txt
skewX opts1 = "skewX(" <> deg opts1 <> ")"

skewY :: Double -> Txt
skewY opts1 = "skewY(" <> deg opts1 <> ")"

rotateX :: Double -> Txt
rotateX opts = "rotateX(" <> deg opts <> ")"

rotateY :: Double -> Txt
rotateY opts = "rotateY(" <> deg opts <> ")"

rotateZ :: Double -> Txt
rotateZ opts = "rotateZ(" <> deg opts <> ")"

rotate3d :: Double -> Double -> Txt
rotate3d opts1 opts2 = "rotate3d(" <> deg opts1 <> "," <> deg opts2 <> ")"

perspective :: Txt
perspective = "perspective"

perspectiveOrigin :: Txt
perspectiveOrigin = "perspective-origin"

backfaceVisibility :: Txt
backfaceVisibility = "backface-visibility"

transition :: Txt
transition = "transition"

transitionDelay :: Txt
transitionDelay = "transition-delay"

transitionDuration :: Txt
transitionDuration = "transition-duration"

transitionProperty :: Txt
transitionProperty = "transition-property"

transitionTimingFunction :: Txt
transitionTimingFunction = "transition-timing-function"

linear :: Txt
linear = "linear"

linearGradientS :: Txt -> Txt
linearGradientS lg = "linear-gradient(" <> lg <> ")"

transform :: Txt
transform = "transform"

transformStyle :: Txt
transformStyle = "transform-style"

transformOrigin :: Txt
transformOrigin = "transform-origin"

easeS :: Txt
easeS = "ease"

easeIn :: Txt
easeIn = "ease-in"

easeOut :: Txt
easeOut = "ease-out"

easeInOut :: Txt
easeInOut = "ease-in-out"

cubicBezier :: (Double,Double,Double,Double) -> Txt
cubicBezier (n1,n2,n3,n4) =
  "cubic-bezier(" <> (Txt.intercalate "," $ map toTxt [n1,n2,n3,n4]) <> ")"

visibility :: Txt
visibility = "visibility"

all :: Txt
all = "all"

direction :: Txt
direction = "direction"

rtl :: Txt
rtl = "rtl"

ltr :: Txt
ltr = "ltr"

alpha :: Txt -> Txt
alpha alph = "alpha(" <> alph <> ")"

opacity :: Txt
opacity = "opacity"

transparent :: Txt
transparent = "transparent"

hex :: Txt -> Txt
hex rgb = "#" <> rgb
  -- "#" <> (pack $ showHex rgb []) -- doesn't work with leading zeros

rgb :: (Int,Int,Int) -> Txt
rgb (r,g,b) = "rgb(" <> (Txt.intercalate "," [int r,int g,int b]) <> ")"

rgba :: (Int,Int,Int,Double) -> Txt
rgba (r,g,b,a) = "rgba(" <> (Txt.intercalate "," $ [int r,int g,int b,dec a]) <> ")"

hsl :: (Int,Double,Double) -> Txt
hsl (h,s,l) = "hsl(" <> (Txt.intercalate "," [int h,per s,per l]) <> ")"

hsla :: (Int,Double,Double,Double) -> Txt
hsla (h,s,l,a) = "hsla(" <> (Txt.intercalate "," [int h,per s, per l,dec a]) <> ")"

aqua :: Txt
aqua = "aqua"

aquamarine :: Txt
aquamarine = "aquamarine"

azure :: Txt
azure = "azure"

beige :: Txt
beige = "beige"

bisque :: Txt
bisque = "bisque"

black :: Txt
black = "black"

blanchedalmond :: Txt
blanchedalmond = "blanchedalmond"

blue :: Txt
blue = "blue"

blueviolet :: Txt
blueviolet = "blueviolet"

brown :: Txt
brown = "brown"

burlywood :: Txt
burlywood = "burlywood"

cadetblue :: Txt
cadetblue = "cadetblue"

chartreuse :: Txt
chartreuse = "chartreuse"

chocolate :: Txt
chocolate = "chocolate"

coral :: Txt
coral = "coral"

cornflowerblue :: Txt
cornflowerblue = "cornflowerblue"

cornsilk :: Txt
cornsilk = "cornsilk"

crimson :: Txt
crimson = "crimson"

cyan :: Txt
cyan = "cyan"

darkblue :: Txt
darkblue = "darkblue"

darkcyan :: Txt
darkcyan = "darkcyan"

darkgoldenrod :: Txt
darkgoldenrod = "darkgoldenrod"

darkgray :: Txt
darkgray = "darkgray"

darkgreen :: Txt
darkgreen = "darkgreen"

darkgrey :: Txt
darkgrey = "darkgrey"

darkkhaki :: Txt
darkkhaki = "darkkhaki"

darkmagenta :: Txt
darkmagenta = "darkmagenta"

darkolivegreen :: Txt
darkolivegreen = "darkolivegreen"

darkorange :: Txt
darkorange = "darkorange"

darkorchid :: Txt
darkorchid = "darkorchid"

darkred :: Txt
darkred = "darkred"

darksalmon :: Txt
darksalmon = "darksalmon"

darkseagreen :: Txt
darkseagreen = "darkseagreen"

darkslateblue :: Txt
darkslateblue = "darkslateblue"

darkslategray :: Txt
darkslategray = "darkslategray"

darkslategrey :: Txt
darkslategrey = "darkslategrey"

darkturquoise :: Txt
darkturquoise = "darkturquoise"

darkviolet :: Txt
darkviolet = "darkviolet"

deeppink :: Txt
deeppink = "deeppink"

deepskyblue :: Txt
deepskyblue = "deepskyblue"

dimgray :: Txt
dimgray = "dimgray"

dimgrey :: Txt
dimgrey = "dimgrey"

dodgerblue :: Txt
dodgerblue = "dodgerblue"

firebrick :: Txt
firebrick = "firebrick"

floralwhite :: Txt
floralwhite = "floralwhite"

forestgreen :: Txt
forestgreen = "forestgreen"

fuchsia :: Txt
fuchsia = "fuchsia"

gainsboro :: Txt
gainsboro = "gainsboro"

ghostwhite :: Txt
ghostwhite = "ghostwhite"

gold :: Txt
gold = "gold"

goldenrod :: Txt
goldenrod = "goldenrod"

gray :: Txt
gray = "gray"

green :: Txt
green = "green"

greenyellow :: Txt
greenyellow = "greenyellow"

grey :: Txt
grey = "grey"

honeydew :: Txt
honeydew = "honeydew"

hotpink :: Txt
hotpink = "hotpink"

indianred :: Txt
indianred = "indianred"

indigo :: Txt
indigo = "indigo"

ivory :: Txt
ivory = "ivory"

khaki :: Txt
khaki = "khaki"

lavender :: Txt
lavender = "lavender"

lavenderblush :: Txt
lavenderblush = "lavenderblush"

lawngreen :: Txt
lawngreen = "lawngreen"

lemonchiffon :: Txt
lemonchiffon = "lemonchiffon"

lightblue :: Txt
lightblue = "lightblue"

lightcoral :: Txt
lightcoral = "lightcoral"

lightcyan :: Txt
lightcyan = "lightcyan"

lightgoldenrodyellow :: Txt
lightgoldenrodyellow = "lightgoldenrodyellow"

lightgray :: Txt
lightgray = "lightgray"

lightgreen :: Txt
lightgreen = "lightgreen"

lightgrey :: Txt
lightgrey = "lightgrey"

lightpink :: Txt
lightpink = "lightpink"

lightsalmon :: Txt
lightsalmon = "lightsalmon"

lightseagreen :: Txt
lightseagreen = "lightseagreen"

lightskyblue :: Txt
lightskyblue = "lightskyblue"

lightslategray :: Txt
lightslategray = "lightslategray"

lightslategrey :: Txt
lightslategrey = "lightslategrey"

lightsteelblue :: Txt
lightsteelblue = "lightsteelblue"

lightyellow :: Txt
lightyellow = "lightyellow"

lime :: Txt
lime = "lime"

limegreen :: Txt
limegreen = "limegreen"

linen :: Txt
linen = "linen"

magenta :: Txt
magenta = "magenta"

maroon :: Txt
maroon = "maroon"

mediumaquamarine :: Txt
mediumaquamarine = "mediumaquamarine"

mediumblue :: Txt
mediumblue = "mediumblue"

mediumorchid :: Txt
mediumorchid = "mediumorchid"

mediumpurple :: Txt
mediumpurple = "mediumpurple"

mediumseagreen :: Txt
mediumseagreen = "mediumseagreen"

mediumslateblue :: Txt
mediumslateblue = "mediumslateblue"

mediumspringgreen :: Txt
mediumspringgreen = "mediumspringgreen"

mediumturquoise :: Txt
mediumturquoise = "mediumturquoise"

mediumvioletred :: Txt
mediumvioletred = "mediumvioletred"

midnightblue :: Txt
midnightblue = "midnightblue"

mintcream :: Txt
mintcream = "mintcream"

mistyrose :: Txt
mistyrose = "mistyrose"

moccasin :: Txt
moccasin = "moccasin"

navajowhite :: Txt
navajowhite = "navajowhite"

navy :: Txt
navy = "navy"

oldlace :: Txt
oldlace = "oldlace"

olive :: Txt
olive = "olive"

olivedrab :: Txt
olivedrab = "olivedrab"

orange :: Txt
orange = "orange"

orangered :: Txt
orangered = "orangered"

orchid :: Txt
orchid = "orchid"

palegoldenrod :: Txt
palegoldenrod = "palegoldenrod"

palegreen :: Txt
palegreen = "palegreen"

paleturquoise :: Txt
paleturquoise = "paleturquoise"

palevioletred :: Txt
palevioletred = "palevioletred"

papayawhip :: Txt
papayawhip = "papayawhip"

peachpuff :: Txt
peachpuff = "peachpuff"

peru :: Txt
peru = "peru"

pink :: Txt
pink = "pink"

plum :: Txt
plum = "plum"

powderblue :: Txt
powderblue = "powderblue"

purple :: Txt
purple = "purple"

red :: Txt
red = "red"

rosybrown :: Txt
rosybrown = "rosybrown"

royalblue :: Txt
royalblue = "royalblue"

saddlebrown :: Txt
saddlebrown = "saddlebrown"

salmon :: Txt
salmon = "salmon"

sandybrown :: Txt
sandybrown = "sandybrown"

seagreen :: Txt
seagreen = "seagreen"

seashell :: Txt
seashell = "seashell"

sienna :: Txt
sienna = "sienna"

silver :: Txt
silver = "silver"

skyblue :: Txt
skyblue = "skyblue"

slateblue :: Txt
slateblue = "slateblue"

slategray :: Txt
slategray = "slategray"

slategrey :: Txt
slategrey = "slategrey"

snow :: Txt
snow = "snow"

springgreen :: Txt
springgreen = "springgreen"

steelblue :: Txt
steelblue = "steelblue"

tan :: Txt
tan = "tan"

teal :: Txt
teal = "teal"

thistle :: Txt
thistle = "thistle"

tomato :: Txt
tomato = "tomato"

turquoise :: Txt
turquoise = "turquoise"

violet :: Txt
violet = "violet"

wheat :: Txt
wheat = "wheat"

white :: Txt
white = "white"

whitesmoke :: Txt
whitesmoke = "whitesmoke"

yellow :: Txt
yellow = "yellow"

yellowgree :: Txt
yellowgree = "yellowgree"

alignSelf :: Txt
alignSelf = "align-self"

webkitJustifyContent :: Txt
webkitJustifyContent = "-webkit-justify-content"

msFlexPack :: Txt
msFlexPack = "-ms-flex-pack"

webkitBoxPack :: Txt
webkitBoxPack = "-webkit-box-pack"

justifyContent :: Txt
justifyContent = "justify-content"

webkitAlignItems :: Txt
webkitAlignItems = "-webkit-align-items"

msFlexAlign :: Txt
msFlexAlign = "-ms-flex-align"

webkitBoxAlign :: Txt
webkitBoxAlign = "-webkit-box-aign"

alignItems :: Txt
alignItems = "align-items"

flexStart :: Txt
flexStart = "flex-start"

flexEnd :: Txt
flexEnd = "flex-end"

endS :: Txt
endS = "end"

spaceAround :: Txt
spaceAround = "space-around"

spaceBetween :: Txt
spaceBetween = "space-between"

distribute :: Txt
distribute = "distribute"

justify :: Txt
justify = "justify"

webkitOrder :: Txt
webkitOrder = "-webkit-order"

msFlexOrder :: Txt
msFlexOrder = "-ms-flex-order"

webkitBoxOrdinalGroup :: Txt
webkitBoxOrdinalGroup = "-webkit-box-ordinal-group"

order :: Txt
order = "order"

msFlexPreferredSize :: Txt
msFlexPreferredSize = "-ms-flex-preferred-size"

webkitFlexBasis :: Txt
webkitFlexBasis = "-webkit-flex-basis"

flexBasis :: Txt
flexBasis = "flex-basis"

webkitFlexGrow :: Txt
webkitFlexGrow = "-webkit-flex-grow"

msFlexPositive :: Txt
msFlexPositive = "-ms-flex-positive"

webkitBoxFlex :: Txt
webkitBoxFlex = "-webkit-box-flex"

flexGrow :: Txt
flexGrow = "flex-grow"

webkitFlex :: Txt
webkitFlex = "-webkit-flex"

msFlex :: Txt
msFlex = "-ms-flex"

flex :: Txt
flex = "flex"

flexFlow :: Txt
flexFlow = "flex-flow"

webkitFlexDirection :: Txt
webkitFlexDirection = "-webkit-flex-direction"

msFlexDirection :: Txt
msFlexDirection = "-ms-flex-direction"

webkitBoxOrient :: Txt
webkitBoxOrient = "-webkit-box-orient"

webkitBoxDirection :: Txt
webkitBoxDirection = "-webkit-box-direction"

flexDirection :: Txt
flexDirection = "flex-direction"

columnReverse :: Txt
columnReverse = "column-reverse"

vertical :: Txt
vertical = "vertical"

reverse :: Txt
reverse = "reverse"

rowReverse :: Txt
rowReverse = "row-reverse"

horizontal :: Txt
horizontal = "horizontal"

webkitFlexWrap :: Txt
webkitFlexWrap = "-webkit-flex-wrap"

msFlexWrap :: Txt
msFlexWrap = "-ms-flex-wrap"

flexWrap :: Txt
flexWrap = "flex-wrap"

msFlexBox :: Txt
msFlexBox = "-ms-flexbox"

webkitBox :: Txt
webkitBox = "-webkit-box"

rowS :: Txt
rowS = "row"

column :: Txt
column = "column"

xmlns :: Txt
xmlns = "xmlns"

accentHeight :: Txt
accentHeight = "accentHeight"

accumulate :: Txt
accumulate = "accumulate"


additive :: Txt
additive = "additive"

alignmentBaseline :: Txt
alignmentBaseline = "alignment-baseline"

allowReorder :: Txt
allowReorder = "allowReorder"

alphabetic :: Txt
alphabetic = "alphabetic"

amplitude :: Txt
amplitude = "amplitude"

arabicForm :: Txt
arabicForm = "arabic-form"

ascent :: Txt
ascent = "ascent"

attributeName :: Txt
attributeName = "attributeName"

attributeType :: Txt
attributeType = "attributeType"

autoReverse :: Txt
autoReverse = "autoReverse"

azimuth :: Txt
azimuth = "azimuth"

baseFrequency :: Txt
baseFrequency = "baseFrequency"

baselineShift :: Txt
baselineShift = "baseline-shift"

baseProfile :: Txt
baseProfile = "baseProfile"

bbox :: Txt
bbox = "bbox"

begin :: Txt
begin = "begin"

bias :: Txt
bias = "bias"

by :: Txt
by = "by"

calcMode :: Txt
calcMode = "calcMode"

capHeight :: Txt
capHeight = "cap-height"

clipPathUnits :: Txt
clipPathUnits = "clipPathUnits"

clipRule :: Txt
clipRule = "clip-rule"

colorInterpolation :: Txt
colorInterpolation = "color-interpolation"

colorInterpolationFilters :: Txt
colorInterpolationFilters = "color-interpolation-filters"

colorRendering :: Txt
colorRendering = "color-rendering"

contentScriptType :: Txt
contentScriptType = "contentScriptType"

contentStyleType :: Txt
contentStyleType = "contentStyleType"

cx :: Txt
cx = "cx"

cy :: Txt
cy = "cy"

dS :: Txt
dS = "d"

decelerate :: Txt
decelerate = "decelerate"

descent :: Txt
descent = "descent"

diffuseConstant :: Txt
diffuseConstant = "diffuseConstant"

divisor :: Txt
divisor = "divisor"

dominantBaseline :: Txt
dominantBaseline = "dominant-baseline"

dur :: Txt
dur = "dur"

dx :: Txt
dx = "dx"

dy :: Txt
dy = "dy"

edgeMode :: Txt
edgeMode = "edgeMode"

elevation :: Txt
elevation = "elevation"

enableBackground :: Txt
enableBackground = "enable-background"

exponent :: Txt
exponent = "exponent"

externalResourcesRequired :: Txt
externalResourcesRequired = "externalResourcesRequired"

fill :: Txt
fill = "fill"

fillOpacity :: Txt
fillOpacity = "fill-opacity"

fillRule :: Txt
fillRule = "fill-rule"

filterRes :: Txt
filterRes = "filterRes"

filterUnits :: Txt
filterUnits = "filterUnits"

floodColor :: Txt
floodColor = "flood-color"

floodOpacity :: Txt
floodOpacity = "flood-opacity"

fontSizeAdjust :: Txt
fontSizeAdjust = "font-size-adjust"

fontStretch :: Txt
fontStretch = "font-stretch"

fontStyle :: Txt
fontStyle = "font-style"

fontVariant :: Txt
fontVariant = "font-variant"

format :: Txt -> Txt
format f = "format('" <> f <> "')"

fx :: Txt
fx = "fx"

fy :: Txt
fy = "fy"

g1 :: Txt
g1 = "g1"

g2 :: Txt
g2 = "g2"

glyphName :: Txt
glyphName = "glyph-name"

glyphOrientationHorizontal :: Txt
glyphOrientationHorizontal = "glyph-orientation-horizontal"

glyphOrientationVertical :: Txt
glyphOrientationVertical = "glyph-orientation-vertical"

gradientTransform :: Txt
gradientTransform = "gradientTransform"

gradientUnits :: Txt
gradientUnits = "gradientUnits"

hanging :: Txt
hanging = "hanging"

horizAdvX :: Txt
horizAdvX = "horiz-adv-x"

horizOriginX :: Txt
horizOriginX = "horiz-origin-x"

ideographic :: Txt
ideographic = "ideographic"

imageRendering :: Txt
imageRendering = "image-rendering"

inS :: Txt
inS = "in"

in2 :: Txt
in2 = "in2"

intercept :: Txt
intercept = "intercept"

k :: Txt
k = "k"

k1 :: Txt
k1 = "k1"

k2 :: Txt
k2 = "k2"

k3 :: Txt
k3 = "k3"

k4 :: Txt
k4 = "k4"

kernelMatrix :: Txt
kernelMatrix = "kernelMatrix"

kernelUnitLength :: Txt
kernelUnitLength = "kernelUnitLength"

kerning :: Txt
kerning = "kerning"

keyPoints :: Txt
keyPoints = "keyPoints"

keySplines :: Txt
keySplines = "keySplines"

keyTimes :: Txt
keyTimes = "keyTimes"

lengthAdjust :: Txt
lengthAdjust = "lengthAdjust"

letterSpacing :: Txt
letterSpacing = "letter-spacing"

lightingColor :: Txt
lightingColor = "lighting-color"

limitingConeAngle :: Txt
limitingConeAngle = "limitingConeAngle"

markerEnd :: Txt
markerEnd = "marker-end"

markerMid :: Txt
markerMid = "marker-mid"

markerStart :: Txt
markerStart = "marker-start"

markerHeight :: Txt
markerHeight = "markerHeight"

markerUnits :: Txt
markerUnits = "markerUnits"

markerWidth :: Txt
markerWidth = "markerWidth"

maskContentUnits :: Txt
maskContentUnits = "maskContentUnits"

maskUnits :: Txt
maskUnits = "maskUnits"

mathematical :: Txt
mathematical = "mathematical"

mode :: Txt
mode = "mode"

nameS :: Txt
nameS = "name"

numOctaves :: Txt
numOctaves = "numOctaves"

offset :: Txt
offset = "offset"

onabort :: Txt
onabort = "onabort"

onactivate :: Txt
onactivate = "onactivate"

onbegin :: Txt
onbegin = "onbegin"

onclick :: Txt
onclick = "onclick"

onend :: Txt
onend = "onend"

onerror :: Txt
onerror = "onerror"

onfocusin :: Txt
onfocusin = "onfocusin"

onfocusout :: Txt
onfocusout = "onfocusout"

onload :: Txt
onload = "onload"

onmousedown :: Txt
onmousedown = "onmousedown"

onmousemove :: Txt
onmousemove = "onmousemove"

onmouseout :: Txt
onmouseout = "onmouseout"

onmouseover :: Txt
onmouseover = "onmouseover"

onmouseup :: Txt
onmouseup = "onmouseup"

onrepeat :: Txt
onrepeat = "onrepeat"

onresize :: Txt
onresize = "onresize"

onscroll :: Txt
onscroll = "onscroll"

onunload :: Txt
onunload = "onunload"

onzoom :: Txt
onzoom = "onzoom"

operator :: Txt
operator = "operator"

orient :: Txt
orient = "orient"

orientation :: Txt
orientation = "orientation"

origin :: Txt
origin = "origin"

overlinePosition :: Txt
overlinePosition = "overline-position"

overlineThickness :: Txt
overlineThickness = "overline-thickness"

panose1 :: Txt
panose1 = "panose-1"

paintOrder :: Txt
paintOrder = "paint-order"

pathLength :: Txt
pathLength = "pathLength"

patternContentUnits :: Txt
patternContentUnits = "patternContentUnits"

patternTransform :: Txt
patternTransform = "patternTransform"

patternUnits :: Txt
patternUnits = "patternUnits"

points :: Txt
points = "points"

pointsAtX :: Txt
pointsAtX = "pointsAtX"

pointsAtY :: Txt
pointsAtY = "pointsAtY"

pointsAtZ :: Txt
pointsAtZ = "pointsAtZ"

preserveAlpha :: Txt
preserveAlpha = "preserveAlpha"

preserveAspectRatio :: Txt
preserveAspectRatio = "preserveAspectRatio"

primitiveUnits :: Txt
primitiveUnits = "primitiveUnits"

r :: Txt
r = "r"

radius :: Txt
radius = "radius"

refX :: Txt
refX = "refX"

refY :: Txt
refY = "refY"

renderingIntent :: Txt
renderingIntent = "rendering-intent"

counterIncrement :: Txt
counterIncrement = "counter-increment"

counterReset :: Txt
counterReset = "counter-reset"

repeatCount :: Txt
repeatCount = "repeatCount"

repeatDur :: Txt
repeatDur = "repeatDur"

requiredExtensions :: Txt
requiredExtensions = "requiredExtensions"

requiredFeatures :: Txt
requiredFeatures = "requiredFeatures"

restart :: Txt
restart = "restart"

result :: Txt
result = "result"

rx :: Txt
rx = "rx"

ry :: Txt
ry = "ry"

rS :: Txt
rS = "r"

seed :: Txt
seed = "seed"

shapeRendering :: Txt
shapeRendering = "shape-rendering"

slope :: Txt
slope = "slope"

spacing :: Txt
spacing = "spacing"

specularConstant :: Txt
specularConstant = "specularConstant"

specularExponent :: Txt
specularExponent = "specularExponent"

speed :: Txt
speed = "speed"

spreadMethod :: Txt
spreadMethod = "spreadMethod"

startOffset :: Txt
startOffset = "startOffset"

stdDeviation :: Txt
stdDeviation = "stdDeviation"

stemh :: Txt
stemh = "stemh"

stemv :: Txt
stemv = "stemv"

stitchTiles :: Txt
stitchTiles = "stitchTiles"

stopColor :: Txt
stopColor = "stop-color"

stopOpacity :: Txt
stopOpacity = "stop-opacity"

strikethroughPosition :: Txt
strikethroughPosition = "strikethrough-position"

strikethroughThickness :: Txt
strikethroughThickness = "strikethrough-thickness"

stringS :: Txt
stringS = "string"

stroke :: Txt
stroke = "stroke"

strokeDasharray :: Txt
strokeDasharray = "stroke-dasharray"

strokeDashoffset :: Txt
strokeDashoffset = "stroke-dashoffset"

strokeLinecap :: Txt

strokeLinecap = "stroke-linecap"

roundS :: Txt
roundS = "round"

butt :: Txt
butt = "butt"

strokeLinejoin :: Txt
strokeLinejoin = "stroke-linejoin"

strokeMiterlimit :: Txt
strokeMiterlimit = "stroke-miterlimit"

strokeOpacity :: Txt
strokeOpacity = "stroke-opacity"

strokeWidth :: Txt
strokeWidth = "stroke-width"

surfaceScale :: Txt
surfaceScale = "surfaceScale"

systemLanguage :: Txt
systemLanguage = "systemLanguage"

tableValues :: Txt
tableValues = "tableValues"

targetX :: Txt
targetX = "targetX"

targetY :: Txt
targetY = "targetY"

textAnchor :: Txt
textAnchor = "text-anchor"

textLength :: Txt
textLength = "textLength"

u1 :: Txt
u1 = "u1"

u2 :: Txt
u2 = "u2"

underlinePosition :: Txt
underlinePosition = "underline-position"

underlineThickness :: Txt
underlineThickness = "underline-thickness"

unicode :: Txt
unicode = "unicode"

unicodeRange :: Txt
unicodeRange = "unicode-range"

unitsPerEm :: Txt
unitsPerEm = "units-per-em"

vAlphabetic :: Txt
vAlphabetic = "v-alphabetic"

vHanging :: Txt
vHanging = "v-hanging"

vIdeographic :: Txt
vIdeographic = "v-ideographic"

vMathematical :: Txt
vMathematical = "v-mathematical"

values :: Txt
values = "values"

version :: Txt
version = "version"

vertAdvY :: Txt
vertAdvY = "vert-adv-y"

vertOriginX :: Txt
vertOriginX = "vert-origin-x"

vertOriginY :: Txt
vertOriginY = "vert-origin-y"

viewBox :: Txt
viewBox = "viewBox"

viewTarget :: Txt
viewTarget = "viewTarget"

widths :: Txt
widths = "widths"

writingMode :: Txt
writingMode = "writing-mode"

xS :: Txt
xS = "x"

xHeight :: Txt
xHeight = "x-height"

x1 :: Txt
x1 = "x1"

x2 :: Txt
x2 = "x2"

xChannelSelector :: Txt
xChannelSelector = "xChannelSelector"

yS :: Txt
yS = "y"

y1 :: Txt
y1 = "y1"

y2 :: Txt
y2 = "y2"

yChannelSelector :: Txt
yChannelSelector = "yChannelSelector"

zS :: Txt
zS = "z"

zoomAndPan :: Txt
zoomAndPan = "zoomAndPan"
