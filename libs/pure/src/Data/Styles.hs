{-# language OverloadedStrings, ViewPatterns, PatternSynonyms, FlexibleInstances #-}
module Data.Styles where

import Data.Txt as Txt
import Data.View hiding (zoom)
import Data.Int

import Data.List as List
import GHC.Exts (IsString(..),IsList(..))
import Numeric (showHex)

pattern Color :: HasFeatures a => Txt -> a -> a
pattern Color v a = Style "color" v a

pattern Bgcolor :: HasFeatures a => Txt -> a -> a
pattern Bgcolor v a = Style "bgcolor" v a

pattern Border :: HasFeatures a => Txt -> a -> a
pattern Border v a = Style "border" v a

pattern Sizes :: HasFeatures a => Txt -> a -> a
pattern Sizes v a = Style "sizes" v a

pattern Srcset :: HasFeatures a => Txt -> a -> a
pattern Srcset v a = Style "srcset" v a

pattern Subject :: HasFeatures a => Txt -> a -> a
pattern Subject v a = Style "subject" v a

pattern Valign :: HasFeatures a => Txt -> a -> a
pattern Valign v a = Style "valign" v a

pattern Content :: HasFeatures a => Txt -> a -> a
pattern Content v a = Style "content" v a

pattern TouchAction :: HasFeatures a => Txt -> a -> a
pattern TouchAction v a = Style "touch-action" v a

pattern PointerEvents :: HasFeatures a => Txt -> a -> a
pattern PointerEvents v a = Style "pointer-events" v a

pattern Margin :: HasFeatures a => Txt -> a -> a
pattern Margin v a = Style "margin" v a

pattern MarginLeft :: HasFeatures a => Txt -> a -> a
pattern MarginLeft v a = Style "margin-left" v a

pattern MarginRight :: HasFeatures a => Txt -> a -> a
pattern MarginRight v a = Style "margin-right" v a

pattern MarginTop :: HasFeatures a => Txt -> a -> a
pattern MarginTop v a = Style "margin-top" v a

pattern MarginBottom :: HasFeatures a => Txt -> a -> a
pattern MarginBottom v a = Style "margin-bottom" v a

pattern Padding :: HasFeatures a => Txt -> a -> a
pattern Padding v a = Style "padding" v a

pattern PaddingLeft :: HasFeatures a => Txt -> a -> a
pattern PaddingLeft v a = Style "padding-left" v a

pattern PaddingRight :: HasFeatures a => Txt -> a -> a
pattern PaddingRight v a = Style "padding-right" v a

pattern PaddingTop :: HasFeatures a => Txt -> a -> a
pattern PaddingTop v a = Style "padding-top" v a

pattern PaddingBottom :: HasFeatures a => Txt -> a -> a
pattern PaddingBottom v a = Style "padding-bottom" v a

pattern BorderImage :: HasFeatures a => Txt -> a -> a
pattern BorderImage v a = Style "border-image" v a

pattern BorderImageSource :: HasFeatures a => Txt -> a -> a
pattern BorderImageSource v a = Style "border-image-source" v a

pattern BorderImageSlice :: HasFeatures a => Txt -> a -> a
pattern BorderImageSlice v a = Style "border-image-slice" v a

pattern BorderImageWidth :: HasFeatures a => Txt -> a -> a
pattern BorderImageWidth v a = Style "border-image-width" v a

pattern BorderImageOutset :: HasFeatures a => Txt -> a -> a
pattern BorderImageOutset v a = Style "border-image-outset" v a

pattern BorderImageRepeat :: HasFeatures a => Txt -> a -> a
pattern BorderImageRepeat v a = Style "border-image-repeat" v a

pattern Outline :: HasFeatures a => Txt -> a -> a
pattern Outline v a = Style "outline" v a

pattern OutlineStyle :: HasFeatures a => Txt -> a -> a
pattern OutlineStyle v a = Style "outline-style" v a

pattern OutlineWidth :: HasFeatures a => Txt -> a -> a
pattern OutlineWidth v a = Style "outline-width" v a

pattern OutlineColor :: HasFeatures a => Txt -> a -> a
pattern OutlineColor v a = Style "outline-color" v a

pattern OutlineOffset :: HasFeatures a => Txt -> a -> a
pattern OutlineOffset v a = Style "outline-offset" v a

pattern BorderTop :: HasFeatures a => Txt -> a -> a
pattern BorderTop v a = Style "border-top" v a

pattern BorderTopImage :: HasFeatures a => Txt -> a -> a
pattern BorderTopImage v a = Style "border-top-image" v a

pattern BorderRight :: HasFeatures a => Txt -> a -> a
pattern BorderRight v a = Style "border-right" v a

pattern BorderRightImage :: HasFeatures a => Txt -> a -> a
pattern BorderRightImage v a = Style "border-right-image" v a

pattern BorderBottom :: HasFeatures a => Txt -> a -> a
pattern BorderBottom v a = Style "border-bottom" v a

pattern BorderBottomImage :: HasFeatures a => Txt -> a -> a
pattern BorderBottomImage v a = Style "border-bottom-image" v a

pattern BorderLeft :: HasFeatures a => Txt -> a -> a
pattern BorderLeft v a = Style "border-left" v a

pattern BorderLeftImage :: HasFeatures a => Txt -> a -> a
pattern BorderLeftImage v a = Style "border-left-image" v a

pattern BorderRadius :: HasFeatures a => Txt -> a -> a
pattern BorderRadius v a = Style "border-radius" v a

pattern BorderTopRightRadius :: HasFeatures a => Txt -> a -> a
pattern BorderTopRightRadius v a = Style "border-top-right-radius" v a

pattern BorderBottomRightRadius :: HasFeatures a => Txt -> a -> a
pattern BorderBottomRightRadius v a = Style "border-bottom-right-radius" v a

pattern BorderBottomLeftRadius :: HasFeatures a => Txt -> a -> a
pattern BorderBottomLeftRadius v a = Style "border-bottom-left-radius" v a

pattern BorderTopLeftRadius :: HasFeatures a => Txt -> a -> a
pattern BorderTopLeftRadius v a = Style "border-top-left-radius" v a

pattern BorderStyle :: HasFeatures a => Txt -> a -> a
pattern BorderStyle v a = Style "border-style" v a

pattern BorderTopStyle :: HasFeatures a => Txt -> a -> a
pattern BorderTopStyle v a = Style "border-top-style" v a

pattern BorderRightStyle :: HasFeatures a => Txt -> a -> a
pattern BorderRightStyle v a = Style "border-right-style" v a

pattern BorderBottomStyle :: HasFeatures a => Txt -> a -> a
pattern BorderBottomStyle v a = Style "border-bottom-style" v a

pattern BorderLeftStyle :: HasFeatures a => Txt -> a -> a
pattern BorderLeftStyle v a = Style "border-left-style" v a

pattern BorderWidth :: HasFeatures a => Txt -> a -> a
pattern BorderWidth v a = Style "border-width" v a

pattern BorderTopWidth :: HasFeatures a => Txt -> a -> a
pattern BorderTopWidth v a = Style "border-top-width" v a

pattern BorderRightWidth :: HasFeatures a => Txt -> a -> a
pattern BorderRightWidth v a = Style "border-right-width" v a

pattern BorderBottomWidth :: HasFeatures a => Txt -> a -> a
pattern BorderBottomWidth v a = Style "border-bottom-width" v a

pattern BorderLeftWidth :: HasFeatures a => Txt -> a -> a
pattern BorderLeftWidth v a = Style "border-left-width" v a

pattern BorderColor :: HasFeatures a => Txt -> a -> a
pattern BorderColor v a = Style "border-color" v a

pattern BorderTopColor :: HasFeatures a => Txt -> a -> a
pattern BorderTopColor v a = Style "border-top-color" v a

pattern BorderRightColor :: HasFeatures a => Txt -> a -> a
pattern BorderRightColor v a = Style "border-right-color" v a

pattern BorderBottomColor :: HasFeatures a => Txt -> a -> a
pattern BorderBottomColor v a = Style "border-bottom-color" v a

pattern BorderLeftColor :: HasFeatures a => Txt -> a -> a
pattern BorderLeftColor v a = Style "border-left-color" v a

pattern BorderCollapse :: HasFeatures a => Txt -> a -> a
pattern BorderCollapse v a = Style "border-collapse" v a

pattern BorderSpacing :: HasFeatures a => Txt -> a -> a
pattern BorderSpacing v a = Style "border-spacing" v a

pattern ListStyle :: HasFeatures a => Txt -> a -> a
pattern ListStyle v a = Style "list-style" v a

pattern ListStyleType :: HasFeatures a => Txt -> a -> a
pattern ListStyleType v a = Style "list-style-type" v a

pattern ListStyleImage :: HasFeatures a => Txt -> a -> a
pattern ListStyleImage v a = Style "list-style-image" v a

pattern ListStylePosition :: HasFeatures a => Txt -> a -> a
pattern ListStylePosition v a = Style "list-style-position" v a

pattern ZIndex :: HasFeatures a => Txt -> a -> a
pattern ZIndex v a = Style "z-index" v a

pattern Display :: HasFeatures a => Txt -> a -> a
pattern Display v a = Style "display" v a

pattern Float :: HasFeatures a => Txt -> a -> a
pattern Float v a = Style "float" v a

pattern TableCaption :: HasFeatures a => Txt -> a -> a
pattern TableCaption v a = Style "table-caption" v a

pattern CaptionSide :: HasFeatures a => Txt -> a -> a
pattern CaptionSide v a = Style "caption-side" v a

pattern EmptyCells :: HasFeatures a => Txt -> a -> a
pattern EmptyCells v a = Style "empty-cells" v a

pattern TableLayout :: HasFeatures a => Txt -> a -> a
pattern TableLayout v a = Style "table-layout" v a

pattern Position :: HasFeatures a => Txt -> a -> a
pattern Position v a = Style "position" v a

pattern Top :: HasFeatures a => Txt -> a -> a
pattern Top v a = Style "top" v a

pattern Right :: HasFeatures a => Txt -> a -> a
pattern Right v a = Style "right" v a

pattern Bottom :: HasFeatures a => Txt -> a -> a
pattern Bottom v a = Style "bottom" v a

pattern Left :: HasFeatures a => Txt -> a -> a
pattern Left v a = Style "left" v a

pattern Overflow :: HasFeatures a => Txt -> a -> a
pattern Overflow v a = Style "overflow" v a

pattern Scroll :: HasFeatures a => Txt -> a -> a
pattern Scroll v a = Style "scroll" v a

pattern OverflowX :: HasFeatures a => Txt -> a -> a
pattern OverflowX v a = Style "overflow-x" v a

pattern OverflowY :: HasFeatures a => Txt -> a -> a
pattern OverflowY v a = Style "overflow-y" v a

pattern Clip :: HasFeatures a => Txt -> a -> a
pattern Clip v a = Style "clip" v a

pattern LineHeight :: HasFeatures a => Txt -> a -> a
pattern LineHeight v a = Style "line-height" v a

pattern Height :: HasFeatures a => Txt -> a -> a
pattern Height v a = Style "height" v a

pattern Width :: HasFeatures a => Txt -> a -> a
pattern Width v a = Style "width" v a

pattern MaxWidth :: HasFeatures a => Txt -> a -> a
pattern MaxWidth v a = Style "max-width" v a

pattern MinWidth :: HasFeatures a => Txt -> a -> a
pattern MinWidth v a = Style "min-width" v a

pattern MaxHeight :: HasFeatures a => Txt -> a -> a
pattern MaxHeight v a = Style "max-height" v a

pattern MinHeight :: HasFeatures a => Txt -> a -> a
pattern MinHeight v a = Style "min-height" v a

pattern BoxSizing :: HasFeatures a => Txt -> a -> a
pattern BoxSizing v a = Style "box-sizing" v a

pattern Font :: HasFeatures a => Txt -> a -> a
pattern Font v a = Style "font" v a

pattern FontFamily :: HasFeatures a => Txt -> a -> a
pattern FontFamily v a = Style "font-family" v a

pattern FontWeight :: HasFeatures a => Txt -> a -> a
pattern FontWeight v a = Style "font-weight" v a

pattern FontSize :: HasFeatures a => Txt -> a -> a
pattern FontSize v a = Style "font-size" v a

pattern BoxShadow :: HasFeatures a => Txt -> a -> a
pattern BoxShadow v a = Style "box-shadow" v a

pattern TextDecoration :: HasFeatures a => Txt -> a -> a
pattern TextDecoration v a = Style "text-decoration" v a

pattern TextDecorationColor :: HasFeatures a => Txt -> a -> a
pattern TextDecorationColor v a = Style "text-decoration-color" v a

pattern TextDecorationLine :: HasFeatures a => Txt -> a -> a
pattern TextDecorationLine v a = Style "text-decoration-line" v a

pattern TextDecorationStyle :: HasFeatures a => Txt -> a -> a
pattern TextDecorationStyle v a = Style "text-decoration-style" v a

pattern TextAlign :: HasFeatures a => Txt -> a -> a
pattern TextAlign v a = Style "text-align" v a

pattern VerticalAlign :: HasFeatures a => Txt -> a -> a
pattern VerticalAlign v a = Style "vertical-align" v a

pattern TextIndent :: HasFeatures a => Txt -> a -> a
pattern TextIndent v a = Style "text-indent" v a

pattern TextJustify :: HasFeatures a => Txt -> a -> a
pattern TextJustify v a = Style "text-justify" v a

pattern TextOverflow :: HasFeatures a => Txt -> a -> a
pattern TextOverflow v a = Style "text-overflow" v a

pattern TextShadow :: HasFeatures a => Txt -> a -> a
pattern TextShadow v a = Style "text-shadow" v a

pattern TextTransform :: HasFeatures a => Txt -> a -> a
pattern TextTransform v a = Style "text-transform" v a

pattern WhiteSpace :: HasFeatures a => Txt -> a -> a
pattern WhiteSpace v a = Style "white-space" v a

pattern Background :: HasFeatures a => Txt -> a -> a
pattern Background v a = Style "background" v a

pattern BackgroundColor :: HasFeatures a => Txt -> a -> a
pattern BackgroundColor v a = Style "background-color" v a

pattern BackgroundImage :: HasFeatures a => Txt -> a -> a
pattern BackgroundImage v a = Style "background-image" v a

pattern BackgroundRepeat :: HasFeatures a => Txt -> a -> a
pattern BackgroundRepeat v a = Style "background-repeat" v a

pattern BackgroundAttachment :: HasFeatures a => Txt -> a -> a
pattern BackgroundAttachment v a = Style "background-attachment" v a

pattern BackgroundPosition :: HasFeatures a => Txt -> a -> a
pattern BackgroundPosition v a = Style "background-position" v a

pattern BackgroundSize :: HasFeatures a => Txt -> a -> a
pattern BackgroundSize v a = Style "background-size" v a

pattern BackgroundOrigin :: HasFeatures a => Txt -> a -> a
pattern BackgroundOrigin v a = Style "background-origin" v a

pattern BackgroundClip :: HasFeatures a => Txt -> a -> a
pattern BackgroundClip v a = Style "background-clip" v a

pattern Cursor :: HasFeatures a => Txt -> a -> a
pattern Cursor v a = Style "cursor" v a

pattern Perspective :: HasFeatures a => Txt -> a -> a
pattern Perspective v a = Style "perspective" v a

pattern PerspectiveOrigin :: HasFeatures a => Txt -> a -> a
pattern PerspectiveOrigin v a = Style "perspective-origin" v a

pattern BackfaceVisibility :: HasFeatures a => Txt -> a -> a
pattern BackfaceVisibility v a = Style "backface-visibility" v a

pattern Transition :: HasFeatures a => Txt -> a -> a
pattern Transition v a = Style "transition" v a

pattern TransitionDelay :: HasFeatures a => Txt -> a -> a
pattern TransitionDelay v a = Style "transition-delay" v a

pattern TransitionDuration :: HasFeatures a => Txt -> a -> a
pattern TransitionDuration v a = Style "transition-duration" v a

pattern TransitionProperty :: HasFeatures a => Txt -> a -> a
pattern TransitionProperty v a = Style "transition-property" v a

pattern TransitionTimingFunction :: HasFeatures a => Txt -> a -> a
pattern TransitionTimingFunction v a = Style "transition-timing-function" v a

pattern WillChange :: HasFeatures a => Txt -> a -> a
pattern WillChange v a = Style "will-change" v a

pattern Animation :: HasFeatures a => Txt -> a -> a
pattern Animation v a = Style "animation" v a

pattern Transform :: HasFeatures a => Txt -> a -> a
pattern Transform v a = Style "transform" v a

pattern TransformStyle :: HasFeatures a => Txt -> a -> a
pattern TransformStyle v a = Style "transform-style" v a

pattern TransformOrigin :: HasFeatures a => Txt -> a -> a
pattern TransformOrigin v a = Style "transform-origin" v a

pattern Visibility :: HasFeatures a => Txt -> a -> a
pattern Visibility v a = Style "visibility" v a

pattern Direction :: HasFeatures a => Txt -> a -> a
pattern Direction v a = Style "direction" v a

pattern Opacity :: HasFeatures a => Txt -> a -> a
pattern Opacity v a = Style "opacity" v a

pattern AlignSelf :: HasFeatures a => Txt -> a -> a
pattern AlignSelf v a = Style "align-self" v a

pattern AlignContent :: HasFeatures a => Txt -> a -> a
pattern AlignContent v a = Style "align-content" v a

pattern JustifyContent :: HasFeatures a => Txt -> a -> a
pattern JustifyContent v a = Style "justify-content" v a

pattern AlignItems :: HasFeatures a => Txt -> a -> a
pattern AlignItems v a = Style "align-items" v a

pattern Order :: HasFeatures a => Txt -> a -> a
pattern Order v a = Style "order" v a

pattern FlexBasis :: HasFeatures a => Txt -> a -> a
pattern FlexBasis v a = Style "flex-basis" v a

pattern FlexGrow :: HasFeatures a => Txt -> a -> a
pattern FlexGrow v a = Style "flex-grow" v a

pattern Flex :: HasFeatures a => Txt -> a -> a
pattern Flex v a = Style "flex" v a

pattern FlexFlow :: HasFeatures a => Txt -> a -> a
pattern FlexFlow v a = Style "flex-flow" v a

pattern FlexDirection :: HasFeatures a => Txt -> a -> a
pattern FlexDirection v a = Style "flex-direction" v a

pattern FlexWrap :: HasFeatures a => Txt -> a -> a
pattern FlexWrap v a = Style "flex-wrap" v a

display :: Txt
display = "display"

inline :: Txt
inline = "inline"

block :: Txt
block = "block"

flex :: Txt
flex = "flex"

flexbox :: Txt
flexbox = "flexbox"

contents :: Txt
contents = "contents"

table :: Txt
table = "table"

layout :: Txt
layout = "layout"

list :: Txt
list = "list"

runin :: Txt
runin = "run-in"

caption :: Txt
caption = "caption"

group :: Txt
group = "group"

visibility :: Txt
visibility = "visibility"

overflow :: Txt
overflow = "overflow"

x :: Txt
x = "x"

y :: Txt
y = "y"

z :: Txt
z = "z"

visible :: Txt
visible = "visible"

hidden :: Txt
hidden = "hidden"

cell :: Txt
cell = "cell"

flow :: Txt
flow = "flow"

context :: Txt
context = "context"

menu :: Txt
menu = "menu"

grid :: Txt
grid = "grid"

column :: Txt
column = "column"

columns :: Txt
columns = "columns"

content :: Txt
content = "content"

place :: Txt
place = "place"

row :: Txt
row = "row"

rows :: Txt
rows = "rows"

item :: Txt
item = "item"

items :: Txt
items = "items"

justify :: Txt
justify = "justify"

gap :: Txt
gap = "gap"

template :: Txt
template = "template"

area :: Txt
area = "area"

areas :: Txt
areas = "areas"

dense :: Txt
dense = "dense"

start :: Txt
start = "start"

end :: Txt
end = "end"

align :: Txt
align = "align"

span :: Txt
span = "span"

self :: Txt
self = "self"

positive :: Txt
positive = "positive"

orient :: Txt
orient = "orient"

stretch :: Txt
stretch = "stretch"

center :: Txt
center = "center"

min :: Txt
min = "min"

max :: Txt
max = "max"

fill :: Txt
fill = "fill"

scroll :: Txt
scroll = "scroll"

alias :: Txt
alias = "alias"


n :: Txt
n = "n"

e :: Txt
e = "e"

w :: Txt
w = "w"

ne :: Txt
ne = "ne"

ns :: Txt
ns = "ns"

nw :: Txt
nw = "nw"

nesw :: Txt
nesw = "nesw"

nwse :: Txt
nwse = "nwse"

ew :: Txt
ew = "ew"

se :: Txt
se = "se"

sw :: Txt
sw = "sw"

cursor :: Txt
cursor = "cursor"

text :: Txt
text = "text"

pointer :: Txt
pointer = "pointer"

col :: Txt
col = "col"

copy :: Txt
copy = "copy"

crosshair :: Txt
crosshair = "crosshair"

grab :: Txt
grab = "grab"

grabbing :: Txt
grabbing = "grabbing"

help :: Txt
help = "help"

move :: Txt
move = "move"

resize :: Txt
resize = "resize"

no :: Txt
no = "no"

drop :: Txt
drop = "drop"

not :: Txt
not = "not"

allowed :: Txt
allowed = "allowed"

wait :: Txt
wait = "wait"

zoom :: Txt
zoom = "zoom"

zoomin :: Txt
zoomin = zoom-"in"

out :: Txt
out = "out"

vertical :: Txt
vertical = "vertical"

background :: Txt
background = "background"

color :: Txt
color = "color"

image :: Txt
image = "image"

repeat :: Txt
repeat = "repeat"

attachment :: Txt
attachment = "attachment"

position :: Txt
position = "position"

size :: Txt
size = "size"

cover :: Txt
cover = "cover"

origin :: Txt
origin = "origin"

clip :: Txt
clip = "clip"

border :: Txt
border = "border"

outline :: Txt
outline = "outline"

style :: Txt
style = "style"

width :: Txt
width = "width"

offset :: Txt
offset = "offset"

source :: Txt
source = "source"

slice :: Txt
slice = "slice"

outset :: Txt
outset = "outset"

top :: Txt
top = "top"

bottom :: Txt
bottom = "bottom"

bottomleft :: Txt
bottomleft = "bottomleft"

bottomright :: Txt
bottomright = "bottomright"

topleft :: Txt
topleft = "topleft"

topright :: Txt
topright = "topright"

left :: Txt
left = "left"

right :: Txt
right = "right"

radius :: Txt
radius = "radius"

gradient :: Txt
gradient = "gradient"

collapse :: Txt
collapse = "collapse"

separate :: Txt
separate = "separate"

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

box :: Txt
box = "box"

sizing :: Txt
sizing = "sizing"

margin :: Txt
margin = "margin"

padding :: Txt
padding = "padding"

height :: Txt
height = "height"

filter_ :: Txt
filter_ = "filter"

space :: Txt
space = "space"

around :: Txt
around = "around"

between :: Txt
between = "between"

evenly :: Txt
evenly = "evenly"

even :: Txt
even = "even"

odd :: Txt
odd = "odd"

distribute :: Txt
distribute = "distribute"

order :: Txt
order = "order"

basis :: Txt
basis = "basis"

grow :: Txt
grow = "grow"

shrink :: Txt
shrink = "shrink"

direction :: Txt
direction = "direction"

reverse :: Txt
reverse = "reverse"

horizontal :: Txt
horizontal = "horizontal"

type_ :: Txt
type_ = "type"

inside :: Txt
inside = "inside"

outside :: Txt
outside = "outside"

disc :: Txt
disc = "disc"

armenian :: Txt
armenian = "armenian"

cjk :: Txt
cjk = "cjk"

ideographic :: Txt
ideographic = "ideographic"

decimal :: Txt
decimal = "decimal"

leading :: Txt
leading = "leading"

zero :: Txt
zero = "zero"

georgian :: Txt
georgian = "georgian"

hebrew :: Txt
hebrew = "hebrew"

hiragana :: Txt
hiragana = "hiragana"

katakana :: Txt
katakana = "katakana"

iroha :: Txt
iroha = "iroha"

lower :: Txt
lower = "lower"

alpha :: Txt
alpha = "alpha"

greek :: Txt
greek = "greek"

latin :: Txt
latin = "latin"

roman :: Txt
roman = "roman"

square :: Txt
square = "square"

none :: Txt
none = "none"

initial :: Txt
initial = "initial"

inherit :: Txt
inherit = "inherit"

normal :: Txt
normal = "normal"

auto :: Txt
auto = "auto"

emptyQuotes :: Txt
emptyQuotes = "\"\""

noBreakSpace :: Txt
noBreakSpace = "\"\\00a0\""

true :: Txt
true = "true"

false :: Txt
false = "false"

nav :: Txt
nav = "nav"

down :: Txt
down = "down"

up :: Txt
up = "up"

sizes :: Txt
sizes = "sizes"

srcset :: Txt
srcset = "srcset"

subject :: Txt
subject = "subject"

button :: Txt
button = "button"

touch :: Txt
touch = "touch"

action :: Txt
action = "action"

events :: Txt
events = "events"

empty :: Txt
empty = "empty"

cells :: Txt
cells = "cells"

default_ :: Txt
default_ = "default"

all :: Txt
all = "all"

xmlns :: Txt
xmlns = "xmlns"

forwards :: Txt
forwards = "forwards"

backwards :: Txt
backwards = "backwards"

side :: Txt
side = "side"

closest :: Txt
closest = "closest"

farthest :: Txt
farthest = "farthest"

corner :: Txt
corner = "corner"

fixed :: Txt
fixed = "fixed"

absolute :: Txt
absolute = "absolute"

static_ :: Txt
static_ = "static"

relative :: Txt
relative = "relative"

sticky :: Txt
sticky = "sticky"

both :: Txt
both = "both"

float :: Txt
float = "float"

clear :: Txt
clear = "clear"

index :: Txt
index = "index"

valign :: Txt
valign = "valign"

to :: Txt
to = "to"

from :: Txt
from = "from"

rtl :: Txt
rtl = "rtl"

ltr :: Txt
ltr = "ltr"

font :: Txt
font = "font"

family :: Txt
family = "family"

weight :: Txt
weight = "weight"

xx :: Txt
xx = "xx"

small :: Txt
small = "small"

medium :: Txt
medium = "medium"

large :: Txt
large = "large"

smaller :: Txt
smaller = "smaller"

larger :: Txt
larger = "larger"

italic :: Txt
italic = "italic"

regular :: Txt
regular = "regular"

antialiased :: Txt
antialiased = "antialiased"

optimized :: Txt
optimized = "optimized"

bold :: Txt
bold = "bold"

bolder :: Txt
bolder = "bolder"

lighter :: Txt
lighter = "lighter"

shadow :: Txt
shadow = "shadow"

decoration :: Txt
decoration = "decoration"

line :: Txt
line = "line"

baseline :: Txt
baseline = "baseline"

middle :: Txt
middle = "middle"

indent :: Txt
indent = "indent"

ellipsis :: Txt
ellipsis = "ellipsis"

underline :: Txt
underline = "underline"

transform :: Txt
transform = "transform"

capitalize :: Txt
capitalize = "capitalize"

uppercase :: Txt
uppercase = "uppercase"

lowercase :: Txt
lowercase = "lowercase"

unicode :: Txt
unicode = "unicode"

bidi :: Txt
bidi = "bidi"

override :: Txt
override = "override"

hyphens :: Txt
hyphens = "hyphens"

wrap :: Txt
wrap = "wrap"

nowrap :: Txt
nowrap = "nowrap"

pack :: Txt
pack = "pack"

pre :: Txt
pre = "pre"

word :: Txt
word = "word"

break :: Txt
break = "break"

spacing :: Txt
spacing = "spacing"

jump :: Txt
jump = "jump"

step :: Txt
step = "step"

perspective :: Txt
perspective = "perspective"

backface :: Txt
backface = "backface"

transition :: Txt
transition = "transition"

delay :: Txt
delay = "delay"

duration :: Txt
duration = "duration"

property :: Txt
property = "property"

timing :: Txt
timing = "timing"

function :: Txt
function = "function"

mode :: Txt
mode = "mode"

infinite :: Txt
infinite = "infinite"

linear :: Txt
linear = "linear"

animation :: Txt
animation = "animation"

name :: Txt
name = "name"

paused :: Txt
paused = "paused"

play :: Txt
play = "play"

state :: Txt
state = "state"

iteration :: Txt
iteration = "iteration"

count :: Txt
count = "count"

ease :: Txt
ease = "ease"

easein :: Txt
easein = ease-"in"

easeinout :: Txt
easeinout = ease-"in"-out

will :: Txt
will = "will"

change :: Txt
change = "change"

user :: Txt
user = "user"

select :: Txt
select = "select"

rendering :: Txt
rendering = "rendering"

optimize :: Txt
optimize = "optimize"

legibility :: Txt
legibility = "legibility"

smoothing :: Txt
smoothing = "smoothing"

letter :: Txt
letter = "letter"

scrolling :: Txt
scrolling = "scrolling"

preserve3d :: Txt
preserve3d = "preserve-3d"

webkit :: Txt
webkit = "-webkit"

moz :: Txt
moz = "-moz"

mozilla :: Txt
mozilla = moz

microsoft :: Txt
microsoft = "-ms"

osx :: Txt
osx = "osx"

grayscale :: Txt
grayscale = "grayscale"

(%) :: Txt -> Txt
(%) p = p <> "%"

px :: Txt
px = "px"

cm :: Txt
cm = "cm"

mm :: Txt
mm = "mm"

q :: Txt
q = "Q"

inch :: Txt
inch = "in"

pc :: Txt
pc = "pc"

pt :: Txt
pt = "pt"

-- <angle>

deg :: Txt
deg = "deg"

rad :: Txt
rad = "rad"

grad :: Txt
grad = "grad"

turn :: Txt
turn = "turn"

-- <resolution>

dpi :: Txt
dpi = "dpi"

dpcm :: Txt
dpcm = "dpcm"

dppx :: Txt
dppx = "dppx"

-- <flex>

fr :: Txt
fr = "fr"

-- <frequency>

hz :: Txt
hz = "hz"

kHz :: Txt
kHz = "kHz"

-- <font-relative lengths>

cap :: Txt
cap = "cap"

ch :: Txt
ch = "ch"

em :: Txt
em = "em"

ex :: Txt
ex = "ex"

ic :: Txt
ic = "ic"

lh :: Txt
lh = "lh"

rem :: Txt
rem = "rem"

rlh :: Txt
rlh = "rlh"

-- <viewport-percentage lengths>

vh :: Txt
vh = "vh"

vw :: Txt
vw = "vw"

vi :: Txt
vi = "vi"

vb :: Txt
vb = "vb"

vmin :: Txt
vmin = "vmin"

vmax :: Txt
vmax = "vmax"

-- <time>

s :: Txt
s = "s"

ms :: Txt
ms = "ms"

bgcolor :: Txt
bgcolor = "bgcolor"

currentColor :: Txt
currentColor = "currentColor"

opacity :: Txt
opacity = "opacity"

transparent :: Txt
transparent = "transparent"

-- There is a corner-case that isn't covered with this approach:
--
-- > hex 0x000fff => #fff => #ffffff
--
hex :: Int -> Txt
hex rgb = "#" <> code
  where
    code :: Txt
    code = toTxt $ pad $ showHex rgb []

    pad :: String -> String
    pad s@(List.length -> n) 
      | n == 3    = s
      | otherwise = List.replicate (6 - n) '0' <> s

-- There is a corner-case that isn't covered with this approach:
--
-- > hex 0x0000ffff => #ffff => #ffffffff
--
hexa :: Int64 -> Txt
hexa rgba = "#" <> code
  where
    code :: Txt
    code = toTxt $ pad $ showHex rgba []

    pad :: String -> String
    pad s@(List.length -> n)
      | n == 4    = s
      | otherwise = List.replicate (8 - n) '0' <> s

{-# INLINE hsba #-}
hsba :: (Int, Double, Double, Double) -> Txt
hsba (_, _, 0, a) = "hsla(0,0%,0%," <> dec a <> ")"
hsba (h, s, b, a) =
  let
    l = (b / 2) * (2 - (s / 100))
    s'
      | l < 50    = (b * s) / (l * 2)
      | l == 100  = (b * s)
      | otherwise = (b * s) / (200 - l * 2)
  in "hsla(" <> elems [int h,percent s',percent l,dec a] <> ")"

hsb :: (Int, Double, Double) -> Txt
hsb (h, s, b) = hsba (h, s, b, 1)

rgb :: (Txt,Txt,Txt) -> Txt
rgb (r,g,b) = "rgb(" <> elems [r,g,b] <> ")"

rgba :: (Txt,Txt,Txt,Txt) -> Txt
rgba (r,g,b,a) = "rgba(" <> elems [r,g,b,a] <> ")"

hsl :: (Txt,Txt,Txt) -> Txt
hsl (h,s,l) = "hsl(" <> elems [h,s,l] <> ")"

hsla :: (Txt,Txt,Txt,Txt) -> Txt
hsla (h,s,l,a) = "hsla(" <> elems [h,s,l,a] <> ")"

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

yellowgreen :: Txt
yellowgreen = "yellowgreen"

infixr 4 <<>>
(<<>>) :: Txt -> Txt -> Txt
(<<>>) x y = x <> " " <> y

int :: Integral a => a -> Txt
int = fromIntegral

dec :: Real a => a -> Txt
dec = fromRational . toRational

neg :: Num a => a -> a
neg = negate

per :: Real a => a -> Txt
per = percent

fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

rtf :: (Real a, Fractional b) => a -> b
rtf = realToFrac

rtn :: (Real a,Num b) => a -> b
rtn = fi . (round :: Double -> Int) . rtf

class ToAppendable a where
  toAppendable :: a -> (Txt -> Txt)

instance ToAppendable Integer where
  toAppendable = fi

instance ToAppendable Int where
  toAppendable = fi

instance ToAppendable Double where
  toAppendable = rtf

instance ToAppendable Float where
  toAppendable = rtf

infixl 5 <#>
(<#>) :: ToAppendable a => a -> Txt -> Txt
(<#>) = toAppendable

percent :: Real a => a -> Txt
percent p = (dec p <> "%")

mils :: Real a => a -> Txt
mils ms = rtf ms ("ms" :: Txt)

secs :: Real a => a -> Txt
secs s = rtf s ("s" :: Txt)

pxs :: Real a => a -> Txt
pxs ps = rtf ps ("px" :: Txt)

ems :: Real a => a -> Txt
ems es = rtf es ("em" :: Txt)

rems :: Real a => a -> Txt
rems rs = rtf rs ("rem" :: Txt)

calc :: Txt -> Txt
calc c = "calc(" <> process c <> ")"
  where
    process = Txt.replace "-" " - "
            . Txt.replace "*" " * "
            . Txt.replace "+" " + "
            . Txt.replace "/" " / "


rect :: (Txt,Txt,Txt,Txt) -> Txt
rect (t,r,b,l) = "rect(" <> elems [t,r,b,l] <> ")"

blur :: Txt -> Txt
blur b = "blur(" <> b <> ")"

brightness :: Txt -> Txt
brightness b = "brightness(" <> b <> ")"

contrast :: Txt -> Txt
contrast b = "contrast(" <> b <> ")"

dropShadow :: Txt -> Txt
dropShadow b = "drop-shadow(" <> b <> ")"

grayscale_ :: Txt -> Txt
grayscale_ b = "grayscale(" <> b <> ")"

hueRotate :: Txt -> Txt
hueRotate b = "hue-rotate(" <> b <> ")"

invert :: Txt -> Txt
invert b = "invert(" <> b <> ")"

opacity_ :: Txt -> Txt
opacity_ b = "opacity(" <> b <> ")"

opac :: Txt -> Txt
opac = opacity_

saturate :: Txt -> Txt
saturate b = "saturate(" <> b <> ")"

sepia :: Txt -> Txt
sepia b = "sepia(" <> b <> ")"

linearGradient :: [Txt] -> Txt
linearGradient g = "linear-gradient(" <> elems g <> ")"

radialGradient :: [Txt] -> Txt
radialGradient g = "radial-gradient(" <> elems g <> ")"

repeatingLinearGradient :: [Txt] -> Txt
repeatingLinearGradient g = "repeating-" <> linearGradient g

repeatingRadialGradient :: [Txt] -> Txt
repeatingRadialGradient g = "repeating-" <> radialGradient g

conicGradient :: [Txt] -> Txt
conicGradient g = "conic-gradient(" <> elems g <> ")"

minmax :: (Txt,Txt) -> Txt
minmax (l,h) = "minmax(" <> elems [l,h] <> ")"

fitContent :: (Txt,Txt) -> Txt
fitContent (a,b) = "fit-content(" <> elems [a,b] <> ")"

repeat_ :: (Txt,Txt) -> Txt
repeat_ (n,x) = "repeat(" <> elems [n,x] <> ")"

screenMinWidth :: Txt -> Txt
screenMinWidth w = "only screen and (min-width:" <> w <> ")"

allMinWidth :: Txt -> Txt
allMinWidth w = "all and (min-width:" <> w <> ")"

screenMaxWidth :: Txt -> Txt
screenMaxWidth w = "only screen and (max-width:" <> w <> ")"

allMaxWidth :: Txt -> Txt
allMaxWidth w = "all and (max-width:" <> w <> ")"

cubicBezier :: (Txt,Txt,Txt,Txt) -> Txt
cubicBezier (x1,y1,x2,y2) = 
  "cubic-bezier(" <> elems [x1,y1,x2,y2] <> ")"

cubez :: (Txt,Txt,Txt,Txt) -> Txt
cubez = cubicBezier

steps :: (Txt,Txt) -> Txt
steps (n,f) =
  "steps(" <> elems [n,f] <> ")"

-- (scale-x,skew-y,skew-x,scale-y,translate-x,translate-y)
matrix :: (Txt,Txt,Txt,Txt,Txt,Txt) -> Txt
matrix (a,b,c,d,tx,th) = 
  "matrix(" <> elems [a,b,c,d,tx,th] <> ")"

mat :: (Txt,Txt,Txt,Txt,Txt,Txt) -> Txt
mat = matrix

matrix3d :: (Txt,Txt,Txt,Txt
            ,Txt,Txt,Txt,Txt
            ,Txt,Txt,Txt,Txt
            ,Txt,Txt,Txt,Txt
            ) -> Txt
matrix3d (a1,b1,c1,d1,a2,b2,c2,d2,a3,b3,c3,d3,a4,b4,c4,d4) =
  "matrix3d(" <> elems [a1,b1,c1,d1,a2,b2,c2,d2,a3,b3,c3,d3,a4,b4,c4,d4] <> ")"

mat3d :: (Txt,Txt,Txt,Txt
         ,Txt,Txt,Txt,Txt
         ,Txt,Txt,Txt,Txt
         ,Txt,Txt,Txt,Txt
         ) -> Txt
mat3d = matrix3d

perspective_ :: Txt -> Txt
perspective_ p = "perspective(" <> p <> ")"

persp :: Txt -> Txt
persp = perspective_

rotate :: Txt -> Txt
rotate r = "rotate(" <> r <> ")"

rot :: Txt -> Txt
rot = rotate

rotate3d :: (Txt,Txt,Txt,Txt) -> Txt
rotate3d (x,y,z,r) = "rotate3d(" <> elems [x,y,z,r] <> ")"

rot3d :: (Txt,Txt,Txt,Txt) -> Txt
rot3d = rotate3d

rotateX :: Txt -> Txt
rotateX r = "rotateX(" <> r <> ")"

rotX :: Txt -> Txt
rotX = rotateX

rotateY :: Txt -> Txt
rotateY r = "rotateY(" <> r <> ")"

rotY :: Txt -> Txt
rotY = rotateY

rotateZ :: Txt -> Txt
rotateZ r = "rotateZ(" <> r <> ")"

rotZ :: Txt -> Txt
rotZ = rotateZ

scale :: Txt -> Txt
scale x = scale2(x,x)

scale2 :: (Txt,Txt) -> Txt
scale2 (sx,sy) = "scale(" <> elems [sx, sy] <> ")"

scale3d :: (Txt,Txt,Txt) -> Txt
scale3d (sx,sy,sz) = "scale3d(" <> elems [sx,sy,sz] <> ")"

scaleX :: Txt -> Txt
scaleX s = "scaleX(" <> s <> ")"

scaleY :: Txt -> Txt
scaleY s = "scaleY(" <> s <> ")"

scaleZ :: Txt -> Txt
scaleZ s = "scaleZ(" <> s <> ")"

skew :: Txt -> Txt
skew s = skew2d(s,0)

skew2d :: (Txt,Txt) -> Txt
skew2d (x,y) = "skew(" <> elems [x,y] <> ")"

skewX :: Txt -> Txt
skewX x = "skewX(" <> x <> ")"

skewY :: Txt -> Txt
skewY y = "skewY(" <> y <> ")"

translate :: Txt -> Txt
translate x = translate2d(x,0)

translate2d :: (Txt,Txt) -> Txt
translate2d (x,y) = "translate(" <> elems [x,y] <> ")"

translate3d :: (Txt,Txt,Txt) -> Txt
translate3d (x,y,z) = "translate3d(" <> elems [x,y,z] <> ")"

translateX :: Txt -> Txt
translateX x = "translateX(" <> x <> ")"

translateY :: Txt -> Txt
translateY y = "translateY(" <> y <> ")"

translateZ :: Txt -> Txt
translateZ z = "translateZ(" <> z <> ")"

url :: Txt -> Txt
url u = "url(" <> u <> ")"

alpha_ :: Txt -> Txt
alpha_ a = "alpha(" <> a <> ")"

elems :: [Txt] -> Txt
elems = Txt.intercalate ","


