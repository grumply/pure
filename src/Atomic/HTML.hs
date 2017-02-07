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

circle :: [Feature e] -> [Atom e] -> Atom e
circle = svgHTML "circle"

clipPath :: [Feature e] -> [Atom e] -> Atom e
clipPath = svgHTML "clipPath"

defs :: [Feature e] -> [Atom e] -> Atom e
defs = svgHTML "defs"

ellipse :: [Feature e] -> [Atom e] -> Atom e
ellipse = svgHTML "ellipse"

g :: [Feature e] -> [Atom e] -> Atom e
g = svgHTML "g"

image :: [Feature e] -> [Atom e] -> Atom e
image = svgHTML "image"

line :: [Feature e] -> [Atom e] -> Atom e
line = svgHTML "line"

linearGradient :: [Feature e] -> [Atom e] -> Atom e
linearGradient = svgHTML "linearGradient"

filter_ :: [Feature e] -> [Atom e] -> Atom e
filter_ = svgHTML "filter"

feGaussianBlur :: [Feature e] -> [Atom e] -> Atom e
feGaussianBlur = svgHTML "feGaussianBlur"

feOffset :: [Feature e] -> [Atom e] -> Atom e
feOffset = svgHTML "feOffset"

feMerge :: [Feature e] -> [Atom e] -> Atom e
feMerge = svgHTML "feMerge"

feMergeNode :: [Feature e] -> [Atom e] -> Atom e
feMergeNode = svgHTML "feMergeNode"

mask :: [Feature e] -> [Atom e] -> Atom e
mask = svgHTML "mask"

path_ :: [Feature e] -> [Atom e] -> Atom e
path_ = svgHTML "path"

pattern_ :: [Feature e] -> [Atom e] -> Atom e
pattern_ = svgHTML "pattern"

polygon :: [Feature e] -> [Atom e] -> Atom e
polygon = svgHTML "polygon"

polyline :: [Feature e] -> [Atom e] -> Atom e
polyline = svgHTML "polyline"

radialGradient :: [Feature e] -> [Atom e] -> Atom e
radialGradient = svgHTML "radialGraedient"

rect :: [Feature e] -> [Atom e] -> Atom e
rect = svgHTML "rect"

stop_ :: [Feature e] -> [Atom e] -> Atom e
stop_ = svgHTML "stop"

svg :: [Feature e] -> [Atom e] -> Atom e
svg = svgHTML "svg"

text :: [Feature e] -> [Atom e] -> Atom e
text = svgHTML "text"

tspan :: [Feature e] -> [Atom e] -> Atom e
tspan = svgHTML "tspan"

svga :: [Feature e] -> [Atom e] -> Atom e
svga = svgHTML "a"
