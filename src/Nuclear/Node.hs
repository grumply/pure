{-# language CPP #-}
{-# language OverloadedStrings #-}
module Nuclear.Node where

import Data.JSText

import Nuclear.Attribute
import Nuclear.Cond
import Nuclear.FromText
import Nuclear.ToText

import Data.String
import Data.Void

#ifdef LENS
import Control.Lens (makePrisms,makeLenses)
#endif

#ifdef __GHCJS__
import qualified GHCJS.DOM.Types as T
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

data Node atom e where
  -- NullNode must have a presence on the page for proper diffing
  NullNode
    :: { _node :: !(Maybe ENode)
       } -> Node atom e

  Text
    ::  { _tnode      :: Maybe TNode
        , _content    :: JSText
        } -> Node atom e

  Raw
    :: { _node        :: Maybe ENode
       , _tag         :: JSText
       , _attributes  :: [Feature e]
       , _content     :: JSText
       } -> Node atom e

  KNode
    ::  { _node       :: Maybe ENode
        , _tag        :: JSText
        , _attributes :: [Feature e]
        , _keyed      :: [(Int,Node atom e)]
        } -> Node atom e

  Node
    ::  { _node       :: Maybe ENode
        , _tag        :: JSText
        , _attributes :: [Feature e]
        , _children   :: [Node atom e]
        } -> Node atom e

  -- TODO: SVG keyed node
  SVGNode
    ::  { _node       :: Maybe ENode
        , _tag        :: JSText
        , _attributes :: [Feature e]
        , _children   :: [Node atom e]
        } -> Node atom e

  Managed
    ::  { _node       :: Maybe ENode
        , _tag        :: JSText
        , _attributes :: [Feature e]
        , _atom'      :: atom
        } -> Node atom e
  deriving Functor

instance Cond (Node atom e) where
  nil = NullNode Nothing

instance IsString (Node atom e) where
  fromString = jss . fromString

instance FromText (Node atom e) where
  fromText = jss

instance IsString [Node atom e] where
  fromString s = [fromString s]

instance FromText [Node atom e] where
  fromText t = [fromText t]

#ifdef LENS
makePrisms ''Node
makeLenses ''Node
#endif

html :: JSText -> [Feature e] -> [Node atom e] -> Node atom e
html _tag _attributes _children =
  let _node = Nothing
  in Node {..}

raw :: JSText -> [Feature e] -> JSText -> Node atom e
raw _tag _attributes _content =
  let _node = Nothing
  in Raw {..}

notNil (NullNode _) = False
notNil _ = True

cnode :: Bool -> Node atom e -> Node atom e
cnode = cond

keyed :: JSText -> [Feature e] -> [(Int,Node atom e)] -> Node atom e
keyed _tag _attributes _keyed0 =
  let _node = Nothing
      _keyed = filter (notNil . snd) _keyed0
  in KNode {..}

svgHTML :: JSText -> [Feature e] -> [Node atom e] -> Node atom e
svgHTML _tag _attributes _children =
  let _node = Nothing
  in SVGNode {..}

jss :: JSText -> Node atom e
jss _content =
  let _tnode = Nothing
  in Text {..}

realize :: Node atom Void -> Node atom ms
realize = fmap absurd



--------------------------------------------------------------------------------
-- Nodes

abbr :: [Feature e] -> [Node atom e] -> Node atom e
abbr = html "abbr"

address :: [Feature e] -> [Node atom e] -> Node atom e
address = html "address"

area :: [Feature e] -> [Node atom e] -> Node atom e
area = html "area"

a :: [Feature e] -> [Node atom e] -> Node atom e
a = html "a"

article :: [Feature e] -> [Node atom e] -> Node atom e
article = html "article"

aside :: [Feature e] -> [Node atom e] -> Node atom e
aside = html "aside"

audio :: [Feature e] -> [Node atom e] -> Node atom e
audio = html "audio"

base :: [Feature e] -> [Node atom e] -> Node atom e
base = html "base"

bdi :: [Feature e] -> [Node atom e] -> Node atom e
bdi = html "bdi"

bdo :: [Feature e] -> [Node atom e] -> Node atom e
bdo = html "bdo"

big :: [Feature e] -> [Node atom e] -> Node atom e
big = html "big"

blockquote :: [Feature e] -> [Node atom e] -> Node atom e
blockquote = html "blockquote"

body :: [Feature e] -> [Node atom e] -> Node atom e
body = html "body"

b :: [Feature e] -> [Node atom e] -> Node atom e
b = html "b"

br :: Node atom e
br = html "br" [] []

button :: [Feature e] -> [Node atom e] -> Node atom e
button = html "button"

canvas :: [Feature e] -> [Node atom e] -> Node atom e
canvas = html "canvas"

caption :: [Feature e] -> [Node atom e] -> Node atom e
caption = html "caption"

cite :: [Feature e] -> [Node atom e] -> Node atom e
cite = html "cite"

code :: [Feature e] -> [Node atom e] -> Node atom e
code = html "code"

col :: [Feature e] -> [Node atom e] -> Node atom e
col = html "col"

colgroup :: [Feature e] -> [Node atom e] -> Node atom e
colgroup = html "colgroup"

dataN :: [Feature e] -> [Node atom e] -> Node atom e
dataN = html "data"

datalist :: [Feature e] -> [Node atom e] -> Node atom e
datalist = html "datalist"

dd :: [Feature e] -> [Node atom e] -> Node atom e
dd = html "dd"

description :: JSText -> Node atom e
description d = meta [ name "description", content d ] []

dl :: [Feature e] -> [Node atom e] -> Node atom e
dl = html "dl"

dt :: [Feature e] -> [Node atom e] -> Node atom e
dt = html "dt"

del :: [Feature e] -> [Node atom e] -> Node atom e
del = html "del"

details :: [Feature e] -> [Node atom e] -> Node atom e
details = html "details"

dfn :: [Feature e] -> [Node atom e] -> Node atom e
dfn = html "dfn"

dialog :: [Feature e] -> [Node atom e] -> Node atom e
dialog = html "dialog"

div :: [Feature e] -> [Node atom e] -> Node atom e
div = html "div"

em :: [Feature e] -> [Node atom e] -> Node atom e
em = html "em"

embed :: [Feature e] -> [Node atom e] -> Node atom e
embed = html "embed"

fieldset :: [Feature e] -> [Node atom e] -> Node atom e
fieldset = html "fieldset"

figcaption :: [Feature e] -> [Node atom e] -> Node atom e
figcaption = html "figcaption"

figure :: [Feature e] -> [Node atom e] -> Node atom e
figure = html "figure"

footer :: [Feature e] -> [Node atom e] -> Node atom e
footer = html "footer"

form :: [Feature e] -> [Node atom e] -> Node atom e
form = html "form"

head :: [Node atom e] -> Node atom e
head = html "head" []

header :: [Feature e] -> [Node atom e] -> Node atom e
header = html "header"

h1 :: [Feature e] -> [Node atom e] -> Node atom e
h1 = html "h1"

h2 :: [Feature e] -> [Node atom e] -> Node atom e
h2 = html "h2"

h3 :: [Feature e] -> [Node atom e] -> Node atom e
h3 = html "h3"

h4 :: [Feature e] -> [Node atom e] -> Node atom e
h4 = html "h4"

h5 :: [Feature e] -> [Node atom e] -> Node atom e
h5 = html "h5"

h6 :: [Feature e] -> [Node atom e] -> Node atom e
h6 = html "h6"

hgroup :: [Feature e] -> [Node atom e] -> Node atom e
hgroup = html "hgroup"

hr :: [Feature e] -> [Node atom e] -> Node atom e
hr = html "hr"

html_ :: [Feature e] -> [Node atom e] -> Node atom e
html_ = html "html"

iframe :: [Feature e] -> [Node atom e] -> Node atom e
iframe = html "iframe"

img :: [Feature e] -> [Node atom e] -> Node atom e
img = html "img"

input :: [Feature e] -> [Node atom e] -> Node atom e
input = html "input"

textInput :: [Feature e] -> [Node atom e] -> Node atom e
textInput fs = html "input" (type_ "text":fs)

ins :: [Feature e] -> [Node atom e] -> Node atom e
ins = html "ins"

iN :: [Feature e] -> [Node atom e] -> Node atom e
iN = html "i"

kbd :: [Feature e] -> [Node atom e] -> Node atom e
kbd = html "kbd"

keygen :: [Feature e] -> [Node atom e] -> Node atom e
keygen = html "keygen"

label :: [Feature e] -> [Node atom e] -> Node atom e
label = html "label"

legend :: [Feature e] -> [Node atom e] -> Node atom e
legend = html "legend"

li :: [Feature e] -> [Node atom e] -> Node atom e
li = html "li"

linkN :: [Feature e] -> [Node atom e] -> Node atom e
linkN = html "link"

mainN :: [Feature e] -> [Node atom e] -> Node atom e
mainN = html "main"

mapN :: [Feature e] -> [Node atom e] -> Node atom e
mapN = html "map"

mark :: [Feature e] -> [Node atom e] -> Node atom e
mark = html "mark"

menu :: [Feature e] -> [Node atom e] -> Node atom e
menu = html "menu"

menuitem :: [Feature e] -> [Node atom e] -> Node atom e
menuitem = html "menuitem"

meta :: [Feature e] -> [Node atom e] -> Node atom e
meta = html "meta"

meter :: [Feature e] -> [Node atom e] -> Node atom e
meter = html "meter"

nav :: [Feature e] -> [Node atom e] -> Node atom e
nav = html "nav"

noscript :: [Feature e] -> [Node atom e] -> Node atom e
noscript = html "noscript"

object_ :: [Feature e] -> [Node atom e] -> Node atom e
object_ = html "object"

optgroup :: [Feature e] -> [Node atom e] -> Node atom e
optgroup = html "optgroup"

option :: [Feature e] -> [Node atom e] -> Node atom e
option = html "option"

ol :: [Feature e] -> [Node atom e] -> Node atom e
ol = html "ol"

output :: [Feature e] -> [Node atom e] -> Node atom e
output = html "output"

p :: [Feature e] -> [Node atom e] -> Node atom e
p = html "p"

param :: [Feature e] -> [Node atom e] -> Node atom e
param = html "param"

picture :: [Feature e] -> [Node atom e] -> Node atom e
picture = html "picture"

pre :: [Feature e] -> [Node atom e] -> Node atom e
pre = html "pre"

progress :: [Feature e] -> [Node atom e] -> Node atom e
progress = html "progress"

q :: [Feature e] -> [Node atom e] -> Node atom e
q = html "q"

rp :: [Feature e] -> [Node atom e] -> Node atom e
rp = html "rp"

rt :: [Feature e] -> [Node atom e] -> Node atom e
rt = html "rt"

ruby :: [Feature e] -> [Node atom e] -> Node atom e
ruby = html "ruby"

samp :: [Feature e] -> [Node atom e] -> Node atom e
samp = html "samp"

script :: [Feature e] -> [Node atom e] -> Node atom e
script = html "script"

s :: [Feature e] -> [Node atom e] -> Node atom e
s = html "s"

section :: [Feature e] -> [Node atom e] -> Node atom e
section = html "section"

selectN :: [Feature e] -> [Node atom e] -> Node atom e
selectN = html "select"

small :: [Feature e] -> [Node atom e] -> Node atom e
small = html "small"

source :: [Feature e] -> [Node atom e] -> Node atom e
source = html "source"

span :: [Feature e] -> [Node atom e] -> Node atom e
span = html "span"

strong :: [Feature e] -> [Node atom e] -> Node atom e
strong = html "strong"

style :: [Feature e] -> [Node atom e] -> Node atom e
style = html "style"

sub :: [Feature e] -> [Node atom e] -> Node atom e
sub = html "sub"

summary :: [Feature e] -> [Node atom e] -> Node atom e
summary = html "summary"

sup :: [Feature e] -> [Node atom e] -> Node atom e
sup = html "sup"

table :: [Feature e] -> [Node atom e] -> Node atom e
table = html "table"

tbody :: [Feature e] -> [Node atom e] -> Node atom e
tbody = html "tbody"

td :: [Feature e] -> [Node atom e] -> Node atom e
td = html "td"

textarea :: [Feature e] -> [Node atom e] -> Node atom e
textarea = html "textarea"

tfoot :: [Feature e] -> [Node atom e] -> Node atom e
tfoot = html "tfoot"

th :: [Feature e] -> [Node atom e] -> Node atom e
th = html "th"

thead :: [Feature e] -> [Node atom e] -> Node atom e
thead = html "thead"

time :: [Feature e] -> [Node atom e] -> Node atom e
time = html "time"

title :: JSText -> Node atom e
title jst = html "title" [] [ jss jst ]

tr :: [Feature e] -> [Node atom e] -> Node atom e
tr = html "tr"

track :: [Feature e] -> [Node atom e] -> Node atom e
track = html "track"

u :: [Feature e] -> [Node atom e] -> Node atom e
u = html "u"

ul :: [Feature e] -> [Node atom e] -> Node atom e
ul = html "ul"

varN :: [Feature e] -> [Node atom e] -> Node atom e
varN = html "var"

video :: [Feature e] -> [Node atom e] -> Node atom e
video = html "video"

viewport :: JSText -> Node atom e
viewport jst = html "meta" [ name "viewport", content jst ] []

wbr :: [Feature e] -> [Node atom e] -> Node atom e
wbr = html "wbr"

--------------------------------------------------------------------------------
-- SVG

circle :: [Feature e] -> [Node atom e] -> Node atom e
circle = svgHTML "circle"

clipPath :: [Feature e] -> [Node atom e] -> Node atom e
clipPath = svgHTML "clipPath"

defs :: [Feature e] -> [Node atom e] -> Node atom e
defs = svgHTML "defs"

ellipse :: [Feature e] -> [Node atom e] -> Node atom e
ellipse = svgHTML "ellipse"

g :: [Feature e] -> [Node atom e] -> Node atom e
g = svgHTML "g"

image :: [Feature e] -> [Node atom e] -> Node atom e
image = svgHTML "image"

line :: [Feature e] -> [Node atom e] -> Node atom e
line = svgHTML "line"

linearGradient :: [Feature e] -> [Node atom e] -> Node atom e
linearGradient = svgHTML "linearGradient"

mask :: [Feature e] -> [Node atom e] -> Node atom e
mask = svgHTML "mask"

path :: [Feature e] -> [Node atom e] -> Node atom e
path = svgHTML "path"

patternN :: [Feature e] -> [Node atom e] -> Node atom e
patternN = svgHTML "pattern"

polygon :: [Feature e] -> [Node atom e] -> Node atom e
polygon = svgHTML "polygon"

polyline :: [Feature e] -> [Node atom e] -> Node atom e
polyline = svgHTML "polyline"

radialGradient :: [Feature e] -> [Node atom e] -> Node atom e
radialGradient = svgHTML "radialGraedient"

rect :: [Feature e] -> [Node atom e] -> Node atom e
rect = svgHTML "rect"

stop_ :: [Feature e] -> [Node atom e] -> Node atom e
stop_ = svgHTML "stop"

svg :: [Feature e] -> [Node atom e] -> Node atom e
svg = svgHTML "svg"

text :: [Feature e] -> [Node atom e] -> Node atom e
text = svgHTML "text"

tspan :: [Feature e] -> [Node atom e] -> Node atom e
tspan = svgHTML "tspan"
