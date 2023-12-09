{-# LANGUAGE PatternSynonyms, OverloadedStrings, ViewPatterns #-}
{-# OPTIONS_GHC -O2 #-}
module Data.HTML where

import Data.Txt
import Data.View (View,pattern SimpleHTML,pattern Attribute,pattern Property)

pattern Abbr :: View
pattern Abbr = SimpleHTML "abbr"

pattern Address :: View
pattern Address = SimpleHTML "address"

pattern Area :: View
pattern Area = SimpleHTML "area"

pattern A :: View
pattern A = SimpleHTML "a"

pattern Article :: View
pattern Article = SimpleHTML "article"

pattern Aside :: View
pattern Aside = SimpleHTML "aside"

pattern Audio :: View
pattern Audio = SimpleHTML "audio"

pattern Base :: View
pattern Base = SimpleHTML "base"

pattern Bdi :: View
pattern Bdi = SimpleHTML "bdi"

pattern Bdo :: View
pattern Bdo = SimpleHTML "bdo"

pattern Big :: View
pattern Big = SimpleHTML "big"

pattern Blockquote :: View
pattern Blockquote = SimpleHTML "blockquote"

pattern Body :: View
pattern Body = SimpleHTML "body"

pattern Bold :: View
pattern Bold = SimpleHTML "b"

pattern Br :: View
pattern Br = SimpleHTML "br"

pattern Button :: View
pattern Button = SimpleHTML "button"

pattern Canvas :: View
pattern Canvas = SimpleHTML "canvas"

pattern Caption :: View
pattern Caption = SimpleHTML "caption"

pattern Cite :: View
pattern Cite = SimpleHTML "cite"

pattern Code :: View
pattern Code = SimpleHTML "code"

pattern Col :: View
pattern Col = SimpleHTML "col"

pattern Colgroup :: View
pattern Colgroup = SimpleHTML "colgroup"

pattern Data :: View
pattern Data = SimpleHTML "data"

pattern Datalist :: View
pattern Datalist = SimpleHTML "datalist"

pattern Dd :: View
pattern Dd = SimpleHTML "dd"

pattern Description :: View
pattern Description = SimpleHTML "description"

pattern Dl :: View
pattern Dl = SimpleHTML "dl"

pattern Dt :: View
pattern Dt = SimpleHTML "dt"

pattern Del :: View
pattern Del = SimpleHTML "del"

pattern Details :: View
pattern Details = SimpleHTML "details"

pattern Dfn :: View
pattern Dfn = SimpleHTML "dfn"

pattern Dialog :: View
pattern Dialog = SimpleHTML "dialog"

pattern Div :: View
pattern Div = SimpleHTML "div"

pattern Em :: View
pattern Em = SimpleHTML "em"

pattern Embed :: View
pattern Embed = SimpleHTML "embed"

pattern Fieldset :: View
pattern Fieldset = SimpleHTML "fieldset"

pattern Figcaption :: View
pattern Figcaption = SimpleHTML "figcaption"

pattern Figure :: View
pattern Figure = SimpleHTML "figure"

pattern Footer :: View
pattern Footer = SimpleHTML "footer"

pattern Form :: View
pattern Form = SimpleHTML "form"

pattern Frame :: View
pattern Frame = SimpleHTML "frame"

pattern Head :: View
pattern Head = SimpleHTML "head"

pattern Header :: View
pattern Header = SimpleHTML "header"

pattern H1 :: View
pattern H1 = SimpleHTML "h1"

pattern H2 :: View
pattern H2 = SimpleHTML "h2"

pattern H3 :: View
pattern H3 = SimpleHTML "h3"

pattern H4 :: View
pattern H4 = SimpleHTML "h4"

pattern H5 :: View
pattern H5 = SimpleHTML "h5"

pattern H6 :: View
pattern H6 = SimpleHTML "h6"

pattern Hgroup :: View
pattern Hgroup = SimpleHTML "hgroup"

pattern Hr :: View
pattern Hr = SimpleHTML "hr"

pattern Html :: View
pattern Html = SimpleHTML "html"

pattern Iframe :: View
pattern Iframe = SimpleHTML "iframe"

pattern Img :: View
pattern Img = SimpleHTML "img"

pattern Input :: View
pattern Input = SimpleHTML "input"

pattern Ins :: View
pattern Ins = SimpleHTML "ins"

pattern I :: View
pattern I = SimpleHTML "i"

pattern Kbd :: View
pattern Kbd = SimpleHTML "kbd"

pattern Keygen :: View
pattern Keygen = SimpleHTML "keygen"

pattern Label :: View
pattern Label = SimpleHTML "label"

pattern Legend :: View
pattern Legend = SimpleHTML "legend"

pattern Li :: View
pattern Li = SimpleHTML "li"

pattern Link :: View
pattern Link = SimpleHTML "link"

pattern Main :: View
pattern Main = SimpleHTML "main"

pattern Map :: View
pattern Map = SimpleHTML "map"

pattern Mark :: View
pattern Mark = SimpleHTML "mark"

pattern Menu :: View
pattern Menu = SimpleHTML "menu"

pattern Menuitem :: View
pattern Menuitem = SimpleHTML "menuitem"

pattern Meta :: View
pattern Meta = SimpleHTML "meta"

pattern Meter :: View
pattern Meter = SimpleHTML "meter"

pattern Nav :: View
pattern Nav = SimpleHTML "nav"

pattern Noscript :: View
pattern Noscript = SimpleHTML "noscript"

pattern Obj :: View
pattern Obj = SimpleHTML "object"

pattern Optgroup :: View
pattern Optgroup = SimpleHTML "optgroup"

pattern Option :: View
pattern Option = SimpleHTML "option"

pattern Ol :: View
pattern Ol = SimpleHTML "ol"

pattern Output :: View
pattern Output = SimpleHTML "output"

pattern P :: View
pattern P = SimpleHTML "p"

pattern Param :: View
pattern Param = SimpleHTML "param"

pattern Picture :: View
pattern Picture = SimpleHTML "picture"

pattern Pre :: View
pattern Pre = SimpleHTML "pre"

pattern Progress :: View
pattern Progress = SimpleHTML "progress"

pattern Q :: View
pattern Q = SimpleHTML "q"

pattern Rp :: View
pattern Rp = SimpleHTML "rp"

pattern Rt :: View
pattern Rt = SimpleHTML "rt"

pattern Ruby :: View
pattern Ruby = SimpleHTML "ruby"

pattern Samp :: View
pattern Samp = SimpleHTML "samp"

pattern Script :: View
pattern Script = SimpleHTML "script"

pattern S :: View
pattern S = SimpleHTML "s"

pattern Section :: View
pattern Section = SimpleHTML "section"

pattern Select :: View
pattern Select = SimpleHTML "select"

pattern Small :: View
pattern Small = SimpleHTML "small"

pattern Source :: View
pattern Source = SimpleHTML "source"

pattern Span :: View
pattern Span = SimpleHTML "span"

pattern Strong :: View
pattern Strong = SimpleHTML "strong"

pattern Style_ :: View
pattern Style_ = SimpleHTML "style"

pattern Sub :: View
pattern Sub = SimpleHTML "sub"

pattern Summary :: View
pattern Summary = SimpleHTML "summary"

pattern Sup :: View
pattern Sup = SimpleHTML "sup"

pattern Table :: View
pattern Table = SimpleHTML "table"

pattern Tbody :: View
pattern Tbody = SimpleHTML "tbody"

pattern Td :: View
pattern Td = SimpleHTML "td"

pattern Textarea :: View
pattern Textarea = SimpleHTML "textarea"

pattern Tfoot :: View
pattern Tfoot = SimpleHTML "tfoot"

pattern Th :: View
pattern Th = SimpleHTML "th"

pattern Thead :: View
pattern Thead = SimpleHTML "thead"

pattern Time_ :: View
pattern Time_ = SimpleHTML "time"

pattern Title_ :: View
pattern Title_ = SimpleHTML "title"

pattern Tr :: View
pattern Tr = SimpleHTML "tr"

pattern Track :: View
pattern Track = SimpleHTML "track"

pattern U :: View
pattern U = SimpleHTML "u"

pattern Ul :: View
pattern Ul = SimpleHTML "ul"

pattern Var :: View
pattern Var = SimpleHTML "var"

pattern Video :: View
pattern Video = SimpleHTML "video"

pattern Viewport :: View
pattern Viewport = SimpleHTML "viewport"

pattern Wbr :: View
pattern Wbr = SimpleHTML "wbr"

--------------------------------------------------------------------------------
-- HTML Attributes

pattern AutoCapitalize :: Txt -> View -> View
pattern AutoCapitalize v a = Attribute "autocapitalize" v a

pattern AutoComplete :: Txt -> View -> View
pattern AutoComplete v a = Attribute "autocomplete" v a

pattern AutoCorrect :: Txt -> View -> View
pattern AutoCorrect v a = Attribute "autocorrect" v a

pattern AutoFocus :: Txt -> View -> View
pattern AutoFocus v a = Attribute "autofocus" v a

pattern Algin :: Txt -> View -> View
pattern Algin v a = Attribute "algin" v a

pattern Bgcolor_ :: Txt -> View -> View
pattern Bgcolor_ v a = Attribute "bgcolor" v a

pattern Border_ :: Txt -> View -> View
pattern Border_ v a = Attribute "border" v a

pattern Buffered :: Txt -> View -> View
pattern Buffered v a = Attribute "buffered" v a

pattern Class_ :: Txt -> View -> View
pattern Class_ v a = Attribute "class" v a

pattern Code_ :: Txt -> View -> View
pattern Code_ v a = Attribute "code" v a

pattern Codebase :: Txt -> View -> View
pattern Codebase v a = Attribute "codebase" v a

pattern Color_ :: Txt -> View -> View
pattern Color_ v a = Attribute "color" v a

pattern Dirname :: Txt -> View -> View
pattern Dirname v a = Attribute "dirname" v a

pattern Dropzone :: Txt -> View -> View
pattern Dropzone v a = Attribute "dropzone" v a

pattern For :: Txt -> View -> View
pattern For v a = Attribute "for" v a

pattern Formaction :: Txt -> View -> View
pattern Formaction v a = Attribute "formaction" v a

pattern Ismap :: Txt -> View -> View
pattern Ismap v a = Attribute "ismap" v a

pattern Ping :: Txt -> View -> View
pattern Ping v a = Attribute "ping" v a

pattern Selected :: Txt -> View -> View
pattern Selected v a = Attribute "selected" v a

pattern Slot :: Txt -> View -> View
pattern Slot v a = Attribute "slot" v a

pattern SpellCheck :: Txt -> View -> View
pattern SpellCheck v a = Attribute "spellcheck" v a

pattern Translate :: Txt -> View -> View
pattern Translate v a = Attribute "translate" v a

--------------------------------------------------------------------------------
-- HTML Properties

pattern Accept :: Txt -> View -> View
pattern Accept v a = Property "accept" v a

pattern AcceptCharset :: Txt -> View -> View
pattern AcceptCharset v a = Property "acceptCharset" v a

pattern AccessKey :: Txt -> View -> View
pattern AccessKey v a = Property "accessKey" v a

pattern Action :: Txt -> View -> View
pattern Action v a = Property "action" v a

pattern AllowFullScreen :: Txt -> View -> View
pattern AllowFullScreen v a = Property "allowfullScreen" v a

pattern Alt :: Txt -> View -> View
pattern Alt v a = Property "alt" v a

pattern As :: Txt -> View -> View
pattern As v a = Property "as" v a

pattern Async :: Txt -> View -> View
pattern Async v a = Property "async" v a

pattern Autoplay :: View -> View
pattern Autoplay a = Property "autoplay" "true" a

pattern Autosave :: Txt -> View -> View
pattern Autosave v a = Property "autosave" v a

pattern Capture :: Txt -> View -> View
pattern Capture v a = Property "capture" v a

pattern CellPadding :: Txt -> View -> View
pattern CellPadding v a = Property "cellPadding" v a

pattern CellSpacing :: Txt -> View -> View
pattern CellSpacing v a = Property "cellSpacing" v a

pattern Challenge :: Txt -> View -> View
pattern Challenge v a = Property "challenge" v a

pattern CharSet :: Txt -> View -> View
pattern CharSet v a = Property "charSet" v a

pattern Checked :: Txt -> View -> View
pattern Checked v a = Property "checked" v a

pattern Children_ :: Txt -> View -> View
pattern Children_ v a = Property "children" v a

pattern Cite_ :: Txt -> View -> View
pattern Cite_ v a = Property "cite" v a

pattern ClassID :: Txt -> View -> View
pattern ClassID v a = Property "classId" v a

pattern ClassName :: Txt -> View -> View
pattern ClassName v a = Property "className" v a

pattern Cols :: Txt -> View -> View
pattern Cols v a = Property "cols" v a

pattern ColSpan :: Txt -> View -> View
pattern ColSpan v a = Property "colSpan" v a

pattern Content_ :: Txt -> View -> View
pattern Content_ v a = Property "content" v a

pattern ContentEditable :: Txt -> View -> View
pattern ContentEditable v a = Property "contentEditable" v a

pattern ContextMenu :: Txt -> View -> View
pattern ContextMenu v a = Property "contextMenu" v a

pattern Controls :: Txt -> View -> View
pattern Controls v a = Property "controls" v a

pattern ControlsList :: Txt -> View -> View
pattern ControlsList v a = Property "controlsList" v a

pattern Coords :: Txt -> View -> View
pattern Coords v a = Property "coords" v a

pattern CrossOrigin :: Txt -> View -> View
pattern CrossOrigin v a = Property "crossOrigin" v a

pattern Data_ :: Txt -> View -> View
pattern Data_ v a = Property "data" v a

pattern DateTime :: Txt -> View -> View
pattern DateTime v a = Property "dateTime" v a

pattern Default :: Txt -> View -> View
pattern Default v a = Property "default" v a

pattern DefaultChecked :: Txt -> View -> View
pattern DefaultChecked v a = Property "defaultChecked" v a

pattern DefaultValue :: Txt -> View -> View
pattern DefaultValue v a = Property "defaultValue" v a

pattern Defer :: Txt -> View -> View
pattern Defer v a = Property "defer" v a

pattern Dir :: Txt -> View -> View
pattern Dir v a = Property "dir" v a

pattern Disabled :: Txt -> View -> View
pattern Disabled v a = Property "disabled" v a

pattern Download :: Txt -> View -> View
pattern Download v a = Property "download" v a

pattern Draggable :: Txt -> View -> View
pattern Draggable v a = Property "draggable" v a

pattern EncType :: Txt -> View -> View
pattern EncType v a = Property "encType" v a

pattern HtmlFor :: Txt -> View -> View
pattern HtmlFor v a = Property "htmlFor" v a

pattern Form_ :: Txt -> View -> View
pattern Form_ v a = Property "form" v a

pattern FormMethod :: Txt -> View -> View
pattern FormMethod v a = Property "formMethod" v a

pattern FormAction :: Txt -> View -> View
pattern FormAction v a = Property "formAction" v a

pattern FormEncType :: Txt -> View -> View
pattern FormEncType v a = Property "formEncType" v a

pattern FormNoValidate :: Txt -> View -> View
pattern FormNoValidate v a = Property "formNoValidate" v a

pattern FormTarget :: Txt -> View -> View
pattern FormTarget v a = Property "formTarget" v a

pattern FrameBorder :: Txt -> View -> View
pattern FrameBorder v a = Property "frameBorder" v a

pattern Headers :: Txt -> View -> View
pattern Headers v a = Property "headers" v a

pattern Height_:: Txt -> View -> View
pattern Height_ v a = Property "height" v a

pattern Hidden :: Txt -> View -> View
pattern Hidden v a = Property "hidden" v a

pattern High :: Txt -> View -> View
pattern High v a = Property "high" v a

pattern Href :: Txt -> View -> View
pattern Href v a = Property "href" v a

pattern HrefLang :: Txt -> View -> View
pattern HrefLang v a = Property "hrefLang" v a

pattern HttpEquiv :: Txt -> View -> View
pattern HttpEquiv v a = Property "httpEquiv" v a

pattern Icon :: Txt -> View -> View
pattern Icon v a = Property "icon" v a

pattern Id :: Txt -> View -> View
pattern Id v a = Property "id" v a

pattern InnerHTML :: Txt -> View -> View
pattern InnerHTML v a = Property "innerHTML" v a

pattern InputMode :: Txt -> View -> View
pattern InputMode v a = Property "inputMode" v a

pattern Integrity :: Txt -> View -> View
pattern Integrity v a = Property "integrity" v a

pattern Is :: Txt -> View -> View
pattern Is v a = Property "is" v a

pattern ItemID :: Txt -> View -> View
pattern ItemID v a = Property "itemID" v a

pattern ItemProp :: Txt -> View -> View
pattern ItemProp v a = Property "itemProp" v a

pattern ItemRef :: Txt -> View -> View
pattern ItemRef v a = Property "itemRef" v a

pattern ItemScope :: Txt -> View -> View
pattern ItemScope v a = Property "itemScope" v a

pattern ItemType :: Txt -> View -> View
pattern ItemType v a = Property "itemType" v a

pattern KeyParams :: Txt -> View -> View
pattern KeyParams v a = Property "keyParams" v a

pattern KeyType :: Txt -> View -> View
pattern KeyType v a = Property "keyType" v a

pattern Kind :: Txt -> View -> View
pattern Kind v a = Property "kind" v a

pattern Label_ :: Txt -> View -> View
pattern Label_ v a = Property "label" v a

pattern Lang :: Txt -> View -> View
pattern Lang v a = Property "lang" v a

pattern List :: Txt -> View -> View
pattern List v a = Property "list" v a

pattern Loop :: View -> View
pattern Loop a = Property "loop" "true" a

pattern Low :: Txt -> View -> View
pattern Low v a = Property "low" v a

pattern Manifest :: Txt -> View -> View
pattern Manifest v a = Property "manifest" v a

pattern MarginWidth :: Txt -> View -> View
pattern MarginWidth v a = Property "marginWidth" v a

pattern MarginHeight :: Txt -> View -> View
pattern MarginHeight v a = Property "marginHeight" v a

pattern Max :: Txt -> View -> View
pattern Max v a = Property "max" v a

pattern MaxLength :: Txt -> View -> View
pattern MaxLength v a = Property "maxLength" v a

pattern Media :: Txt -> View -> View
pattern Media v a = Property "media" v a

pattern MediaGroup :: Txt -> View -> View
pattern MediaGroup v a = Property "mediaGroup" v a

pattern Method :: Txt -> View -> View
pattern Method v a = Property "method" v a

pattern Min :: Txt -> View -> View
pattern Min v a = Property "min" v a

pattern MinLength :: Txt -> View -> View
pattern MinLength v a = Property "minLength" v a

pattern Multiple :: Txt -> View -> View
pattern Multiple v a = Property "multiple" v a

pattern Muted :: View -> View
pattern Muted a = Property "muted" "true" a

pattern Name :: Txt -> View -> View
pattern Name v a = Property "name" v a

pattern Nonce :: Txt -> View -> View
pattern Nonce v a = Property "nonce" v a

pattern NoValidate :: Txt -> View -> View
pattern NoValidate v a = Property "noValidate" v a

pattern Open :: Txt -> View -> View
pattern Open v a = Property "open" v a

pattern Optimum :: Txt -> View -> View
pattern Optimum v a = Property "optimum" v a

pattern Pattern :: Txt -> View -> View
pattern Pattern v a = Property "pattern" v a

pattern Placeholder :: Txt -> View -> View
pattern Placeholder v a = Property "placeholder" v a

pattern PlaysInline :: View -> View
pattern PlaysInline a = Property "playsinline" "true" a

pattern Poster :: Txt -> View -> View
pattern Poster v a = Property "poster" v a

pattern Preload :: Txt -> View -> View
pattern Preload v a = Property "preload" v a

pattern Profile :: Txt -> View -> View
pattern Profile v a = Property "profile" v a

pattern RadioGroup :: Txt -> View -> View
pattern RadioGroup v a = Property "radiogroup" v a

pattern ReadOnly :: Txt -> View -> View
pattern ReadOnly v a = Property "readOnly" v a

pattern ReferrerPolicy :: Txt -> View -> View
pattern ReferrerPolicy v a = Property "referrerPolicy" v a

pattern Rel :: Txt -> View -> View
pattern Rel v a = Property "rel" v a

pattern Required :: Txt -> View -> View
pattern Required v a = Property "required" v a

pattern Reversed :: Txt -> View -> View
pattern Reversed v a = Property "reversed" v a

pattern Role :: Txt -> View -> View
pattern Role v a = Property "role" v a

pattern Rows :: Txt -> View -> View
pattern Rows v a = Property "rows" v a

pattern RowSpan :: Txt -> View -> View
pattern RowSpan v a = Property "rowSpan" v a

pattern Sandbox :: Txt -> View -> View
pattern Sandbox v a = Property "sandbox" v a

pattern Scope :: Txt -> View -> View
pattern Scope v a = Property "scope" v a

pattern Scoped :: Txt -> View -> View
pattern Scoped v a = Property "scoped" v a

pattern Scrolling :: Txt -> View -> View
pattern Scrolling v a = Property "scrolling" v a

pattern Seamless :: Txt -> View -> View
pattern Seamless v a = Property "seamless" v a

pattern Shape :: Txt -> View -> View
pattern Shape v a = Property "shape" v a

pattern Size :: Txt -> View -> View
pattern Size v a = Property "size" v a

-- pattern Sizes :: Txt -> View -> View
-- pattern Sizes v a = Property "sizes" v a

pattern Span_ :: Txt -> View -> View
pattern Span_ v a = Property "span" v a

pattern Src :: Txt -> View -> View
pattern Src v a = Property "src" v a

pattern SrcDoc :: Txt -> View -> View
pattern SrcDoc v a = Property "srcdoc" v a

pattern SrcLang :: Txt -> View -> View
pattern SrcLang v a = Property "srclang" v a

pattern SrcSet :: Txt -> View -> View
pattern SrcSet v a = Property "srcset" v a

pattern Start :: Txt -> View -> View
pattern Start v a = Property "start" v a

pattern Step :: Txt -> View -> View
pattern Step v a = Property "step" v a

pattern Style :: Txt -> View -> View
pattern Style v a = Property "style" v a

pattern Summary_ :: Txt -> View -> View
pattern Summary_ v a = Property "summary" v a

pattern TabIndex :: Txt -> View -> View
pattern TabIndex v a = Property "tabIndex" v a

pattern Target :: Txt -> View -> View
pattern Target v a = Property "target" v a

pattern Title :: Txt -> View -> View
pattern Title v a = Property "title" v a

pattern Type :: Txt -> View -> View
pattern Type v a = Property "type" v a

pattern UseMap :: Txt -> View -> View
pattern UseMap v a = Property "useMap" v a

pattern Value :: Txt -> View -> View
pattern Value v a = Property "value" v a

-- pattern Width :: Txt -> View -> View
-- pattern Width v a = Property "width" v a

pattern Wmode :: Txt -> View -> View
pattern Wmode v a = Property "wmode" v a

pattern Wrap :: Txt -> View -> View
pattern Wrap v a = Property "wrap" v a

