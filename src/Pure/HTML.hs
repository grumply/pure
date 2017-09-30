{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
module Pure.HTML (module Pure.HTML, module Export) where

import Pure.Types

import Pure.Attributes as Export
import Pure.CSS        as Export hiding (Alt,intercept)

pattern Abbr :: [Feature e] -> [View e] -> View e
pattern Abbr fs cs <- HTMLView mn "abbr" fs cs where
  Abbr fs cs = mkHTML "abbr" fs cs

pattern Address :: [Feature e] -> [View e] -> View e
pattern Address fs cs <- HTMLView mn "address" fs cs where
  Address fs cs = mkHTML "address" fs cs

pattern Area :: [Feature e] -> [View e] -> View e
pattern Area fs cs <- HTMLView mn "area" fs cs where
  Area fs cs = mkHTML "area" fs cs

pattern A :: [Feature e] -> [View e] -> View e
pattern A fs cs <- HTMLView mn "a" fs cs where
  A fs cs = mkHTML "a" fs cs

pattern Article :: [Feature e] -> [View e] -> View e
pattern Article fs cs <- HTMLView mn "article" fs cs where
  Article fs cs = mkHTML "article" fs cs

pattern Aside :: [Feature e] -> [View e] -> View e
pattern Aside fs cs <- HTMLView mn "aside" fs cs where
  Aside fs cs = mkHTML "aside" fs cs

pattern Audio :: [Feature e] -> [View e] -> View e
pattern Audio fs cs <- HTMLView mn "audio" fs cs where
  Audio fs cs = mkHTML "audio" fs cs

pattern Base :: [Feature e] -> [View e] -> View e
pattern Base fs cs <- HTMLView mn "base" fs cs where
  Base fs cs = mkHTML "base" fs cs

pattern Bdi :: [Feature e] -> [View e] -> View e
pattern Bdi fs cs <- HTMLView mn "bdi" fs cs where
  Bdi fs cs = mkHTML "bdi" fs cs

pattern Bdo :: [Feature e] -> [View e] -> View e
pattern Bdo fs cs <- HTMLView mn "bdo" fs cs where
  Bdo fs cs = mkHTML "bdo" fs cs

pattern Big :: [Feature e] -> [View e] -> View e
pattern Big fs cs <- HTMLView mn "big" fs cs where
  Big fs cs = mkHTML "big" fs cs

pattern Blockquote :: [Feature e] -> [View e] -> View e
pattern Blockquote fs cs <- HTMLView mn "blockquote" fs cs where
  Blockquote fs cs = mkHTML "blockquote" fs cs

pattern Body :: [Feature e] -> [View e] -> View e
pattern Body fs cs <- HTMLView mn "body" fs cs where
  Body fs cs = mkHTML "body" fs cs

pattern Bold :: [Feature e] -> [View e] -> View e
pattern Bold fs cs <- HTMLView mn "b" fs cs where
  Bold fs cs = mkHTML "b" fs cs

pattern Br :: [Feature e] -> [View e] -> View e
pattern Br fs cs <- HTMLView mn "br" fs cs where
  Br fs cs = mkHTML "br" fs cs

pattern Button :: [Feature e] -> [View e] -> View e
pattern Button fs cs <- HTMLView mn "button" fs cs where
  Button fs cs = mkHTML "button" fs cs

pattern Canvas :: [Feature e] -> [View e] -> View e
pattern Canvas fs cs <- HTMLView mn "canvas" fs cs where
  Canvas fs cs = mkHTML "canvas" fs cs

pattern Caption :: [Feature e] -> [View e] -> View e
pattern Caption fs cs <- HTMLView mn "caption" fs cs where
  Caption fs cs = mkHTML "caption" fs cs

pattern Cite :: [Feature e] -> [View e] -> View e
pattern Cite fs cs <- HTMLView mn "cite" fs cs where
  Cite fs cs = mkHTML "cite" fs cs

pattern Code :: [Feature e] -> [View e] -> View e
pattern Code fs cs <- HTMLView mn "code" fs cs where
  Code fs cs = mkHTML "code" fs cs

pattern Col :: [Feature e] -> [View e] -> View e
pattern Col fs cs <- HTMLView mn "col" fs cs where
  Col fs cs = mkHTML "col" fs cs

pattern Colgroup :: [Feature e] -> [View e] -> View e
pattern Colgroup fs cs <- HTMLView mn "colgroup" fs cs where
  Colgroup fs cs = mkHTML "colgroup" fs cs

pattern Data :: [Feature e] -> [View e] -> View e
pattern Data fs cs <- HTMLView mn "data" fs cs where
  Data fs cs = mkHTML "data" fs cs

pattern Datalist :: [Feature e] -> [View e] -> View e
pattern Datalist fs cs <- HTMLView mn "datalist" fs cs where
  Datalist fs cs = mkHTML "datalist" fs cs

pattern Dd :: [Feature e] -> [View e] -> View e
pattern Dd fs cs <- HTMLView mn "dd" fs cs where
  Dd fs cs = mkHTML "dd" fs cs

pattern Description :: [Feature e] -> [View e] -> View e
pattern Description fs cs <- HTMLView mn "description" fs cs where
  Description fs cs = mkHTML "description" fs cs

pattern Dl :: [Feature e] -> [View e] -> View e
pattern Dl fs cs <- HTMLView mn "dl" fs cs where
  Dl fs cs = mkHTML "dl" fs cs

pattern Dt :: [Feature e] -> [View e] -> View e
pattern Dt fs cs <- HTMLView mn "dt" fs cs where
  Dt fs cs = mkHTML "dt" fs cs

pattern Del :: [Feature e] -> [View e] -> View e
pattern Del fs cs <- HTMLView mn "del" fs cs where
  Del fs cs = mkHTML "del" fs cs

pattern Details :: [Feature e] -> [View e] -> View e
pattern Details fs cs <- HTMLView mn "details" fs cs where
  Details fs cs = mkHTML "details" fs cs

pattern Dfn :: [Feature e] -> [View e] -> View e
pattern Dfn fs cs <- HTMLView mn "dfn" fs cs where
  Dfn fs cs = mkHTML "dfn" fs cs

pattern Dialog :: [Feature e] -> [View e] -> View e
pattern Dialog fs cs <- HTMLView mn "dialog" fs cs where
  Dialog fs cs = mkHTML "dialog" fs cs

pattern Div :: [Feature e] -> [View e] -> View e
pattern Div fs cs <- HTMLView mn "div" fs cs where
  Div fs cs = mkHTML "div" fs cs

pattern Em :: [Feature e] -> [View e] -> View e
pattern Em fs cs <- HTMLView mn "em" fs cs where
  Em fs cs = mkHTML "em" fs cs

pattern Embed :: [Feature e] -> [View e] -> View e
pattern Embed fs cs <- HTMLView mn "embed" fs cs where
  Embed fs cs = mkHTML "embed" fs cs

pattern Fieldset :: [Feature e] -> [View e] -> View e
pattern Fieldset fs cs <- HTMLView mn "fieldset" fs cs where
  Fieldset fs cs = mkHTML "fieldset" fs cs

pattern Figcaption :: [Feature e] -> [View e] -> View e
pattern Figcaption fs cs <- HTMLView mn "figcaption" fs cs where
  Figcaption fs cs = mkHTML "figcaption" fs cs

pattern Figure :: [Feature e] -> [View e] -> View e
pattern Figure fs cs <- HTMLView mn "figure" fs cs where
  Figure fs cs = mkHTML "figure" fs cs

pattern Footer :: [Feature e] -> [View e] -> View e
pattern Footer fs cs <- HTMLView mn "footer" fs cs where
  Footer fs cs = mkHTML "footer" fs cs

pattern Form :: [Feature e] -> [View e] -> View e
pattern Form fs cs <- HTMLView mn "form" fs cs where
  Form fs cs = mkHTML "form" fs cs

pattern Frame :: [Feature e] -> [View e] -> View e
pattern Frame fs cs <- HTMLView mn "frame" fs cs where
  Frame fs cs = mkHTML "frame" fs cs

pattern Head :: [Feature e] -> [View e] -> View e
pattern Head fs cs <- HTMLView mn "head" fs cs where
  Head fs cs = mkHTML "head" fs cs

pattern Header :: [Feature e] -> [View e] -> View e
pattern Header fs cs <- HTMLView mn "header" fs cs where
  Header fs cs = mkHTML "header" fs cs

pattern H1 :: [Feature e] -> [View e] -> View e
pattern H1 fs cs <- HTMLView mn "h1" fs cs where
  H1 fs cs = mkHTML "h1" fs cs

pattern H2 :: [Feature e] -> [View e] -> View e
pattern H2 fs cs <- HTMLView mn "h2" fs cs where
  H2 fs cs = mkHTML "h2" fs cs

pattern H3 :: [Feature e] -> [View e] -> View e
pattern H3 fs cs <- HTMLView mn "h3" fs cs where
  H3 fs cs = mkHTML "h3" fs cs

pattern H4 :: [Feature e] -> [View e] -> View e
pattern H4 fs cs <- HTMLView mn "h4" fs cs where
  H4 fs cs = mkHTML "h4" fs cs

pattern H5 :: [Feature e] -> [View e] -> View e
pattern H5 fs cs <- HTMLView mn "h5" fs cs where
  H5 fs cs = mkHTML "h5" fs cs

pattern H6 :: [Feature e] -> [View e] -> View e
pattern H6 fs cs <- HTMLView mn "h6" fs cs where
  H6 fs cs = mkHTML "h6" fs cs

pattern Hgroup :: [Feature e] -> [View e] -> View e
pattern Hgroup fs cs <- HTMLView mn "hgroup" fs cs where
  Hgroup fs cs = mkHTML "hgroup" fs cs

pattern Hr :: [Feature e] -> [View e] -> View e
pattern Hr fs cs <- HTMLView mn "hr" fs cs where
  Hr fs cs = mkHTML "hr" fs cs

pattern Html :: [Feature e] -> [View e] -> View e
pattern Html fs cs <- HTMLView mn "html" fs cs where
  Html fs cs = mkHTML "html" fs cs

pattern Iframe :: [Feature e] -> [View e] -> View e
pattern Iframe fs cs <- HTMLView mn "iframe" fs cs where
  Iframe fs cs = mkHTML "iframe" fs cs

pattern Img :: [Feature e] -> [View e] -> View e
pattern Img fs cs <- HTMLView mn "img" fs cs where
  Img fs cs = mkHTML "img" fs cs

pattern Input :: [Feature e] -> [View e] -> View e
pattern Input fs cs <- HTMLView mn "input" fs cs where
  Input fs cs = mkHTML "input" fs cs

pattern Ins :: [Feature e] -> [View e] -> View e
pattern Ins fs cs <- HTMLView mn "ins" fs cs where
  Ins fs cs = mkHTML "ins" fs cs

pattern I :: [Feature e] -> [View e] -> View e
pattern I fs cs <- HTMLView mn "i" fs cs where
  I fs cs = mkHTML "i" fs cs

pattern Kbd :: [Feature e] -> [View e] -> View e
pattern Kbd fs cs <- HTMLView mn "kbd" fs cs where
  Kbd fs cs = mkHTML "kbd" fs cs

pattern Keygen :: [Feature e] -> [View e] -> View e
pattern Keygen fs cs <- HTMLView mn "keygen" fs cs where
  Keygen fs cs = mkHTML "keygen" fs cs

pattern Label :: [Feature e] -> [View e] -> View e
pattern Label fs cs <- HTMLView mn "label" fs cs where
  Label fs cs = mkHTML "label" fs cs

pattern Legend :: [Feature e] -> [View e] -> View e
pattern Legend fs cs <- HTMLView mn "legend" fs cs where
  Legend fs cs = mkHTML "legend" fs cs

pattern Li :: [Feature e] -> [View e] -> View e
pattern Li fs cs <- HTMLView mn "li" fs cs where
  Li fs cs = mkHTML "li" fs cs

pattern Link :: [Feature e] -> [View e] -> View e
pattern Link fs cs <- HTMLView mn "link" fs cs where
  Link fs cs = mkHTML "link" fs cs

pattern Main :: [Feature e] -> [View e] -> View e
pattern Main fs cs <- HTMLView mn "main" fs cs where
  Main fs cs = mkHTML "main" fs cs

pattern Map :: [Feature e] -> [View e] -> View e
pattern Map fs cs <- HTMLView mn "map" fs cs where
  Map fs cs = mkHTML "map" fs cs

pattern Mark :: [Feature e] -> [View e] -> View e
pattern Mark fs cs <- HTMLView mn "mark" fs cs where
  Mark fs cs = mkHTML "mark" fs cs

pattern Menu :: [Feature e] -> [View e] -> View e
pattern Menu fs cs <- HTMLView mn "menu" fs cs where
  Menu fs cs = mkHTML "menu" fs cs

pattern Menuitem :: [Feature e] -> [View e] -> View e
pattern Menuitem fs cs <- HTMLView mn "menuitem" fs cs where
  Menuitem fs cs = mkHTML "menuitem" fs cs

pattern Meta :: [Feature e] -> [View e] -> View e
pattern Meta fs cs <- HTMLView mn "meta" fs cs where
  Meta fs cs = mkHTML "meta" fs cs

pattern Meter :: [Feature e] -> [View e] -> View e
pattern Meter fs cs <- HTMLView mn "meter" fs cs where
  Meter fs cs = mkHTML "meter" fs cs

pattern Nav :: [Feature e] -> [View e] -> View e
pattern Nav fs cs <- HTMLView mn "nav" fs cs where
  Nav fs cs = mkHTML "nav" fs cs

pattern Noscript :: [Feature e] -> [View e] -> View e
pattern Noscript fs cs <- HTMLView mn "noscript" fs cs where
  Noscript fs cs = mkHTML "noscript" fs cs

pattern Obj :: [Feature e] -> [View e] -> View e
pattern Obj fs cs <- HTMLView mn "object" fs cs where
  Obj fs cs = mkHTML "object" fs cs

pattern Optgroup :: [Feature e] -> [View e] -> View e
pattern Optgroup fs cs <- HTMLView mn "optgroup" fs cs where
  Optgroup fs cs = mkHTML "optgroup" fs cs

pattern Option :: [Feature e] -> [View e] -> View e
pattern Option fs cs <- HTMLView mn "option" fs cs where
  Option fs cs = mkHTML "option" fs cs

pattern Ol :: [Feature e] -> [View e] -> View e
pattern Ol fs cs <- HTMLView mn "ol" fs cs where
  Ol fs cs = mkHTML "ol" fs cs

pattern Output :: [Feature e] -> [View e] -> View e
pattern Output fs cs <- HTMLView mn "output" fs cs where
  Output fs cs = mkHTML "output" fs cs

pattern P :: [Feature e] -> [View e] -> View e
pattern P fs cs <- HTMLView mn "p" fs cs where
  P fs cs = mkHTML "p" fs cs

pattern Param :: [Feature e] -> [View e] -> View e
pattern Param fs cs <- HTMLView mn "param" fs cs where
  Param fs cs = mkHTML "param" fs cs

pattern Picture :: [Feature e] -> [View e] -> View e
pattern Picture fs cs <- HTMLView mn "picture" fs cs where
  Picture fs cs = mkHTML "picture" fs cs

pattern Pre :: [Feature e] -> [View e] -> View e
pattern Pre fs cs <- HTMLView mn "pre" fs cs where
  Pre fs cs = mkHTML "pre" fs cs

pattern Progress :: [Feature e] -> [View e] -> View e
pattern Progress fs cs <- HTMLView mn "progress" fs cs where
  Progress fs cs = mkHTML "progress" fs cs

pattern Q :: [Feature e] -> [View e] -> View e
pattern Q fs cs <- HTMLView mn "q" fs cs where
  Q fs cs = mkHTML "q" fs cs

pattern Rp :: [Feature e] -> [View e] -> View e
pattern Rp fs cs <- HTMLView mn "rp" fs cs where
  Rp fs cs = mkHTML "rp" fs cs

pattern Rt :: [Feature e] -> [View e] -> View e
pattern Rt fs cs <- HTMLView mn "rt" fs cs where
  Rt fs cs = mkHTML "rt" fs cs

pattern Ruby :: [Feature e] -> [View e] -> View e
pattern Ruby fs cs <- HTMLView mn "ruby" fs cs where
  Ruby fs cs = mkHTML "ruby" fs cs

pattern Samp :: [Feature e] -> [View e] -> View e
pattern Samp fs cs <- HTMLView mn "samp" fs cs where
  Samp fs cs = mkHTML "samp" fs cs

pattern Script :: [Feature e] -> [View e] -> View e
pattern Script fs cs <- HTMLView mn "script" fs cs where
  Script fs cs = mkHTML "script" fs cs

pattern S :: [Feature e] -> [View e] -> View e
pattern S fs cs <- HTMLView mn "s" fs cs where
  S fs cs = mkHTML "s" fs cs

pattern Section :: [Feature e] -> [View e] -> View e
pattern Section fs cs <- HTMLView mn "section" fs cs where
  Section fs cs = mkHTML "section" fs cs

pattern Select :: [Feature e] -> [View e] -> View e
pattern Select fs cs <- HTMLView mn "select" fs cs where
  Select fs cs = mkHTML "select" fs cs

pattern Small :: [Feature e] -> [View e] -> View e
pattern Small fs cs <- HTMLView mn "small" fs cs where
  Small fs cs = mkHTML "small" fs cs

pattern Source :: [Feature e] -> [View e] -> View e
pattern Source fs cs <- HTMLView mn "source" fs cs where
  Source fs cs = mkHTML "source" fs cs

pattern Span :: [Feature e] -> [View e] -> View e
pattern Span fs cs <- HTMLView mn "span" fs cs where
  Span fs cs = mkHTML "span" fs cs

pattern Strong :: [Feature e] -> [View e] -> View e
pattern Strong fs cs <- HTMLView mn "strong" fs cs where
  Strong fs cs = mkHTML "strong" fs cs

pattern Style :: [Feature e] -> [View e] -> View e
pattern Style fs cs <- HTMLView mn "style" fs cs where
  Style fs cs = mkHTML "style" fs cs

pattern Sub :: [Feature e] -> [View e] -> View e
pattern Sub fs cs <- HTMLView mn "sub" fs cs where
  Sub fs cs = mkHTML "sub" fs cs

pattern Summary :: [Feature e] -> [View e] -> View e
pattern Summary fs cs <- HTMLView mn "summary" fs cs where
  Summary fs cs = mkHTML "summary" fs cs

pattern Sup :: [Feature e] -> [View e] -> View e
pattern Sup fs cs <- HTMLView mn "sup" fs cs where
  Sup fs cs = mkHTML "sup" fs cs

pattern Table :: [Feature e] -> [View e] -> View e
pattern Table fs cs <- HTMLView mn "table" fs cs where
  Table fs cs = mkHTML "table" fs cs

pattern Tbody :: [Feature e] -> [View e] -> View e
pattern Tbody fs cs <- HTMLView mn "tbody" fs cs where
  Tbody fs cs = mkHTML "tbody" fs cs

pattern Td :: [Feature e] -> [View e] -> View e
pattern Td fs cs <- HTMLView mn "td" fs cs where
  Td fs cs = mkHTML "td" fs cs

pattern Textarea :: [Feature e] -> [View e] -> View e
pattern Textarea fs cs <- HTMLView mn "textarea" fs cs where
  Textarea fs cs = mkHTML "textarea" fs cs

pattern Tfoot :: [Feature e] -> [View e] -> View e
pattern Tfoot fs cs <- HTMLView mn "tfoot" fs cs where
  Tfoot fs cs = mkHTML "tfoot" fs cs

pattern Th :: [Feature e] -> [View e] -> View e
pattern Th fs cs <- HTMLView mn "th" fs cs where
  Th fs cs = mkHTML "th" fs cs

pattern Thead :: [Feature e] -> [View e] -> View e
pattern Thead fs cs <- HTMLView mn "thead" fs cs where
  Thead fs cs = mkHTML "thead" fs cs

pattern Time :: [Feature e] -> [View e] -> View e
pattern Time fs cs <- HTMLView mn "time" fs cs where
  Time fs cs = mkHTML "time" fs cs

pattern Title :: [Feature e] -> [View e] -> View e
pattern Title fs cs <- HTMLView mn "title" fs cs where
  Title fs cs = mkHTML "title" fs cs

pattern Tr :: [Feature e] -> [View e] -> View e
pattern Tr fs cs <- HTMLView mn "tr" fs cs where
  Tr fs cs = mkHTML "tr" fs cs

pattern Track :: [Feature e] -> [View e] -> View e
pattern Track fs cs <- HTMLView mn "track" fs cs where
  Track fs cs = mkHTML "track" fs cs

pattern U :: [Feature e] -> [View e] -> View e
pattern U fs cs <- HTMLView mn "u" fs cs where
  U fs cs = mkHTML "u" fs cs

pattern Ul :: [Feature e] -> [View e] -> View e
pattern Ul fs cs <- HTMLView mn "ul" fs cs where
  Ul fs cs = mkHTML "ul" fs cs

pattern Var :: [Feature e] -> [View e] -> View e
pattern Var fs cs <- HTMLView mn "var" fs cs where
  Var fs cs = mkHTML "var" fs cs

pattern Video :: [Feature e] -> [View e] -> View e
pattern Video fs cs <- HTMLView mn "video" fs cs where
  Video fs cs = mkHTML "video" fs cs

pattern Viewport :: [Feature e] -> [View e] -> View e
pattern Viewport fs cs <- HTMLView mn "viewport" fs cs where
  Viewport fs cs = mkHTML "viewport" fs cs

pattern Wbr :: [Feature e] -> [View e] -> View e
pattern Wbr fs cs <- HTMLView mn "wbr" fs cs where
  Wbr fs cs = mkHTML "wbr" fs cs

