{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
module Atomic.HTML where

import Ef.Base

import Data.Txt (Txt)

import Data.Typeable

import Atomic.Attribute
import Atomic.Component

pattern Abbr fs cs <- (fromAtom -> Just (HTML mn "abbr" fs cs)) where
  Abbr fs cs = mkAtom "abbr" fs cs

pattern Address fs cs <- (fromAtom -> Just (HTML mn "address" fs cs)) where
  Address fs cs = mkAtom "address" fs cs

pattern Area fs cs <- (fromAtom -> Just (HTML mn "area" fs cs)) where
  Area fs cs = mkAtom "area" fs cs

pattern A fs cs <- (fromAtom -> Just (HTML mn "a" fs cs)) where
  A fs cs = mkAtom "a" fs cs

pattern Article fs cs <- (fromAtom -> Just (HTML mn "article" fs cs)) where
  Article fs cs = mkAtom "article" fs cs

pattern Aside fs cs <- (fromAtom -> Just (HTML mn "aside" fs cs)) where
  Aside fs cs = mkAtom "aside" fs cs

pattern Audio fs cs <- (fromAtom -> Just (HTML mn "audio" fs cs)) where
  Audio fs cs = mkAtom "audio" fs cs

pattern Base fs cs <- (fromAtom -> Just (HTML mn "base" fs cs)) where
  Base fs cs = mkAtom "base" fs cs

pattern Bdi fs cs <- (fromAtom -> Just (HTML mn "bdi" fs cs)) where
  Bdi fs cs = mkAtom "bdi" fs cs

pattern Bdo fs cs <- (fromAtom -> Just (HTML mn "bdo" fs cs)) where
  Bdo fs cs = mkAtom "bdo" fs cs

pattern Big fs cs <- (fromAtom -> Just (HTML mn "big" fs cs)) where
  Big fs cs = mkAtom "big" fs cs

pattern Blockquote fs cs <- (fromAtom -> Just (HTML mn "blockquote" fs cs)) where
  Blockquote fs cs = mkAtom "blockquote" fs cs

pattern Body fs cs <- (fromAtom -> Just (HTML mn "body" fs cs)) where
  Body fs cs = mkAtom "body" fs cs

pattern B fs cs <- (fromAtom -> Just (HTML mn "b" fs cs)) where
  B fs cs = mkAtom "b" fs cs

pattern Br fs cs <- (fromAtom -> Just (HTML mn "br" fs cs)) where
  Br fs cs = mkAtom "br" fs cs

pattern Button fs cs <- (fromAtom -> Just (HTML mn "button" fs cs)) where
  Button fs cs = mkAtom "button" fs cs

pattern Canvas fs cs <- (fromAtom -> Just (HTML mn "canvas" fs cs)) where
  Canvas fs cs = mkAtom "canvas" fs cs

pattern Caption fs cs <- (fromAtom -> Just (HTML mn "caption" fs cs)) where
  Caption fs cs = mkAtom "caption" fs cs

pattern Cite fs cs <- (fromAtom -> Just (HTML mn "cite" fs cs)) where
  Cite fs cs = mkAtom "cite" fs cs

pattern Code fs cs <- (fromAtom -> Just (HTML mn "code" fs cs)) where
  Code fs cs = mkAtom "code" fs cs

pattern Col fs cs <- (fromAtom -> Just (HTML mn "col" fs cs)) where
  Col fs cs = mkAtom "col" fs cs

pattern Colgroup fs cs <- (fromAtom -> Just (HTML mn "colgroup" fs cs)) where
  Colgroup fs cs = mkAtom "colgroup" fs cs

pattern Data fs cs <- (fromAtom -> Just (HTML mn "data" fs cs)) where
  Data fs cs = mkAtom "data" fs cs

pattern Datalist fs cs <- (fromAtom -> Just (HTML mn "datalist" fs cs)) where
  Datalist fs cs = mkAtom "datalist" fs cs

pattern Dd fs cs <- (fromAtom -> Just (HTML mn "dd" fs cs)) where
  Dd fs cs = mkAtom "dd" fs cs

pattern Description fs cs <- (fromAtom -> Just (HTML mn "description" fs cs)) where
  Description fs cs = mkAtom "description" fs cs

pattern Dl fs cs <- (fromAtom -> Just (HTML mn "dl" fs cs)) where
  Dl fs cs = mkAtom "dl" fs cs

pattern Dt fs cs <- (fromAtom -> Just (HTML mn "dt" fs cs)) where
  Dt fs cs = mkAtom "dt" fs cs

pattern Del fs cs <- (fromAtom -> Just (HTML mn "del" fs cs)) where
  Del fs cs = mkAtom "del" fs cs

pattern Details fs cs <- (fromAtom -> Just (HTML mn "details" fs cs)) where
  Details fs cs = mkAtom "details" fs cs

pattern Dfn fs cs <- (fromAtom -> Just (HTML mn "dfn" fs cs)) where
  Dfn fs cs = mkAtom "dfn" fs cs

pattern Dialog fs cs <- (fromAtom -> Just (HTML mn "dialog" fs cs)) where
  Dialog fs cs = mkAtom "dialog" fs cs

pattern Div fs cs <- (fromAtom -> Just (HTML mn "div" fs cs)) where
  Div fs cs = mkAtom "div" fs cs

pattern Em fs cs <- (fromAtom -> Just (HTML mn "em" fs cs)) where
  Em fs cs = mkAtom "em" fs cs

pattern Embed fs cs <- (fromAtom -> Just (HTML mn "embed" fs cs)) where
  Embed fs cs = mkAtom "embed" fs cs

pattern Fieldset fs cs <- (fromAtom -> Just (HTML mn "fieldset" fs cs)) where
  Fieldset fs cs = mkAtom "fieldset" fs cs

pattern Figcaption fs cs <- (fromAtom -> Just (HTML mn "figcaption" fs cs)) where
  Figcaption fs cs = mkAtom "figcaption" fs cs

pattern Figure fs cs <- (fromAtom -> Just (HTML mn "figure" fs cs)) where
  Figure fs cs = mkAtom "figure" fs cs

pattern Footer fs cs <- (fromAtom -> Just (HTML mn "footer" fs cs)) where
  Footer fs cs = mkAtom "footer" fs cs

pattern Form fs cs <- (fromAtom -> Just (HTML mn "form" fs cs)) where
  Form fs cs = mkAtom "form" fs cs

pattern Frame fs cs <- (fromAtom -> Just (HTML mn "frame" fs cs)) where
  Frame fs cs = mkAtom "frame" fs cs

pattern Head fs cs <- (fromAtom -> Just (HTML mn "head" fs cs)) where
  Head fs cs = mkAtom "head" fs cs

pattern Header fs cs <- (fromAtom -> Just (HTML mn "header" fs cs)) where
  Header fs cs = mkAtom "header" fs cs

pattern H1 fs cs <- (fromAtom -> Just (HTML mn "h1" fs cs)) where
  H1 fs cs = mkAtom "h1" fs cs

pattern H2 fs cs <- (fromAtom -> Just (HTML mn "h2" fs cs)) where
  H2 fs cs = mkAtom "h2" fs cs

pattern H3 fs cs <- (fromAtom -> Just (HTML mn "h3" fs cs)) where
  H3 fs cs = mkAtom "h3" fs cs

pattern H4 fs cs <- (fromAtom -> Just (HTML mn "h4" fs cs)) where
  H4 fs cs = mkAtom "h4" fs cs

pattern H5 fs cs <- (fromAtom -> Just (HTML mn "h5" fs cs)) where
  H5 fs cs = mkAtom "h5" fs cs

pattern H6 fs cs <- (fromAtom -> Just (HTML mn "h6" fs cs)) where
  H6 fs cs = mkAtom "h6" fs cs

pattern Hgroup fs cs <- (fromAtom -> Just (HTML mn "hgroup" fs cs)) where
  Hgroup fs cs = mkAtom "hgroup" fs cs

pattern Hr fs cs <- (fromAtom -> Just (HTML mn "hr" fs cs)) where
  Hr fs cs = mkAtom "hr" fs cs

pattern Html fs cs <- (fromAtom -> Just (HTML mn "html" fs cs)) where
  Html fs cs = mkAtom "html" fs cs

pattern Iframe fs cs <- (fromAtom -> Just (HTML mn "iframe" fs cs)) where
  Iframe fs cs = mkAtom "iframe" fs cs

pattern Img fs cs <- (fromAtom -> Just (HTML mn "img" fs cs)) where
  Img fs cs = mkAtom "img" fs cs

pattern Input fs cs <- (fromAtom -> Just (HTML mn "input" fs cs)) where
  Input fs cs = mkAtom "input" fs cs

pattern Ins fs cs <- (fromAtom -> Just (HTML mn "ins" fs cs)) where
  Ins fs cs = mkAtom "ins" fs cs

pattern I fs cs <- (fromAtom -> Just (HTML mn "i" fs cs)) where
  I fs cs = mkAtom "i" fs cs

pattern Kbd fs cs <- (fromAtom -> Just (HTML mn "kbd" fs cs)) where
  Kbd fs cs = mkAtom "kbd" fs cs

pattern Keygen fs cs <- (fromAtom -> Just (HTML mn "keygen" fs cs)) where
  Keygen fs cs = mkAtom "keygen" fs cs

pattern Label fs cs <- (fromAtom -> Just (HTML mn "label" fs cs)) where
  Label fs cs = mkAtom "label" fs cs

pattern Legend fs cs <- (fromAtom -> Just (HTML mn "legend" fs cs)) where
  Legend fs cs = mkAtom "legend" fs cs

pattern Li fs cs <- (fromAtom -> Just (HTML mn "li" fs cs)) where
  Li fs cs = mkAtom "li" fs cs

pattern Link fs cs <- (fromAtom -> Just (HTML mn "link" fs cs)) where
  Link fs cs = mkAtom "link" fs cs

pattern Main fs cs <- (fromAtom -> Just (HTML mn "main" fs cs)) where
  Main fs cs = mkAtom "main" fs cs

pattern Map fs cs <- (fromAtom -> Just (HTML mn "map" fs cs)) where
  Map fs cs = mkAtom "map" fs cs

pattern Mark fs cs <- (fromAtom -> Just (HTML mn "mark" fs cs)) where
  Mark fs cs = mkAtom "mark" fs cs

pattern Menu fs cs <- (fromAtom -> Just (HTML mn "menu" fs cs)) where
  Menu fs cs = mkAtom "menu" fs cs

pattern Menuitem fs cs <- (fromAtom -> Just (HTML mn "menuitem" fs cs)) where
  Menuitem fs cs = mkAtom "menuitem" fs cs

pattern Meta fs cs <- (fromAtom -> Just (HTML mn "meta" fs cs)) where
  Meta fs cs = mkAtom "meta" fs cs

pattern Meter fs cs <- (fromAtom -> Just (HTML mn "meter" fs cs)) where
  Meter fs cs = mkAtom "meter" fs cs

pattern Nav fs cs <- (fromAtom -> Just (HTML mn "nav" fs cs)) where
  Nav fs cs = mkAtom "nav" fs cs

pattern Noscript fs cs <- (fromAtom -> Just (HTML mn "noscript" fs cs)) where
  Noscript fs cs = mkAtom "noscript" fs cs

pattern Obj fs cs <- (fromAtom -> Just (HTML mn "object" fs cs)) where
  Obj fs cs = mkAtom "object" fs cs

pattern Optgroup fs cs <- (fromAtom -> Just (HTML mn "optgroup" fs cs)) where
  Optgroup fs cs = mkAtom "optgroup" fs cs

pattern Option fs cs <- (fromAtom -> Just (HTML mn "option" fs cs)) where
  Option fs cs = mkAtom "option" fs cs

pattern Ol fs cs <- (fromAtom -> Just (HTML mn "ol" fs cs)) where
  Ol fs cs = mkAtom "ol" fs cs

pattern Output fs cs <- (fromAtom -> Just (HTML mn "output" fs cs)) where
  Output fs cs = mkAtom "output" fs cs

pattern P fs cs <- (fromAtom -> Just (HTML mn "p" fs cs)) where
  P fs cs = mkAtom "p" fs cs

pattern Param fs cs <- (fromAtom -> Just (HTML mn "param" fs cs)) where
  Param fs cs = mkAtom "param" fs cs

pattern Picture fs cs <- (fromAtom -> Just (HTML mn "picture" fs cs)) where
  Picture fs cs = mkAtom "picture" fs cs

pattern Pre fs cs <- (fromAtom -> Just (HTML mn "pre" fs cs)) where
  Pre fs cs = mkAtom "pre" fs cs

pattern Progress fs cs <- (fromAtom -> Just (HTML mn "progress" fs cs)) where
  Progress fs cs = mkAtom "progress" fs cs

pattern Q fs cs <- (fromAtom -> Just (HTML mn "q" fs cs)) where
  Q fs cs = mkAtom "q" fs cs

pattern Rp fs cs <- (fromAtom -> Just (HTML mn "rp" fs cs)) where
  Rp fs cs = mkAtom "rp" fs cs

pattern Rt fs cs <- (fromAtom -> Just (HTML mn "rt" fs cs)) where
  Rt fs cs = mkAtom "rt" fs cs

pattern Ruby fs cs <- (fromAtom -> Just (HTML mn "ruby" fs cs)) where
  Ruby fs cs = mkAtom "ruby" fs cs

pattern Samp fs cs <- (fromAtom -> Just (HTML mn "samp" fs cs)) where
  Samp fs cs = mkAtom "samp" fs cs

pattern Script fs cs <- (fromAtom -> Just (HTML mn "script" fs cs)) where
  Script fs cs = mkAtom "script" fs cs

pattern S fs cs <- (fromAtom -> Just (HTML mn "s" fs cs)) where
  S fs cs = mkAtom "s" fs cs

pattern Section fs cs <- (fromAtom -> Just (HTML mn "section" fs cs)) where
  Section fs cs = mkAtom "section" fs cs

pattern Select fs cs <- (fromAtom -> Just (HTML mn "select" fs cs)) where
  Select fs cs = mkAtom "select" fs cs

pattern Small fs cs <- (fromAtom -> Just (HTML mn "small" fs cs)) where
  Small fs cs = mkAtom "small" fs cs

pattern Source fs cs <- (fromAtom -> Just (HTML mn "source" fs cs)) where
  Source fs cs = mkAtom "source" fs cs

pattern Span fs cs <- (fromAtom -> Just (HTML mn "span" fs cs)) where
  Span fs cs = mkAtom "span" fs cs

pattern Strong fs cs <- (fromAtom -> Just (HTML mn "strong" fs cs)) where
  Strong fs cs = mkAtom "strong" fs cs

pattern Style fs cs <- (fromAtom -> Just (HTML mn "style" fs cs)) where
  Style fs cs = mkAtom "style" fs cs

pattern Sub fs cs <- (fromAtom -> Just (HTML mn "sub" fs cs)) where
  Sub fs cs = mkAtom "sub" fs cs

pattern Summary fs cs <- (fromAtom -> Just (HTML mn "summary" fs cs)) where
  Summary fs cs = mkAtom "summary" fs cs

pattern Sup fs cs <- (fromAtom -> Just (HTML mn "sup" fs cs)) where
  Sup fs cs = mkAtom "sup" fs cs

pattern Table fs cs <- (fromAtom -> Just (HTML mn "table" fs cs)) where
  Table fs cs = mkAtom "table" fs cs

pattern Tbody fs cs <- (fromAtom -> Just (HTML mn "tbody" fs cs)) where
  Tbody fs cs = mkAtom "tbody" fs cs

pattern Td fs cs <- (fromAtom -> Just (HTML mn "td" fs cs)) where
  Td fs cs = mkAtom "td" fs cs

pattern Textarea fs cs <- (fromAtom -> Just (HTML mn "textarea" fs cs)) where
  Textarea fs cs = mkAtom "textarea" fs cs

pattern Tfoot fs cs <- (fromAtom -> Just (HTML mn "tfoot" fs cs)) where
  Tfoot fs cs = mkAtom "tfoot" fs cs

pattern Th fs cs <- (fromAtom -> Just (HTML mn "th" fs cs)) where
  Th fs cs = mkAtom "th" fs cs

pattern Thead fs cs <- (fromAtom -> Just (HTML mn "thead" fs cs)) where
  Thead fs cs = mkAtom "thead" fs cs

pattern Time fs cs <- (fromAtom -> Just (HTML mn "time" fs cs)) where
  Time fs cs = mkAtom "time" fs cs

pattern Title fs cs <- (fromAtom -> Just (HTML mn "title" fs cs)) where
  Title fs cs = mkAtom "title" fs cs

pattern Tr fs cs <- (fromAtom -> Just (HTML mn "tr" fs cs)) where
  Tr fs cs = mkAtom "tr" fs cs

pattern Track fs cs <- (fromAtom -> Just (HTML mn "track" fs cs)) where
  Track fs cs = mkAtom "track" fs cs

pattern U fs cs <- (fromAtom -> Just (HTML mn "u" fs cs)) where
  U fs cs = mkAtom "u" fs cs

pattern Ul fs cs <- (fromAtom -> Just (HTML mn "ul" fs cs)) where
  Ul fs cs = mkAtom "ul" fs cs

pattern Var fs cs <- (fromAtom -> Just (HTML mn "var" fs cs)) where
  Var fs cs = mkAtom "var" fs cs

pattern Video fs cs <- (fromAtom -> Just (HTML mn "video" fs cs)) where
  Video fs cs = mkAtom "video" fs cs

pattern Viewport fs cs <- (fromAtom -> Just (HTML mn "viewport" fs cs)) where
  Viewport fs cs = mkAtom "viewport" fs cs

pattern Wbr fs cs <- (fromAtom -> Just (HTML mn "wbr" fs cs)) where
  Wbr fs cs = mkAtom "wbr" fs cs

--------------------------------------------------------------------------------
-- SVG

pattern SVGA fs cs <- (fromAtom -> Just (SVGHTML mn "a" fs cs)) where
  SVGA fs cs = mkSVGAtom "a" fs cs

pattern SVGAudio fs cs <- (fromAtom -> Just (SVGHTML mn "audio" fs cs)) where
  SVGAudio fs cs = mkSVGAtom "audio" fs cs

pattern SVGAltGlyph fs cs <- (fromAtom -> Just (SVGHTML mn "altGlyph" fs cs)) where
  SVGAltGlyph fs cs = mkSVGAtom "altGlyph" fs cs

pattern SVGAltGlyphDef fs cs <- (fromAtom -> Just (SVGHTML mn "altGlyphDef" fs cs)) where
  SVGAltGlyphDef fs cs = mkSVGAtom "altGlyphDef" fs cs

pattern SVGAltGlyphItem fs cs <- (fromAtom -> Just (SVGHTML mn "altGlyphItem" fs cs)) where
  SVGAltGlyphItem fs cs = mkSVGAtom "altGlyphItem" fs cs

pattern SVGAnimate fs cs <- (fromAtom -> Just (SVGHTML mn "animate" fs cs)) where
  SVGAnimate fs cs = mkSVGAtom "animate" fs cs

pattern SVGAnimateColor fs cs <- (fromAtom -> Just (SVGHTML mn "animateColor" fs cs)) where
  SVGAnimateColor fs cs = mkSVGAtom "animateColor" fs cs

pattern SVGAnimateMotion fs cs <- (fromAtom -> Just (SVGHTML mn "animateMotion" fs cs)) where
  SVGAnimateMotion fs cs = mkSVGAtom "animateMotion" fs cs

pattern SVGAnimateTransform fs cs <- (fromAtom -> Just (SVGHTML mn "animateTransform" fs cs)) where
  SVGAnimateTransform fs cs = mkSVGAtom "animateTransform" fs cs

pattern SVGCanvas fs cs <- (fromAtom -> Just (SVGHTML mn "canvas" fs cs)) where
  SVGCanvas fs cs = mkSVGAtom "canvas" fs cs

pattern SVGCircle fs cs <- (fromAtom -> Just (SVGHTML mn "circle" fs cs)) where
  SVGCircle fs cs = mkSVGAtom "circle" fs cs

pattern SVGClipPath fs cs <- (fromAtom -> Just (SVGHTML mn "clipPath" fs cs)) where
  SVGClipPath fs cs = mkSVGAtom "clipPath" fs cs

pattern SVGColorProfile fs cs <- (fromAtom -> Just (SVGHTML mn "color-profile" fs cs)) where
  SVGColorProfile fs cs = mkSVGAtom "color-profile" fs cs

pattern SVGCursor fs cs <- (fromAtom -> Just (SVGHTML mn "cursor" fs cs)) where
  SVGCursor fs cs = mkSVGAtom "cursor" fs cs

pattern SVGDefs fs cs <- (fromAtom -> Just (SVGHTML mn "defs" fs cs)) where
  SVGDefs fs cs = mkSVGAtom "defs" fs cs

pattern SVGDesc fs cs <- (fromAtom -> Just (SVGHTML mn "desc" fs cs)) where
  SVGDesc fs cs = mkSVGAtom "desc" fs cs

pattern SVGDiscard fs cs <- (fromAtom -> Just (SVGHTML mn "discard" fs cs)) where
  SVGDiscard fs cs = mkSVGAtom "discard" fs cs

pattern SVGEllipse fs cs <- (fromAtom -> Just (SVGHTML mn "ellipse" fs cs)) where
  SVGEllipse fs cs = mkSVGAtom "ellipse" fs cs

pattern SVGFeBlend fs cs <- (fromAtom -> Just (SVGHTML mn "feBlend" fs cs)) where
  SVGFeBlend fs cs = mkSVGAtom "feBlend" fs cs

pattern SVGFeColorMatrix fs cs <- (fromAtom -> Just (SVGHTML mn "feColorMatrix" fs cs)) where
  SVGFeColorMatrix fs cs = mkSVGAtom "feColorMatrix" fs cs

pattern SVGFeComponentTransfer fs cs <- (fromAtom -> Just (SVGHTML mn "feComponentTransfer" fs cs)) where
  SVGFeComponentTransfer fs cs = mkSVGAtom "feComponentTransfer" fs cs

pattern SVGFeComposite fs cs <- (fromAtom -> Just (SVGHTML mn "feComposite" fs cs)) where
  SVGFeComposite fs cs = mkSVGAtom "feComposite" fs cs

pattern SVGFeConvolveMatrix fs cs <- (fromAtom -> Just (SVGHTML mn "feConvolveMatrix" fs cs)) where
  SVGFeConvolveMatrix fs cs = mkSVGAtom "feConvolveMatrix" fs cs

pattern SVGFeDiffuseLighting fs cs <- (fromAtom -> Just (SVGHTML mn "feDiffuseLighting" fs cs)) where
  SVGFeDiffuseLighting fs cs = mkSVGAtom "feDiffuseLighting" fs cs

pattern SVGFeDisplacementMap fs cs <- (fromAtom -> Just (SVGHTML mn "feDisplacementMap" fs cs)) where
  SVGFeDisplacementMap fs cs = mkSVGAtom "feDisplacementMap" fs cs

pattern SVGFeDistantLight fs cs <- (fromAtom -> Just (SVGHTML mn "feDistantLight" fs cs)) where
  SVGFeDistantLight fs cs = mkSVGAtom "feDistantLight" fs cs

pattern SVGFeDropShadow fs cs <- (fromAtom -> Just (SVGHTML mn "feDropShadow" fs cs)) where
  SVGFeDropShadow fs cs = mkSVGAtom "feDropShadow" fs cs

pattern SVGFeFlood fs cs <- (fromAtom -> Just (SVGHTML mn "feFlood" fs cs)) where
  SVGFeFlood fs cs = mkSVGAtom "feFlood" fs cs

pattern SVGFeFuncA fs cs <- (fromAtom -> Just (SVGHTML mn "feFuncA" fs cs)) where
  SVGFeFuncA fs cs = mkSVGAtom "feFuncA" fs cs

pattern SVGFeFuncB fs cs <- (fromAtom -> Just (SVGHTML mn "feFuncB" fs cs)) where
  SVGFeFuncB fs cs = mkSVGAtom "feFuncB" fs cs

pattern SVGFeFuncG fs cs <- (fromAtom -> Just (SVGHTML mn "feFuncG" fs cs)) where
  SVGFeFuncG fs cs = mkSVGAtom "feFuncG" fs cs

pattern SVGFeFuncR fs cs <- (fromAtom -> Just (SVGHTML mn "feFuncR" fs cs)) where
  SVGFeFuncR fs cs = mkSVGAtom "feFuncR" fs cs

pattern SVGFeGaussianBlur fs cs <- (fromAtom -> Just (SVGHTML mn "feGaussianBlur" fs cs)) where
  SVGFeGaussianBlur fs cs = mkSVGAtom "feGaussianBlur" fs cs

pattern SVGFeImage fs cs <- (fromAtom -> Just (SVGHTML mn "feImage" fs cs)) where
  SVGFeImage fs cs = mkSVGAtom "feImage" fs cs

pattern SVGFeMerge fs cs <- (fromAtom -> Just (SVGHTML mn "feMerge" fs cs)) where
  SVGFeMerge fs cs = mkSVGAtom "feMerge" fs cs

pattern SVGFeMergeNode fs cs <- (fromAtom -> Just (SVGHTML mn "feMergeNode" fs cs)) where
  SVGFeMergeNode fs cs = mkSVGAtom "feMergeNode" fs cs

pattern SVGFeMorphology fs cs <- (fromAtom -> Just (SVGHTML mn "feMorphology" fs cs)) where
  SVGFeMorphology fs cs = mkSVGAtom "feMorphology" fs cs

pattern SVGFeOffset fs cs <- (fromAtom -> Just (SVGHTML mn "feOffset" fs cs)) where
  SVGFeOffset fs cs = mkSVGAtom "feOffset" fs cs

pattern SVGFePointLight fs cs <- (fromAtom -> Just (SVGHTML mn "fePointLight" fs cs)) where
  SVGFePointLight fs cs = mkSVGAtom "fePointLight" fs cs

pattern SVGFeSpecularLighting fs cs <- (fromAtom -> Just (SVGHTML mn "feSpecularLighting" fs cs)) where
  SVGFeSpecularLighting fs cs = mkSVGAtom "feSpecularLighting" fs cs

pattern SVGFeSpotLight fs cs <- (fromAtom -> Just (SVGHTML mn "feSpotLight" fs cs)) where
  SVGFeSpotLight fs cs = mkSVGAtom "feSpotLight" fs cs

pattern SVGFeTile fs cs <- (fromAtom -> Just (SVGHTML mn "feTile" fs cs)) where
  SVGFeTile fs cs = mkSVGAtom "feTile" fs cs

pattern SVGFeTurbulence fs cs <- (fromAtom -> Just (SVGHTML mn "feTurbulence" fs cs)) where
  SVGFeTurbulence fs cs = mkSVGAtom "feTurbulence" fs cs

pattern SVGFilter fs cs <- (fromAtom -> Just (SVGHTML mn "filter" fs cs)) where
  SVGFilter fs cs = mkSVGAtom "filter" fs cs

pattern SVGFont fs cs <- (fromAtom -> Just (SVGHTML mn "font" fs cs)) where
  SVGFont fs cs = mkSVGAtom "font" fs cs

pattern SVGFontFace fs cs <- (fromAtom -> Just (SVGHTML mn "font-face" fs cs)) where
  SVGFontFace fs cs = mkSVGAtom "font-face" fs cs

pattern SVGFontFaceFormat fs cs <- (fromAtom -> Just (SVGHTML mn "font-face-format" fs cs)) where
  SVGFontFaceFormat fs cs = mkSVGAtom "font-face-format" fs cs

pattern SVGFontFaceName fs cs <- (fromAtom -> Just (SVGHTML mn "font-face-name" fs cs)) where
  SVGFontFaceName fs cs = mkSVGAtom "font-face-name" fs cs

pattern SVGFontFaceSrc fs cs <- (fromAtom -> Just (SVGHTML mn "font-face-src" fs cs)) where
  SVGFontFaceSrc fs cs = mkSVGAtom "font-face-src" fs cs

pattern SVGFontFaceURI fs cs <- (fromAtom -> Just (SVGHTML mn "font-face-uri" fs cs)) where
  SVGFontFaceURI fs cs = mkSVGAtom "font-face-uri" fs cs

pattern SVGForeignObject fs cs <- (fromAtom -> Just (SVGHTML mn "foreignObject" fs cs)) where
  SVGForeignObject fs cs = mkSVGAtom "foreignObject" fs cs

pattern SVGG fs cs <- (fromAtom -> Just (SVGHTML mn "g" fs cs)) where
  SVGG fs cs = mkSVGAtom "g" fs cs

pattern SVGGlyph fs cs <- (fromAtom -> Just (SVGHTML mn "glyph" fs cs)) where
  SVGGlyph fs cs = mkSVGAtom "glyph" fs cs

pattern SVGGlyphRef fs cs <- (fromAtom -> Just (SVGHTML mn "glyphRef" fs cs)) where
  SVGGlyphRef fs cs = mkSVGAtom "glyphRef" fs cs

pattern SVGHatch fs cs <- (fromAtom -> Just (SVGHTML mn "hatch" fs cs)) where
  SVGHatch fs cs = mkSVGAtom "hatch" fs cs

pattern SVGHatchpath fs cs <- (fromAtom -> Just (SVGHTML mn "hatchpath" fs cs)) where
  SVGHatchpath fs cs = mkSVGAtom "hatchpath" fs cs

pattern SVGHkern fs cs <- (fromAtom -> Just (SVGHTML mn "hkern" fs cs)) where
  SVGHkern fs cs = mkSVGAtom "hkern" fs cs

pattern SVGIframe fs cs <- (fromAtom -> Just (SVGHTML mn "iframe" fs cs)) where
  SVGIframe fs cs = mkSVGAtom "iframe" fs cs

pattern SVGImage fs cs <- (fromAtom -> Just (SVGHTML mn "image" fs cs)) where
  SVGImage fs cs = mkSVGAtom "image" fs cs

pattern SVGLine fs cs <- (fromAtom -> Just (SVGHTML mn "line" fs cs)) where
  SVGLine fs cs = mkSVGAtom "line" fs cs

pattern SVGLinearGradient fs cs <- (fromAtom -> Just (SVGHTML mn "linearGradient" fs cs)) where
  SVGLinearGradient fs cs = mkSVGAtom "linearGradient" fs cs

pattern SVGMarker fs cs <- (fromAtom -> Just (SVGHTML mn "marker" fs cs)) where
  SVGMarker fs cs = mkSVGAtom "marker" fs cs

pattern SVGMask fs cs <- (fromAtom -> Just (SVGHTML mn "mask" fs cs)) where
  SVGMask fs cs = mkSVGAtom "mask" fs cs

pattern SVGMesh fs cs <- (fromAtom -> Just (SVGHTML mn "mesh" fs cs)) where
  SVGMesh fs cs = mkSVGAtom "mesh" fs cs

pattern SVGMeshgradient fs cs <- (fromAtom -> Just (SVGHTML mn "meshgradient" fs cs)) where
  SVGMeshgradient fs cs = mkSVGAtom "meshgradient" fs cs

pattern SVGMeshpatch fs cs <- (fromAtom -> Just (SVGHTML mn "meshpatch" fs cs)) where
  SVGMeshpatch fs cs = mkSVGAtom "meshpatch" fs cs

pattern SVGMeshrow fs cs <- (fromAtom -> Just (SVGHTML mn "meshrow" fs cs)) where
  SVGMeshrow fs cs = mkSVGAtom "meshrow" fs cs

pattern SVGMetadata fs cs <- (fromAtom -> Just (SVGHTML mn "metadata" fs cs)) where
  SVGMetadata fs cs = mkSVGAtom "metadata" fs cs

pattern SVGMissingGlyph fs cs <- (fromAtom -> Just (SVGHTML mn "missing-glyph" fs cs)) where
  SVGMissingGlyph fs cs = mkSVGAtom "missing-glyph" fs cs

pattern SVGMpath fs cs <- (fromAtom -> Just (SVGHTML mn "mpath" fs cs)) where
  SVGMpath fs cs = mkSVGAtom "mpath" fs cs

pattern SVGPath fs cs <- (fromAtom -> Just (SVGHTML mn "path" fs cs)) where
  SVGPath fs cs = mkSVGAtom "path" fs cs

pattern SVGPattern fs cs <- (fromAtom -> Just (SVGHTML mn "pattern" fs cs)) where
  SVGPattern fs cs = mkSVGAtom "pattern" fs cs

pattern SVGPolygon fs cs <- (fromAtom -> Just (SVGHTML mn "polygon" fs cs)) where
  SVGPolygon fs cs = mkSVGAtom "polygon" fs cs

pattern SVGPolyline fs cs <- (fromAtom -> Just (SVGHTML mn "polyline" fs cs)) where
  SVGPolyline fs cs = mkSVGAtom "polyline" fs cs

pattern SVGRadialGradient fs cs <- (fromAtom -> Just (SVGHTML mn "radialGradient" fs cs)) where
  SVGRadialGradient fs cs = mkSVGAtom "radialGradient" fs cs

pattern SVGRect fs cs <- (fromAtom -> Just (SVGHTML mn "rect" fs cs)) where
  SVGRect fs cs = mkSVGAtom "rect" fs cs

pattern SVGScript fs cs <- (fromAtom -> Just (SVGHTML mn "script" fs cs)) where
  SVGScript fs cs = mkSVGAtom "script" fs cs

pattern SVGSet fs cs <- (fromAtom -> Just (SVGHTML mn "set" fs cs)) where
  SVGSet fs cs = mkSVGAtom "set" fs cs

pattern SVGSolidcolor fs cs <- (fromAtom -> Just (SVGHTML mn "solidcolor" fs cs)) where
  SVGSolidcolor fs cs = mkSVGAtom "solidcolor" fs cs

pattern SVGStop fs cs <- (fromAtom -> Just (SVGHTML mn "stop" fs cs)) where
  SVGStop fs cs = mkSVGAtom "stop" fs cs

pattern SVGStyle fs cs <- (fromAtom -> Just (SVGHTML mn "style" fs cs)) where
  SVGStyle fs cs = mkSVGAtom "style" fs cs

pattern SVGSvg fs cs <- (fromAtom -> Just (SVGHTML mn "svg" fs cs)) where
  SVGSvg fs cs = mkSVGAtom "svg" fs cs

pattern SVGSwitch fs cs <- (fromAtom -> Just (SVGHTML mn "switch" fs cs)) where
  SVGSwitch fs cs = mkSVGAtom "switch" fs cs

pattern SVGSymbol fs cs <- (fromAtom -> Just (SVGHTML mn "symbol" fs cs)) where
  SVGSymbol fs cs = mkSVGAtom "symbol" fs cs

pattern SVGText fs cs <- (fromAtom -> Just (SVGHTML mn "text" fs cs)) where
  SVGText fs cs = mkSVGAtom "text" fs cs

pattern SVGTextPath fs cs <- (fromAtom -> Just (SVGHTML mn "textPath" fs cs)) where
  SVGTextPath fs cs = mkSVGAtom "textPath" fs cs

pattern SVGTitle fs cs <- (fromAtom -> Just (SVGHTML mn "title" fs cs)) where
  SVGTitle fs cs = mkSVGAtom "title" fs cs

pattern SVGTref fs cs <- (fromAtom -> Just (SVGHTML mn "tref" fs cs)) where
  SVGTref fs cs = mkSVGAtom "tref" fs cs

pattern SVGTspan fs cs <- (fromAtom -> Just (SVGHTML mn "tspan" fs cs)) where
  SVGTspan fs cs = mkSVGAtom "tspan" fs cs

pattern SVGUnknown fs cs <- (fromAtom -> Just (SVGHTML mn "unknown" fs cs)) where
  SVGUnknown fs cs = mkSVGAtom "unknown" fs cs

pattern SVGUse fs cs <- (fromAtom -> Just (SVGHTML mn "use" fs cs)) where
  SVGUse fs cs = mkSVGAtom "use" fs cs

pattern SVGVideo fs cs <- (fromAtom -> Just (SVGHTML mn "video" fs cs)) where
  SVGVideo fs cs = mkSVGAtom "video" fs cs

pattern SVGView fs cs <- (fromAtom -> Just (SVGHTML mn "view" fs cs)) where
  SVGView fs cs = mkSVGAtom "view" fs cs

pattern SVGVkern fs cs <- (fromAtom -> Just (SVGHTML mn "vkern" fs cs)) where
  SVGVkern fs cs = mkSVGAtom "vkern" fs cs
