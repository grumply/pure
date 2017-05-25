{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
module Atomic.HTML where

import Ef.Base

import Data.Txt (Txt)

import Data.Typeable

import Atomic.Attribute
import Atomic.Component

pattern Abbr fs cs <- HTML mn "abbr" fs cs where
  Abbr fs cs = mkHTML "abbr" fs cs

pattern Address fs cs <- HTML mn "address" fs cs where
  Address fs cs = mkHTML "address" fs cs

pattern Area fs cs <- HTML mn "area" fs cs where
  Area fs cs = mkHTML "area" fs cs

pattern A fs cs <- HTML mn "a" fs cs where
  A fs cs = mkHTML "a" fs cs

pattern Article fs cs <- HTML mn "article" fs cs where
  Article fs cs = mkHTML "article" fs cs

pattern Aside fs cs <- HTML mn "aside" fs cs where
  Aside fs cs = mkHTML "aside" fs cs

pattern Audio fs cs <- HTML mn "audio" fs cs where
  Audio fs cs = mkHTML "audio" fs cs

pattern Base fs cs <- HTML mn "base" fs cs where
  Base fs cs = mkHTML "base" fs cs

pattern Bdi fs cs <- HTML mn "bdi" fs cs where
  Bdi fs cs = mkHTML "bdi" fs cs

pattern Bdo fs cs <- HTML mn "bdo" fs cs where
  Bdo fs cs = mkHTML "bdo" fs cs

pattern Big fs cs <- HTML mn "big" fs cs where
  Big fs cs = mkHTML "big" fs cs

pattern Blockquote fs cs <- HTML mn "blockquote" fs cs where
  Blockquote fs cs = mkHTML "blockquote" fs cs

pattern Body fs cs <- HTML mn "body" fs cs where
  Body fs cs = mkHTML "body" fs cs

pattern B fs cs <- HTML mn "b" fs cs where
  B fs cs = mkHTML "b" fs cs

pattern Br fs cs <- HTML mn "br" fs cs where
  Br fs cs = mkHTML "br" fs cs

pattern Button fs cs <- HTML mn "button" fs cs where
  Button fs cs = mkHTML "button" fs cs

pattern Canvas fs cs <- HTML mn "canvas" fs cs where
  Canvas fs cs = mkHTML "canvas" fs cs

pattern Caption fs cs <- HTML mn "caption" fs cs where
  Caption fs cs = mkHTML "caption" fs cs

pattern Cite fs cs <- HTML mn "cite" fs cs where
  Cite fs cs = mkHTML "cite" fs cs

pattern Code fs cs <- HTML mn "code" fs cs where
  Code fs cs = mkHTML "code" fs cs

pattern Col fs cs <- HTML mn "col" fs cs where
  Col fs cs = mkHTML "col" fs cs

pattern Colgroup fs cs <- HTML mn "colgroup" fs cs where
  Colgroup fs cs = mkHTML "colgroup" fs cs

pattern Data fs cs <- HTML mn "data" fs cs where
  Data fs cs = mkHTML "data" fs cs

pattern Datalist fs cs <- HTML mn "datalist" fs cs where
  Datalist fs cs = mkHTML "datalist" fs cs

pattern Dd fs cs <- HTML mn "dd" fs cs where
  Dd fs cs = mkHTML "dd" fs cs

pattern Description fs cs <- HTML mn "description" fs cs where
  Description fs cs = mkHTML "description" fs cs

pattern Dl fs cs <- HTML mn "dl" fs cs where
  Dl fs cs = mkHTML "dl" fs cs

pattern Dt fs cs <- HTML mn "dt" fs cs where
  Dt fs cs = mkHTML "dt" fs cs

pattern Del fs cs <- HTML mn "del" fs cs where
  Del fs cs = mkHTML "del" fs cs

pattern Details fs cs <- HTML mn "details" fs cs where
  Details fs cs = mkHTML "details" fs cs

pattern Dfn fs cs <- HTML mn "dfn" fs cs where
  Dfn fs cs = mkHTML "dfn" fs cs

pattern Dialog fs cs <- HTML mn "dialog" fs cs where
  Dialog fs cs = mkHTML "dialog" fs cs

pattern Div fs cs <- HTML mn "div" fs cs where
  Div fs cs = mkHTML "div" fs cs

pattern Em fs cs <- HTML mn "em" fs cs where
  Em fs cs = mkHTML "em" fs cs

pattern Embed fs cs <- HTML mn "embed" fs cs where
  Embed fs cs = mkHTML "embed" fs cs

pattern Fieldset fs cs <- HTML mn "fieldset" fs cs where
  Fieldset fs cs = mkHTML "fieldset" fs cs

pattern Figcaption fs cs <- HTML mn "figcaption" fs cs where
  Figcaption fs cs = mkHTML "figcaption" fs cs

pattern Figure fs cs <- HTML mn "figure" fs cs where
  Figure fs cs = mkHTML "figure" fs cs

pattern Footer fs cs <- HTML mn "footer" fs cs where
  Footer fs cs = mkHTML "footer" fs cs

pattern Form fs cs <- HTML mn "form" fs cs where
  Form fs cs = mkHTML "form" fs cs

pattern Frame fs cs <- HTML mn "frame" fs cs where
  Frame fs cs = mkHTML "frame" fs cs

pattern Head fs cs <- HTML mn "head" fs cs where
  Head fs cs = mkHTML "head" fs cs

pattern Header fs cs <- HTML mn "header" fs cs where
  Header fs cs = mkHTML "header" fs cs

pattern H1 fs cs <- HTML mn "h1" fs cs where
  H1 fs cs = mkHTML "h1" fs cs

pattern H2 fs cs <- HTML mn "h2" fs cs where
  H2 fs cs = mkHTML "h2" fs cs

pattern H3 fs cs <- HTML mn "h3" fs cs where
  H3 fs cs = mkHTML "h3" fs cs

pattern H4 fs cs <- HTML mn "h4" fs cs where
  H4 fs cs = mkHTML "h4" fs cs

pattern H5 fs cs <- HTML mn "h5" fs cs where
  H5 fs cs = mkHTML "h5" fs cs

pattern H6 fs cs <- HTML mn "h6" fs cs where
  H6 fs cs = mkHTML "h6" fs cs

pattern Hgroup fs cs <- HTML mn "hgroup" fs cs where
  Hgroup fs cs = mkHTML "hgroup" fs cs

pattern Hr fs cs <- HTML mn "hr" fs cs where
  Hr fs cs = mkHTML "hr" fs cs

pattern Html fs cs <- HTML mn "html" fs cs where
  Html fs cs = mkHTML "html" fs cs

pattern Iframe fs cs <- HTML mn "iframe" fs cs where
  Iframe fs cs = mkHTML "iframe" fs cs

pattern Img fs cs <- HTML mn "img" fs cs where
  Img fs cs = mkHTML "img" fs cs

pattern Input fs cs <- HTML mn "input" fs cs where
  Input fs cs = mkHTML "input" fs cs

pattern Ins fs cs <- HTML mn "ins" fs cs where
  Ins fs cs = mkHTML "ins" fs cs

pattern I fs cs <- HTML mn "i" fs cs where
  I fs cs = mkHTML "i" fs cs

pattern Kbd fs cs <- HTML mn "kbd" fs cs where
  Kbd fs cs = mkHTML "kbd" fs cs

pattern Keygen fs cs <- HTML mn "keygen" fs cs where
  Keygen fs cs = mkHTML "keygen" fs cs

pattern Label fs cs <- HTML mn "label" fs cs where
  Label fs cs = mkHTML "label" fs cs

pattern Legend fs cs <- HTML mn "legend" fs cs where
  Legend fs cs = mkHTML "legend" fs cs

pattern Li fs cs <- HTML mn "li" fs cs where
  Li fs cs = mkHTML "li" fs cs

pattern Link fs cs <- HTML mn "link" fs cs where
  Link fs cs = mkHTML "link" fs cs

pattern Main fs cs <- HTML mn "main" fs cs where
  Main fs cs = mkHTML "main" fs cs

pattern Map fs cs <- HTML mn "map" fs cs where
  Map fs cs = mkHTML "map" fs cs

pattern Mark fs cs <- HTML mn "mark" fs cs where
  Mark fs cs = mkHTML "mark" fs cs

pattern Menu fs cs <- HTML mn "menu" fs cs where
  Menu fs cs = mkHTML "menu" fs cs

pattern Menuitem fs cs <- HTML mn "menuitem" fs cs where
  Menuitem fs cs = mkHTML "menuitem" fs cs

pattern Meta fs cs <- HTML mn "meta" fs cs where
  Meta fs cs = mkHTML "meta" fs cs

pattern Meter fs cs <- HTML mn "meter" fs cs where
  Meter fs cs = mkHTML "meter" fs cs

pattern Nav fs cs <- HTML mn "nav" fs cs where
  Nav fs cs = mkHTML "nav" fs cs

pattern Noscript fs cs <- HTML mn "noscript" fs cs where
  Noscript fs cs = mkHTML "noscript" fs cs

pattern Obj fs cs <- HTML mn "object" fs cs where
  Obj fs cs = mkHTML "object" fs cs

pattern Optgroup fs cs <- HTML mn "optgroup" fs cs where
  Optgroup fs cs = mkHTML "optgroup" fs cs

pattern Option fs cs <- HTML mn "option" fs cs where
  Option fs cs = mkHTML "option" fs cs

pattern Ol fs cs <- HTML mn "ol" fs cs where
  Ol fs cs = mkHTML "ol" fs cs

pattern Output fs cs <- HTML mn "output" fs cs where
  Output fs cs = mkHTML "output" fs cs

pattern P fs cs <- HTML mn "p" fs cs where
  P fs cs = mkHTML "p" fs cs

pattern Param fs cs <- HTML mn "param" fs cs where
  Param fs cs = mkHTML "param" fs cs

pattern Picture fs cs <- HTML mn "picture" fs cs where
  Picture fs cs = mkHTML "picture" fs cs

pattern Pre fs cs <- HTML mn "pre" fs cs where
  Pre fs cs = mkHTML "pre" fs cs

pattern Progress fs cs <- HTML mn "progress" fs cs where
  Progress fs cs = mkHTML "progress" fs cs

pattern Q fs cs <- HTML mn "q" fs cs where
  Q fs cs = mkHTML "q" fs cs

pattern Rp fs cs <- HTML mn "rp" fs cs where
  Rp fs cs = mkHTML "rp" fs cs

pattern Rt fs cs <- HTML mn "rt" fs cs where
  Rt fs cs = mkHTML "rt" fs cs

pattern Ruby fs cs <- HTML mn "ruby" fs cs where
  Ruby fs cs = mkHTML "ruby" fs cs

pattern Samp fs cs <- HTML mn "samp" fs cs where
  Samp fs cs = mkHTML "samp" fs cs

pattern Script fs cs <- HTML mn "script" fs cs where
  Script fs cs = mkHTML "script" fs cs

pattern S fs cs <- HTML mn "s" fs cs where
  S fs cs = mkHTML "s" fs cs

pattern Section fs cs <- HTML mn "section" fs cs where
  Section fs cs = mkHTML "section" fs cs

pattern Select fs cs <- HTML mn "select" fs cs where
  Select fs cs = mkHTML "select" fs cs

pattern Small fs cs <- HTML mn "small" fs cs where
  Small fs cs = mkHTML "small" fs cs

pattern Source fs cs <- HTML mn "source" fs cs where
  Source fs cs = mkHTML "source" fs cs

pattern Span fs cs <- HTML mn "span" fs cs where
  Span fs cs = mkHTML "span" fs cs

pattern Strong fs cs <- HTML mn "strong" fs cs where
  Strong fs cs = mkHTML "strong" fs cs

pattern Style fs cs <- HTML mn "style" fs cs where
  Style fs cs = mkHTML "style" fs cs

pattern Sub fs cs <- HTML mn "sub" fs cs where
  Sub fs cs = mkHTML "sub" fs cs

pattern Summary fs cs <- HTML mn "summary" fs cs where
  Summary fs cs = mkHTML "summary" fs cs

pattern Sup fs cs <- HTML mn "sup" fs cs where
  Sup fs cs = mkHTML "sup" fs cs

pattern Table fs cs <- HTML mn "table" fs cs where
  Table fs cs = mkHTML "table" fs cs

pattern Tbody fs cs <- HTML mn "tbody" fs cs where
  Tbody fs cs = mkHTML "tbody" fs cs

pattern Td fs cs <- HTML mn "td" fs cs where
  Td fs cs = mkHTML "td" fs cs

pattern Textarea fs cs <- HTML mn "textarea" fs cs where
  Textarea fs cs = mkHTML "textarea" fs cs

pattern Tfoot fs cs <- HTML mn "tfoot" fs cs where
  Tfoot fs cs = mkHTML "tfoot" fs cs

pattern Th fs cs <- HTML mn "th" fs cs where
  Th fs cs = mkHTML "th" fs cs

pattern Thead fs cs <- HTML mn "thead" fs cs where
  Thead fs cs = mkHTML "thead" fs cs

pattern Time fs cs <- HTML mn "time" fs cs where
  Time fs cs = mkHTML "time" fs cs

pattern Title fs cs <- HTML mn "title" fs cs where
  Title fs cs = mkHTML "title" fs cs

pattern Tr fs cs <- HTML mn "tr" fs cs where
  Tr fs cs = mkHTML "tr" fs cs

pattern Track fs cs <- HTML mn "track" fs cs where
  Track fs cs = mkHTML "track" fs cs

pattern U fs cs <- HTML mn "u" fs cs where
  U fs cs = mkHTML "u" fs cs

pattern Ul fs cs <- HTML mn "ul" fs cs where
  Ul fs cs = mkHTML "ul" fs cs

pattern Var fs cs <- HTML mn "var" fs cs where
  Var fs cs = mkHTML "var" fs cs

pattern Video fs cs <- HTML mn "video" fs cs where
  Video fs cs = mkHTML "video" fs cs

pattern Viewport fs cs <- HTML mn "viewport" fs cs where
  Viewport fs cs = mkHTML "viewport" fs cs

pattern Wbr fs cs <- HTML mn "wbr" fs cs where
  Wbr fs cs = mkHTML "wbr" fs cs

--------------------------------------------------------------------------------
-- SVG

pattern SVGA fs cs <- SVGHTML mn "a" fs cs where
  SVGA fs cs = mkSVG "a" fs cs

pattern SVGAudio fs cs <- SVGHTML mn "audio" fs cs where
  SVGAudio fs cs = mkSVG "audio" fs cs

pattern SVGAltGlyph fs cs <- SVGHTML mn "altGlyph" fs cs where
  SVGAltGlyph fs cs = mkSVG "altGlyph" fs cs

pattern SVGAltGlyphDef fs cs <- SVGHTML mn "altGlyphDef" fs cs where
  SVGAltGlyphDef fs cs = mkSVG "altGlyphDef" fs cs

pattern SVGAltGlyphItem fs cs <- SVGHTML mn "altGlyphItem" fs cs where
  SVGAltGlyphItem fs cs = mkSVG "altGlyphItem" fs cs

pattern SVGAnimate fs cs <- SVGHTML mn "animate" fs cs where
  SVGAnimate fs cs = mkSVG "animate" fs cs

pattern SVGAnimateColor fs cs <- SVGHTML mn "animateColor" fs cs where
  SVGAnimateColor fs cs = mkSVG "animateColor" fs cs

pattern SVGAnimateMotion fs cs <- SVGHTML mn "animateMotion" fs cs where
  SVGAnimateMotion fs cs = mkSVG "animateMotion" fs cs

pattern SVGAnimateTransform fs cs <- SVGHTML mn "animateTransform" fs cs where
  SVGAnimateTransform fs cs = mkSVG "animateTransform" fs cs

pattern SVGCanvas fs cs <- SVGHTML mn "canvas" fs cs where
  SVGCanvas fs cs = mkSVG "canvas" fs cs

pattern SVGCircle fs cs <- SVGHTML mn "circle" fs cs where
  SVGCircle fs cs = mkSVG "circle" fs cs

pattern SVGClipPath fs cs <- SVGHTML mn "clipPath" fs cs where
  SVGClipPath fs cs = mkSVG "clipPath" fs cs

pattern SVGColorProfile fs cs <- SVGHTML mn "color-profile" fs cs where
  SVGColorProfile fs cs = mkSVG "color-profile" fs cs

pattern SVGCursor fs cs <- SVGHTML mn "cursor" fs cs where
  SVGCursor fs cs = mkSVG "cursor" fs cs

pattern SVGDefs fs cs <- SVGHTML mn "defs" fs cs where
  SVGDefs fs cs = mkSVG "defs" fs cs

pattern SVGDesc fs cs <- SVGHTML mn "desc" fs cs where
  SVGDesc fs cs = mkSVG "desc" fs cs

pattern SVGDiscard fs cs <- SVGHTML mn "discard" fs cs where
  SVGDiscard fs cs = mkSVG "discard" fs cs

pattern SVGEllipse fs cs <- SVGHTML mn "ellipse" fs cs where
  SVGEllipse fs cs = mkSVG "ellipse" fs cs

pattern SVGFeBlend fs cs <- SVGHTML mn "feBlend" fs cs where
  SVGFeBlend fs cs = mkSVG "feBlend" fs cs

pattern SVGFeColorMatrix fs cs <- SVGHTML mn "feColorMatrix" fs cs where
  SVGFeColorMatrix fs cs = mkSVG "feColorMatrix" fs cs

pattern SVGFeComponentTransfer fs cs <- SVGHTML mn "feComponentTransfer" fs cs where
  SVGFeComponentTransfer fs cs = mkSVG "feComponentTransfer" fs cs

pattern SVGFeComposite fs cs <- SVGHTML mn "feComposite" fs cs where
  SVGFeComposite fs cs = mkSVG "feComposite" fs cs

pattern SVGFeConvolveMatrix fs cs <- SVGHTML mn "feConvolveMatrix" fs cs where
  SVGFeConvolveMatrix fs cs = mkSVG "feConvolveMatrix" fs cs

pattern SVGFeDiffuseLighting fs cs <- SVGHTML mn "feDiffuseLighting" fs cs where
  SVGFeDiffuseLighting fs cs = mkSVG "feDiffuseLighting" fs cs

pattern SVGFeDisplacementMap fs cs <- SVGHTML mn "feDisplacementMap" fs cs where
  SVGFeDisplacementMap fs cs = mkSVG "feDisplacementMap" fs cs

pattern SVGFeDistantLight fs cs <- SVGHTML mn "feDistantLight" fs cs where
  SVGFeDistantLight fs cs = mkSVG "feDistantLight" fs cs

pattern SVGFeDropShadow fs cs <- SVGHTML mn "feDropShadow" fs cs where
  SVGFeDropShadow fs cs = mkSVG "feDropShadow" fs cs

pattern SVGFeFlood fs cs <- SVGHTML mn "feFlood" fs cs where
  SVGFeFlood fs cs = mkSVG "feFlood" fs cs

pattern SVGFeFuncA fs cs <- SVGHTML mn "feFuncA" fs cs where
  SVGFeFuncA fs cs = mkSVG "feFuncA" fs cs

pattern SVGFeFuncB fs cs <- SVGHTML mn "feFuncB" fs cs where
  SVGFeFuncB fs cs = mkSVG "feFuncB" fs cs

pattern SVGFeFuncG fs cs <- SVGHTML mn "feFuncG" fs cs where
  SVGFeFuncG fs cs = mkSVG "feFuncG" fs cs

pattern SVGFeFuncR fs cs <- SVGHTML mn "feFuncR" fs cs where
  SVGFeFuncR fs cs = mkSVG "feFuncR" fs cs

pattern SVGFeGaussianBlur fs cs <- SVGHTML mn "feGaussianBlur" fs cs where
  SVGFeGaussianBlur fs cs = mkSVG "feGaussianBlur" fs cs

pattern SVGFeImage fs cs <- SVGHTML mn "feImage" fs cs where
  SVGFeImage fs cs = mkSVG "feImage" fs cs

pattern SVGFeMerge fs cs <- SVGHTML mn "feMerge" fs cs where
  SVGFeMerge fs cs = mkSVG "feMerge" fs cs

pattern SVGFeMergeNode fs cs <- SVGHTML mn "feMergeNode" fs cs where
  SVGFeMergeNode fs cs = mkSVG "feMergeNode" fs cs

pattern SVGFeMorphology fs cs <- SVGHTML mn "feMorphology" fs cs where
  SVGFeMorphology fs cs = mkSVG "feMorphology" fs cs

pattern SVGFeOffset fs cs <- SVGHTML mn "feOffset" fs cs where
  SVGFeOffset fs cs = mkSVG "feOffset" fs cs

pattern SVGFePointLight fs cs <- SVGHTML mn "fePointLight" fs cs where
  SVGFePointLight fs cs = mkSVG "fePointLight" fs cs

pattern SVGFeSpecularLighting fs cs <- SVGHTML mn "feSpecularLighting" fs cs where
  SVGFeSpecularLighting fs cs = mkSVG "feSpecularLighting" fs cs

pattern SVGFeSpotLight fs cs <- SVGHTML mn "feSpotLight" fs cs where
  SVGFeSpotLight fs cs = mkSVG "feSpotLight" fs cs

pattern SVGFeTile fs cs <- SVGHTML mn "feTile" fs cs where
  SVGFeTile fs cs = mkSVG "feTile" fs cs

pattern SVGFeTurbulence fs cs <- SVGHTML mn "feTurbulence" fs cs where
  SVGFeTurbulence fs cs = mkSVG "feTurbulence" fs cs

pattern SVGFilter fs cs <- SVGHTML mn "filter" fs cs where
  SVGFilter fs cs = mkSVG "filter" fs cs

pattern SVGFont fs cs <- SVGHTML mn "font" fs cs where
  SVGFont fs cs = mkSVG "font" fs cs

pattern SVGFontFace fs cs <- SVGHTML mn "font-face" fs cs where
  SVGFontFace fs cs = mkSVG "font-face" fs cs

pattern SVGFontFaceFormat fs cs <- SVGHTML mn "font-face-format" fs cs where
  SVGFontFaceFormat fs cs = mkSVG "font-face-format" fs cs

pattern SVGFontFaceName fs cs <- SVGHTML mn "font-face-name" fs cs where
  SVGFontFaceName fs cs = mkSVG "font-face-name" fs cs

pattern SVGFontFaceSrc fs cs <- SVGHTML mn "font-face-src" fs cs where
  SVGFontFaceSrc fs cs = mkSVG "font-face-src" fs cs

pattern SVGFontFaceURI fs cs <- SVGHTML mn "font-face-uri" fs cs where
  SVGFontFaceURI fs cs = mkSVG "font-face-uri" fs cs

pattern SVGForeignObject fs cs <- SVGHTML mn "foreignObject" fs cs where
  SVGForeignObject fs cs = mkSVG "foreignObject" fs cs

pattern SVGG fs cs <- SVGHTML mn "g" fs cs where
  SVGG fs cs = mkSVG "g" fs cs

pattern SVGGlyph fs cs <- SVGHTML mn "glyph" fs cs where
  SVGGlyph fs cs = mkSVG "glyph" fs cs

pattern SVGGlyphRef fs cs <- SVGHTML mn "glyphRef" fs cs where
  SVGGlyphRef fs cs = mkSVG "glyphRef" fs cs

pattern SVGHatch fs cs <- SVGHTML mn "hatch" fs cs where
  SVGHatch fs cs = mkSVG "hatch" fs cs

pattern SVGHatchpath fs cs <- SVGHTML mn "hatchpath" fs cs where
  SVGHatchpath fs cs = mkSVG "hatchpath" fs cs

pattern SVGHkern fs cs <- SVGHTML mn "hkern" fs cs where
  SVGHkern fs cs = mkSVG "hkern" fs cs

pattern SVGIframe fs cs <- SVGHTML mn "iframe" fs cs where
  SVGIframe fs cs = mkSVG "iframe" fs cs

pattern SVGImage fs cs <- SVGHTML mn "image" fs cs where
  SVGImage fs cs = mkSVG "image" fs cs

pattern SVGLine fs cs <- SVGHTML mn "line" fs cs where
  SVGLine fs cs = mkSVG "line" fs cs

pattern SVGLinearGradient fs cs <- SVGHTML mn "linearGradient" fs cs where
  SVGLinearGradient fs cs = mkSVG "linearGradient" fs cs

pattern SVGMarker fs cs <- SVGHTML mn "marker" fs cs where
  SVGMarker fs cs = mkSVG "marker" fs cs

pattern SVGMask fs cs <- SVGHTML mn "mask" fs cs where
  SVGMask fs cs = mkSVG "mask" fs cs

pattern SVGMesh fs cs <- SVGHTML mn "mesh" fs cs where
  SVGMesh fs cs = mkSVG "mesh" fs cs

pattern SVGMeshgradient fs cs <- SVGHTML mn "meshgradient" fs cs where
  SVGMeshgradient fs cs = mkSVG "meshgradient" fs cs

pattern SVGMeshpatch fs cs <- SVGHTML mn "meshpatch" fs cs where
  SVGMeshpatch fs cs = mkSVG "meshpatch" fs cs

pattern SVGMeshrow fs cs <- SVGHTML mn "meshrow" fs cs where
  SVGMeshrow fs cs = mkSVG "meshrow" fs cs

pattern SVGMetadata fs cs <- SVGHTML mn "metadata" fs cs where
  SVGMetadata fs cs = mkSVG "metadata" fs cs

pattern SVGMissingGlyph fs cs <- SVGHTML mn "missing-glyph" fs cs where
  SVGMissingGlyph fs cs = mkSVG "missing-glyph" fs cs

pattern SVGMpath fs cs <- SVGHTML mn "mpath" fs cs where
  SVGMpath fs cs = mkSVG "mpath" fs cs

pattern SVGPath fs cs <- SVGHTML mn "path" fs cs where
  SVGPath fs cs = mkSVG "path" fs cs

pattern SVGPattern fs cs <- SVGHTML mn "pattern" fs cs where
  SVGPattern fs cs = mkSVG "pattern" fs cs

pattern SVGPolygon fs cs <- SVGHTML mn "polygon" fs cs where
  SVGPolygon fs cs = mkSVG "polygon" fs cs

pattern SVGPolyline fs cs <- SVGHTML mn "polyline" fs cs where
  SVGPolyline fs cs = mkSVG "polyline" fs cs

pattern SVGRadialGradient fs cs <- SVGHTML mn "radialGradient" fs cs where
  SVGRadialGradient fs cs = mkSVG "radialGradient" fs cs

pattern SVGRect fs cs <- SVGHTML mn "rect" fs cs where
  SVGRect fs cs = mkSVG "rect" fs cs

pattern SVGScript fs cs <- SVGHTML mn "script" fs cs where
  SVGScript fs cs = mkSVG "script" fs cs

pattern SVGSet fs cs <- SVGHTML mn "set" fs cs where
  SVGSet fs cs = mkSVG "set" fs cs

pattern SVGSolidcolor fs cs <- SVGHTML mn "solidcolor" fs cs where
  SVGSolidcolor fs cs = mkSVG "solidcolor" fs cs

pattern SVGStop fs cs <- SVGHTML mn "stop" fs cs where
  SVGStop fs cs = mkSVG "stop" fs cs

pattern SVGStyle fs cs <- SVGHTML mn "style" fs cs where
  SVGStyle fs cs = mkSVG "style" fs cs

pattern SVGSvg fs cs <- SVGHTML mn "svg" fs cs where
  SVGSvg fs cs = mkSVG "svg" fs cs

pattern SVGSwitch fs cs <- SVGHTML mn "switch" fs cs where
  SVGSwitch fs cs = mkSVG "switch" fs cs

pattern SVGSymbol fs cs <- SVGHTML mn "symbol" fs cs where
  SVGSymbol fs cs = mkSVG "symbol" fs cs

pattern SVGText fs cs <- SVGHTML mn "text" fs cs where
  SVGText fs cs = mkSVG "text" fs cs

pattern SVGTextPath fs cs <- SVGHTML mn "textPath" fs cs where
  SVGTextPath fs cs = mkSVG "textPath" fs cs

pattern SVGTitle fs cs <- SVGHTML mn "title" fs cs where
  SVGTitle fs cs = mkSVG "title" fs cs

pattern SVGTref fs cs <- SVGHTML mn "tref" fs cs where
  SVGTref fs cs = mkSVG "tref" fs cs

pattern SVGTspan fs cs <- SVGHTML mn "tspan" fs cs where
  SVGTspan fs cs = mkSVG "tspan" fs cs

pattern SVGUnknown fs cs <- SVGHTML mn "unknown" fs cs where
  SVGUnknown fs cs = mkSVG "unknown" fs cs

pattern SVGUse fs cs <- SVGHTML mn "use" fs cs where
  SVGUse fs cs = mkSVG "use" fs cs

pattern SVGVideo fs cs <- SVGHTML mn "video" fs cs where
  SVGVideo fs cs = mkSVG "video" fs cs

pattern SVGView fs cs <- SVGHTML mn "view" fs cs where
  SVGView fs cs = mkSVG "view" fs cs

pattern SVGVkern fs cs <- SVGHTML mn "vkern" fs cs where
  SVGVkern fs cs = mkSVG "vkern" fs cs
