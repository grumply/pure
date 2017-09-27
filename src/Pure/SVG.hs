{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
module Pure.SVG (module Pure.SVG, module Export) where

import Pure.Types

import Pure.Attributes as Export hiding (ClipPath,ColorProfile,Cursor,Filter,GlyphRef,Mask,Pattern)
import Pure.CSS        as Export hiding (Alt,Text)

pattern A :: [Feature e] -> [View e] -> View e
pattern A fs cs <- SVGView mn "a" fs cs where
  A fs cs = mkSVG "a" fs cs

pattern Audio :: [Feature e] -> [View e] -> View e
pattern Audio fs cs <- SVGView mn "audio" fs cs where
  Audio fs cs = mkSVG "audio" fs cs

pattern AltGlyph :: [Feature e] -> [View e] -> View e
pattern AltGlyph fs cs <- SVGView mn "altGlyph" fs cs where
  AltGlyph fs cs = mkSVG "altGlyph" fs cs

pattern AltGlyphDef :: [Feature e] -> [View e] -> View e
pattern AltGlyphDef fs cs <- SVGView mn "altGlyphDef" fs cs where
  AltGlyphDef fs cs = mkSVG "altGlyphDef" fs cs

pattern AltGlyphItem :: [Feature e] -> [View e] -> View e
pattern AltGlyphItem fs cs <- SVGView mn "altGlyphItem" fs cs where
  AltGlyphItem fs cs = mkSVG "altGlyphItem" fs cs

pattern Animate :: [Feature e] -> [View e] -> View e
pattern Animate fs cs <- SVGView mn "animate" fs cs where
  Animate fs cs = mkSVG "animate" fs cs

pattern AnimateColor :: [Feature e] -> [View e] -> View e
pattern AnimateColor fs cs <- SVGView mn "animateColor" fs cs where
  AnimateColor fs cs = mkSVG "animateColor" fs cs

pattern AnimateMotion :: [Feature e] -> [View e] -> View e
pattern AnimateMotion fs cs <- SVGView mn "animateMotion" fs cs where
  AnimateMotion fs cs = mkSVG "animateMotion" fs cs

pattern AnimateTransform :: [Feature e] -> [View e] -> View e
pattern AnimateTransform fs cs <- SVGView mn "animateTransform" fs cs where
  AnimateTransform fs cs = mkSVG "animateTransform" fs cs

pattern Canvas :: [Feature e] -> [View e] -> View e
pattern Canvas fs cs <- SVGView mn "canvas" fs cs where
  Canvas fs cs = mkSVG "canvas" fs cs

pattern Circle :: [Feature e] -> [View e] -> View e
pattern Circle fs cs <- SVGView mn "circle" fs cs where
  Circle fs cs = mkSVG "circle" fs cs

pattern ClipPath :: [Feature e] -> [View e] -> View e
pattern ClipPath fs cs <- SVGView mn "clipPath" fs cs where
  ClipPath fs cs = mkSVG "clipPath" fs cs

pattern ColorProfile :: [Feature e] -> [View e] -> View e
pattern ColorProfile fs cs <- SVGView mn "color-profile" fs cs where
  ColorProfile fs cs = mkSVG "color-profile" fs cs

pattern Cursor :: [Feature e] -> [View e] -> View e
pattern Cursor fs cs <- SVGView mn "cursor" fs cs where
  Cursor fs cs = mkSVG "cursor" fs cs

pattern Defs :: [Feature e] -> [View e] -> View e
pattern Defs fs cs <- SVGView mn "defs" fs cs where
  Defs fs cs = mkSVG "defs" fs cs

pattern Desc :: [Feature e] -> [View e] -> View e
pattern Desc fs cs <- SVGView mn "desc" fs cs where
  Desc fs cs = mkSVG "desc" fs cs

pattern Discard :: [Feature e] -> [View e] -> View e
pattern Discard fs cs <- SVGView mn "discard" fs cs where
  Discard fs cs = mkSVG "discard" fs cs

pattern Ellipse :: [Feature e] -> [View e] -> View e
pattern Ellipse fs cs <- SVGView mn "ellipse" fs cs where
  Ellipse fs cs = mkSVG "ellipse" fs cs

pattern FeBlend :: [Feature e] -> [View e] -> View e
pattern FeBlend fs cs <- SVGView mn "feBlend" fs cs where
  FeBlend fs cs = mkSVG "feBlend" fs cs

pattern FeColorMatrix :: [Feature e] -> [View e] -> View e
pattern FeColorMatrix fs cs <- SVGView mn "feColorMatrix" fs cs where
  FeColorMatrix fs cs = mkSVG "feColorMatrix" fs cs

pattern FeComponentTransfer :: [Feature e] -> [View e] -> View e
pattern FeComponentTransfer fs cs <- SVGView mn "feComponentTransfer" fs cs where
  FeComponentTransfer fs cs = mkSVG "feComponentTransfer" fs cs

pattern FeComposite :: [Feature e] -> [View e] -> View e
pattern FeComposite fs cs <- SVGView mn "feComposite" fs cs where
  FeComposite fs cs = mkSVG "feComposite" fs cs

pattern FeConvolveMatrix :: [Feature e] -> [View e] -> View e
pattern FeConvolveMatrix fs cs <- SVGView mn "feConvolveMatrix" fs cs where
  FeConvolveMatrix fs cs = mkSVG "feConvolveMatrix" fs cs

pattern FeDiffuseLighting :: [Feature e] -> [View e] -> View e
pattern FeDiffuseLighting fs cs <- SVGView mn "feDiffuseLighting" fs cs where
  FeDiffuseLighting fs cs = mkSVG "feDiffuseLighting" fs cs

pattern FeDisplacementMap :: [Feature e] -> [View e] -> View e
pattern FeDisplacementMap fs cs <- SVGView mn "feDisplacementMap" fs cs where
  FeDisplacementMap fs cs = mkSVG "feDisplacementMap" fs cs

pattern FeDistantLight :: [Feature e] -> [View e] -> View e
pattern FeDistantLight fs cs <- SVGView mn "feDistantLight" fs cs where
  FeDistantLight fs cs = mkSVG "feDistantLight" fs cs

pattern FeDropShadow :: [Feature e] -> [View e] -> View e
pattern FeDropShadow fs cs <- SVGView mn "feDropShadow" fs cs where
  FeDropShadow fs cs = mkSVG "feDropShadow" fs cs

pattern FeFlood :: [Feature e] -> [View e] -> View e
pattern FeFlood fs cs <- SVGView mn "feFlood" fs cs where
  FeFlood fs cs = mkSVG "feFlood" fs cs

pattern FeFuncA :: [Feature e] -> [View e] -> View e
pattern FeFuncA fs cs <- SVGView mn "feFuncA" fs cs where
  FeFuncA fs cs = mkSVG "feFuncA" fs cs

pattern FeFuncB :: [Feature e] -> [View e] -> View e
pattern FeFuncB fs cs <- SVGView mn "feFuncB" fs cs where
  FeFuncB fs cs = mkSVG "feFuncB" fs cs

pattern FeFuncG :: [Feature e] -> [View e] -> View e
pattern FeFuncG fs cs <- SVGView mn "feFuncG" fs cs where
  FeFuncG fs cs = mkSVG "feFuncG" fs cs

pattern FeFuncR :: [Feature e] -> [View e] -> View e
pattern FeFuncR fs cs <- SVGView mn "feFuncR" fs cs where
  FeFuncR fs cs = mkSVG "feFuncR" fs cs

pattern FeGaussianBlur :: [Feature e] -> [View e] -> View e
pattern FeGaussianBlur fs cs <- SVGView mn "feGaussianBlur" fs cs where
  FeGaussianBlur fs cs = mkSVG "feGaussianBlur" fs cs

pattern FeImage :: [Feature e] -> [View e] -> View e
pattern FeImage fs cs <- SVGView mn "feImage" fs cs where
  FeImage fs cs = mkSVG "feImage" fs cs

pattern FeMerge :: [Feature e] -> [View e] -> View e
pattern FeMerge fs cs <- SVGView mn "feMerge" fs cs where
  FeMerge fs cs = mkSVG "feMerge" fs cs

pattern FeMergeNode :: [Feature e] -> [View e] -> View e
pattern FeMergeNode fs cs <- SVGView mn "feMergeNode" fs cs where
  FeMergeNode fs cs = mkSVG "feMergeNode" fs cs

pattern FeMorphology :: [Feature e] -> [View e] -> View e
pattern FeMorphology fs cs <- SVGView mn "feMorphology" fs cs where
  FeMorphology fs cs = mkSVG "feMorphology" fs cs

pattern FeOffset :: [Feature e] -> [View e] -> View e
pattern FeOffset fs cs <- SVGView mn "feOffset" fs cs where
  FeOffset fs cs = mkSVG "feOffset" fs cs

pattern FePointLight :: [Feature e] -> [View e] -> View e
pattern FePointLight fs cs <- SVGView mn "fePointLight" fs cs where
  FePointLight fs cs = mkSVG "fePointLight" fs cs

pattern FeSpecularLighting :: [Feature e] -> [View e] -> View e
pattern FeSpecularLighting fs cs <- SVGView mn "feSpecularLighting" fs cs where
  FeSpecularLighting fs cs = mkSVG "feSpecularLighting" fs cs

pattern FeSpotLight :: [Feature e] -> [View e] -> View e
pattern FeSpotLight fs cs <- SVGView mn "feSpotLight" fs cs where
  FeSpotLight fs cs = mkSVG "feSpotLight" fs cs

pattern FeTile :: [Feature e] -> [View e] -> View e
pattern FeTile fs cs <- SVGView mn "feTile" fs cs where
  FeTile fs cs = mkSVG "feTile" fs cs

pattern FeTurbulence :: [Feature e] -> [View e] -> View e
pattern FeTurbulence fs cs <- SVGView mn "feTurbulence" fs cs where
  FeTurbulence fs cs = mkSVG "feTurbulence" fs cs

pattern Filter :: [Feature e] -> [View e] -> View e
pattern Filter fs cs <- SVGView mn "filter" fs cs where
  Filter fs cs = mkSVG "filter" fs cs

pattern Font :: [Feature e] -> [View e] -> View e
pattern Font fs cs <- SVGView mn "font" fs cs where
  Font fs cs = mkSVG "font" fs cs

pattern FontFace :: [Feature e] -> [View e] -> View e
pattern FontFace fs cs <- SVGView mn "font-face" fs cs where
  FontFace fs cs = mkSVG "font-face" fs cs

pattern FontFaceFormat :: [Feature e] -> [View e] -> View e
pattern FontFaceFormat fs cs <- SVGView mn "font-face-format" fs cs where
  FontFaceFormat fs cs = mkSVG "font-face-format" fs cs

pattern FontFaceName :: [Feature e] -> [View e] -> View e
pattern FontFaceName fs cs <- SVGView mn "font-face-name" fs cs where
  FontFaceName fs cs = mkSVG "font-face-name" fs cs

pattern FontFaceSrc :: [Feature e] -> [View e] -> View e
pattern FontFaceSrc fs cs <- SVGView mn "font-face-src" fs cs where
  FontFaceSrc fs cs = mkSVG "font-face-src" fs cs

pattern FontFaceURI :: [Feature e] -> [View e] -> View e
pattern FontFaceURI fs cs <- SVGView mn "font-face-uri" fs cs where
  FontFaceURI fs cs = mkSVG "font-face-uri" fs cs

pattern ForeignObject :: [Feature e] -> [View e] -> View e
pattern ForeignObject fs cs <- SVGView mn "foreignObject" fs cs where
  ForeignObject fs cs = mkSVG "foreignObject" fs cs

pattern G :: [Feature e] -> [View e] -> View e
pattern G fs cs <- SVGView mn "g" fs cs where
  G fs cs = mkSVG "g" fs cs

pattern Glyph :: [Feature e] -> [View e] -> View e
pattern Glyph fs cs <- SVGView mn "glyph" fs cs where
  Glyph fs cs = mkSVG "glyph" fs cs

pattern GlyphRef :: [Feature e] -> [View e] -> View e
pattern GlyphRef fs cs <- SVGView mn "glyphRef" fs cs where
  GlyphRef fs cs = mkSVG "glyphRef" fs cs

pattern Hatch :: [Feature e] -> [View e] -> View e
pattern Hatch fs cs <- SVGView mn "hatch" fs cs where
  Hatch fs cs = mkSVG "hatch" fs cs

pattern Hatchpath :: [Feature e] -> [View e] -> View e
pattern Hatchpath fs cs <- SVGView mn "hatchpath" fs cs where
  Hatchpath fs cs = mkSVG "hatchpath" fs cs

pattern Hkern :: [Feature e] -> [View e] -> View e
pattern Hkern fs cs <- SVGView mn "hkern" fs cs where
  Hkern fs cs = mkSVG "hkern" fs cs

pattern Iframe :: [Feature e] -> [View e] -> View e
pattern Iframe fs cs <- SVGView mn "iframe" fs cs where
  Iframe fs cs = mkSVG "iframe" fs cs

pattern Image :: [Feature e] -> [View e] -> View e
pattern Image fs cs <- SVGView mn "image" fs cs where
  Image fs cs = mkSVG "image" fs cs

pattern Line :: [Feature e] -> [View e] -> View e
pattern Line fs cs <- SVGView mn "line" fs cs where
  Line fs cs = mkSVG "line" fs cs

pattern LinearGradient :: [Feature e] -> [View e] -> View e
pattern LinearGradient fs cs <- SVGView mn "linearGradient" fs cs where
  LinearGradient fs cs = mkSVG "linearGradient" fs cs

pattern Marker :: [Feature e] -> [View e] -> View e
pattern Marker fs cs <- SVGView mn "marker" fs cs where
  Marker fs cs = mkSVG "marker" fs cs

pattern Mask :: [Feature e] -> [View e] -> View e
pattern Mask fs cs <- SVGView mn "mask" fs cs where
  Mask fs cs = mkSVG "mask" fs cs

pattern Mesh :: [Feature e] -> [View e] -> View e
pattern Mesh fs cs <- SVGView mn "mesh" fs cs where
  Mesh fs cs = mkSVG "mesh" fs cs

pattern Meshgradient :: [Feature e] -> [View e] -> View e
pattern Meshgradient fs cs <- SVGView mn "meshgradient" fs cs where
  Meshgradient fs cs = mkSVG "meshgradient" fs cs

pattern Meshpatch :: [Feature e] -> [View e] -> View e
pattern Meshpatch fs cs <- SVGView mn "meshpatch" fs cs where
  Meshpatch fs cs = mkSVG "meshpatch" fs cs

pattern Meshrow :: [Feature e] -> [View e] -> View e
pattern Meshrow fs cs <- SVGView mn "meshrow" fs cs where
  Meshrow fs cs = mkSVG "meshrow" fs cs

pattern Metadata :: [Feature e] -> [View e] -> View e
pattern Metadata fs cs <- SVGView mn "metadata" fs cs where
  Metadata fs cs = mkSVG "metadata" fs cs

pattern MissingGlyph :: [Feature e] -> [View e] -> View e
pattern MissingGlyph fs cs <- SVGView mn "missing-glyph" fs cs where
  MissingGlyph fs cs = mkSVG "missing-glyph" fs cs

pattern Mpath :: [Feature e] -> [View e] -> View e
pattern Mpath fs cs <- SVGView mn "mpath" fs cs where
  Mpath fs cs = mkSVG "mpath" fs cs

pattern Path :: [Feature e] -> [View e] -> View e
pattern Path fs cs <- SVGView mn "path" fs cs where
  Path fs cs = mkSVG "path" fs cs

pattern Pattern :: [Feature e] -> [View e] -> View e
pattern Pattern fs cs <- SVGView mn "pattern" fs cs where
  Pattern fs cs = mkSVG "pattern" fs cs

pattern Polygon :: [Feature e] -> [View e] -> View e
pattern Polygon fs cs <- SVGView mn "polygon" fs cs where
  Polygon fs cs = mkSVG "polygon" fs cs

pattern Polyline :: [Feature e] -> [View e] -> View e
pattern Polyline fs cs <- SVGView mn "polyline" fs cs where
  Polyline fs cs = mkSVG "polyline" fs cs

pattern RadialGradient :: [Feature e] -> [View e] -> View e
pattern RadialGradient fs cs <- SVGView mn "radialGradient" fs cs where
  RadialGradient fs cs = mkSVG "radialGradient" fs cs

pattern Rect :: [Feature e] -> [View e] -> View e
pattern Rect fs cs <- SVGView mn "rect" fs cs where
  Rect fs cs = mkSVG "rect" fs cs

pattern Script :: [Feature e] -> [View e] -> View e
pattern Script fs cs <- SVGView mn "script" fs cs where
  Script fs cs = mkSVG "script" fs cs

pattern Set :: [Feature e] -> [View e] -> View e
pattern Set fs cs <- SVGView mn "set" fs cs where
  Set fs cs = mkSVG "set" fs cs

pattern Solidcolor :: [Feature e] -> [View e] -> View e
pattern Solidcolor fs cs <- SVGView mn "solidcolor" fs cs where
  Solidcolor fs cs = mkSVG "solidcolor" fs cs

pattern Stop :: [Feature e] -> [View e] -> View e
pattern Stop fs cs <- SVGView mn "stop" fs cs where
  Stop fs cs = mkSVG "stop" fs cs

pattern Style :: [Feature e] -> [View e] -> View e
pattern Style fs cs <- SVGView mn "style" fs cs where
  Style fs cs = mkSVG "style" fs cs

pattern Svg :: [Feature e] -> [View e] -> View e
pattern Svg fs cs <- SVGView mn "svg" fs cs where
  Svg fs cs = mkSVG "svg" fs cs

pattern Switch :: [Feature e] -> [View e] -> View e
pattern Switch fs cs <- SVGView mn "switch" fs cs where
  Switch fs cs = mkSVG "switch" fs cs

pattern Symbol :: [Feature e] -> [View e] -> View e
pattern Symbol fs cs <- SVGView mn "symbol" fs cs where
  Symbol fs cs = mkSVG "symbol" fs cs

pattern Text :: [Feature e] -> [View e] -> View e
pattern Text fs cs <- SVGView mn "text" fs cs where
  Text fs cs = mkSVG "text" fs cs

pattern TextPath :: [Feature e] -> [View e] -> View e
pattern TextPath fs cs <- SVGView mn "textPath" fs cs where
  TextPath fs cs = mkSVG "textPath" fs cs

pattern Title :: [Feature e] -> [View e] -> View e
pattern Title fs cs <- SVGView mn "title" fs cs where
  Title fs cs = mkSVG "title" fs cs

pattern Tref :: [Feature e] -> [View e] -> View e
pattern Tref fs cs <- SVGView mn "tref" fs cs where
  Tref fs cs = mkSVG "tref" fs cs

pattern Tspan :: [Feature e] -> [View e] -> View e
pattern Tspan fs cs <- SVGView mn "tspan" fs cs where
  Tspan fs cs = mkSVG "tspan" fs cs

pattern Unknown :: [Feature e] -> [View e] -> View e
pattern Unknown fs cs <- SVGView mn "unknown" fs cs where
  Unknown fs cs = mkSVG "unknown" fs cs

pattern Use :: [Feature e] -> [View e] -> View e
pattern Use fs cs <- SVGView mn "use" fs cs where
  Use fs cs = mkSVG "use" fs cs

pattern Video :: [Feature e] -> [View e] -> View e
pattern Video fs cs <- SVGView mn "video" fs cs where
  Video fs cs = mkSVG "video" fs cs

pattern View :: [Feature e] -> [View e] -> View e
pattern View fs cs <- SVGView mn "view" fs cs where
  View fs cs = mkSVG "view" fs cs

pattern Vkern :: [Feature e] -> [View e] -> View e
pattern Vkern fs cs <- SVGView mn "vkern" fs cs where
  Vkern fs cs = mkSVG "vkern" fs cs
