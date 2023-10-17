{-# LANGUAGE PatternSynonyms, OverloadedStrings #-}
{-# OPTIONS_GHC -O2 #-}
module Data.SVG where

import Data.Txt
import Data.View (View,pattern SimpleSVG,pattern Attribute,pattern Property,pattern XLinks,pattern XLink)

pattern A_ :: View
pattern A_ = SimpleSVG "a"

pattern Audio :: View
pattern Audio = SimpleSVG "audio"

pattern AltGlyph :: View
pattern AltGlyph = SimpleSVG "altGlyph"

pattern AltGlyphDef :: View
pattern AltGlyphDef = SimpleSVG "altGlyphDef"

pattern AltGlyphItem :: View
pattern AltGlyphItem = SimpleSVG "altGlyphItem"

pattern Animate :: View
pattern Animate = SimpleSVG "animate"

pattern AnimateColor :: View
pattern AnimateColor = SimpleSVG "animateColor"

pattern AnimateMotion :: View
pattern AnimateMotion = SimpleSVG "animateMotion"

pattern AnimateTransform :: View
pattern AnimateTransform = SimpleSVG "animateTransform"

pattern Canvas :: View
pattern Canvas = SimpleSVG "canvas"

pattern Circle :: View
pattern Circle = SimpleSVG "circle"

pattern ClipPath :: View
pattern ClipPath = SimpleSVG "clipPath"

pattern ColorProfile :: View
pattern ColorProfile = SimpleSVG "color-profile"

pattern Cursor :: View
pattern Cursor = SimpleSVG "cursor"

pattern Defs :: View
pattern Defs = SimpleSVG "defs"

pattern Desc :: View
pattern Desc = SimpleSVG "desc"

pattern Discard :: View
pattern Discard = SimpleSVG "discard"

pattern Ellipse :: View
pattern Ellipse = SimpleSVG "ellipse"

pattern FeBlend :: View
pattern FeBlend = SimpleSVG "feBlend"

pattern FeColorMatrix :: View
pattern FeColorMatrix = SimpleSVG "feColorMatrix"

pattern FeComponentTransfer :: View
pattern FeComponentTransfer = SimpleSVG "feComponentTransfer"

pattern FeComposite :: View
pattern FeComposite = SimpleSVG "feComposite"

pattern FeConvolveMatrix :: View
pattern FeConvolveMatrix = SimpleSVG "feConvolveMatrix"

pattern FeDiffuseLighting :: View
pattern FeDiffuseLighting = SimpleSVG "feDiffuseLighting"

pattern FeDisplacementMap :: View
pattern FeDisplacementMap = SimpleSVG "feDisplacementMap"

pattern FeDistantLight :: View
pattern FeDistantLight = SimpleSVG "feDistantLight"

pattern FeDropShadow :: View
pattern FeDropShadow = SimpleSVG "feDropShadow"

pattern FeFlood :: View
pattern FeFlood = SimpleSVG "feFlood"

pattern FeFuncA :: View
pattern FeFuncA = SimpleSVG "feFuncA"

pattern FeFuncB :: View
pattern FeFuncB = SimpleSVG "feFuncB"

pattern FeFuncG :: View
pattern FeFuncG = SimpleSVG "feFuncG"

pattern FeFuncR :: View
pattern FeFuncR = SimpleSVG "feFuncR"

pattern FeGaussianBlur :: View
pattern FeGaussianBlur = SimpleSVG "feGaussianBlur"

pattern FeImage :: View
pattern FeImage = SimpleSVG "feImage"

pattern FeMerge :: View
pattern FeMerge = SimpleSVG "feMerge"

pattern FeMergeNode :: View
pattern FeMergeNode = SimpleSVG "feMergeNode"

pattern FeMorphology :: View
pattern FeMorphology = SimpleSVG "feMorphology"

pattern FeOffset :: View
pattern FeOffset = SimpleSVG "feOffset"

pattern FePointLight :: View
pattern FePointLight = SimpleSVG "fePointLight"

pattern FeSpecularLighting :: View
pattern FeSpecularLighting = SimpleSVG "feSpecularLighting"

pattern FeSpotLight :: View
pattern FeSpotLight = SimpleSVG "feSpotLight"

pattern FeTile :: View
pattern FeTile = SimpleSVG "feTile"

pattern FeTurbulence :: View
pattern FeTurbulence = SimpleSVG "feTurbulence"

pattern Filter :: View
pattern Filter = SimpleSVG "filter"

pattern Font :: View
pattern Font = SimpleSVG "font"

pattern FontFace :: View
pattern FontFace = SimpleSVG "font-face"

pattern FontFaceFormat :: View
pattern FontFaceFormat = SimpleSVG "font-face-format"

pattern FontFaceName :: View
pattern FontFaceName = SimpleSVG "font-face-name"

pattern FontFaceSrc :: View
pattern FontFaceSrc = SimpleSVG "font-face-src"

pattern FontFaceURI :: View
pattern FontFaceURI = SimpleSVG "font-face-uri"

pattern ForeignObject :: View
pattern ForeignObject = SimpleSVG "foreignObject"

pattern G :: View
pattern G = SimpleSVG "g"

pattern Glyph :: View
pattern Glyph = SimpleSVG "glyph"

pattern GlyphRef :: View
pattern GlyphRef = SimpleSVG "glyphRef"

pattern Hatch :: View
pattern Hatch = SimpleSVG "hatch"

pattern Hatchpath :: View
pattern Hatchpath = SimpleSVG "hatchpath"

pattern Hkern :: View
pattern Hkern = SimpleSVG "hkern"

pattern Iframe :: View
pattern Iframe = SimpleSVG "iframe"

pattern Image :: View
pattern Image = SimpleSVG "image"

pattern Line :: View
pattern Line = SimpleSVG "line"

pattern LinearGradient :: View
pattern LinearGradient = SimpleSVG "linearGradient"

pattern Marker :: View
pattern Marker = SimpleSVG "marker"

pattern Mask :: View
pattern Mask = SimpleSVG "mask"

pattern Mesh :: View
pattern Mesh = SimpleSVG "mesh"

pattern Meshgradient :: View
pattern Meshgradient = SimpleSVG "meshgradient"

pattern Meshpatch :: View
pattern Meshpatch = SimpleSVG "meshpatch"

pattern Meshrow :: View
pattern Meshrow = SimpleSVG "meshrow"

pattern Metadata :: View
pattern Metadata = SimpleSVG "metadata"

pattern MissingGlyph :: View
pattern MissingGlyph = SimpleSVG "missing-glyph"

pattern Mpath :: View
pattern Mpath = SimpleSVG "mpath"

pattern Path :: View
pattern Path = SimpleSVG "path"

pattern Pattern :: View
pattern Pattern = SimpleSVG "pattern"

pattern Polygon :: View
pattern Polygon = SimpleSVG "polygon"

pattern Polyline :: View
pattern Polyline = SimpleSVG "polyline"

pattern RadialGradient :: View
pattern RadialGradient = SimpleSVG "radialGradient"

pattern Rect :: View
pattern Rect = SimpleSVG "rect"

pattern Script :: View
pattern Script = SimpleSVG "script"

pattern Set :: View
pattern Set = SimpleSVG "set"

pattern Solidcolor :: View
pattern Solidcolor = SimpleSVG "solidcolor"

pattern Stop :: View
pattern Stop = SimpleSVG "stop"

pattern Style :: View
pattern Style = SimpleSVG "style"

pattern Svg :: View
pattern Svg = SimpleSVG "svg"

pattern Switch :: View
pattern Switch = SimpleSVG "switch"

pattern Symbol :: View
pattern Symbol = SimpleSVG "symbol"

pattern Text :: View
pattern Text = SimpleSVG "text"

pattern TextPath :: View
pattern TextPath = SimpleSVG "textPath"

pattern Title :: View
pattern Title = SimpleSVG "title"

pattern Tref :: View
pattern Tref = SimpleSVG "tref"

pattern Tspan :: View
pattern Tspan = SimpleSVG "tspan"

pattern Unknown :: View
pattern Unknown = SimpleSVG "unknown"

pattern Use :: View
pattern Use = SimpleSVG "use"

pattern Video :: View
pattern Video = SimpleSVG "video"

pattern View_ :: View
pattern View_ = SimpleSVG "view"

pattern Vkern :: View
pattern Vkern = SimpleSVG "vkern"

--------------------------------------------------------------------------------
-- SVG XLinks

pattern XLinkActuate ::  Txt -> View -> View
pattern XLinkActuate v a = XLink "xlink:actuate" v a

pattern XLinkArcrole ::  Txt -> View -> View
pattern XLinkArcrole v a = XLink "xlink:arcrole" v a

pattern XLinkHref ::  Txt -> View -> View
pattern XLinkHref v a = XLink "xlink:href" v a

pattern XLinkRole ::  Txt -> View -> View
pattern XLinkRole v a = XLink "xlink:role" v a

pattern XLinkShow ::  Txt -> View -> View
pattern XLinkShow v a = XLink "xlink:show" v a

pattern XLinkTitle ::  Txt -> View -> View
pattern XLinkTitle v a = XLink "xlink:title" v a

pattern XLinkType ::  Txt -> View -> View
pattern XLinkType v a = XLink "xlink:type" v a

--------------------------------------------------------------------------------
-- SVG Properties

pattern About :: Txt -> View -> View
pattern About v a = Attribute "about" v a

pattern AccentHeight :: Txt -> View -> View
pattern AccentHeight v a = Attribute "accent-height" v a

pattern Accumulate :: Txt -> View -> View
pattern Accumulate v a = Attribute "accumulate" v a

pattern Additive :: Txt -> View -> View
pattern Additive v a = Attribute "additive" v a

pattern AlignmentBaseline :: Txt -> View -> View
pattern AlignmentBaseline v a = Attribute "alignment-baseline" v a

pattern AllowReorder :: Txt -> View -> View
pattern AllowReorder v a = Attribute "allowReorder" v a

pattern Alphabetic :: Txt -> View -> View
pattern Alphabetic v a = Attribute "alphabetic" v a

pattern Amplitude :: Txt -> View -> View
pattern Amplitude v a = Attribute "amplitude" v a

pattern ArabicForm :: Txt -> View -> View
pattern ArabicForm v a = Attribute "arabic-form" v a

pattern Ascent :: Txt -> View -> View
pattern Ascent v a = Attribute "ascent" v a

pattern AttributeName :: Txt -> View -> View
pattern AttributeName v a = Attribute "attributeName" v a

pattern AttributeType :: Txt -> View -> View
pattern AttributeType v a = Attribute "attributeType" v a

pattern AutoReverse :: Txt -> View -> View
pattern AutoReverse v a = Attribute "autoReverse" v a

pattern Azimuth :: Txt -> View -> View
pattern Azimuth v a = Attribute "azimuth" v a

pattern BaseFrequency :: Txt -> View -> View
pattern BaseFrequency v a = Attribute "baseFrequency" v a

pattern BaselineShift :: Txt -> View -> View
pattern BaselineShift v a = Attribute "baseline-shift" v a

pattern BaseProfile :: Txt -> View -> View
pattern BaseProfile v a = Attribute "baseProfile" v a

pattern Bbox :: Txt -> View -> View
pattern Bbox v a = Attribute "bbox" v a

pattern Begin :: Txt -> View -> View
pattern Begin v a = Attribute "begin" v a

pattern Bias :: Txt -> View -> View
pattern Bias v a = Attribute "bias" v a

pattern By :: Txt -> View -> View
pattern By v a = Attribute "by" v a

pattern CalcMode :: Txt -> View -> View
pattern CalcMode v a = Attribute "calcMode" v a

pattern CapHeight :: Txt -> View -> View
pattern CapHeight v a = Attribute "cap-height" v a

pattern Clip :: Txt -> View -> View
pattern Clip v a = Attribute "clip" v a

pattern ClipPath_ :: Txt -> View -> View
pattern ClipPath_ v a = Attribute "clipPath" v a

pattern ClipPathUnits :: Txt -> View -> View
pattern ClipPathUnits v a = Attribute "clipPathUnits" v a

pattern ClipRule :: Txt -> View -> View
pattern ClipRule v a = Attribute "clip-rule" v a

pattern Color :: Txt -> View -> View
pattern Color v a = Attribute "color" v a

pattern ColorInterpolation :: Txt -> View -> View
pattern ColorInterpolation v a = Attribute "color-interpolation" v a

pattern ColorInterpolationFilters :: Txt -> View -> View
pattern ColorInterpolationFilters v a = Attribute "color-interpolation-filters" v a

pattern ColorProfile_ :: Txt -> View -> View
pattern ColorProfile_ v a = Attribute "color-profile" v a

pattern ColorRendering :: Txt -> View -> View
pattern ColorRendering v a = Attribute "color-rendering" v a

pattern ContentScriptType :: Txt -> View -> View
pattern ContentScriptType v a = Attribute "contentScriptType" v a

pattern ContentStyleType :: Txt -> View -> View
pattern ContentStyleType v a = Attribute "contentStyleType" v a

pattern Cursor_ :: Txt -> View -> View
pattern Cursor_ v a = Attribute "cursor" v a

pattern Cx :: Txt -> View -> View
pattern Cx v a = Attribute "cx" v a

pattern Cy :: Txt -> View -> View
pattern Cy v a = Attribute "cy" v a

pattern D :: Txt -> View -> View
pattern D v a = Attribute "d" v a

pattern Datatype :: Txt -> View -> View
pattern Datatype v a = Attribute "datatype" v a

pattern Decelerate :: Txt -> View -> View
pattern Decelerate v a = Attribute "decelerate" v a

pattern Descent :: Txt -> View -> View
pattern Descent v a = Attribute "descent" v a

pattern DiffuseConstant :: Txt -> View -> View
pattern DiffuseConstant v a = Attribute "diffuseConstant" v a

pattern Direction :: Txt -> View -> View
pattern Direction v a = Attribute "direction" v a

pattern Display :: Txt -> View -> View
pattern Display v a = Attribute "display" v a

pattern Divisor :: Txt -> View -> View
pattern Divisor v a = Attribute "divisor" v a

pattern DominantBaseline :: Txt -> View -> View
pattern DominantBaseline v a = Attribute "dominant-baseline" v a

pattern Dur :: Txt -> View -> View
pattern Dur v a = Attribute "dur" v a

pattern Dx :: Txt -> View -> View
pattern Dx v a = Attribute "dx" v a

pattern Dy :: Txt -> View -> View
pattern Dy v a = Attribute "dy" v a

pattern EdgeMode :: Txt -> View -> View
pattern EdgeMode v a = Attribute "edgeMode" v a

pattern Elevation :: Txt -> View -> View
pattern Elevation v a = Attribute "elevation" v a

pattern EnableBackground :: Txt -> View -> View
pattern EnableBackground v a = Attribute "enable-background" v a

pattern End :: Txt -> View -> View
pattern End v a = Attribute "end" v a

pattern Exponent :: Txt -> View -> View
pattern Exponent v a = Attribute "exponent" v a

pattern ExternalResourcesRequired :: Txt -> View -> View
pattern ExternalResourcesRequired v a = Attribute "externalResourcesRequired" v a

pattern Fill :: Txt -> View -> View
pattern Fill v a = Attribute "fill" v a

pattern FillOpacity :: Txt -> View -> View
pattern FillOpacity v a = Attribute "fill-opacity" v a

pattern FillRule :: Txt -> View -> View
pattern FillRule v a = Attribute "fill-rule" v a

pattern Filter_ :: Txt -> View -> View
pattern Filter_ v a = Attribute "filter" v a

pattern FilterRes :: Txt -> View -> View
pattern FilterRes v a = Attribute "filterRes" v a

pattern FilterUnits :: Txt -> View -> View
pattern FilterUnits v a = Attribute "filterUnits" v a

pattern FloodOpacity :: Txt -> View -> View
pattern FloodOpacity v a = Attribute "flood-opacity" v a

pattern FloodColor :: Txt -> View -> View
pattern FloodColor v a = Attribute "flood-color" v a

pattern Focusable :: Txt -> View -> View
pattern Focusable v a = Attribute "focusable" v a

pattern FontFamily :: Txt -> View -> View
pattern FontFamily v a = Attribute "font-family" v a

pattern FontSize :: Txt -> View -> View
pattern FontSize v a = Attribute "font-size" v a

pattern FontSizeAdjust :: Txt -> View -> View
pattern FontSizeAdjust v a = Attribute "font-size-adjust" v a

pattern FontStretch :: Txt -> View -> View
pattern FontStretch v a = Attribute "font-stretch" v a

pattern FontStyle :: Txt -> View -> View
pattern FontStyle v a = Attribute "font-style" v a

pattern FontVariant :: Txt -> View -> View
pattern FontVariant v a = Attribute "font-variant" v a

pattern FontWeight :: Txt -> View -> View
pattern FontWeight v a = Attribute "font-weight" v a

pattern Format :: Txt -> View -> View
pattern Format v a = Attribute "format" v a

pattern From :: Txt -> View -> View
pattern From v a = Attribute "from" v a

pattern Fx :: Txt -> View -> View
pattern Fx v a = Attribute "fx" v a

pattern Fy :: Txt -> View -> View
pattern Fy v a = Attribute "fy" v a

pattern G1 :: Txt -> View -> View
pattern G1 v a = Attribute "g1" v a

pattern G2 :: Txt -> View -> View
pattern G2 v a = Attribute "g2" v a

pattern GlyphName :: Txt -> View -> View
pattern GlyphName v a = Attribute "glyph-name" v a

pattern GlyphOrientationHorizontal :: Txt -> View -> View
pattern GlyphOrientationHorizontal v a = Attribute "glyph-orientation-horizontal" v a

pattern GlyphOrientationVertical :: Txt -> View -> View
pattern GlyphOrientationVertical v a = Attribute "glyph-orientation-vertical" v a

pattern GlyphRef_ :: Txt -> View -> View
pattern GlyphRef_ v a = Attribute "glyphRef" v a

pattern GradientTransform :: Txt -> View -> View
pattern GradientTransform v a = Attribute "gradientTransform" v a

pattern GradientUnits :: Txt -> View -> View
pattern GradientUnits v a = Attribute "gradientUnits" v a

pattern Hanging :: Txt -> View -> View
pattern Hanging v a = Attribute "hanging" v a

pattern HorizAdvX :: Txt -> View -> View
pattern HorizAdvX v a = Attribute "horiz-adv-x" v a

pattern HorizOriginX :: Txt -> View -> View
pattern HorizOriginX v a = Attribute "horiz-origin-x" v a

pattern Ideographic :: Txt -> View -> View
pattern Ideographic v a = Attribute "ideographic" v a

pattern ImageRendering :: Txt -> View -> View
pattern ImageRendering v a = Attribute "image-rendering" v a

pattern In2 :: Txt -> View -> View
pattern In2 v a = Attribute "in2" v a

pattern In :: Txt -> View -> View
pattern In v a = Attribute "in" v a

pattern Inlist :: Txt -> View -> View
pattern Inlist v a = Attribute "inlist" v a

pattern Intercept :: Txt -> View -> View
pattern Intercept v a = Attribute "intercept" v a

pattern K1 :: Txt -> View -> View
pattern K1 v a = Attribute "k1" v a

pattern K2 :: Txt -> View -> View
pattern K2 v a = Attribute "k2" v a

pattern K3 :: Txt -> View -> View
pattern K3 v a = Attribute "k3" v a

pattern K4 :: Txt -> View -> View
pattern K4 v a = Attribute "k4" v a

pattern K :: Txt -> View -> View
pattern K v a = Attribute "k" v a

pattern KernelMatrix :: Txt -> View -> View
pattern KernelMatrix v a = Attribute "kernelMatrix" v a

pattern KernelUnitLength :: Txt -> View -> View
pattern KernelUnitLength v a = Attribute "kernelUnitLength" v a

pattern Kerning :: Txt -> View -> View
pattern Kerning v a = Attribute "kerning" v a

pattern KeyPoints :: Txt -> View -> View
pattern KeyPoints v a = Attribute "keyPoints" v a

pattern KeySplines :: Txt -> View -> View
pattern KeySplines v a = Attribute "keySplines" v a

pattern KeyTimes :: Txt -> View -> View
pattern KeyTimes v a = Attribute "keyTimes" v a

pattern LengthAdjust :: Txt -> View -> View
pattern LengthAdjust v a = Attribute "lengthAdjust" v a

pattern LetterSpacing :: Txt -> View -> View
pattern LetterSpacing v a = Attribute "letter-spacing" v a

pattern LightingColor :: Txt -> View -> View
pattern LightingColor v a = Attribute "lighting-color" v a

pattern LimitingConeAngle :: Txt -> View -> View
pattern LimitingConeAngle v a = Attribute "limitingConeAngle" v a

pattern Local :: Txt -> View -> View
pattern Local v a = Attribute "local" v a

pattern MarkerEnd :: Txt -> View -> View
pattern MarkerEnd v a = Attribute "markerEnd" v a

pattern MarkerHeight :: Txt -> View -> View
pattern MarkerHeight v a = Attribute "markerHeight" v a

pattern MarkerMid :: Txt -> View -> View
pattern MarkerMid v a = Attribute "markerMid" v a

pattern MarkerStart :: Txt -> View -> View
pattern MarkerStart v a = Attribute "markerStart" v a

pattern MarkerUnits :: Txt -> View -> View
pattern MarkerUnits v a = Attribute "markerUnits" v a

pattern MarkerWidth :: Txt -> View -> View
pattern MarkerWidth v a = Attribute "markerWidth" v a

pattern Mask_ :: Txt -> View -> View
pattern Mask_ v a = Attribute "mask" v a

pattern MaskContentUnits :: Txt -> View -> View
pattern MaskContentUnits v a = Attribute "maskContentUnits" v a

pattern MaskUnits :: Txt -> View -> View
pattern MaskUnits v a = Attribute "maskUnits" v a

pattern Mathematical :: Txt -> View -> View
pattern Mathematical v a = Attribute "mathematical" v a

pattern Mode :: Txt -> View -> View
pattern Mode v a = Attribute "mode" v a

pattern NumOctaves :: Txt -> View -> View
pattern NumOctaves v a = Attribute "numOctaves" v a

pattern Ofaet :: Txt -> View -> View
pattern Ofaet v a = Attribute "offset" v a

pattern Opacity :: Txt -> View -> View
pattern Opacity v a = Attribute "opacity" v a

pattern Operator :: Txt -> View -> View
pattern Operator v a = Attribute "operator" v a

pattern Order :: Txt -> View -> View
pattern Order v a = Attribute "order" v a

pattern Orient :: Txt -> View -> View
pattern Orient v a = Attribute "orient" v a

pattern Orientation :: Txt -> View -> View
pattern Orientation v a = Attribute "orientation" v a

pattern Origin :: Txt -> View -> View
pattern Origin v a = Attribute "origin" v a

pattern Overflow :: Txt -> View -> View
pattern Overflow v a = Attribute "overflow" v a

pattern OverlinePosition :: Txt -> View -> View
pattern OverlinePosition v a = Attribute "overline-position" v a

pattern OverlineThickness :: Txt -> View -> View
pattern OverlineThickness v a = Attribute "overline-thickness" v a

pattern PaintOrder :: Txt -> View -> View
pattern PaintOrder v a = Attribute "paint-order" v a

pattern Panose1 :: Txt -> View -> View
pattern Panose1 v a = Attribute "panose1" v a

pattern PathLength :: Txt -> View -> View
pattern PathLength v a = Attribute "pathLength" v a

pattern PatternContentUnits :: Txt -> View -> View
pattern PatternContentUnits v a = Attribute "patternContentUnits" v a

pattern PatternTransform :: Txt -> View -> View
pattern PatternTransform v a = Attribute "patternTransform" v a

pattern PatternUnits :: Txt -> View -> View
pattern PatternUnits v a = Attribute "patternUnits" v a

pattern PointerEvents :: Txt -> View -> View
pattern PointerEvents v a = Attribute "pointerEvents" v a

pattern Points :: Txt -> View -> View
pattern Points v a = Attribute "points" v a

pattern PointsAtX :: Txt -> View -> View
pattern PointsAtX v a = Attribute "pointsAtX" v a

pattern PointsAtY :: Txt -> View -> View
pattern PointsAtY v a = Attribute "pointsAtY" v a

pattern PointsAtZ :: Txt -> View -> View
pattern PointsAtZ v a = Attribute "pointsAtZ" v a

pattern Prefix :: Txt -> View -> View
pattern Prefix v a = Attribute "prefix" v a

pattern PreserveAlpha :: Txt -> View -> View
pattern PreserveAlpha v a = Attribute "preserveAlpha" v a

pattern PreserveAspectRatio :: Txt -> View -> View
pattern PreserveAspectRatio v a = Attribute "preserveAspectRatio" v a

pattern PrimitiveUnits :: Txt -> View -> View
pattern PrimitiveUnits v a = Attribute "primitiveUnits" v a

pattern R :: Txt -> View -> View
pattern R v a = Attribute "r" v a

pattern Radius :: Txt -> View -> View
pattern Radius v a = Attribute "radius" v a

pattern RefX :: Txt -> View -> View
pattern RefX v a = Attribute "refX" v a

pattern RefY :: Txt -> View -> View
pattern RefY v a = Attribute "refY" v a

pattern RenderingIntent :: Txt -> View -> View
pattern RenderingIntent v a = Attribute "rendering-intent" v a

pattern RepeatCount :: Txt -> View -> View
pattern RepeatCount v a = Attribute "repeatCount" v a

pattern RepeatDur :: Txt -> View -> View
pattern RepeatDur v a = Attribute "repeatDur" v a

pattern RequiredExtensions :: Txt -> View -> View
pattern RequiredExtensions v a = Attribute "requiredExtensions" v a

pattern RequiredFeatures :: Txt -> View -> View
pattern RequiredFeatures v a = Attribute "requiredFeatures" v a

pattern Resource :: Txt -> View -> View
pattern Resource v a = Attribute "resource" v a

pattern Restart :: Txt -> View -> View
pattern Restart v a = Attribute "restart" v a

pattern Result :: Txt -> View -> View
pattern Result v a = Attribute "result" v a

pattern Results :: Txt -> View -> View
pattern Results v a = Attribute "results" v a

pattern Rotate :: Txt -> View -> View
pattern Rotate v a = Attribute "rotate" v a

pattern Rx :: Txt -> View -> View
pattern Rx v a = Attribute "rx" v a

pattern Ry :: Txt -> View -> View
pattern Ry v a = Attribute "ry" v a

pattern Scale :: Txt -> View -> View
pattern Scale v a = Attribute "scale" v a

pattern Security :: Txt -> View -> View
pattern Security v a = Attribute "security" v a

pattern Seed :: Txt -> View -> View
pattern Seed v a = Attribute "seed" v a

pattern ShapeRendering :: Txt -> View -> View
pattern ShapeRendering v a = Attribute "shape-rendering" v a

pattern Slope :: Txt -> View -> View
pattern Slope v a = Attribute "slope" v a

pattern Spacing :: Txt -> View -> View
pattern Spacing v a = Attribute "spacing" v a

pattern SpecularConstant :: Txt -> View -> View
pattern SpecularConstant v a = Attribute "specularConstant" v a

pattern SpecularExponent :: Txt -> View -> View
pattern SpecularExponent v a = Attribute "specularExponent" v a

pattern Speed :: Txt -> View -> View
pattern Speed v a = Attribute "speed" v a

pattern SpreadMethod :: Txt -> View -> View
pattern SpreadMethod v a = Attribute "spreadMethod" v a

pattern StartOfaet :: Txt -> View -> View
pattern StartOfaet v a = Attribute "startOfaet" v a

pattern StdDeviation :: Txt -> View -> View
pattern StdDeviation v a = Attribute "stdDeviation" v a

pattern Stemh :: Txt -> View -> View
pattern Stemh v a = Attribute "stemh" v a

pattern Stemv :: Txt -> View -> View
pattern Stemv v a = Attribute "stemv" v a

pattern StitchTiles :: Txt -> View -> View
pattern StitchTiles v a = Attribute "stitchTiles" v a

pattern StopColor :: Txt -> View -> View
pattern StopColor v a = Attribute "stop-color" v a

pattern StopOpacity :: Txt -> View -> View
pattern StopOpacity v a = Attribute "stop-opacity" v a

pattern StrikethroughPosition :: Txt -> View -> View
pattern StrikethroughPosition v a = Attribute "strikethrough-position" v a

pattern StrikethroughThickness :: Txt -> View -> View
pattern StrikethroughThickness v a = Attribute "strikethrough-thickness" v a

pattern String :: Txt -> View -> View
pattern String v a = Attribute "string" v a

pattern Stroke :: Txt -> View -> View
pattern Stroke v a = Attribute "stroke" v a

pattern StrokeDasharray :: Txt -> View -> View
pattern StrokeDasharray v a = Attribute "stroke-dasharray" v a

pattern StrokeDashoffset :: Txt -> View -> View
pattern StrokeDashoffset v a = Attribute "stroke-dashoffset" v a

pattern StrokeLinecap :: Txt -> View -> View
pattern StrokeLinecap v a = Attribute "stroke-linecap" v a

pattern StrokeLinejoin :: Txt -> View -> View
pattern StrokeLinejoin v a = Attribute "stroke-linejoin" v a

pattern StrokeMiterlimit :: Txt -> View -> View
pattern StrokeMiterlimit v a = Attribute "stroke-miterlimit" v a

pattern StrokeWidth :: Txt -> View -> View
pattern StrokeWidth v a = Attribute "stroke-width" v a

pattern StrokeOpacity :: Txt -> View -> View
pattern StrokeOpacity v a = Attribute "stroke-opacity" v a

pattern SurfaceScale :: Txt -> View -> View
pattern SurfaceScale v a = Attribute "surfaceScale" v a

pattern SystemLanguage :: Txt -> View -> View
pattern SystemLanguage v a = Attribute "systemLanguage" v a

pattern TableValues :: Txt -> View -> View
pattern TableValues v a = Attribute "tableValues" v a

pattern TargetX :: Txt -> View -> View
pattern TargetX v a = Attribute "targetX" v a

pattern TargetY :: Txt -> View -> View
pattern TargetY v a = Attribute "targetY" v a

pattern TextAnchor :: Txt -> View -> View
pattern TextAnchor v a = Attribute "text-anchor" v a

pattern TextDecoration :: Txt -> View -> View
pattern TextDecoration v a = Attribute "text-decoration" v a

pattern TextLength :: Txt -> View -> View
pattern TextLength v a = Attribute "textLength" v a

pattern TextRendering :: Txt -> View -> View
pattern TextRendering v a = Attribute "text-rendering" v a

pattern To :: Txt -> View -> View
pattern To v a = Attribute "to" v a

pattern Transform :: Txt -> View -> View
pattern Transform v a = Attribute "transform" v a

pattern Typeof :: Txt -> View -> View
pattern Typeof v a = Attribute "typeof" v a

pattern U1 :: Txt -> View -> View
pattern U1 v a = Attribute "u1" v a

pattern U2 :: Txt -> View -> View
pattern U2 v a = Attribute "u2" v a

pattern UnderlinePosition :: Txt -> View -> View
pattern UnderlinePosition v a = Attribute "underline-position" v a

pattern UnderlineThickness :: Txt -> View -> View
pattern UnderlineThickness v a = Attribute "underline-thickness" v a

pattern Unicode :: Txt -> View -> View
pattern Unicode v a = Attribute "unicode" v a

pattern UnicodeBidi :: Txt -> View -> View
pattern UnicodeBidi v a = Attribute "unicode-bidi" v a

pattern UnicodeRange :: Txt -> View -> View
pattern UnicodeRange v a = Attribute "unicode-range" v a

pattern UnitsPerEm :: Txt -> View -> View
pattern UnitsPerEm v a = Attribute "units-per-em" v a

pattern Unselectable :: Txt -> View -> View
pattern Unselectable v a = Attribute "unselectable" v a

pattern VAlphabetic :: Txt -> View -> View
pattern VAlphabetic v a = Attribute "v-alphabetic" v a

pattern Values :: Txt -> View -> View
pattern Values v a = Attribute "values" v a

pattern VectorEffect :: Txt -> View -> View
pattern VectorEffect v a = Attribute "vector-effect" v a

pattern Version :: Txt -> View -> View
pattern Version v a = Attribute "version" v a

pattern VertAdvY :: Txt -> View -> View
pattern VertAdvY v a = Attribute "vert-adv-y" v a

pattern VertOriginX :: Txt -> View -> View
pattern VertOriginX v a = Attribute "vert-origin-x" v a

pattern VertOriginY :: Txt -> View -> View
pattern VertOriginY v a = Attribute "vert-origin-y" v a

pattern VHanging :: Txt -> View -> View
pattern VHanging v a = Attribute "v-hanging" v a

pattern Id :: Txt -> View -> View
pattern Id v a = Attribute "id" v a

pattern VIdeographic :: Txt -> View -> View
pattern VIdeographic v a = Attribute "v-ideographic" v a

pattern ViewBox :: Txt -> View -> View
pattern ViewBox v a = Attribute "viewBox" v a

pattern ViewTarget :: Txt -> View -> View
pattern ViewTarget v a = Attribute "viewTarget" v a

pattern Visibility :: Txt -> View -> View
pattern Visibility v a = Attribute "visibility" v a

pattern VMathematical :: Txt -> View -> View
pattern VMathematical v a = Attribute "v-mathematical" v a

pattern Vocab :: Txt -> View -> View
pattern Vocab v a = Attribute "vocab" v a

pattern Widths :: Txt -> View -> View
pattern Widths v a = Attribute "widths" v a

pattern WordSpacing :: Txt -> View -> View
pattern WordSpacing v a = Attribute "word-spacing" v a

pattern WritingMode :: Txt -> View -> View
pattern WritingMode v a = Attribute "writing-mode" v a

pattern X1 :: Txt -> View -> View
pattern X1 v a = Attribute "x1" v a

pattern X2 :: Txt -> View -> View
pattern X2 v a = Attribute "x2" v a

pattern X :: Txt -> View -> View
pattern X v a = Attribute "x" v a

pattern XChannelSelector :: Txt -> View -> View
pattern XChannelSelector v a = Attribute "xChannelSelector" v a

pattern XHeight :: Txt -> View -> View
pattern XHeight v a = Attribute "x-height" v a

pattern XlinkActuate ::  Txt -> View -> View
pattern XlinkActuate v a = XLink "xlink:actuate" v a

pattern XlinkArcrole ::  Txt -> View -> View
pattern XlinkArcrole v a = XLink "xlink:arcrole" v a

pattern XlinkHref ::  Txt -> View -> View
pattern XlinkHref v a = XLink "xlink:href" v a

pattern XlinkRole ::  Txt -> View -> View
pattern XlinkRole v a = XLink "xlink:role" v a

pattern XlinkShow ::  Txt -> View -> View
pattern XlinkShow v a = XLink "xlink:show" v a

pattern XlinkTitle ::  Txt -> View -> View
pattern XlinkTitle v a = XLink "xlink:title" v a

pattern XlinkType ::  Txt -> View -> View
pattern XlinkType v a = XLink "xlink:type" v a

pattern XmlBase ::  Txt -> View -> View
pattern XmlBase v a = XLink "xml:base" v a

pattern XmlLang ::  Txt -> View -> View
pattern XmlLang v a = XLink "xml:lang" v a

pattern Xmlns :: Txt -> View -> View
pattern Xmlns v a = Attribute "xmlns" v a

pattern XmlnsXlink ::  Txt -> View -> View
pattern XmlnsXlink v a = XLink "xmlns:xlink" v a

pattern XmlSpace ::  Txt -> View -> View
pattern XmlSpace v a = XLink "xml:space" v a

pattern Y1 :: Txt -> View -> View
pattern Y1 v a = Attribute "y1" v a

pattern Y2 :: Txt -> View -> View
pattern Y2 v a = Attribute "y2" v a

pattern Y :: Txt -> View -> View
pattern Y v a = Attribute "y" v a

pattern YChannelSelector :: Txt -> View -> View
pattern YChannelSelector v a = Attribute "yChannelSelector" v a

pattern Z :: Txt -> View -> View
pattern Z v a = Attribute "z" v a

pattern ZoomAndPan :: Txt -> View -> View
pattern ZoomAndPan v a = Attribute "zoomAndPan" v a
