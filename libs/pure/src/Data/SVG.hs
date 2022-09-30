{-# LANGUAGE PatternSynonyms, OverloadedStrings #-}
module Data.SVG where

import Data.Txt
import Data.View (View,pattern SimpleSVG,HasFeatures(..),pattern Attribute,pattern Property,HasXLinks(..),pattern XLinks,pattern XLink)

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

pattern XLinkActuate :: HasXLinks a => Txt -> a -> a
pattern XLinkActuate v a = XLink "xlink:actuate" v a

pattern XLinkArcrole :: HasXLinks a => Txt -> a -> a
pattern XLinkArcrole v a = XLink "xlink:arcrole" v a

pattern XLinkHref :: HasXLinks a => Txt -> a -> a
pattern XLinkHref v a = XLink "xlink:href" v a

pattern XLinkRole :: HasXLinks a => Txt -> a -> a
pattern XLinkRole v a = XLink "xlink:role" v a

pattern XLinkShow :: HasXLinks a => Txt -> a -> a
pattern XLinkShow v a = XLink "xlink:show" v a

pattern XLinkTitle :: HasXLinks a => Txt -> a -> a
pattern XLinkTitle v a = XLink "xlink:title" v a

pattern XLinkType :: HasXLinks a => Txt -> a -> a
pattern XLinkType v a = XLink "xlink:type" v a

--------------------------------------------------------------------------------
-- SVG Properties

pattern About :: HasFeatures a => Txt -> a -> a
pattern About v a = Attribute "about" v a

pattern AccentHeight :: HasFeatures a => Txt -> a -> a
pattern AccentHeight v a = Attribute "accent-height" v a

pattern Accumulate :: HasFeatures a => Txt -> a -> a
pattern Accumulate v a = Attribute "accumulate" v a

pattern Additive :: HasFeatures a => Txt -> a -> a
pattern Additive v a = Attribute "additive" v a

pattern AlignmentBaseline :: HasFeatures a => Txt -> a -> a
pattern AlignmentBaseline v a = Attribute "alignment-baseline" v a

pattern AllowReorder :: HasFeatures a => Txt -> a -> a
pattern AllowReorder v a = Attribute "allowReorder" v a

pattern Alphabetic :: HasFeatures a => Txt -> a -> a
pattern Alphabetic v a = Attribute "alphabetic" v a

pattern Amplitude :: HasFeatures a => Txt -> a -> a
pattern Amplitude v a = Attribute "amplitude" v a

pattern ArabicForm :: HasFeatures a => Txt -> a -> a
pattern ArabicForm v a = Attribute "arabic-form" v a

pattern Ascent :: HasFeatures a => Txt -> a -> a
pattern Ascent v a = Attribute "ascent" v a

pattern AttributeName :: HasFeatures a => Txt -> a -> a
pattern AttributeName v a = Attribute "attributeName" v a

pattern AttributeType :: HasFeatures a => Txt -> a -> a
pattern AttributeType v a = Attribute "attributeType" v a

pattern AutoReverse :: HasFeatures a => Txt -> a -> a
pattern AutoReverse v a = Attribute "autoReverse" v a

pattern Azimuth :: HasFeatures a => Txt -> a -> a
pattern Azimuth v a = Attribute "azimuth" v a

pattern BaseFrequency :: HasFeatures a => Txt -> a -> a
pattern BaseFrequency v a = Attribute "baseFrequency" v a

pattern BaselineShift :: HasFeatures a => Txt -> a -> a
pattern BaselineShift v a = Attribute "baseline-shift" v a

pattern BaseProfile :: HasFeatures a => Txt -> a -> a
pattern BaseProfile v a = Attribute "baseProfile" v a

pattern Bbox :: HasFeatures a => Txt -> a -> a
pattern Bbox v a = Attribute "bbox" v a

pattern Begin :: HasFeatures a => Txt -> a -> a
pattern Begin v a = Attribute "begin" v a

pattern Bias :: HasFeatures a => Txt -> a -> a
pattern Bias v a = Attribute "bias" v a

pattern By :: HasFeatures a => Txt -> a -> a
pattern By v a = Attribute "by" v a

pattern CalcMode :: HasFeatures a => Txt -> a -> a
pattern CalcMode v a = Attribute "calcMode" v a

pattern CapHeight :: HasFeatures a => Txt -> a -> a
pattern CapHeight v a = Attribute "cap-height" v a

pattern Clip :: HasFeatures a => Txt -> a -> a
pattern Clip v a = Attribute "clip" v a

pattern ClipPath_ :: HasFeatures a => Txt -> a -> a
pattern ClipPath_ v a = Attribute "clipPath" v a

pattern ClipPathUnits :: HasFeatures a => Txt -> a -> a
pattern ClipPathUnits v a = Attribute "clipPathUnits" v a

pattern ClipRule :: HasFeatures a => Txt -> a -> a
pattern ClipRule v a = Attribute "clip-rule" v a

pattern Color :: HasFeatures a => Txt -> a -> a
pattern Color v a = Attribute "color" v a

pattern ColorInterpolation :: HasFeatures a => Txt -> a -> a
pattern ColorInterpolation v a = Attribute "color-interpolation" v a

pattern ColorInterpolationFilters :: HasFeatures a => Txt -> a -> a
pattern ColorInterpolationFilters v a = Attribute "color-interpolation-filters" v a

pattern ColorProfile_ :: HasFeatures a => Txt -> a -> a
pattern ColorProfile_ v a = Attribute "color-profile" v a

pattern ColorRendering :: HasFeatures a => Txt -> a -> a
pattern ColorRendering v a = Attribute "color-rendering" v a

pattern ContentScriptType :: HasFeatures a => Txt -> a -> a
pattern ContentScriptType v a = Attribute "contentScriptType" v a

pattern ContentStyleType :: HasFeatures a => Txt -> a -> a
pattern ContentStyleType v a = Attribute "contentStyleType" v a

pattern Cursor_ :: HasFeatures a => Txt -> a -> a
pattern Cursor_ v a = Attribute "cursor" v a

pattern Cx :: HasFeatures a => Txt -> a -> a
pattern Cx v a = Attribute "cx" v a

pattern Cy :: HasFeatures a => Txt -> a -> a
pattern Cy v a = Attribute "cy" v a

pattern D :: HasFeatures a => Txt -> a -> a
pattern D v a = Attribute "d" v a

pattern Datatype :: HasFeatures a => Txt -> a -> a
pattern Datatype v a = Attribute "datatype" v a

pattern Decelerate :: HasFeatures a => Txt -> a -> a
pattern Decelerate v a = Attribute "decelerate" v a

pattern Descent :: HasFeatures a => Txt -> a -> a
pattern Descent v a = Attribute "descent" v a

pattern DiffuseConstant :: HasFeatures a => Txt -> a -> a
pattern DiffuseConstant v a = Attribute "diffuseConstant" v a

pattern Direction :: HasFeatures a => Txt -> a -> a
pattern Direction v a = Attribute "direction" v a

pattern Display :: HasFeatures a => Txt -> a -> a
pattern Display v a = Attribute "display" v a

pattern Divisor :: HasFeatures a => Txt -> a -> a
pattern Divisor v a = Attribute "divisor" v a

pattern DominantBaseline :: HasFeatures a => Txt -> a -> a
pattern DominantBaseline v a = Attribute "dominant-baseline" v a

pattern Dur :: HasFeatures a => Txt -> a -> a
pattern Dur v a = Attribute "dur" v a

pattern Dx :: HasFeatures a => Txt -> a -> a
pattern Dx v a = Attribute "dx" v a

pattern Dy :: HasFeatures a => Txt -> a -> a
pattern Dy v a = Attribute "dy" v a

pattern EdgeMode :: HasFeatures a => Txt -> a -> a
pattern EdgeMode v a = Attribute "edgeMode" v a

pattern Elevation :: HasFeatures a => Txt -> a -> a
pattern Elevation v a = Attribute "elevation" v a

pattern EnableBackground :: HasFeatures a => Txt -> a -> a
pattern EnableBackground v a = Attribute "enable-background" v a

pattern End :: HasFeatures a => Txt -> a -> a
pattern End v a = Attribute "end" v a

pattern Exponent :: HasFeatures a => Txt -> a -> a
pattern Exponent v a = Attribute "exponent" v a

pattern ExternalResourcesRequired :: HasFeatures a => Txt -> a -> a
pattern ExternalResourcesRequired v a = Attribute "externalResourcesRequired" v a

pattern Fill :: HasFeatures a => Txt -> a -> a
pattern Fill v a = Attribute "fill" v a

pattern FillOpacity :: HasFeatures a => Txt -> a -> a
pattern FillOpacity v a = Attribute "fill-opacity" v a

pattern FillRule :: HasFeatures a => Txt -> a -> a
pattern FillRule v a = Attribute "fill-rule" v a

pattern Filter_ :: HasFeatures a => Txt -> a -> a
pattern Filter_ v a = Attribute "filter" v a

pattern FilterRes :: HasFeatures a => Txt -> a -> a
pattern FilterRes v a = Attribute "filterRes" v a

pattern FilterUnits :: HasFeatures a => Txt -> a -> a
pattern FilterUnits v a = Attribute "filterUnits" v a

pattern FloodOpacity :: HasFeatures a => Txt -> a -> a
pattern FloodOpacity v a = Attribute "flood-opacity" v a

pattern FloodColor :: HasFeatures a => Txt -> a -> a
pattern FloodColor v a = Attribute "flood-color" v a

pattern Focusable :: HasFeatures a => Txt -> a -> a
pattern Focusable v a = Attribute "focusable" v a

pattern FontFamily :: HasFeatures a => Txt -> a -> a
pattern FontFamily v a = Attribute "font-family" v a

pattern FontSize :: HasFeatures a => Txt -> a -> a
pattern FontSize v a = Attribute "font-size" v a

pattern FontSizeAdjust :: HasFeatures a => Txt -> a -> a
pattern FontSizeAdjust v a = Attribute "font-size-adjust" v a

pattern FontStretch :: HasFeatures a => Txt -> a -> a
pattern FontStretch v a = Attribute "font-stretch" v a

pattern FontStyle :: HasFeatures a => Txt -> a -> a
pattern FontStyle v a = Attribute "font-style" v a

pattern FontVariant :: HasFeatures a => Txt -> a -> a
pattern FontVariant v a = Attribute "font-variant" v a

pattern FontWeight :: HasFeatures a => Txt -> a -> a
pattern FontWeight v a = Attribute "font-weight" v a

pattern Format :: HasFeatures a => Txt -> a -> a
pattern Format v a = Attribute "format" v a

pattern From :: HasFeatures a => Txt -> a -> a
pattern From v a = Attribute "from" v a

pattern Fx :: HasFeatures a => Txt -> a -> a
pattern Fx v a = Attribute "fx" v a

pattern Fy :: HasFeatures a => Txt -> a -> a
pattern Fy v a = Attribute "fy" v a

pattern G1 :: HasFeatures a => Txt -> a -> a
pattern G1 v a = Attribute "g1" v a

pattern G2 :: HasFeatures a => Txt -> a -> a
pattern G2 v a = Attribute "g2" v a

pattern GlyphName :: HasFeatures a => Txt -> a -> a
pattern GlyphName v a = Attribute "glyph-name" v a

pattern GlyphOrientationHorizontal :: HasFeatures a => Txt -> a -> a
pattern GlyphOrientationHorizontal v a = Attribute "glyph-orientation-horizontal" v a

pattern GlyphOrientationVertical :: HasFeatures a => Txt -> a -> a
pattern GlyphOrientationVertical v a = Attribute "glyph-orientation-vertical" v a

pattern GlyphRef_ :: HasFeatures a => Txt -> a -> a
pattern GlyphRef_ v a = Attribute "glyphRef" v a

pattern GradientTransform :: HasFeatures a => Txt -> a -> a
pattern GradientTransform v a = Attribute "gradientTransform" v a

pattern GradientUnits :: HasFeatures a => Txt -> a -> a
pattern GradientUnits v a = Attribute "gradientUnits" v a

pattern Hanging :: HasFeatures a => Txt -> a -> a
pattern Hanging v a = Attribute "hanging" v a

pattern HorizAdvX :: HasFeatures a => Txt -> a -> a
pattern HorizAdvX v a = Attribute "horiz-adv-x" v a

pattern HorizOriginX :: HasFeatures a => Txt -> a -> a
pattern HorizOriginX v a = Attribute "horiz-origin-x" v a

pattern Ideographic :: HasFeatures a => Txt -> a -> a
pattern Ideographic v a = Attribute "ideographic" v a

pattern ImageRendering :: HasFeatures a => Txt -> a -> a
pattern ImageRendering v a = Attribute "image-rendering" v a

pattern In2 :: HasFeatures a => Txt -> a -> a
pattern In2 v a = Attribute "in2" v a

pattern In :: HasFeatures a => Txt -> a -> a
pattern In v a = Attribute "in" v a

pattern Inlist :: HasFeatures a => Txt -> a -> a
pattern Inlist v a = Attribute "inlist" v a

pattern Intercept :: HasFeatures a => Txt -> a -> a
pattern Intercept v a = Attribute "intercept" v a

pattern K1 :: HasFeatures a => Txt -> a -> a
pattern K1 v a = Attribute "k1" v a

pattern K2 :: HasFeatures a => Txt -> a -> a
pattern K2 v a = Attribute "k2" v a

pattern K3 :: HasFeatures a => Txt -> a -> a
pattern K3 v a = Attribute "k3" v a

pattern K4 :: HasFeatures a => Txt -> a -> a
pattern K4 v a = Attribute "k4" v a

pattern K :: HasFeatures a => Txt -> a -> a
pattern K v a = Attribute "k" v a

pattern KernelMatrix :: HasFeatures a => Txt -> a -> a
pattern KernelMatrix v a = Attribute "kernelMatrix" v a

pattern KernelUnitLength :: HasFeatures a => Txt -> a -> a
pattern KernelUnitLength v a = Attribute "kernelUnitLength" v a

pattern Kerning :: HasFeatures a => Txt -> a -> a
pattern Kerning v a = Attribute "kerning" v a

pattern KeyPoints :: HasFeatures a => Txt -> a -> a
pattern KeyPoints v a = Attribute "keyPoints" v a

pattern KeySplines :: HasFeatures a => Txt -> a -> a
pattern KeySplines v a = Attribute "keySplines" v a

pattern KeyTimes :: HasFeatures a => Txt -> a -> a
pattern KeyTimes v a = Attribute "keyTimes" v a

pattern LengthAdjust :: HasFeatures a => Txt -> a -> a
pattern LengthAdjust v a = Attribute "lengthAdjust" v a

pattern LetterSpacing :: HasFeatures a => Txt -> a -> a
pattern LetterSpacing v a = Attribute "letter-spacing" v a

pattern LightingColor :: HasFeatures a => Txt -> a -> a
pattern LightingColor v a = Attribute "lighting-color" v a

pattern LimitingConeAngle :: HasFeatures a => Txt -> a -> a
pattern LimitingConeAngle v a = Attribute "limitingConeAngle" v a

pattern Local :: HasFeatures a => Txt -> a -> a
pattern Local v a = Attribute "local" v a

pattern MarkerEnd :: HasFeatures a => Txt -> a -> a
pattern MarkerEnd v a = Attribute "markerEnd" v a

pattern MarkerHeight :: HasFeatures a => Txt -> a -> a
pattern MarkerHeight v a = Attribute "markerHeight" v a

pattern MarkerMid :: HasFeatures a => Txt -> a -> a
pattern MarkerMid v a = Attribute "markerMid" v a

pattern MarkerStart :: HasFeatures a => Txt -> a -> a
pattern MarkerStart v a = Attribute "markerStart" v a

pattern MarkerUnits :: HasFeatures a => Txt -> a -> a
pattern MarkerUnits v a = Attribute "markerUnits" v a

pattern MarkerWidth :: HasFeatures a => Txt -> a -> a
pattern MarkerWidth v a = Attribute "markerWidth" v a

pattern Mask_ :: HasFeatures a => Txt -> a -> a
pattern Mask_ v a = Attribute "mask" v a

pattern MaskContentUnits :: HasFeatures a => Txt -> a -> a
pattern MaskContentUnits v a = Attribute "maskContentUnits" v a

pattern MaskUnits :: HasFeatures a => Txt -> a -> a
pattern MaskUnits v a = Attribute "maskUnits" v a

pattern Mathematical :: HasFeatures a => Txt -> a -> a
pattern Mathematical v a = Attribute "mathematical" v a

pattern Mode :: HasFeatures a => Txt -> a -> a
pattern Mode v a = Attribute "mode" v a

pattern NumOctaves :: HasFeatures a => Txt -> a -> a
pattern NumOctaves v a = Attribute "numOctaves" v a

pattern Ofaet :: HasFeatures a => Txt -> a -> a
pattern Ofaet v a = Attribute "offset" v a

pattern Opacity :: HasFeatures a => Txt -> a -> a
pattern Opacity v a = Attribute "opacity" v a

pattern Operator :: HasFeatures a => Txt -> a -> a
pattern Operator v a = Attribute "operator" v a

pattern Order :: HasFeatures a => Txt -> a -> a
pattern Order v a = Attribute "order" v a

pattern Orient :: HasFeatures a => Txt -> a -> a
pattern Orient v a = Attribute "orient" v a

pattern Orientation :: HasFeatures a => Txt -> a -> a
pattern Orientation v a = Attribute "orientation" v a

pattern Origin :: HasFeatures a => Txt -> a -> a
pattern Origin v a = Attribute "origin" v a

pattern Overflow :: HasFeatures a => Txt -> a -> a
pattern Overflow v a = Attribute "overflow" v a

pattern OverlinePosition :: HasFeatures a => Txt -> a -> a
pattern OverlinePosition v a = Attribute "overline-position" v a

pattern OverlineThickness :: HasFeatures a => Txt -> a -> a
pattern OverlineThickness v a = Attribute "overline-thickness" v a

pattern PaintOrder :: HasFeatures a => Txt -> a -> a
pattern PaintOrder v a = Attribute "paint-order" v a

pattern Panose1 :: HasFeatures a => Txt -> a -> a
pattern Panose1 v a = Attribute "panose1" v a

pattern PathLength :: HasFeatures a => Txt -> a -> a
pattern PathLength v a = Attribute "pathLength" v a

pattern PatternContentUnits :: HasFeatures a => Txt -> a -> a
pattern PatternContentUnits v a = Attribute "patternContentUnits" v a

pattern PatternTransform :: HasFeatures a => Txt -> a -> a
pattern PatternTransform v a = Attribute "patternTransform" v a

pattern PatternUnits :: HasFeatures a => Txt -> a -> a
pattern PatternUnits v a = Attribute "patternUnits" v a

pattern PointerEvents :: HasFeatures a => Txt -> a -> a
pattern PointerEvents v a = Attribute "pointerEvents" v a

pattern Points :: HasFeatures a => Txt -> a -> a
pattern Points v a = Attribute "points" v a

pattern PointsAtX :: HasFeatures a => Txt -> a -> a
pattern PointsAtX v a = Attribute "pointsAtX" v a

pattern PointsAtY :: HasFeatures a => Txt -> a -> a
pattern PointsAtY v a = Attribute "pointsAtY" v a

pattern PointsAtZ :: HasFeatures a => Txt -> a -> a
pattern PointsAtZ v a = Attribute "pointsAtZ" v a

pattern Prefix :: HasFeatures a => Txt -> a -> a
pattern Prefix v a = Attribute "prefix" v a

pattern PreserveAlpha :: HasFeatures a => Txt -> a -> a
pattern PreserveAlpha v a = Attribute "preserveAlpha" v a

pattern PreserveAspectRatio :: HasFeatures a => Txt -> a -> a
pattern PreserveAspectRatio v a = Attribute "preserveAspectRatio" v a

pattern PrimitiveUnits :: HasFeatures a => Txt -> a -> a
pattern PrimitiveUnits v a = Attribute "primitiveUnits" v a

pattern R :: HasFeatures a => Txt -> a -> a
pattern R v a = Attribute "r" v a

pattern Radius :: HasFeatures a => Txt -> a -> a
pattern Radius v a = Attribute "radius" v a

pattern RefX :: HasFeatures a => Txt -> a -> a
pattern RefX v a = Attribute "refX" v a

pattern RefY :: HasFeatures a => Txt -> a -> a
pattern RefY v a = Attribute "refY" v a

pattern RenderingIntent :: HasFeatures a => Txt -> a -> a
pattern RenderingIntent v a = Attribute "rendering-intent" v a

pattern RepeatCount :: HasFeatures a => Txt -> a -> a
pattern RepeatCount v a = Attribute "repeatCount" v a

pattern RepeatDur :: HasFeatures a => Txt -> a -> a
pattern RepeatDur v a = Attribute "repeatDur" v a

pattern RequiredExtensions :: HasFeatures a => Txt -> a -> a
pattern RequiredExtensions v a = Attribute "requiredExtensions" v a

pattern RequiredFeatures :: HasFeatures a => Txt -> a -> a
pattern RequiredFeatures v a = Attribute "requiredFeatures" v a

pattern Resource :: HasFeatures a => Txt -> a -> a
pattern Resource v a = Attribute "resource" v a

pattern Restart :: HasFeatures a => Txt -> a -> a
pattern Restart v a = Attribute "restart" v a

pattern Result :: HasFeatures a => Txt -> a -> a
pattern Result v a = Attribute "result" v a

pattern Results :: HasFeatures a => Txt -> a -> a
pattern Results v a = Attribute "results" v a

pattern Rotate :: HasFeatures a => Txt -> a -> a
pattern Rotate v a = Attribute "rotate" v a

pattern Rx :: HasFeatures a => Txt -> a -> a
pattern Rx v a = Attribute "rx" v a

pattern Ry :: HasFeatures a => Txt -> a -> a
pattern Ry v a = Attribute "ry" v a

pattern Scale :: HasFeatures a => Txt -> a -> a
pattern Scale v a = Attribute "scale" v a

pattern Security :: HasFeatures a => Txt -> a -> a
pattern Security v a = Attribute "security" v a

pattern Seed :: HasFeatures a => Txt -> a -> a
pattern Seed v a = Attribute "seed" v a

pattern ShapeRendering :: HasFeatures a => Txt -> a -> a
pattern ShapeRendering v a = Attribute "shape-rendering" v a

pattern Slope :: HasFeatures a => Txt -> a -> a
pattern Slope v a = Attribute "slope" v a

pattern Spacing :: HasFeatures a => Txt -> a -> a
pattern Spacing v a = Attribute "spacing" v a

pattern SpecularConstant :: HasFeatures a => Txt -> a -> a
pattern SpecularConstant v a = Attribute "specularConstant" v a

pattern SpecularExponent :: HasFeatures a => Txt -> a -> a
pattern SpecularExponent v a = Attribute "specularExponent" v a

pattern Speed :: HasFeatures a => Txt -> a -> a
pattern Speed v a = Attribute "speed" v a

pattern SpreadMethod :: HasFeatures a => Txt -> a -> a
pattern SpreadMethod v a = Attribute "spreadMethod" v a

pattern StartOfaet :: HasFeatures a => Txt -> a -> a
pattern StartOfaet v a = Attribute "startOfaet" v a

pattern StdDeviation :: HasFeatures a => Txt -> a -> a
pattern StdDeviation v a = Attribute "stdDeviation" v a

pattern Stemh :: HasFeatures a => Txt -> a -> a
pattern Stemh v a = Attribute "stemh" v a

pattern Stemv :: HasFeatures a => Txt -> a -> a
pattern Stemv v a = Attribute "stemv" v a

pattern StitchTiles :: HasFeatures a => Txt -> a -> a
pattern StitchTiles v a = Attribute "stitchTiles" v a

pattern StopColor :: HasFeatures a => Txt -> a -> a
pattern StopColor v a = Attribute "stop-color" v a

pattern StopOpacity :: HasFeatures a => Txt -> a -> a
pattern StopOpacity v a = Attribute "stop-opacity" v a

pattern StrikethroughPosition :: HasFeatures a => Txt -> a -> a
pattern StrikethroughPosition v a = Attribute "strikethrough-position" v a

pattern StrikethroughThickness :: HasFeatures a => Txt -> a -> a
pattern StrikethroughThickness v a = Attribute "strikethrough-thickness" v a

pattern String :: HasFeatures a => Txt -> a -> a
pattern String v a = Attribute "string" v a

pattern Stroke :: HasFeatures a => Txt -> a -> a
pattern Stroke v a = Attribute "stroke" v a

pattern StrokeDasharray :: HasFeatures a => Txt -> a -> a
pattern StrokeDasharray v a = Attribute "stroke-dasharray" v a

pattern StrokeDashoffset :: HasFeatures a => Txt -> a -> a
pattern StrokeDashoffset v a = Attribute "stroke-dashoffset" v a

pattern StrokeLinecap :: HasFeatures a => Txt -> a -> a
pattern StrokeLinecap v a = Attribute "stroke-linecap" v a

pattern StrokeLinejoin :: HasFeatures a => Txt -> a -> a
pattern StrokeLinejoin v a = Attribute "stroke-linejoin" v a

pattern StrokeMiterlimit :: HasFeatures a => Txt -> a -> a
pattern StrokeMiterlimit v a = Attribute "stroke-miterlimit" v a

pattern StrokeWidth :: HasFeatures a => Txt -> a -> a
pattern StrokeWidth v a = Attribute "stroke-width" v a

pattern StrokeOpacity :: HasFeatures a => Txt -> a -> a
pattern StrokeOpacity v a = Attribute "stroke-opacity" v a

pattern SurfaceScale :: HasFeatures a => Txt -> a -> a
pattern SurfaceScale v a = Attribute "surfaceScale" v a

pattern SystemLanguage :: HasFeatures a => Txt -> a -> a
pattern SystemLanguage v a = Attribute "systemLanguage" v a

pattern TableValues :: HasFeatures a => Txt -> a -> a
pattern TableValues v a = Attribute "tableValues" v a

pattern TargetX :: HasFeatures a => Txt -> a -> a
pattern TargetX v a = Attribute "targetX" v a

pattern TargetY :: HasFeatures a => Txt -> a -> a
pattern TargetY v a = Attribute "targetY" v a

pattern TextAnchor :: HasFeatures a => Txt -> a -> a
pattern TextAnchor v a = Attribute "text-anchor" v a

pattern TextDecoration :: HasFeatures a => Txt -> a -> a
pattern TextDecoration v a = Attribute "text-decoration" v a

pattern TextLength :: HasFeatures a => Txt -> a -> a
pattern TextLength v a = Attribute "textLength" v a

pattern TextRendering :: HasFeatures a => Txt -> a -> a
pattern TextRendering v a = Attribute "text-rendering" v a

pattern To :: HasFeatures a => Txt -> a -> a
pattern To v a = Attribute "to" v a

pattern Transform :: HasFeatures a => Txt -> a -> a
pattern Transform v a = Attribute "transform" v a

pattern Typeof :: HasFeatures a => Txt -> a -> a
pattern Typeof v a = Attribute "typeof" v a

pattern U1 :: HasFeatures a => Txt -> a -> a
pattern U1 v a = Attribute "u1" v a

pattern U2 :: HasFeatures a => Txt -> a -> a
pattern U2 v a = Attribute "u2" v a

pattern UnderlinePosition :: HasFeatures a => Txt -> a -> a
pattern UnderlinePosition v a = Attribute "underline-position" v a

pattern UnderlineThickness :: HasFeatures a => Txt -> a -> a
pattern UnderlineThickness v a = Attribute "underline-thickness" v a

pattern Unicode :: HasFeatures a => Txt -> a -> a
pattern Unicode v a = Attribute "unicode" v a

pattern UnicodeBidi :: HasFeatures a => Txt -> a -> a
pattern UnicodeBidi v a = Attribute "unicode-bidi" v a

pattern UnicodeRange :: HasFeatures a => Txt -> a -> a
pattern UnicodeRange v a = Attribute "unicode-range" v a

pattern UnitsPerEm :: HasFeatures a => Txt -> a -> a
pattern UnitsPerEm v a = Attribute "units-per-em" v a

pattern Unselectable :: HasFeatures a => Txt -> a -> a
pattern Unselectable v a = Attribute "unselectable" v a

pattern VAlphabetic :: HasFeatures a => Txt -> a -> a
pattern VAlphabetic v a = Attribute "v-alphabetic" v a

pattern Values :: HasFeatures a => Txt -> a -> a
pattern Values v a = Attribute "values" v a

pattern VectorEffect :: HasFeatures a => Txt -> a -> a
pattern VectorEffect v a = Attribute "vector-effect" v a

pattern Version :: HasFeatures a => Txt -> a -> a
pattern Version v a = Attribute "version" v a

pattern VertAdvY :: HasFeatures a => Txt -> a -> a
pattern VertAdvY v a = Attribute "vert-adv-y" v a

pattern VertOriginX :: HasFeatures a => Txt -> a -> a
pattern VertOriginX v a = Attribute "vert-origin-x" v a

pattern VertOriginY :: HasFeatures a => Txt -> a -> a
pattern VertOriginY v a = Attribute "vert-origin-y" v a

pattern VHanging :: HasFeatures a => Txt -> a -> a
pattern VHanging v a = Attribute "v-hanging" v a

pattern Id :: HasFeatures a => Txt -> a -> a
pattern Id v a = Attribute "id" v a

pattern VIdeographic :: HasFeatures a => Txt -> a -> a
pattern VIdeographic v a = Attribute "v-ideographic" v a

pattern ViewBox :: HasFeatures a => Txt -> a -> a
pattern ViewBox v a = Attribute "viewBox" v a

pattern ViewTarget :: HasFeatures a => Txt -> a -> a
pattern ViewTarget v a = Attribute "viewTarget" v a

pattern Visibility :: HasFeatures a => Txt -> a -> a
pattern Visibility v a = Attribute "visibility" v a

pattern VMathematical :: HasFeatures a => Txt -> a -> a
pattern VMathematical v a = Attribute "v-mathematical" v a

pattern Vocab :: HasFeatures a => Txt -> a -> a
pattern Vocab v a = Attribute "vocab" v a

pattern Widths :: HasFeatures a => Txt -> a -> a
pattern Widths v a = Attribute "widths" v a

pattern WordSpacing :: HasFeatures a => Txt -> a -> a
pattern WordSpacing v a = Attribute "word-spacing" v a

pattern WritingMode :: HasFeatures a => Txt -> a -> a
pattern WritingMode v a = Attribute "writing-mode" v a

pattern X1 :: HasFeatures a => Txt -> a -> a
pattern X1 v a = Attribute "x1" v a

pattern X2 :: HasFeatures a => Txt -> a -> a
pattern X2 v a = Attribute "x2" v a

pattern X :: HasFeatures a => Txt -> a -> a
pattern X v a = Attribute "x" v a

pattern XChannelSelector :: HasFeatures a => Txt -> a -> a
pattern XChannelSelector v a = Attribute "xChannelSelector" v a

pattern XHeight :: HasFeatures a => Txt -> a -> a
pattern XHeight v a = Attribute "x-height" v a

pattern XlinkActuate :: HasXLinks a => Txt -> a -> a
pattern XlinkActuate v a = XLink "xlink:actuate" v a

pattern XlinkArcrole :: HasXLinks a => Txt -> a -> a
pattern XlinkArcrole v a = XLink "xlink:arcrole" v a

pattern XlinkHref :: HasXLinks a => Txt -> a -> a
pattern XlinkHref v a = XLink "xlink:href" v a

pattern XlinkRole :: HasXLinks a => Txt -> a -> a
pattern XlinkRole v a = XLink "xlink:role" v a

pattern XlinkShow :: HasXLinks a => Txt -> a -> a
pattern XlinkShow v a = XLink "xlink:show" v a

pattern XlinkTitle :: HasXLinks a => Txt -> a -> a
pattern XlinkTitle v a = XLink "xlink:title" v a

pattern XlinkType :: HasXLinks a => Txt -> a -> a
pattern XlinkType v a = XLink "xlink:type" v a

pattern XmlBase :: HasXLinks a => Txt -> a -> a
pattern XmlBase v a = XLink "xml:base" v a

pattern XmlLang :: HasXLinks a => Txt -> a -> a
pattern XmlLang v a = XLink "xml:lang" v a

pattern Xmlns :: HasFeatures a => Txt -> a -> a
pattern Xmlns v a = Attribute "xmlns" v a

pattern XmlnsXlink :: HasXLinks a => Txt -> a -> a
pattern XmlnsXlink v a = XLink "xmlns:xlink" v a

pattern XmlSpace :: HasXLinks a => Txt -> a -> a
pattern XmlSpace v a = XLink "xml:space" v a

pattern Y1 :: HasFeatures a => Txt -> a -> a
pattern Y1 v a = Attribute "y1" v a

pattern Y2 :: HasFeatures a => Txt -> a -> a
pattern Y2 v a = Attribute "y2" v a

pattern Y :: HasFeatures a => Txt -> a -> a
pattern Y v a = Attribute "y" v a

pattern YChannelSelector :: HasFeatures a => Txt -> a -> a
pattern YChannelSelector v a = Attribute "yChannelSelector" v a

pattern Z :: HasFeatures a => Txt -> a -> a
pattern Z v a = Attribute "z" v a

pattern ZoomAndPan :: HasFeatures a => Txt -> a -> a
pattern ZoomAndPan v a = Attribute "zoomAndPan" v a
