{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
module Atomic.Normalize where

import Atomic.CSS

import Data.Txt (Txt)

import Prelude hiding (and,or)

-- A port of Nicolas Gallagher's normalize.css from: https://github.com/necolas/normalize.css; MIT licensed
normalize :: StaticCSS
normalize =
  $( let nrmlz = staticCSS $ do
          is "html" .> do
            fontFamily =: "sans-serif"
            lineHeight =: dec 1.15
            msTextSizeAdjust =: per 100
            webkitTextSizeAdjust =: per 100

          is "body" .> do
            margin =: int 0

          is "h1" .> do
            fontSize =: ems 2
            margin   =: ems 0.67 <<>> int 0

          is "figcaption" .
            or is "figure" .
              or is "main" .> do
                display =: blockS

          is "hr" .> do
            boxSizing =: contentBox
            height    =: int 0
            overflow  =: visible

          is "pre" .> do
            fontFamily =: "monospace" <&>> "monospace"
            fontSize   =: ems 1

          is "a" .> do
            backgroundColor          =: transparent
            webkitTextDecorationSkip =: "objects"

          is "a" . active .
            or is "a" . hovered .>
              outlineWidth =: int 0

          is "abbr" .
            and attr "title" .> do
              borderBottom   =: noneS
              textDecoration =: underline
              textDecoration =: underline <<>> dotted

          is "b" .
            or is "strong" .>
              fontWeight =: inherit

          is "b" .
            or is "strong" .>
              fontWeight =: "bolder"

          is "code" .
            or is "kbd" .
              or is "samp" .> do
                fontFamily =: "monospace" <&>> "monospace"
                fontSize   =: ems 1

          is "dfn" .>
            fontStyle =: italic

          is "mark" .> do
            backgroundColor =: "#ff0"
            color           =: "#000"

          is "small" .> do
            fontSize =: per 80

          is "sub" .
            or is "sup" .> do
              fontSize      =: per 75
              lineHeight    =: int 0
              position      =: relative
              verticalAlign =: "baseline"

          is "sub" .>
            bottom =: ems (-0.25)

          is "sup" .>
            top =: ems (-0.5)

          is "img" .>
            borderStyle =: noneS

          is "svg" .
            and isn't ":root" .>
              overflow =: hiddenS

          is "buton" .
            or is "input"  . or is "optgroup" .
            or is "select" . or is "textarea" .> do
              font   =: inherit
              margin =: int 0

          is "button" .
            or is "input" .>
              overflow =: visible

          is "button" .
            or is "select" .>
              textTransform =: noneS

          is "button" .
            or is "html"             . or is "[type=\"button\"]" .
            or is "[type=\"reset\"]" . or is "[type=\"submit\"]" .>
              webkitAppearance =: buttonS

          is "button" . and pseudo "-moz-focus-inner" .
            or is "[type=\"button\"]" . and pseudo "-moz-focus-inner" .
            or is "[type=\"reset\"]"  . and pseudo "-moz-focus-inner" .
            or is "[type=\"submit\"]" . and pseudo "-moz-focus-inner" .> do
              borderStyle =: noneS
              padding     =: int 0

          is "button" . and pseudo "-moz-focusring" .
            or is "[type=\"button\"]" . and pseudo "-moz-focusring" .
            or is "[type=\"reset\"]"  . and pseudo "-moz-focusring" .
            or is "[type=\"submit\"]" . and pseudo "-moz-focusring" .>
              outline =: pxs 1 <<>> dotted <<>> "ButtonText"

          is "fieldset" .> do
            border  =: pxs 1 <<>> solid <<>> "#c0c0c0"
            margin  =: int 0 <<>> pxs 2
            padding =: ems 0.35 <<>> ems 0.625 <<>> ems 0.75

          is "legend" .> do
            boxSizing  =: borderBox
            color      =: inherit
            display    =: tableS
            maxWidth   =: per 100
            padding    =: int 0
            whiteSpace =: normal

          is "progress" .> do
            display       =: inlineBlock
            verticalAlign =: baseline

          is "textarea" .>
            overflow =: auto

          is "[type=\"checkbox\"]" .
            or is "[type=\"radio\"]" .> do
              boxSizing =: borderBox
              padding   =: int 0

          is "[type=\"number\"]" .
            and pseudo "-webkit-inner-spin-button" .
              or is "[type=\"number\"]" .
                and pseudo "-webkit-outer-spin-button" .>
                  height =: auto

          is "[type=\"search\"]" .> do
            "-webkit-appearance" =: "textfield"
            outlineOffset =: neg (pxs 2)

          is "[type=\"search\"]" .
            and pseudo "-webkit-search-cancel-button" .
              or is "[type=\"search\"]" .
                and pseudo "-webkit-search-decoration" .>
                  webkitAppearance =: noneS

          pseudo "-webkit-file-upload-button" .> do
            webkitAppearance =: buttonS
            font             =: inherit

          is "details" .>
            display =: blockS

          is "summary" .> do
            display =: listItem

          is "[hidden]" .
            or is "template" .>
              display =: noneS

     in [| nrmlz |]
   )
