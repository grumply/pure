{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
module Atomic.Normalize where

import Atomic.CSS
import Atomic.Component

import Data.Txt (Txt)

import Prelude hiding (and,or)

-- A port of Nicolas Gallagher's normalize.css from: https://github.com/necolas/normalize.css; MIT licensed
-- Modified to remove support for IE 9-
normalize :: StaticCSS
normalize =
  $( let nrmlz = staticCSS $ do
          is "html" .> do
            lineHeight                 =: dec 1.15
            "-ms-text-size-adjust"     =: per 100
            "-webkit-text-size-adjust" =: per 100

          is "body" .> do
            margin =: int 0

          is "h1" .> do
            fontSize =: ems 2
            margin   =: ems 0.67 <<>> int 0

          is "hr" .> do
            boxSizing =: contentBox
            height    =: int 0
            overflow  =: visible

          is "pre" .> do
            fontFamily =: "monospace" <&>> "monospace"
            fontSize   =: ems 1

          is "a" .> do
            backgroundColor                =: transparent
            "-webkit-text-decoration-skip" =: "objects"

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

          is "button" .
            or is "input"  . or is "optgroup" .
            or is "select" . or is "textarea" .> do
              fontFamily =: "sans-serif"
              fontSize   =: per 100
              lineHeight =: dec 1.15
              margin     =: int 0

          is "button" .
            or is "input" .>
              overflow =: visible

          is "button" .
            or is "select" .>
              textTransform =: noneS

          is "button" .
            or is "html" . has "[type=\"button\"]" .
            or is "[type=\"reset\"]" . or is "[type=\"submit\"]" .>
              "-webkit-appearance" =: button

          is "button" . pseudo ":-moz-focus-inner" .
            or is "[type=\"button\"]" . pseudo ":-moz-focus-inner" .
            or is "[type=\"reset\"]"  . pseudo ":-moz-focus-inner" .
            or is "[type=\"submit\"]" . pseudo ":-moz-focus-inner" .> do
              borderStyle =: noneS
              padding     =: int 0

          is "button" . pseudo "-moz-focusring" .
            or is "[type=\"button\"]" . pseudo "-moz-focusring" .
            or is "[type=\"reset\"]"  . pseudo "-moz-focusring" .
            or is "[type=\"submit\"]" . pseudo "-moz-focusring" .>
              outline =: pxs 1 <<>> dotted <<>> "ButtonText"

          is "fieldset" .> do
            padding =: ems3 0.35 0.75 0.625

          is "legend" .> do
            boxSizing  =: borderBox
            color      =: inherit
            display    =: tableS
            maxWidth   =: per 100
            padding    =: zero
            whiteSpace =: normal

          is "progress" .> do
            verticalAlign =: baseline

          is "textarea" .>
            overflow =: auto

          is "[type=\"checkbox\"]" .
            or is "[type=\"radio\"]" .> do
              boxSizing =: borderBox
              padding   =: zero

          is "[type=\"number\"]" . pseudo ":-webkit-inner-spin-button" .
            or is "[type=\"number\"]" . pseudo ":-webkit-outer-spin-button" .>
              height =: auto

          is "[type=\"search\"]" .> do
            "-webkit-appearance" =: "textfield"
            outlineOffset =: neg (pxs 2)

          is "[type=\"search\"]" . pseudo ":-webkit-search-cancel-button" .
            or is "[type=\"search\"]" . pseudo ":-webkit-search-decoration" .>
              "-webkit-appearance" =: noneS

          pseudo ":-webkit-file-upload-button" .> do
            "-webkit-appearance" =: button
            font                 =: inherit

          is "details" .>
            display =: blockS

          is "summary" .>
            display =: listItem

          is "template" .>
            display =: noneS

          is "[hidden]" .>
            display =: noneS

     in [| nrmlz |]
   )
