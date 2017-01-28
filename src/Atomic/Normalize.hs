{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
module Atomic.Normalize where

import Atomic.CSS

normalize :: StaticCSS
normalize =
  $( let nrmlz = staticCSS $ do
          select "html" $ do
            fontFamily =: "sans-serif"
            lineHeight =: dec 1.15
            "-ms-text-size-adjust" =: per 100
            "-webkit-text-size-adjust" =: per 100

          select "body" $ do
            margin =: int 0

          select "h1" $ do
            fontSize =: ems 2
            margin =: ems 0.67 <<>> int 0

          select ("figcaption" <&>> "figure" <&>> "main") $ do
            display =: block

          select "hr" $ do
            boxSizing =: contentBox
            height =: int 0
            overflow =: visible

          select "pre" $ do
            fontFamily =: "monospace" <&>> "monospace"
            fontSize =: ems 1

          select "a" $ do
            backgroundColor =: transparent
            "-webkit-text-decoration-skip" =: "objects"

          select ("a:active" <&>> "a:hover") $ do
            outlineWidth =: int 0

          select "abbr[title]" $ do
            borderBottom =: noneS
            textDecoration =: underline
            textDecoration =: underline <<>> dotted

          select ("b" <&>> "strong") $ do
            fontWeight =: inherit

          select ("b" <&>> "strong") $ do
            fontWeight =: "bolder"

          select ("code" <&>> "kbd" <&>> "samp") $ do
            fontFamily =: "monospace" <&>> "monospace"
            fontSize =: ems 1

          select "dfn" $ do
            fontStyle =: italic

          select "mark" $ do
            backgroundColor =: "#ff0"
            color =: "#000"

          select "small" $ do
            fontSize =: per 80

          select ("sub" <&>> "sup") $ do
            fontSize =: per 75
            lineHeight =: int 0
            position =: relative
            verticalAlign =: "baseline"

          select "sub" $ do
            bottom =: ems (-0.25)

          select "sup" $ do
            top =: ems (-0.5)

          select "img" $ do
            borderStyle =: noneS

          select "svg:not(:root)" $ do
            overflow =: "hidden"

          select ("buton" <&>> "input" <&>> "optgroup" <&>> "select" <&>> "textarea") $ do
            fontFamily =: "sans-serif"
            fontSize =: per 100
            lineHeight =: dec 1.15
            margin =: int 0

          select ("button" <&>> "input") $ do
            overflow =: visible

          select ("button" <&>> "select") $ do
            textTransform =: noneS

          select ("button" <&>> "html [type=\"button\"]" <&>> "[type=\"reset\"]" <&>> "[type=\"submit\"]") $ do
            "-webkit-appearance" =: "button"

          select ("button::-moz-focus-inner"
                  <&>> "[type=\"button\"]::-moz-focus-inner"
                  <&>> "[type=\"reset\"]::-moz-focus-inner"
                  <&>> "[type=\"submit\"]::-moz-focus-inner") $ do
            borderStyle =: noneS
            padding =: int 0

          select ("button:-moz-focusring"
                  <&>> "[type=\"button\"]:-moz-focusring"
                  <&>> "[type=\"reset\"]:-moz-focusring"
                  <&>> "[type=\"submit\"]:-moz-focusring") $ do
            outline =: px 1 <<>> dotted <<>> "ButtonText"

          select "fieldset" $ do
            border =: px 1 <<>> solid <<>> "#c0c0c0"
            margin =: int 0 <<>> px 2
            padding =: ems 0.35 <<>> ems 0.625 <<>> ems 0.75

          select "legend" $ do
            boxSizing =: borderBox
            color =: inherit
            display =: "table"
            maxWidth =: per 100
            padding =: int 0
            whiteSpace =: normal

          select "progress" $ do
            display =: inlineBlock
            verticalAlign =: "baseline"

          select "textarea" $ do
            overflow =: auto

          select ("[type=\"checkbox\"]" <&>> "[type=\"radio\"]") $ do
            boxSizing =: borderBox
            padding =: int 0

          select ("[type=\"number\"]::-webkit-inner-spin-button"
                  <&>> "[type=\"number\"]::-webkit-outer-spin-button") $ do
            height =: auto

          select "[type=\"search\"]" $ do
            "-webkit-appearance" =: "textfield"
            outlineOffset =: px (-2)

          select ("[type=\"search\"]::-webkit-search-cancel-button"
                  <&>> "[type=\"search\"]::-webkit-search-decoration") $ do
            "-webkit-appearance" =: noneS

          select "::-webkit-file-upload-button" $ do
            "-webkit-appearance" =: "button"
            font =: inherit

          select "details" $ do
            display =: block

          select "summary" $ do
            display =: listItem

          select "[hidden]" $ do
            display =: noneS
     in [| nrmlz |]
   )
