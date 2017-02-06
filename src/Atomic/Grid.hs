{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
module Atomic.Grid where

import Atomic.CSS
import Atomic.ToTxt

import Control.Monad
import Data.Monoid

import Prelude hiding (reverse)

-- a port of Kristofer Joseph's flexboxgrid css from: https://github.com/kristoferjoseph/flexboxgrid; Apache licensed
grid :: StaticCSS
grid =
  $( let g = staticCSS $ do
           select ".u-container, .u-container-fluid" $ do
             marginRight =: auto
             marginLeft =: auto

           select ".u-container-fluid" $ do
             paddingRight =: rems 2
             paddingLeft =: rems 2

           select ".u-row" $ do
             boxSizing =: borderBox
             display =: webkitBox
             display =: msFlexBox
             display =: flex
             webkitBoxFlex =: zero
             msFlex =: "0 1 auto"
             flex =: "0 1 auto"
             webkitBoxOrient =: horizontal
             webkitBoxDirection =: normal
             msFlexDirection =: rowS
             flexDirection =: rowS
             msFlexWrap =: wrapS
             flexWrap =: wrapS
             marginRight =: rems (-0.5)
             marginLeft =: rems (-0.5)

           select ".u-row.u-reverse" $ do
             webkitBoxOrient =: horizontal
             webkitBoxDirection =: reverse
             msFlexDirection =: rowReverse
             flexDirection =: rowReverse

           select ".u-col.u-reverse" $ do
             webkitBoxOrient =: vertical
             webkitBoxDirection =: reverse
             msFlexDirection =: columnReverse
             flexDirection =: columnReverse

           forM_ [("xs",Nothing),("sm",Just 48),("md",Just 64),("lg",Just 75),("xl",Just 90)] $ \(sz,mn) ->
            maybe id (\n -> atMedia ("only screen and (min-width: " <> toTxt n <> "em)")) mn $ do

              forM_ mn $ \n ->
                select ".u-container" $ do
                  width =: rems (n + 1)

              select (".u-col-" <> sz <> ", .u-col-" <> sz <> "-1, .u-col-" <> sz <> "-2, .u-col-" <> sz <> "-3, .u-col-" <> sz <> "-4, .u-col-" <> sz <> "-5, .u-col-" <> sz <> "-6, .u-col-" <> sz <> "-7, .u-col-" <> sz <> "-8, .u-col-" <> sz <> "-9, .u-col-" <> sz <> "-10, .u-col-" <> sz <> "-11, .u-col-" <> sz <> "-12, .u-col-" <> sz <> "-offset-0, .u-col-" <> sz <> "-offset-1, .u-col-" <> sz <> "-offset-2, .u-col-" <> sz <> "-offset-3, .u-col-" <> sz <> "-offset-4, .u-col-" <> sz <> "-offset-5, .u-col-" <> sz <> "-offset-6, .u-col-" <> sz <> "-offset-7, .u-col-" <> sz <> "-offset-8, .u-col-" <> sz <> "-offset-9, .u-col-" <> sz <> "-offset-10, .u-col-" <> sz <> "-offset-11") $ do
                boxSizing =: borderBox
                webkitBoxFlex =: zero
                msFlex =: "0 0 auto"
                flex =: "0 0 auto"
                paddingRight =: rems 0.5
                paddingLeft =: rems 0.5

              select (".u-col-" <> sz) $ do
                webkitBoxFlex =: one
                msFlexPositive =: one
                flexGrow =: one
                msFlexPreferredSize =: zero
                flexBasis =: zero
                maxWidth =: per 100

              select (".u-col-" <> sz <> "-offset-0") $ do
                marginLeft =: zero

              select (".u-col-" <> sz <> "-12") $ do
                msFlexPreferredSize =: per 100
                flexBasis =: per 100
                maxWidth =: per 100

              forM [1..11] $ \(i :: Int) -> do
                -- close enough?
                let p = per (fromIntegral i * 8.33333333)

                select (".u-col-" <> sz <> "-" <> toTxt i) $ do
                  msFlexPreferredSize =: p
                  flexBasis =: p
                  maxWidth =: p

                select (".u-col-" <> sz <> "-offset-" <> toTxt i) $ do
                  marginLeft =: p

              select (".u-start-" <> sz) $ do
                webkitBoxPack =: "start"
                msFlexPack =: "start"
                justifyContent =: flexStart
                textAlign =: "start"

              select (".u-center-" <> sz) $ do
                webkitBoxPack =: center
                msFlexPack =: center
                justifyContent =: center
                textAlign =: center

              select (".u-end-" <> sz) $ do
                webkitBoxPack =: endS
                msFlexPack =: endS
                justifyContent =: flexEnd
                textAlign =: endS

              select (".u-top-" <> sz) $ do
                webkitBoxAlign =: "start"
                msFlexAlign =: "start"
                alignItems =: flexStart

              select (".u-middle-" <> sz) $ do
                webkitBoxAlign =: center
                msFlexAlign =: center
                alignItems =: center

              select (".u-bottom-" <> sz) $ do
                webkitBoxAlign =: endS
                msFlexAlign =: endS
                alignItems =: flexEnd

              select (".u-around-" <> sz) $ do
                msFlexPack =: distribute
                justifyContent =: spaceAround

              select (".u-between-" <> sz) $ do
                webkitBoxPack =: justify
                msFlexPack =: justify
                justifyContent =: spaceBetween

              select (".u-first-" <> sz) $ do
                webkitBoxOrdinalGroup =: zero
                msFlexOrder =: int (-1)
                order =: int (-1)

              select (".u-last-" <> sz) $ do
                webkitBoxOrdinalGroup =: int 2
                msFlexOrder =: one
                order =: one

     in [| g |]
   )
