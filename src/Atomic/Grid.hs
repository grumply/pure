{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
module Atomic.Grid where

import Atomic.CSS
import Atomic.ToTxt

import Control.Monad
import Data.Foldable (foldr1)
import Data.Monoid

import Prelude hiding (rem,reverse)

onXS = id

onSM = atMedia (screenMinWidth (ems 48))

onMD = atMedia (screenMinWidth (ems 64))

onLG = atMedia (screenMinWidth (ems 75))

onXL = atMedia (screenMinWidth (ems 90))

-- a port of Kristofer Joseph's flexboxgrid css from: https://github.com/kristoferjoseph/flexboxgrid; Apache licensed
grid :: StaticCSS
grid =
  $( let g = staticCSS $ do
           select ".u-container, .u-container-fluid" $ do
             marginRight =: auto
             marginLeft  =: auto
             maxWidth =: calc(per 100 <<>> "-" <<>> rems 1)
             paddingLeft =: rems 0.5
             paddingRight =: rems 0.5

           select ".u-container-fluid" $ do
             paddingRight =: rems 2
             paddingLeft  =: rems 2
             marginLeft =: neg (rems 0.5)
             marginRight =: neg (rems 0.5)

           select ".u-row" $ do
             boxSizing          =: borderBox
             display            =: webkitBox
             display            =: msFlexBox
             display            =: flex
             webkitBoxFlex      =: zero
             msFlex             =: zero <<>> one <<>> auto
             flex               =: zero <<>> one <<>> auto
             webkitBoxOrient    =: horizontal
             webkitBoxDirection =: normal
             msFlexDirection    =: rowS
             flexDirection      =: rowS
             msFlexWrap         =: wrapS
             flexWrap           =: wrapS
             marginRight        =: neg (rems 0.5)
             marginLeft         =: neg (rems 0.5)

           select ".u-row.u-reverse" $ do
             webkitBoxOrient    =: horizontal
             webkitBoxDirection =: reverse
             msFlexDirection    =: rowReverse
             flexDirection      =: rowReverse

           select ".u-col.u-reverse" $ do
             webkitBoxOrient    =: vertical
             webkitBoxDirection =: reverse
             msFlexDirection    =: columnReverse
             flexDirection      =: columnReverse

           forM_ [("xs",Nothing),("sm",Just 48),("md",Just 64),("lg",Just 75),("xl",Just 90)] $ \(sz,mn) ->
            maybe id (atMedia . screenMinWidth . ems) mn $ do

              forM_ mn $ \n ->
                select ".u-container" $
                  width =: rems (n + 1)

              let col n | n == 0    = ".u-col-" <> sz <&>> ".u-col-" <> sz <> "-offset-0"
                        | n == 12   = ".u-col-" <> sz <> "-" <> int 12
                        | otherwise = ".u-col-" <> sz <> "-" <> int n <&>> ".u-col-" <> sz <> "-offset-" <> int n
                  cols = foldr1 (<&>>) (map col [0..12])

              select cols $ do
                boxSizing     =: borderBox
                webkitBoxFlex =: zero
                msFlex        =: zero <<>> zero <<>> auto
                flex          =: zero <<>> zero <<>> auto
                paddingRight  =: rems 0.5
                paddingLeft   =: rems 0.5

              select (".u-col-" <> sz) $ do
                webkitBoxFlex       =: one
                msFlexPositive      =: one
                flexGrow            =: one
                msFlexPreferredSize =: zero
                flexBasis           =: zero
                maxWidth            =: per 100

              select (".u-col-" <> sz <> "-offset-0") $ do
                marginLeft =: zero

              select (".u-col-" <> sz <> "-12") $ do
                msFlexPreferredSize =: per 100
                flexBasis           =: per 100
                maxWidth            =: per 100

              forM [1..11] $ \i -> do
                -- close enough?
                let p = per (fromIntegral i * 8.33333333)

                select (".u-col-" <> sz <> "-" <> int i) $ do
                  msFlexPreferredSize =: p
                  flexBasis           =: p
                  maxWidth            =: p

                select (".u-col-" <> sz <> "-offset-" <> int i) $ do
                  marginLeft =: p

              select (".u-start-" <> sz) $ do
                webkitBoxPack  =: startS
                msFlexPack     =: startS
                justifyContent =: flexStart
                textAlign      =: startS

              select (".u-center-" <> sz) $ do
                webkitBoxPack  =: center
                msFlexPack     =: center
                justifyContent =: center
                textAlign      =: center

              select (".u-end-" <> sz) $ do
                webkitBoxPack  =: endS
                msFlexPack     =: endS
                textAlign      =: endS
                justifyContent =: flexEnd

              select (".u-top-" <> sz) $ do
                webkitBoxAlign =: startS
                msFlexAlign    =: startS
                alignItems     =: flexStart

              select (".u-middle-" <> sz) $ do
                webkitBoxAlign =: center
                msFlexAlign    =: center
                alignItems     =: center

              select (".u-bottom-" <> sz) $ do
                webkitBoxAlign =: endS
                msFlexAlign    =: endS
                alignItems     =: flexEnd

              select (".u-around-" <> sz) $ do
                msFlexPack     =: distribute
                justifyContent =: spaceAround

              select (".u-between-" <> sz) $ do
                webkitBoxPack  =: justify
                msFlexPack     =: justify
                justifyContent =: spaceBetween

              select (".u-first-" <> sz) $ do
                webkitBoxOrdinalGroup =: zero
                msFlexOrder           =: neg (int 1)
                order                 =: neg (int 1)

              select (".u-last-" <> sz) $ do
                webkitBoxOrdinalGroup =: int 2
                msFlexOrder           =: one
                order                 =: one

     in [| g |]
   )
