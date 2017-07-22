-- a port of Kristofer Joseph's Apache licensed Flexbox Grid css
-- from: https://github.com/kristoferjoseph/flexboxgrid
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language NoOverloadedLists #-}
{-# language ViewPatterns #-}
module Atomic.Grid where

import Atomic.CSS
import Atomic.ToTxt

import Atomic.Component

import Data.Txt (Txt)

import Control.Monad
import Data.Foldable (foldr1)
import Data.Traversable (for)
import Data.Monoid

import Prelude hiding (or,and,rem,reverse)

onXS = id

onSM = atMedia (screenMinWidth (ems 48))

onMD = atMedia (screenMinWidth (ems 64))

onLG = atMedia (screenMinWidth (ems 75))

onXL = atMedia (screenMinWidth (ems 90))

data FlexSize = Xs | Sm | Md | Lg | Xl
instance ToTxt FlexSize where
  toTxt sz =
    case sz of
      Xs -> "xs"
      Sm -> "sm"
      Md -> "md"
      Lg -> "lg"
      Xl -> "xl"

uContainer :: Txt
uContainer = "u-container"

uContainerFluid :: Txt
uContainerFluid = "u-container-fluid"

uRow :: Txt
uRow = "u-row"

uReverse :: Txt
uReverse = "u-reverse"

uCol :: Txt
uCol = "u-col"

uHiddenUp :: FlexSize -> Txt
uHiddenUp (toTxt -> sz) = "u-hidden-" <> sz <> "-up"

uHiddenDown :: FlexSize -> Txt
uHiddenDown (toTxt -> sz) = "u-hidden-" <> sz <> "-down"

uColsAt :: FlexSize -> Int -> Txt
uColsAt (toTxt -> sz) (toTxt -> n) = "u-col-" <> sz <> "-" <> n

uCols :: Int -> Txt
uCols = uColsAt Xs

uColsGrowAt :: FlexSize -> Txt
uColsGrowAt (toTxt -> sz) = "u-cols-" <> sz

uColsGrow :: Txt
uColsGrow = uColsGrowAt Xs

uColsOffsetAt :: FlexSize -> Int -> Txt
uColsOffsetAt (toTxt -> sz) (toTxt -> n) = "u-col-" <> sz <> "-offset-" <> n

uColsOffset :: Int -> Txt
uColsOffset = uColsOffsetAt Xs

uStartAt :: FlexSize -> Txt
uStartAt (toTxt -> sz)= "u-start-" <> sz

uStart :: Txt
uStart = uStartAt Xs

uCenterAt :: FlexSize -> Txt
uCenterAt (toTxt -> sz) = "u-center-" <> sz

uCenter :: Txt
uCenter = uCenterAt Xs

uEndAt :: FlexSize -> Txt
uEndAt (toTxt -> sz) = "u-end-" <> sz

uEnd :: Txt
uEnd = uEndAt Xs

uTopAt :: FlexSize -> Txt
uTopAt (toTxt -> sz) = "u-top-" <> sz

uTop :: Txt
uTop = uTopAt Xs

uMiddleAt :: FlexSize -> Txt
uMiddleAt (toTxt -> sz) = "u-middle-" <> sz

uMiddle :: Txt
uMiddle = uMiddleAt Xs

uBottomAt :: FlexSize -> Txt
uBottomAt (toTxt -> sz) = "u-bottom-" <> sz

uBottom :: Txt
uBottom = uBottomAt Xs

uAroundAt :: FlexSize -> Txt
uAroundAt (toTxt -> sz) = "u-around-" <> sz

uAround :: Txt
uAround = uAroundAt Xs

uBetweenAt :: FlexSize -> Txt
uBetweenAt (toTxt -> sz) = "u-between-" <> sz

uBetween :: Txt
uBetween = uBetweenAt Xs

uFirstAt :: FlexSize -> Txt
uFirstAt (toTxt -> sz) = "u-first-" <> sz

uFirst :: Txt
uFirst = uFirstAt Xs

uLastAt :: FlexSize -> Txt
uLastAt (toTxt -> sz) = "u-last-" <> sz

uLast :: Txt
uLast = uLastAt Xs

flexboxGrid = let c = classify in void $ do
  is (c uContainer) .
    or is (c uContainerFluid) . apply . important $ do
      marginRight  =: auto
      marginLeft   =: auto
      maxWidth     =: calc(per 100 <<>> "-" <<>> rems 1)
      paddingLeft  =: rems 0.5
      paddingRight =: rems 0.5

  is (c uContainerFluid) . apply . important $ do
    paddingRight =: rems 2
    paddingLeft  =: rems 2
    marginLeft   =: neg (rems 0.5)
    marginRight  =: neg (rems 0.5)

  is (c uRow) . apply . important $ do
    boxSizing               =: borderBox
    display                 =: "-webkit-box"
    display                 =: "-ms-flexbox"
    display                 =: flex
    "-webkit-box-flex"      =: zero
    "-ms-flex"              =: zero <<>> one <<>> auto
    flex                    =: zero <<>> one <<>> auto
    "-webkit-box-orient"    =: horizontal
    "-webkit-box-direction" =: normal
    "-ms-flex-direction"    =: rowS
    flexDirection           =: rowS
    "-ms-flex-wrap"         =: wrapS
    flexWrap                =: wrapS
    marginRight             =: neg (rems 0.5)
    marginLeft              =: neg (rems 0.5)

  is (c uRow) .
    and is (c uReverse) . apply . important $ do
      "-webkit-box-orient"    =: horizontal
      "-webkit-box-direction" =: reverse
      "-ms-flex-direction"    =: rowReverse
      flexDirection           =: rowReverse

  is (c uCol) .
    and is (c uReverse) . apply . important $ do
      "-webkit-box-orient"    =: vertical
      "-webkit-box-direction" =: reverse
      "-ms-flex-direction"    =: columnReverse
      flexDirection           =: columnReverse

  is (c $ uHiddenUp Xs) . apply . important $
    display =: noneS

  is (c $ uHiddenDown Xl) . apply . important $
    display =: noneS

  for [(Xs,Nothing),(Sm,Just 48),(Md,Just 64),(Lg,Just 75),(Xl,Just 90)] $ \(sz,mn) -> do

    maybe id (atMedia . screenMaxWidth . ems) mn $
      is (c $ uHiddenDown sz) . apply . important $
        display =: noneS

    maybe id (atMedia . screenMinWidth . ems) mn $ do

      is (c $ uHiddenUp sz) . apply . important $
        display =: noneS

      for mn $ \n ->
        is (c uContainer) . apply . important $
          -- really, relative ems? Our breakpoint is em-based....
          -- I don't quite understand the interaction here.
          width =: rems (n + 1)

      columns <-
        is (c $ uColsGrowAt sz) .
          or is (c $ uColsOffsetAt sz 0) .> do
            extendable $ important $ do
              boxSizing          =: borderBox
              "-webkit-box-flex" =: zero
              "-ms-flex"         =: zero <<>> zero <<>> auto
              flex               =: zero <<>> zero <<>> auto
              paddingRight       =: rems 0.5
              paddingLeft        =: rems 0.5

      is (c $ uColsAt sz 12) .>
        extends columns

      for [1..11] $ \i ->
        is (c $ uColsAt sz i) .
          or is (c $ uColsOffsetAt sz i) .>
            extends columns

      is (c $ uColsGrowAt sz) . apply . important $ do
        "-webkit-box-flex"        =: one
        "-ms-flex-positive"       =: one
        flexGrow                  =: one
        "-ms-flex-preferred-size" =: zero
        flexBasis                 =: zero
        maxWidth                  =: per 100

      is (c $ uColsOffsetAt sz 0) . apply . important $
        marginLeft =: zero

      is (c $ uColsOffsetAt sz 12) . apply . important $ do
        "-ms-flex-preferred-size" =: per 100
        flexBasis                 =: per 100
        maxWidth                  =: per 100

      is (c $ uColsAt sz 0) . apply . important $ do
        overflow =: hiddenS
        height   =: per 0

      for [0..11] $ \i -> do
        -- close enough?
        let p = per (fromIntegral i * 8.33333333)

        is (c $ uColsAt sz i) . apply . important $ do
          "-ms-flex-preferred-size" =: p
          flexBasis                 =: p
          maxWidth                  =: p

        is (c $ uColsOffsetAt sz i) . apply . important $
          marginLeft =: p

      is (c $ uStartAt sz) . apply . important $ do
        "-webkit-box-pack"  =: startS
        "-ms-flex-pack"     =: startS
        justifyContent      =: flexStart
        textAlign           =: startS

      is (c $ uCenterAt sz) . apply . important $ do
        "-webkit-box-pack"  =: center
        "-ms-flex-pack"     =: center
        justifyContent      =: center
        textAlign           =: center

      is (c $ uEndAt sz) . apply . important $ do
        "-webkit-box-pack"  =: endS
        "-ms-flex-pack"     =: endS
        textAlign           =: endS
        justifyContent      =: flexEnd

      is (c $ uTopAt sz) . apply . important $ do
        "-webkit-box-align" =: startS
        "-ms-flex-align"    =: startS
        alignItems          =: flexStart

      is (c $ uMiddleAt sz) . apply . important $ do
        "-webkit-box-align" =: center
        "-ms-flex-align"    =: center
        alignItems          =: center

      is (c $ uBottomAt sz) . apply . important $ do
        "-webkit-box-align" =: endS
        "-ms-flex-align"    =: endS
        alignItems          =: flexEnd

      is (c $ uAroundAt sz) . apply . important $ do
        "-ms-flex-pack"     =: distribute
        justifyContent      =: spaceAround

      is (c $ uBetweenAt sz) . apply . important $ do
        "-webkit-box-pack"  =: justify
        "-ms-flex-pack"     =: justify
        justifyContent      =: spaceBetween

      is (c $ uFirstAt sz) . apply . important $ do
        "-webkit-box-ordinal-group" =: zero
        "-ms-flex-order"            =: neg (int 1)
        order                       =: neg (int 1)

      is (c $ uLastAt sz) .apply . important $ do
        "-webkit-box-ordinal-group" =: int 2
        "-ms-flex-order"            =: one
        order                       =: one
