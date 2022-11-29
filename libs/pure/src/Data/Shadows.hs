{-# language LambdaCase, ViewPatterns #-}
module Data.Shadows (ShadowDirection(..),toShadow) where

import Pure hiding (e)

data ShadowDirection 
  = InsetTop | OffsetTop 
  | InsetRight | OffsetRight 
  | InsetBottom | OffsetBottom 
  | InsetLeft | OffsetLeft
  deriving (Eq,Ord,Enum,Bounded)

toShadow :: ShadowDirection -> Int -> Txt
toShadow dir depth = 
  case dir of
    InsetTop     -> defaultShadowsTopInset    !! depth
    InsetRight   -> defaultShadowsRightInset  !! depth
    InsetBottom  -> defaultShadowsBottomInset !! depth
    InsetLeft    -> defaultShadowsLeftInset   !! depth
    OffsetTop    -> defaultShadowsTop         !! depth
    OffsetRight  -> defaultShadowsRight       !! depth
    OffsetBottom -> defaultShadowsBottom      !! depth
    OffsetLeft   -> defaultShadowsLeft        !! depth

isInset :: ShadowDirection -> Bool
isInset = \case
  InsetTop    -> True
  InsetRight  -> True
  InsetBottom -> True
  InsetLeft   -> True
  _           -> False

{-# NOINLINE defaultShadowsBottom #-}
defaultShadowsBottom :: [Txt]
defaultShadowsBottom = fmap (createDefaultShadow OffsetBottom) [0..]

{-# NOINLINE defaultShadowsBottomInset #-}
defaultShadowsBottomInset :: [Txt]
defaultShadowsBottomInset = fmap (createDefaultShadow InsetBottom) [0..]

{-# NOINLINE defaultShadowsTop #-}
defaultShadowsTop :: [Txt]
defaultShadowsTop = fmap (createDefaultShadow OffsetTop) [0..]

{-# NOINLINE defaultShadowsTopInset #-}
defaultShadowsTopInset :: [Txt]
defaultShadowsTopInset = fmap (createDefaultShadow InsetTop) [0..]

{-# NOINLINE defaultShadowsLeft #-}
defaultShadowsLeft :: [Txt]
defaultShadowsLeft = fmap (createDefaultShadow OffsetLeft) [0..]

{-# NOINLINE defaultShadowsLeftInset #-}
defaultShadowsLeftInset :: [Txt]
defaultShadowsLeftInset = fmap (createDefaultShadow InsetLeft) [0..]

{-# NOINLINE defaultShadowsRight #-}
defaultShadowsRight :: [Txt]
defaultShadowsRight = fmap (createDefaultShadow OffsetRight) [0..]

{-# NOINLINE defaultShadowsRightInset #-}
defaultShadowsRightInset :: [Txt]
defaultShadowsRightInset = fmap (createDefaultShadow InsetRight) [0..]

createDefaultShadow :: ShadowDirection -> Int -> Txt
createDefaultShadow dir n = createShadow dir (isInset dir) n umbra_color penumbra_color ambient_color
  where
    umbra_color = rgba(0,0,0,0.2)
    penumbra_color = rgba(0,0,0,0.14)
    ambient_color = rgba(0,0,0,0.12)

-- An approximated implementation of material shadows
createShadow :: ShadowDirection -> Bool -> Int -> Txt -> Txt -> Txt -> Txt
createShadow d i (fromIntegral -> n) umbra_color penumbra_color ambient_color = elems [umbra,penumbra,ambient]
  where
    ins | i = inset | otherwise = mempty

    dir :: Int -> Int -> Txt
    dir x y =
      case d of
        InsetTop     -> pxs x <<>> pxs (negate y)
        OffsetTop    -> pxs x <<>> pxs (negate y)
        InsetBottom  -> pxs x <<>> pxs y
        OffsetBottom -> pxs x <<>> pxs y
        InsetLeft    -> pxs (negate y) <<>> pxs x
        OffsetLeft   -> pxs (negate y) <<>> pxs x
        InsetRight   -> pxs y <<>> pxs x
        OffsetRight  -> pxs y <<>> pxs x

    umbra = dir 0 (ceiling offset_y) <<>> pxs (ceiling blur_radius) <<>> pxs (ceiling spread_radius) <<>> umbra_color <<>> ins
      where
        offset_y :: Double
        offset_y = log(1 + blur_radius)

        blur_radius :: Double
        blur_radius = blur 0.1 n

        spread_radius :: Double
        spread_radius = blur (exp 0.1 - 1) n
          
    penumbra = dir 0 (ceiling offset_y) <<>> pxs (ceiling blur_radius) <<>> pxs (ceiling spread_radius) <<>> penumbra_color <<>> ins
      where
        offset_y :: Double
        offset_y = log(1 + blur_radius)

        blur_radius :: Double
        blur_radius = blur 0.125 n

        spread_radius :: Double
        spread_radius = blur (exp 0.125 - 1) n

    ambient = dir 0 (ceiling offset_y) <<>> pxs (ceiling blur_radius) <<>> pxs (ceiling spread_radius) <<>> ambient_color <<>> ins
      where
        offset_y :: Double
        offset_y = log(1 + blur_radius)

        blur_radius :: Double
        blur_radius = blur 0.15 n

        spread_radius :: Double
        spread_radius = blur (exp 0.15 - 1) n

    blur x n = 2 * log(cosh(exp(x * n)))

    e :: Double
    e = exp 1
