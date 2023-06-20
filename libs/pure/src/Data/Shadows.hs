{-# language LambdaCase, ViewPatterns, RecordWildCards #-}
module Data.Shadows (shadow,Shadow(..)) where

import qualified Pure
import Pure hiding (shadow)

data Shadow = Shadow
  { layers     :: Int
  , horizontal :: Double 
  , vertical   :: Double
  , color      :: (Int,Int,Int)
  , intensity  :: Double
  , diffusion  :: Double
  , spread     :: Double
  }

shadow :: Shadow -> CSS Txt
shadow Shadow { color = (r,g,b), .. } = 
  box-Pure.shadow =: elems (Prelude.reverse (fmap go [1..layers]))
  where
    go :: Int -> Txt
    go layer = let (xOff,yOff,zOff,i) = generateLayer layer in
      toTxt (negate xOff) <> px <<>> 
      toTxt yOff <> px <<>>
      toTxt zOff <> px <<>>
      rgba(toTxt r,toTxt g,toTxt b,toTxt i)
   
    generateLayer :: Int -> (Double,Double,Double,Double)
    generateLayer n = 
      let
        d = fromIntegral n / fromIntegral layers
        i = intensity * exp(-diffusion * d)
      in
        (horizontal * d, vertical * d, spread * d, i)


