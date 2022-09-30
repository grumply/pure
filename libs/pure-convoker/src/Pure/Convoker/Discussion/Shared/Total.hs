module Pure.Convoker.Discussion.Shared.Total (simplified) where

import Pure

import Text.Printf

simplified :: Int -> Txt
simplified t
  | t < 1000 = toTxt t
  | otherwise =
    case t `divMod` 1000 of
      (n,0) -> toTxt n <> "k"
      _     -> toTxt @String (printf "%.1f" (fromIntegral t / 1000 :: Double)) <> "k"
