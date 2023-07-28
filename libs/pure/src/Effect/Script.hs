module Effect.Script (scripts,script) where

import Control.Fold
import Data.CSS as CSS
import Data.DOM as DOM
import Data.Foldable
import Data.HTML
import Data.Marker
import Data.Traversable
import Data.Txt
import Data.View
import Browser

scripts :: [Txt] -> View -> View
scripts srcs = weak srcs . foldM (\() -> pure) initialize
  where
    initialize = do
      is <- for srcs $ \src -> do
        m <- markIO
        let i = toTxt m
        inject DOM.head (Id i (Script <| Src src))
        pure i
      pure ((),\_ -> for_ is $ \i -> findById i >>= traverse_ removeNode)

script :: Txt -> View -> View
script src = scripts [src]
