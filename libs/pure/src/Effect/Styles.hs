{-# language OverloadedStrings #-}
module Effect.Styles (css,stylesheets,stylesheet) where

import Control.Fold
import Data.CSS as CSS hiding (css,stylesheet)
import qualified Data.CSS as CSS (css)
import Data.DOM as DOM
import Data.Foldable
import Data.HTML
import Data.Marker
import Data.Traversable
import Data.Txt
import Data.View
import Browser

css :: CSS a -> View -> View
css styles = weak styles . foldM (\() -> pure) initialize
  where
    initialize = do
      m <- markIO
      let i = toTxt m
      inject DOM.head (Id i (CSS.css styles))
      pure ((),\_ -> findById i >>= traverse_ removeNode)

stylesheets :: [Txt] -> View -> View
stylesheets sheets = weak sheets . foldM (\() -> pure) initialize
  where
    initialize = do
      is <- for sheets $ \sheet -> do
        m <- markIO
        let i = toTxt m
        inject DOM.head (Id i (Link <| Rel "stylesheet" . Href sheet))
        pure i
      pure ((),\_ -> for_ is $ \i -> findById i >>= traverse_ removeNode)

stylesheet :: Txt -> View -> View
stylesheet sheet = stylesheets [sheet]
