{-# language OverloadedStrings #-}
module Effect.Meta (metas,meta) where

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

metas :: [View -> View] -> View -> View
metas withMetas = eager withMetas . foldM (\() -> pure) initialize
  where
    initialize = do
      is <- for withMetas $ \withMeta -> do
        m <- markIO
        let i = toTxt m
        inject DOM.head (Id i (withMeta Meta))
        pure i
      pure ((),\_ -> for_ is $ \i -> findById i >>= traverse_ removeNode)

meta :: (View -> View) -> View -> View
meta withMeta = metas [withMeta]