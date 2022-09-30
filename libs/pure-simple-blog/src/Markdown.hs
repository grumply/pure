module Markdown where

import Shared (Markdown(..))

import Pure (View)
import Data.View.Parse (parseView)
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtml5String)
import Text.Pandoc.Options (def,ReaderOptions(..),pandocExtensions)

render :: Markdown -> [View]
render (Markdown md) = either (const []) id $ runPure $ do
  one <- readMarkdown def { readerExtensions = pandocExtensions } md
  two <- writeHtml5String def one
  pure (parseView two)
