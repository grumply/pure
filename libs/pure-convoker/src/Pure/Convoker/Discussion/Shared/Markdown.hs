module Pure.Convoker.Discussion.Shared.Markdown (Markdown(..),parseMarkdown) where

import Control.Exception (evaluate)
import Data.Maybe
import Control.Monad
import Data.List as List
import Data.Typeable
import GHC.Generics
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

import Data.JSON (ToJSON,FromJSON)
import Data.Txt (Txt,ToTxt(..),FromTxt(..))
import Data.View
import Data.View.Parse (parseView)
import qualified Data.View.Sanitize as Sanitize

#ifndef __GHCJS__
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtml5String)
import Text.Pandoc.Options as Pandoc (def,ReaderOptions(..),pandocExtensions)

unsafeRender :: Txt -> [View]
unsafeRender md = either (const []) id $ runPure $ do
  one <- readMarkdown Pandoc.def { readerExtensions = pandocExtensions } md
  two <- writeHtml5String Pandoc.def one
  pure (parseView two)
#else
unsafeRender :: Txt -> [View]
unsafeRender md = []
#endif

newtype Markdown = Markdown Txt
  deriving (ToJSON,FromJSON,ToTxt,FromTxt) via Txt

parseMarkdown :: Markdown -> [View]
parseMarkdown = maybe [] id . unsafePerformIO . timeout 3000000 . evaluate . fmap (Sanitize.sanitize opts) . unsafeRender . toTxt
  where
    whitelistClasses =
      ["sourceCode","sourceLine","footnotes"
      ,"hide","node","warn","info","more","prev","next"
      ,"co","dt","kw","cf","op","sc","ss","vs","cn"
      ,"dv","bn","fl","ch","st","va","fu","al","er"
      ,"wa","im","bu","ex","do","an","cv","in"
      ]

    opts = Sanitize.defaultOptions
      { Sanitize.processAttribute = Sanitize.allowDataAttribute
      , Sanitize.processClass     = \c -> if c `elem` whitelistClasses then Sanitize.Allow else Sanitize.Disallow
      }