module Pure.Convoker.Discussion.Shared.Markdown (Markdown(..),parseMarkdown) where

import Control.Exception (evaluate)
import Data.Either (fromRight)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)
import System.Timeout (timeout)

import Data.Default (Default)
import Data.JSON (ToJSON,FromJSON)
import Data.Txt (Txt,ToTxt(..),FromTxt(..))
import Data.View (View)
import Data.View.Parse (parseView)
import qualified Data.View.Sanitize as Sanitize

#ifndef __GHCJS__
import Text.Pandoc.Class (runPure)
import Text.Pandoc.Readers.Markdown (readMarkdown)
import Text.Pandoc.Writers.HTML (writeHtml5String)
import Text.Pandoc.Options as Pandoc (def,ReaderOptions(..),pandocExtensions)

unsafeRender :: Txt -> [View]
unsafeRender md = 
  fromRight [] do
    runPure do
      one <- readMarkdown Pandoc.def { readerExtensions = pandocExtensions } md
      two <- writeHtml5String Pandoc.def one
      pure (parseView two)
#else
unsafeRender :: Txt -> [View]
unsafeRender md = []
#endif

data Markdown = Markdown Txt
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (ToJSON,FromJSON,Default)

instance ToTxt Markdown where
  toTxt (Markdown md) = md
  
instance FromTxt Markdown where
  fromTxt = Markdown

parseMarkdown :: Markdown -> [View]
parseMarkdown = fromMaybe [] . unsafePerformIO . timeout 3000000 . evaluate . fmap (Sanitize.sanitize opts) . unsafeRender . toTxt
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