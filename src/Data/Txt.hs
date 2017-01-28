{-# language CPP #-}
{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
module Data.Txt (module Data.Txt, module Export) where
import Data.Monoid as Export
import Language.Haskell.TH.Syntax
#ifdef __GHCJS__
import Data.JSString as Export
import Data.JSString.Text as Export
import Data.Hashable

type Txt = JSString

-- this instance must be the same as Text to guarantee compatability
instance Hashable Txt where
  hashWithSalt salt jss = hashWithSalt salt (textFromJSString jss)

instance Lift JSString where
  lift (unpack -> str) = [| pack str |]
#else
import Data.Text as Export
import Data.Text.IO as Export

type Txt = Text

instance Lift Text where
  lift (unpack -> str) = [| pack str |]
#endif


