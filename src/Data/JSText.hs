{-# language CPP #-}
#ifdef __GHCJS__
{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
module Data.JSText (module Data.JSText, module Export) where
import Data.JSString
import Data.JSString.Text as Export
import Data.Monoid as Export
import Data.Hashable
import JavaScript.JSON.Types.Instances as Export
import JavaScript.JSON.Types as Export
import JavaScript.JSON.Types.Internal as Export
import JavaScript.JSON.Types.Generic as Export
import Language.Haskell.TH.Syntax
import GHCJS.Types (JSVal)
import Data.Coerce

type JSText = JSString

-- this instance must be the same as Text to guarantee compatability
instance Hashable JSText where
  hashWithSalt salt jss = hashWithSalt salt (textFromJSString jss)

instance Lift JSString where
  lift (unpack -> str) = [| pack str |]

#else
module Data.JSText (module Data.JSText, module Export) where
import Data.Text
import Data.Monoid as Export
import Data.Aeson as Export
import Data.Aeson.Types as Export

type JSText = Text
#endif


