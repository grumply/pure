{-# language CPP #-}
#ifdef __GHCJS__
module Data.JSText (module Data.JSText, module Export) where
import Data.JSString
import Data.JSString.Text as Export
import Data.Monoid as Export
import Data.Hashable
import JavaScript.JSON.Types.Instances as Export
import JavaScript.JSON.Types as Export
import JavaScript.JSON.Types.Internal as Export
import JavaScript.JSON.Types.Generic as Export

type JSText = JSString

-- this instance must be the same as Text to guarantee compatability
instance Hashable JSText where
  hashWithSalt salt jss = hashWithSalt salt (textFromJSString jss)
#else
module Data.JSText (module Data.JSText, module Export) where
import Data.Text
import Data.Monoid as Export
import Data.Aeson as Export

type JSText = Text
#endif


