{-# language CPP #-}
{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
#ifdef __GHCJS__
module Data.Txt (module Data.Txt, module Export) where
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

import Unsafe.Coerce

import GHCJS.Marshal
import GHCJS.Marshal.Pure

import qualified GHCJS.DOM.Types as T

instance ToJSVal Object where
  toJSVal = return . unsafeCoerce

instance PToJSVal Object where
  pToJSVal = unsafeCoerce

instance FromJSVal Object where
  fromJSVal = return . Just . unsafeCoerce

instance PFromJSVal Object where
  pFromJSVal = unsafeCoerce

instance T.IsGObject Export.Object where
  toGObject = unsafeCoerce
  unsafeCastGObject = unsafeCoerce

instance T.IsEvent Export.Object

instance FromJSON Object where
  parseJSON = withObject "object" pure

type Txt = JSString

-- this instance must be the same as Text to guarantee compatability
instance Hashable Txt where
  hashWithSalt salt jss = hashWithSalt salt (textFromJSString jss)

instance Lift JSString where
  lift (unpack -> str) = [| pack str |]

#else
module Data.Txt (module Data.Txt, module Export) where
import Data.Text
import Data.Monoid as Export
import Data.Aeson as Export
import Data.Aeson.Types as Export
import Language.Haskell.TH.Syntax

type Txt = Text

instance Lift Text where
  lift (unpack -> str) = [| pack str |]
#endif


