{-# LANGUAGE CPP #-}
{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Pure.Data.JSON (module Pure.Data.JSON, module Export) where

import Pure.Data.Txt

import Data.Monoid as Export
import Data.Typeable

#ifdef __GHCJS__
import JavaScript.JSON.Types.Instances as Export
import JavaScript.JSON.Types as Export hiding (Options,Object,parse)
import JavaScript.JSON.Types.Internal as Export hiding (Options,Object,parse)
import JavaScript.JSON.Types.Generic as Export

import GHCJS.Types (JSVal)
import GHCJS.Marshal
import GHCJS.Marshal.Pure

import qualified JavaScript.JSON.Types as O (Object)

import Unsafe.Coerce

foreign import javascript unsafe
  "JSON.parse($1)" js_JSON_parse :: Txt -> Value

type Obj = O.Object

instance ToJSVal Obj where
  toJSVal = return . unsafeCoerce

instance PToJSVal Obj where
  pToJSVal = unsafeCoerce

instance FromJSVal Obj where
  fromJSVal = return . Just . unsafeCoerce

instance PFromJSVal Obj where
  pFromJSVal = unsafeCoerce

instance FromJSON Obj where
  parseJSON = withObject "object" pure

instance Eq Value where
  (==) a b = encode a == encode b

instance Show Value where
  show = show . encode

instance ToJSON Obj where
  toJSON = objectValue

foreign import javascript unsafe
  "for (var x in $2) { $1[x] = $2[x]; }" merge_objects_js :: Obj -> Obj -> Obj
  -- shallow, should conform to HashMap (<>)

instance Monoid Obj where
  mempty = emptyObject
  mappend = merge_objects_js

instance ToTxt Value where
  {-# INLINE toTxt #-}
  toTxt = encode

#else
import qualified Data.Aeson as Aeson
import Data.Aeson as Export hiding (Options,Object)
import Data.Aeson.Types as Export hiding (Options,Object,parse)

import Control.Exception
import Data.Maybe

import qualified Data.HashMap.Strict as Map
import qualified Data.Vector as V

import qualified Data.Aeson.Types as O (Object)

import Prelude hiding (lookup)

type Obj = O.Object

-- copied from GHCJS JSON for compatability
data JSONException = UnknownKey
  deriving (Show, Typeable)

instance Exception JSONException

-- from GHCJS.JSON
class Lookup k a where
  (!)       :: k -> a -> Value
  lookup    :: k -> a -> Maybe Value

instance Lookup Txt O.Object where
  (!) k v  = fromMaybe (throw UnknownKey) (lookup k v)
  lookup = Map.lookup

instance Lookup Txt Value where
  (!) k v = fromMaybe (throw UnknownKey) (lookup k v)
  lookup k v =
    case v of
      Aeson.Object o -> lookup k o
      _ -> Nothing

instance Lookup Int Aeson.Array where
  (!) i a = fromMaybe (throw UnknownKey) (lookup i a)
  lookup = flip (V.!?)

instance Lookup Int Value where
  (!) i a      = fromMaybe (throw UnknownKey) (lookup i a)
  lookup i v =
    case v of
      Array arr -> lookup i arr
      _ -> Nothing

instance ToTxt Value where
  {-# INLINE toTxt #-}
  toTxt = toTxt . encode
#endif

parse = flip parseMaybe

