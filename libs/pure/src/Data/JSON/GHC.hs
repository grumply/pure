{-# LANGUAGE MultiParamTypeClasses, TypeSynonymInstances, FlexibleInstances #-}
module Data.JSON.GHC (module Data.JSON.GHC, module Aeson, module Export) where

import qualified Data.Aeson as Aeson
import Data.Aeson as Export hiding (Options,Object)
import qualified Data.Aeson.Types as O (Object)
import Data.Aeson.Types as Export hiding (Options,Object,parse)

import Data.Aeson.Encode.Pretty
import qualified Data.Aeson.KeyMap as KM

import Control.Exception
import Data.Maybe
import Data.Typeable
import Prelude hiding (lookup)
import System.IO

import Data.Txt

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL

import qualified Data.HashMap.Strict as Map

import qualified Data.Vector as V

pretty :: ToJSON a => a -> Txt
pretty = TL.toStrict . TL.decodeUtf8 . encodePretty

logJSON :: ToJSON a => a -> IO ()
logJSON = System.IO.putStrLn . fromTxt . pretty

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
  lookup t o = Map.lookup t (KM.toHashMapText o)

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
   toTxt = TL.toStrict . TL.decodeUtf8 . encode

