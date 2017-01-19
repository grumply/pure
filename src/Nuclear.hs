{-# language CPP #-}
module Nuclear
  ( module Data.Typeable
  , module Data.Hashable
  , module Export
  , LazyByteString
  , LazyText
  ) where

import Data.Hashable
import Data.Typeable
import GHC.Generics as Export (Generic)

import Data.JSText as Export hiding (defaultOptions,Options,(!))
import Data.ByteString as Export (ByteString)

import Nuclear.ToBS as Export
import Nuclear.FromBS as Export
import Nuclear.ToText as Export
import Nuclear.FromText as Export
import Nuclear.Strict as Export
import Nuclear.Nuclear as Export
import Nuclear.Request as Export
import Nuclear.Message as Export
import Nuclear.TypeRep as Export
import Nuclear.Indexed as Export
import Nuclear.API as Export
import Nuclear.Endpoint as Export
import Nuclear.Try as Export

import Data.JSTime as Export
import Data.MicroTime as Export

import Data.HashMap.Strict as Map

import qualified Data.Text.Lazy as TL (Text)
import qualified Data.ByteString.Lazy as BSL (ByteString)

type LazyByteString = BSL.ByteString
type LazyText = TL.Text

instance FromJSTime MicroTime where
 --  fromJSTime :: JSTime -> MicroTime
  fromJSTime jt = MicroTime $ (millis jt) * 1000

instance FromMicroTime JSTime where
  -- fromMicrotime :: MicroTime -> JSTime
  -- truncate rather than round
  fromMicroTime mt = JSTime $ (micros mt) `div` 1000

#ifndef __GHCJS__
instance (ToJSON v,ToText k) => ToJSON (HashMap k v) where
  toJSON = Object . Map.fromList . Prelude.map (\(k,v) -> (toText k,toJSON v)) . Map.toList
  {-# INLINE toJSON #-}
#else
-- should be a faster encoding to Value that can be more quickly encoded to JSText for transfer
instance (ToJSON v,ToText k) => ToJSON (HashMap k v) where
  toJSON = objectValue . object . Prelude.map (\(k,v) -> (toText k,toJSON v)) . Map.toList
  {-# INLINE toJSON #-}
#endif

instance (FromJSON v,Hashable k,Eq k,FromText k) => FromJSON (HashMap k v) where
  parseJSON x = do
    o <- parseJSON x
    kvs <- flip mapM (Map.toList o) $ \(k,v) -> do
      v' <- parseJSON v
      pure (fromText k,v')
    pure $ Map.fromList kvs
