{-# language CPP #-}
module Nuclear
  ( module Nuclear
  , module Export
  ) where

import Data.Hashable as Export
import Data.Typeable as Export
import GHC.Generics as Export (Generic)

import Data.JSText        as Export hiding (defaultOptions,Options,(!))
import Data.JSTime        as Export
import Data.MicroTime     as Export
import Nuclear.API        as Export
import Nuclear.Attribute  as Export
import Nuclear.CSS        as Export
import Nuclear.Cond       as Export
import Nuclear.Debounce   as Export
import Nuclear.Default    as Export
import Nuclear.Ease       as Export
import Nuclear.Endpoint   as Export
import Nuclear.FromBS     as Export
import Nuclear.FromText   as Export
import Nuclear.Indexed    as Export
import Nuclear.Key        as Export
import Nuclear.Lazy       as Export
import Nuclear.Limit      as Export
import Nuclear.Memo       as Export
import Nuclear.Message    as Export
import Nuclear.Node       as Export hiding (path)
import Nuclear.Normalize  as Export
import Nuclear.Nuclear    as Export
import Nuclear.Observable as Export
import Nuclear.Render     as Export
import Nuclear.Request    as Export
import Nuclear.Revent     as Export
import Nuclear.Route      as Export
import Nuclear.Router     as Export
import Nuclear.Services   as Export
import Nuclear.Signals    as Export
import Nuclear.Strict     as Export
import Nuclear.Throttle   as Export
import Nuclear.ToBS       as Export
import Nuclear.ToText     as Export
import Nuclear.Try        as Export
import Nuclear.TypeRep    as Export
import Nuclear.UnsafeEq   as Export
import Nuclear.Vault      as Export
import Nuclear.With       as Export

import Data.ByteString as Export (ByteString)

import Data.HashMap.Strict as Map

import qualified Data.Text.Lazy as TL (Text)
import qualified Data.ByteString.Lazy as BSL (ByteString)

import Data.JSText as JSText

type LazyByteString = BSL.ByteString
type LazyText = TL.Text

instance FromJSTime MicroTime where
 --  fromJSTime :: JSTime -> MicroTime
  fromJSTime jt = MicroTime $ (millis jt) * 1000

instance FromMicroTime JSTime where
  -- fromMicrotime :: MicroTime -> JSTime
  -- truncate rather than round
  fromMicroTime mt = JSTime $ (micros mt) `Prelude.div` 1000

#ifndef __GHCJS__
instance (ToJSON v,ToText k) => ToJSON (HashMap k v) where
  toJSON = Object . Map.fromList . Prelude.map (\(k,v) -> (toText k,toJSON v)) . Map.toList
  {-# INLINE toJSON #-}
#else
-- should be a faster encoding to Value that can be more quickly encoded to JSText for transfer
instance (ToJSON v,ToText k) => ToJSON (HashMap k v) where
  toJSON = objectValue . JSText.object . Prelude.map (\(k,v) -> (toText k,toJSON v)) . Map.toList
  {-# INLINE toJSON #-}
#endif

instance (FromJSON v,Hashable k,Eq k,FromText k) => FromJSON (HashMap k v) where
  parseJSON x = do
    o <- parseJSON x
    kvs <- flip mapM (Map.toList o) $ \(k,v) -> do
      v' <- parseJSON v
      pure (fromText k,v')
    pure $ Map.fromList kvs

ghc :: Monad m => m () -> m ()
ghc =
#ifndef __GHCJS__
  id
#else
  const (return ())
#endif

ghcjs :: Monad m => m () -> m ()
ghcjs =
#ifdef __GHCJS__
  id
#else
  const (return ())
#endif
