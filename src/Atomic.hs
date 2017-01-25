{-# language CPP #-}
module Atomic
  ( module Atomic
  , module Export
  ) where

import Ef.Base as Export hiding (Object,watch,transform)

import Data.Hashable as Export
import Data.Typeable as Export
import GHC.Generics as Export (Generic)

import Data.Txt          as Export hiding (defaultOptions,Options,(!))
import Data.Millis       as Export
import Data.Micros       as Export
import Atomic.API        as Export
import Atomic.Attribute  as Export
import Atomic.CSS        as Export
import Atomic.Cond       as Export
import Atomic.Debounce   as Export
import Atomic.Default    as Export
import Atomic.Dispatch   as Export
import Atomic.Ease       as Export
import Atomic.Endpoint   as Export
import Atomic.FromBS     as Export
import Atomic.FromTxt    as Export
import Atomic.Indexed    as Export
import Atomic.Key        as Export
import Atomic.Lazy       as Export
import Atomic.Limit      as Export
import Atomic.Memo       as Export
import Atomic.Message    as Export
import Atomic.Normalize  as Export
import Atomic.Observable as Export
import Atomic.Render     as Export
import Atomic.Request    as Export
import Atomic.Revent     as Export
import Atomic.Route      as Export
import Atomic.Router     as Export
import Atomic.Mediators  as Export
import Atomic.Signals    as Export
import Atomic.Strict     as Export
import Atomic.Throttle   as Export
import Atomic.ToBS       as Export
import Atomic.ToTxt      as Export
import Atomic.Try        as Export
import Atomic.TypeRep    as Export
import Atomic.UnsafeEq   as Export
import Atomic.Vault      as Export
#ifdef __GHCJS__
import Atomic.WebSocket  as Export hiding (LazyByteString)
#else
import Atomic.WebSocket  as Export hiding (LazyByteString,accept)
#endif
import Atomic.With       as Export

import Data.ByteString as Export (ByteString)

import Data.HashMap.Strict as Map

import qualified Data.Text.Lazy as TL (Text)
import qualified Data.ByteString.Lazy as BSL (ByteString)

import Data.Txt as Txt

type LazyByteString = BSL.ByteString
type LazyText = TL.Text

instance FromMillis Micros where
 --  fromMillis :: Millis -> Micros
  fromMillis jt = Micros $ (getMillis jt) * 1000

instance FromMicros Millis where
  -- fromMicrotime :: Micros -> Millis
  -- truncate rather than round
  fromMicros mt = Millis $ (getMicros mt) `Prelude.div` 1000

#ifndef __GHCJS__
instance {-# OVERLAPS #-} (ToJSON v,ToTxt k) => ToJSON (HashMap k v) where
  toJSON = Txt.Object . Map.fromList . Prelude.map (\(k,v) -> (toTxt k,toJSON v)) . Map.toList
  {-# INLINE toJSON #-}
#else
-- should be a faster encoding to Value that can be more quickly encoded to Txt for transfer
instance {-# OVERLAPS #-} (ToJSON v,ToTxt k) => ToJSON (HashMap k v) where
  toJSON = objectValue . Txt.object . Prelude.map (\(k,v) -> (toTxt k,toJSON v)) . Map.toList
  {-# INLINE toJSON #-}
#endif

instance {-# OVERLAPS #-} (FromJSON v,Hashable k,Eq k,FromTxt k) => FromJSON (HashMap k v) where
  parseJSON x = do
    o <- parseJSON x
    kvs <- flip mapM (Map.toList o) $ \(k,v) -> do
      v' <- parseJSON v
      pure (fromTxt k,v')
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
