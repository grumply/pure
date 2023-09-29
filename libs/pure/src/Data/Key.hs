{-# language DerivingVia, DerivingStrategies, DeriveGeneric, CPP, PolyKinds #-}
{-# OPTIONS_GHC -Wno-warnings-deprecations #-}
module Data.Key 
  ( Marker
  , Key()
  , key
  , keyIO
  , timestamp
  , toKey
  , unsafeFromKey
  , keyToFingerprint
  , fingerprintToKey
  ) where

import qualified Data.Marker (timestamp)
import Data.Marker (Marker(),mark,markIO,markerToFingerprint,unsafeFingerprintToMarker)
import Data.Hashable (Hashable)
import Data.JSON (ToJSON,FromJSON)
#ifndef __GHCJS__
import Data.JSON (ToJSONKey,FromJSONKey)
#endif
import Data.Time (Time)
import Data.Txt (ToTxt,FromTxt)
import Data.Random (Generator)
import GHC.Fingerprint (Fingerprint(..))
import GHC.Generics (Generic)
import Unsafe.Coerce (unsafeCoerce)

newtype Key = Key (Marker Key)
  deriving (Generic)
  deriving 
    (Eq,Ord,Show,ToJSON,FromJSON,Hashable,ToTxt,FromTxt
#ifndef __GHCJS__
    ,ToJSONKey,FromJSONKey
#endif
    ) via (Marker Key)

-- | Convert any type of `Marker` to a `Key`.
toKey :: Marker a -> Key
toKey m = Key (unsafeCoerce m)

-- | Unsafely convert a `Key` to any type of `Marker`.
unsafeFromKey :: Key -> Marker (a :: k) 
unsafeFromKey (Key m) = unsafeCoerce m

key :: Generator (IO Key)
key = fmap Key <$> mark

keyIO :: IO Key
keyIO = Key <$> markIO

timestamp :: Key -> Time
timestamp (Key k) = Data.Marker.timestamp k

fingerprintToKey :: Fingerprint -> Key
fingerprintToKey = toKey . unsafeFingerprintToMarker

keyToFingerprint :: Key -> Fingerprint
keyToFingerprint = markerToFingerprint . unsafeFromKey

