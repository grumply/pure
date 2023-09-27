{-# language DerivingVia, DerivingStrategies, DeriveGeneric, CPP #-}
module Data.Key (Marker,Key(),key,keyIO,toKey,unsafeFromKey) where

import Data.Marker (Marker(),mark,markIO)
import Data.Hashable (Hashable)
import Data.JSON (ToJSON,FromJSON)
#ifndef __GHCJS__
import Data.JSON (ToJSONKey,FromJSONKey)
#endif
import Data.Txt (ToTxt,FromTxt)
import Data.Random (Generator)
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
unsafeFromKey :: Key -> Marker a 
unsafeFromKey (Key m) = unsafeCoerce m

key :: Generator (IO Key)
key = fmap Key <$> mark

keyIO :: IO Key
keyIO = Key <$> markIO
