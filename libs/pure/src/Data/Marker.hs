{-# language DeriveAnyClass, BlockArguments, DerivingStrategies, DeriveGeneric, PatternSynonyms, ViewPatterns, MagicHash, UnboxedTuples, BangPatterns, TypeApplications, PartialTypeSignatures, CPP, TupleSections, GADTs, OverloadedStrings, RoleAnnotations #-}
module Data.Marker 
  ( Marker
  , markIO
  , mark
  , base62
  , hex
  , uuid
  , timestamp
  , encodeBase62
  , decodeBase62
  , encodeBase16
  , decodeBase16
  , encodeUUID
  , decodeUUID
  ) where

import Data.JSON (ToJSON(..),FromJSON(..))
#ifndef __GHCJS__
import Data.JSON (ToJSONKey,FromJSONKey)
#endif
import Data.Txt as Txt (ToTxt(..),FromTxt(..),Txt,length,unfoldrN,reverse,drop,dropEnd,foldl',filter,uncons)
import Data.Time (Time,pattern Milliseconds,time)
import Data.Random (Variate(..), Generator, Seed, generate, newSeed) 

import Data.Text.Internal.Unsafe.Char as T (ord)
import Data.Char (chr)
import Data.Hashable (Hashable(..))

import Control.Monad (join)
import Data.Bits (shift,(.|.))
import Data.IORef (IORef,newIORef,atomicModifyIORef')
import GHC.Word (Word64(..))
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)

-- Reasonably performant UUID-compatible time-tagged randoms based, roughly, 
-- on ideas from https://github.com/anthonynsimon/timeflake
--
-- Combines a 48-bit time in milliseconds with an 80 bit random. Not 
-- cryptographically secure.
--
data Marker a = Marker {-# UNPACK #-}!Word64 {-# UNPACK #-}!Word64
  deriving (Generic,Eq,Ord,Show)
type role Marker nominal

instance ToJSON (Marker a) where
  toJSON = toJSON . encodeBase62

instance FromJSON (Marker a) where
  parseJSON = fmap decodeBase62 . parseJSON

#ifndef __GHCJS__
instance ToJSONKey (Marker a)
instance FromJSONKey (Marker a)
#endif

instance Hashable (Marker a) where
  hashWithSalt salt (Marker w1 w2) = 
    hashWithSalt salt (w1,w2)

instance ToTxt (Marker a) where 
  toTxt = encodeBase62

instance FromTxt (Marker a) where 
  fromTxt t = 
    case Txt.length t of
      22 -> decodeBase62 t
      32 -> decodeBase16 t
      36 -> decodeUUID t
      _  -> error "Data.Marker (fromTxt :: Txt -> Marker): invalid encoded Marker length"


-- | Generate an action to create a `Marker` in IO.
--
{-# INLINE mark #-}
mark :: Generator (IO (Marker a))
mark = do
  r1 <- uniformR 0 (2 ^ 16 - 1) 
  r2 <- uniform
  pure do
    Milliseconds ms _ <- time -- ms is whole in the Milliseconds pattern
    pure (Marker (shift (fromIntegral (round ms)) 16 .|. r1) r2)

{-# INLINE timestamp #-}
timestamp :: Marker a -> Time
timestamp (Marker w1 _) = Milliseconds (fromIntegral (shift w1 (-16))) 0

{-# NOINLINE globalMarkerSeed #-}
globalMarkerSeed :: IORef Seed
globalMarkerSeed = unsafePerformIO do
  s <- newSeed
  newIORef s

{-# INLINABLE markIO #-}
markIO :: IO (Marker a)
markIO = join (atomicModifyIORef' globalMarkerSeed (generate mark))

{-# INLINE base62 #-}
base62 :: Marker a -> Txt
base62 = encodeBase62

{-# INLINABLE encodeBase62 #-}
encodeBase62 :: Marker a -> Txt
encodeBase62 (Marker w1 w2) = Txt.reverse (t1 <> t2)
  where 
    t1 = encodeWord64 w2
    t2 = encodeWord64 w1

    encodeWord64 :: Word64 -> Txt
    encodeWord64 = Txt.unfoldrN 11 (Just . go)
      where
        go (flip quotRem 62 -> (n,d)) = (base62_encode_char d,n)
        
    base62_encode_char :: Word64 -> Char
    base62_encode_char (fromIntegral -> w)
      | w < 10    = chr (ord '0' + w)
      | w < 36    = chr (ord 'A' + w - 10)
      | w < 62    = chr (ord 'a' + w - 36)
      | otherwise = '0'

{-# INLINABLE decodeBase62 #-}
decodeBase62 :: Txt -> Marker a
decodeBase62 t = Marker w1 w2
  where 
    w1 = decodeWord64 (Txt.dropEnd 11 t)
    w2 = decodeWord64 (Txt.drop 11 t)

    decodeWord64 :: Txt -> Word64
    decodeWord64 = Txt.foldl' go 0
      where
        go w (base62_decode_char -> c) = w * 62 + c

    base62_decode_char :: Char -> Word64
    base62_decode_char (fromIntegral . ord -> c)
      | c >= 48 , c <= 57  = c - 48
      | c >= 64 , c <= 90  = c - 55
      | c >= 97 , c <= 122 = c - 61
      | otherwise          = 0

{-# INLINE hex #-}
hex :: Marker a -> Txt
hex = encodeBase16

{-# INLINABLE encodeBase16 #-}
encodeBase16 :: Marker a -> Txt
encodeBase16 (Marker w1 w2) = Txt.reverse (t1 <> t2)
  where 
    t1 = encodeWord64 w2
    t2 = encodeWord64 w1

    encodeWord64 :: Word64 -> Txt
    encodeWord64 = Txt.unfoldrN 16 (Just . go)
      where
        go (flip quotRem 16 -> (n,d)) = (base16_encode_char d,n)
        
    base16_encode_char :: Word64 -> Char
    base16_encode_char (fromIntegral -> w)
      | w < 10    = chr (ord '0' + w)
      | w < 16    = chr (ord 'a' + w - 10)
      | otherwise = '0'

{-# INLINABLE decodeBase16 #-}
decodeBase16 :: Txt -> Marker a
decodeBase16 t = Marker w1 w2
  where 
    w1 = decodeWord64 (Txt.dropEnd 16 t)
    w2 = decodeWord64 (Txt.drop 16 t)

    decodeWord64 :: Txt -> Word64
    decodeWord64 = Txt.foldl' go 0
      where
        go w (base16_decode_char -> c) = w * 16 + c

    base16_decode_char :: Char -> Word64
    base16_decode_char (fromIntegral . ord -> c)
      | c >= 48 , c <= 57  = c - 48
      | c >= 97 , c <= 102 = c - 87
      | otherwise          = 0

{-# INLINE uuid #-}
uuid :: Marker a -> Txt
uuid = encodeUUID

{-# INLINABLE encodeUUID #-}
encodeUUID :: Marker a -> Txt
encodeUUID = Txt.unfoldrN 36 go . (36,) . encodeBase16
  where
    go :: (Int,Txt) -> Maybe (Char,(Int,Txt))
    go (n,x) 
      | n == 9 || n == 14 || n == 19 || n == 24 = Just ('-',(n - 1,x))
      | otherwise = fmap (\(x,y) -> (x,(n - 1,y))) (Txt.uncons x)

{-# INLINABLE decodeUUID #-}
decodeUUID :: Txt -> Marker a
decodeUUID = decodeBase16 . Txt.filter (/= '-')
