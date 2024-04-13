{-# LANGUAGE CPP, ConstraintKinds, TypeSynonymInstances, FlexibleInstances, DerivingVia, PatternSynonyms, ViewPatterns, BlockArguments, RecordWildCards #-}
module Data.JSON (parse,object,traceJSON,DecodingFailure(..),Isoformable(..),JSON,module Export,alert,alertJSON,pattern JSON,pattern FromJSON) where

import Data.Txt as Txt (Txt,ToTxt(..),FromTxt(..),length)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import Data.String (IsString(..))

#ifdef __GHCJS__
import           Data.JSON.GHCJS as Export hiding (encode,decode,decodeEither,object,Null)
import qualified Data.JSON.GHCJS as GHCJS
#else
import           Data.JSON.GHC   as Export hiding (encode,decode,decode',decodeEither,decodeEither',object,Null)
import qualified Data.JSON.GHC   as GHC
#endif

import Control.Exception
import System.IO.Unsafe (unsafePerformIO)

#ifdef __GHCJS__
import Data.Bifunctor
import Data.HashMap.Strict as H
import Data.Hashable
import Data.Map as M
import Data.Tree as T
import Data.Traversable (for)
import qualified GHC.Exts as GHC
#endif

type JSON a = (ToJSON a, FromJSON a)

newtype DecodingFailure = DecodingFailure String deriving (Show,ToTxt,FromTxt,ToJSON,FromJSON,IsString) via String
instance Exception DecodingFailure

class Isoformable a where
  encode :: ToJSON x => x -> a
  decodeEither :: FromJSON x => a -> Either String x

  decode :: FromJSON x => a -> Maybe x
  decode = either (const Nothing) Just . decodeEither

  encode' :: ToJSON x => x -> a
  encode' x = unsafePerformIO (evaluate (encode x))

  decode' :: FromJSON x => a -> Maybe x
  decode' a = unsafePerformIO (evaluate (decode a))

  decodeEither' :: FromJSON x => a -> Either String x
  decodeEither' a = unsafePerformIO (evaluate (decodeEither a))

  decodeThrow :: FromJSON x => a -> x
  decodeThrow = either (throw . DecodingFailure) id . decodeEither

  decodeThrow' :: FromJSON x => a -> x
  decodeThrow' = either (throw . DecodingFailure) id . decodeEither'

instance Isoformable Value where
  encode = toJSON
  decodeEither v = 
    case fromJSON v of
      Success a -> Right a
      Error f -> Left f

instance Isoformable Txt where
#ifdef __GHCJS__
  encode = GHCJS.encode . toJSON
  encode' a = let x = encode a in Txt.length x `seq` x
  decodeEither = GHCJS.decodeEither
#else
  encode = toTxt . GHC.encode
  encode' a = let x = encode a in Txt.length x `seq` x
  decodeEither = GHC.eitherDecodeStrict' . fromTxt
  decodeEither' = decodeEither
  decode' = decode
#endif

instance Isoformable BSL.ByteString where
#ifdef __GHCJS__
  encode = fromTxt . GHCJS.encode . toJSON
  encode' a = let x = encode a in BSL.length x `seq` x
  decodeEither = GHCJS.decodeEither . toTxt
#else
  encode = GHC.encode
  encode' a = let x = encode a in BSL.length x `seq` x
  decodeEither = GHC.eitherDecodeStrict' . BSL.toStrict
  decodeEither' = decodeEither
  decode' = decode
#endif

instance Isoformable BS.ByteString where
#ifdef __GHCJS__
  encode = fromTxt . GHCJS.encode . toJSON
  encode' a = let x = encode a in BS.length x `seq` x
  decodeEither = GHCJS.decodeEither . toTxt
#else
  encode = BSL.toStrict . GHC.encode
  encode' a = let x = encode a in BS.length x `seq` x
  decodeEither = GHC.eitherDecodeStrict'
  decodeEither' = decodeEither
  decode' = decode
#endif

instance Isoformable String where
#ifdef __GHCJS__
  encode = fromTxt . GHCJS.encode . toJSON
  encode' a = let x = encode a in Prelude.length x `seq` x
  decodeEither = GHCJS.decodeEither . toTxt
#else
  encode = BSLC.unpack . GHC.encode
  encode' a = let x = encode a in Prelude.length x `seq` x
  decodeEither = GHC.eitherDecodeStrict' . BSC.pack
  decodeEither' = decodeEither
  decode' = decode
#endif

pattern JSON :: (JSON a, Isoformable t) => a -> t
pattern JSON a <- (Data.JSON.decode' -> Just a) where
  JSON a = encode a

pattern FromJSON :: (FromJSON a, Isoformable t) => a -> t
pattern FromJSON a <- (Data.JSON.decode' -> Just a)

#ifdef __GHCJS__
instance IsString Value where
  fromString = toJSON . toTxt
#endif

{-# INLINE parse #-}
parse = flip parseMaybe

{-# INLINE object #-}
object :: [Pair] -> Value
#ifdef __GHCJS__
object = GHCJS.objectValue . GHCJS.object
#else
object = GHC.object
#endif

traceJSON :: ToJSON a => a -> b -> b
traceJSON a b = 
  let 
    x = unsafePerformIO (logJSON a)
  in 
    x `seq` b

alert :: Show a => a -> b -> b
alert a = alertJSON (toTxt (Prelude.show a))

alertJSON :: ToJSON a => a -> b -> b
alertJSON a b =
#ifdef __GHCJS__
  let
    x = unsafePerformIO (alert_js (toJSON a))
  in
    x `seq` b
#else
  let
    x = unsafePerformIO (logJSON a >> putStr "<Enter To Continue Evaluation>" >> getLine)
  in
    x `seq` b
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "alert($1)" alert_js :: Value -> IO ()
#endif

#ifdef __GHCJS__
instance (ToTxt k, ToJSON v) => ToJSON (HashMap k v) where
  toJSON = object . fmap (\(k,v) -> (toTxt k,toJSON v)) . H.toList
instance (Hashable k, FromTxt k, FromJSON v) => FromJSON (HashMap k v) where
  parseJSON = withObject "(FromTxt k, FromJSON v) => HashMap k v" \o -> do
    H.fromList <$> for (GHC.toList o) \(k_,v_) -> do
      v <- parseJSON v_
      pure (fromTxt k_,v)

instance (ToTxt k, ToJSON v) => ToJSON (Map k v) where
  toJSON = object . fmap (\(k,v) -> (toTxt k,toJSON v)) . M.toList
instance (Ord k, FromTxt k, FromJSON v) => FromJSON (Map k v) where
  parseJSON = withObject "(FromTxt k, FromJSON v) => Map k v" \o -> do
    M.fromList <$> for (GHC.toList o) \(k_,v_) -> do
      v <- parseJSON v_
      pure (fromTxt k_,v)

instance ToJSON a => ToJSON (Tree a) where
  toJSON Node {..} = toJSON (rootLabel,subForest)
instance FromJSON a => FromJSON (Tree a) where
  parseJSON o = do
    (rootLabel,subForest) <- parseJSON o
    pure Node {..}
#endif