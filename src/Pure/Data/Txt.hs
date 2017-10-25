{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, UnliftedFFITypes #-}
module Pure.Data.Txt (module Pure.Data.Txt, module Export) where

import Data.Monoid

import Data.Maybe
import Data.String

import Data.Coerce

import Data.Int
import Data.Word

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL

import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Numeric

#ifdef __GHCJS__
import Data.JSString as Export
import Data.JSString.Text as Export
import Data.Hashable
import Data.JSString.Read as T
import Data.JSString.RealFloat
import Data.JSString.Int
import GHCJS.Types
import GHCJS.Marshal.Pure
import GHCJS.Marshal
import System.IO.Unsafe
#else
import Data.Text as Export hiding (Text)
import Data.Text.IO as Export hiding (Text)
import Data.Text hiding (Text)
import Data.Text.Read as T
#endif

#ifdef __GHCJS__
type Txt = JSString
#else
type Txt = T.Text
#endif

class FromTxt a where
  fromTxt :: Txt -> a
  default fromTxt :: Coercible Txt a => Txt -> a
  fromTxt = coerce

-- ToTxt is representational and is thus uni-directional. For a fully
-- bidirectional encoding, see ToBS/FromBS where the expectation is that
-- fromBS (toBS a) = Right and fmap toBS . fromBS = Right
--
-- ToTxt is used to construct, possibly unique, resource identifiers.
--
-- Note the default instance uses a ToBS instance to construct a text value
-- from a full encoding of the term; this is slow since ToBS generates a
-- lazy bytestring and we must use lazy decoding and subsequent strictness
-- conversion. For small terms, which is the intended use-case for the
-- default instance, this won't matter much.
class ToTxt a where
  toTxt :: a -> Txt
  default toTxt :: Coercible a Txt => a -> Txt
  toTxt = coerce

instance ToTxt Bool where
  toTxt True  = "true"
  toTxt False = "false"

shortText :: Builder.Builder -> T.Text
shortText = TL.toStrict . Builder.toLazyTextWith 32

instance ToTxt a => ToTxt (Maybe a) where
  toTxt (Just a) = toTxt a
  toTxt Nothing = mempty

readError :: String -> Txt -> a
readError ty t = error ("Atomic.FromTxt: FromTxt failed to read an " ++ ty ++ " when given: " ++ show t)

instance FromTxt a => FromTxt (Maybe a) where
  fromTxt x = if x == mempty then Nothing else Just (fromTxt x)

instance ToTxt () where
  toTxt _ = "()"

instance ToTxt BSL.ByteString where
  -- can this fail at runtime from a bad encoding?
  toTxt = toTxt . TL.decodeUtf8

instance ToTxt B.ByteString where
  -- can this fail at runtime from a bad encoding?
  toTxt = toTxt . T.decodeUtf8

instance ToTxt Txt where
  toTxt = id

#ifdef __GHCJS__

----------------------------------------
-- GHCJS

-- this instance must be the same as Text to guarantee compatability
instance Hashable Txt where
  hashWithSalt salt jss = hashWithSalt salt (textFromJSString jss)

instance FromTxt TL.Text where
  fromTxt = lazyTextFromJSString

instance FromTxt T.Text where
  fromTxt = textFromJSString

instance FromTxt JSString where
  fromTxt = id

instance FromTxt [Char] where
  fromTxt = unpack

instance FromTxt B.ByteString where
  fromTxt = BC.pack . unpack

instance FromTxt BSL.ByteString where
  fromTxt = BSLC.pack . unpack

instance FromTxt Int where
  fromTxt t = fromMaybe (readError "Int" t) (T.readIntMaybe t)

instance FromTxt Double where
  fromTxt t = fromMaybe (readError "Double" t) (T.readDoubleMaybe t)

instance FromTxt Integer where
  fromTxt t = fromMaybe (readError "Integer" t) (T.readIntegerMaybe t)

instance FromTxt Int64 where
  fromTxt t = fromMaybe (readError "Int64" t) (T.readInt64Maybe t)

instance FromTxt Int32 where
  fromTxt t = maybe (readError "Int32" t) fromIntegral (T.readIntMaybe t)

instance FromTxt Int16 where
  fromTxt t = maybe (readError "Int16" t) fromIntegral (T.readIntMaybe t)

instance FromTxt Int8 where
  fromTxt t = maybe (readError "Int8" t) fromIntegral (T.readIntMaybe t)

instance FromTxt Word64 where
  fromTxt t = fromMaybe (readError "Word64" t) (T.readWord64Maybe t)

instance FromTxt Word32 where
  fromTxt t = maybe (readError "Word32" t) fromIntegral (T.readWord64Maybe t)

instance FromTxt Word16 where
  fromTxt t = maybe (readError "Word16" t) fromIntegral (T.readWord64Maybe t)

instance FromTxt Word8 where
  fromTxt t = maybe (readError "Word8" t) fromIntegral (T.readWord64Maybe t)

instance FromTxt Word where -- Word is Word32 in GHCJS? I dunno....
  fromTxt t = maybe (readError "Word" t) fromIntegral (T.readWord64Maybe t)

instance ToTxt T.Text where
  toTxt = textToJSString

instance ToTxt TL.Text where
  toTxt = lazyTextToJSString

instance ToTxt Char where
  toTxt = singleton

instance ToTxt String where
  toTxt = pack

instance ToTxt Int where
  toTxt = decimal

instance ToTxt Word where
  toTxt = decimal

instance ToTxt Integer where
  toTxt = decimal

instance ToTxt Float where
  toTxt = realFloat

instance ToTxt Double where
  toTxt = realFloat

instance ToTxt Int64 where
  toTxt = decimal

instance ToTxt Int32 where
  toTxt = decimal

instance ToTxt Int16 where
  toTxt = decimal

instance ToTxt Int8 where
  toTxt = decimal

instance ToTxt Word64 where
  toTxt = decimal

instance ToTxt Word32 where
  toTxt = decimal

instance ToTxt Word16 where
  toTxt = decimal

instance ToTxt Word8 where
  toTxt = decimal

#else

----------------------------------------
-- GHC

instance FromTxt TL.Text where
  fromTxt = TL.fromStrict

instance FromTxt T.Text where
  fromTxt = id

instance FromTxt [Char] where
  fromTxt = T.unpack

instance FromTxt B.ByteString where
  fromTxt = T.encodeUtf8

instance FromTxt BSL.ByteString where
  fromTxt = BSL.fromStrict . T.encodeUtf8

instance FromTxt Int where
  fromTxt t = either (readError "Int" t) fst (T.signed T.decimal t)

instance FromTxt Double where
  fromTxt t = either (readError "Double" t) fst (T.double t)

instance FromTxt Integer where
  fromTxt t = either (readError "Integer" t) fst (T.signed T.decimal t)

instance FromTxt Int8 where
  fromTxt t = either (readError "Int8" t) fst (T.signed T.decimal t)

instance FromTxt Int16 where
  fromTxt t = either (readError "Int16" t) fst (T.signed T.decimal t)

instance FromTxt Int32 where
  fromTxt t = either (readError "Int32" t) fst (T.signed T.decimal t)

instance FromTxt Int64 where
  fromTxt t = either (readError "Int64" t) fst (T.signed T.decimal t)

instance FromTxt Word where
  fromTxt t = either (readError "Word" t) fst (T.decimal t)

instance FromTxt Word8 where
  fromTxt t = either (readError "Word8" t) fst (T.decimal t)

instance FromTxt Word16 where
  fromTxt t = either (readError "Word16" t) fst (T.decimal t)

instance FromTxt Word32 where
  fromTxt t = either (readError "Word32" t) fst (T.decimal t)

instance FromTxt Word64 where
  fromTxt t = either (readError "Word64" t) fst (T.decimal t)


instance ToTxt TL.Text where
  toTxt = TL.toStrict

instance ToTxt Char where
  toTxt = T.singleton

instance ToTxt String where
  toTxt = T.pack

instance ToTxt Int where
  toTxt = shortText . Builder.decimal

instance ToTxt Word where
  toTxt = shortText . Builder.decimal

instance ToTxt Integer where
  toTxt = shortText . Builder.decimal

instance ToTxt Float where
  toTxt = toTxt . ($ "") . showFFloat Nothing

instance ToTxt Double where
  toTxt = toTxt . ($ "") . showFFloat Nothing

instance ToTxt Int64 where
  toTxt = shortText . Builder.decimal

instance ToTxt Int32 where
  toTxt = shortText . Builder.decimal

instance ToTxt Int16 where
  toTxt = shortText . Builder.decimal

instance ToTxt Int8 where
  toTxt = shortText . Builder.decimal

instance ToTxt Word64 where
  toTxt = shortText . Builder.decimal

instance ToTxt Word32 where
  toTxt = shortText . Builder.decimal

instance ToTxt Word16 where
  toTxt = shortText . Builder.decimal

instance ToTxt Word8 where
  toTxt = shortText . Builder.decimal

#endif

-- instance {-# OVERLAPPABLE #-} FromTxt a => IsString a where
--   fromString = fromTxt . pack

pattern Translated :: (ToTxt t, FromTxt t, ToTxt f, FromTxt f) => f -> t
pattern Translated t <- (fromTxt . toTxt -> t) where
  Translated f = fromTxt $ toTxt f

instance Lift Txt where
  lift (unpack -> str) = [| pack str |]

