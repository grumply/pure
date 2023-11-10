{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI, UnliftedFFITypes #-}
module Data.Txt.GHCJS (module Data.Txt.GHCJS, module Export) where

import Data.Coerce
import Data.Int
import Data.Maybe
import Data.Word

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BSLC

import Data.JSString as Export
import Data.JSString.Text as Export
import Data.JSString.Internal as Export
import Data.JSString.Read as T
import Data.JSString.RealFloat
import Data.JSString.Int
import GHCJS.Types
import GHCJS.Marshal.Pure
import GHCJS.Marshal

import Data.Hashable

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder

type Txt = JSString

class FromTxt a where
  fromTxt :: Txt -> a
  default fromTxt :: Coercible Txt a => Txt -> a
  fromTxt = coerce

class ToTxt a where
  toTxt :: a -> Txt
  default toTxt :: Coercible a Txt => a -> Txt
  toTxt = coerce

readError :: String -> Txt -> a
readError ty t = error ("Data.Txt.fromTxt: failed to read an " ++ ty ++ " when given: " ++ show t)

shortText :: Builder.Builder -> T.Text
shortText = TL.toStrict . Builder.toLazyTextWith 32

-- this instance must be the same as Text to guarantee compatability
instance Hashable Txt where
  hashWithSalt salt jss = hashWithSalt salt (textFromJSString jss)

instance FromTxt TL.Text where
  fromTxt = lazyTextFromJSString

instance FromTxt T.Text where
  fromTxt = textFromJSString

instance FromTxt JSString where
  fromTxt = id

instance FromTxt String where
  fromTxt = unpack

instance FromTxt BC.ByteString where
  fromTxt = BC.pack . unpack

instance FromTxt BSLC.ByteString where
  fromTxt = BSLC.pack . unpack

instance FromTxt Bool where
  fromTxt "true" = True
  fromTxt _ = False

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

instance ToTxt Bool where
  toTxt True = "true"
  toTxt False = "false"

instance ToTxt Int where
  toTxt = decimal

instance ToTxt Word where
  toTxt = decimal

instance ToTxt Integer where
  -- There is a bug in the implementation of
  -- h$jsstringDecInteger that can result in
  --
  -- > decimal someInteger => "-"
  --
  -- The solution for now is to just go through
  -- the show instance. Definitely needs some 
  -- looking into, as it's a pretty nasty bug. 
  toTxt = toTxt . show

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

