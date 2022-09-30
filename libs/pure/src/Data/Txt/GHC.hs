{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances #-}
module Data.Txt.GHC (module Data.Txt.GHC, module Export) where

import Data.Coerce
import Data.Int
import Data.Monoid
import Data.Maybe
import Data.String
import Data.Word
import Numeric

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BSLC

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.Text.Lazy.Builder as Builder
import qualified Data.Text.Lazy.Builder.Int as Builder
import Data.Text.IO as Export hiding (Text)
import Data.Text.Read as T

import Data.Text as Export

type Txt = T.Text

class FromTxt a where
  fromTxt :: Txt -> a
  {-# INLINE fromTxt #-}
  default fromTxt :: Coercible Txt a => Txt -> a
  fromTxt = coerce

class ToTxt a where
  toTxt :: a -> Txt
  {-# INLINE toTxt #-}
  default toTxt :: Coercible a Txt => a -> Txt
  toTxt = coerce

readError :: String -> Txt -> a
readError ty t = error ("Data.Txt.fromTxt: failed to read an " ++ ty ++ " when given: " ++ show t)

{-# INLINE shortText #-}
shortText :: Builder.Builder -> T.Text
shortText = TL.toStrict . Builder.toLazyTextWith 32

instance FromTxt TL.Text where
  {-# INLINE fromTxt #-}
  fromTxt = TL.fromStrict

instance FromTxt T.Text where
  {-# INLINE fromTxt #-}
  fromTxt = id

instance FromTxt () where
  {-# INLINE fromTxt #-}
  fromTxt "()" = ()

instance FromTxt [Char] where
  {-# INLINE fromTxt #-}
  fromTxt = T.unpack

instance FromTxt BC.ByteString where
  {-# INLINE fromTxt #-}
  fromTxt = T.encodeUtf8

instance FromTxt BSLC.ByteString where
  {-# INLINE fromTxt #-}
  fromTxt = BSLC.fromStrict . T.encodeUtf8

instance FromTxt Bool where
  {-# INLINE fromTxt #-}
  fromTxt "true" = True
  fromTxt _ = False

instance FromTxt Int where
  {-# INLINE fromTxt #-}
  fromTxt t = either (readError "Int" t) fst (T.signed T.decimal t)

instance FromTxt Double where
  {-# INLINE fromTxt #-}
  fromTxt t = either (readError "Double" t) fst (T.double t)

instance FromTxt Integer where
  {-# INLINE fromTxt #-}
  fromTxt t = either (readError "Integer" t) fst (T.signed T.decimal t)

instance FromTxt Int8 where
  {-# INLINE fromTxt #-}
  fromTxt t = either (readError "Int8" t) fst (T.signed T.decimal t)

instance FromTxt Int16 where
  {-# INLINE fromTxt #-}
  fromTxt t = either (readError "Int16" t) fst (T.signed T.decimal t)

instance FromTxt Int32 where
  {-# INLINE fromTxt #-}
  fromTxt t = either (readError "Int32" t) fst (T.signed T.decimal t)

instance FromTxt Int64 where
  {-# INLINE fromTxt #-}
  fromTxt t = either (readError "Int64" t) fst (T.signed T.decimal t)

instance FromTxt Word where
  {-# INLINE fromTxt #-}
  fromTxt t = either (readError "Word" t) fst (T.decimal t)

instance FromTxt Word8 where
  {-# INLINE fromTxt #-}
  fromTxt t = either (readError "Word8" t) fst (T.decimal t)

instance FromTxt Word16 where
  {-# INLINE fromTxt #-}
  fromTxt t = either (readError "Word16" t) fst (T.decimal t)

instance FromTxt Word32 where
  {-# INLINE fromTxt #-}
  fromTxt t = either (readError "Word32" t) fst (T.decimal t)

instance FromTxt Word64 where
  {-# INLINE fromTxt #-}
  fromTxt t = either (readError "Word64" t) fst (T.decimal t)

instance ToTxt TL.Text where
  {-# INLINE toTxt #-}
  toTxt = TL.toStrict

instance ToTxt Char where
  {-# INLINE toTxt #-}
  toTxt = T.singleton

instance ToTxt String where
  {-# INLINE toTxt #-}
  toTxt = T.pack

instance ToTxt Bool where
  {-# INLINE toTxt #-}
  toTxt True = "true"
  toTxt False = "false"

instance ToTxt Int where
  {-# INLINE toTxt #-}
  toTxt = shortText . Builder.decimal

instance ToTxt Word where
  {-# INLINE toTxt #-}
  toTxt = shortText . Builder.decimal

instance ToTxt Integer where
  {-# INLINE toTxt #-}
  toTxt = shortText . Builder.decimal

instance ToTxt Float where
  {-# INLINE toTxt #-}
  toTxt = toTxt . ($ "") . showFFloat Nothing

instance ToTxt Double where
  {-# INLINE toTxt #-}
  toTxt = toTxt . ($ "") . showFFloat Nothing

instance ToTxt Int64 where
  {-# INLINE toTxt #-}
  toTxt = shortText . Builder.decimal

instance ToTxt Int32 where
  {-# INLINE toTxt #-}
  toTxt = shortText . Builder.decimal

instance ToTxt Int16 where
  {-# INLINE toTxt #-}
  toTxt = shortText . Builder.decimal

instance ToTxt Int8 where
  {-# INLINE toTxt #-}
  toTxt = shortText . Builder.decimal

instance ToTxt Word64 where
  {-# INLINE toTxt #-}
  toTxt = shortText . Builder.decimal

instance ToTxt Word32 where
  {-# INLINE toTxt #-}
  toTxt = shortText . Builder.decimal

instance ToTxt Word16 where
  {-# INLINE toTxt #-}
  toTxt = shortText . Builder.decimal

instance ToTxt Word8 where
  {-# INLINE toTxt #-}
  toTxt = shortText . Builder.decimal
