{-# language CPP #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language UndecidableInstances #-}
module Atomic.FromTxt where

#ifdef __GHCJS__
import Data.JSString
import Data.JSString.Read as T
#else
import Data.Text.Read as T
#endif

import Data.Maybe

import Data.Txt

import Data.String

import Data.Coerce

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy as TL

class FromTxt a where
  fromTxt :: Txt -> a
  default fromTxt :: Coercible Txt a => Txt -> a
  fromTxt = coerce
  {-# INLINE fromTxt #-}

instance {-# OVERLAPPABLE #-} FromTxt a => IsString a where
  fromString = fromTxt . pack

instance FromTxt TL.Text where
  {-# INLINE fromTxt #-}
#ifdef __GHCJS__
  fromTxt = lazyTextFromJSString
#else
  fromTxt = TL.fromStrict
#endif

instance FromTxt T.Text where
  {-# INLINE fromTxt #-}
#ifdef __GHCJS__
  fromTxt = textFromJSString
#else
  fromTxt = id
#endif

#ifdef __GHCJS__
instance FromTxt JSString where
  {-# INLINE fromTxt #-}
  fromTxt = id
#endif

instance FromTxt [Char] where
  {-# INLINE fromTxt #-}
#ifdef __GHCJS__
  fromTxt = unpack
#else
  fromTxt = T.unpack
#endif

instance FromTxt B.ByteString where
  {-# INLINE fromTxt #-}
#ifdef __GHCJS__
  fromTxt = BC.pack . unpack
#else
  fromTxt = T.encodeUtf8
#endif

instance FromTxt BSL.ByteString where
  {-# INLINE fromTxt #-}
#ifdef __GHCJS__
  fromTxt = BSLC.pack . unpack
#else
  fromTxt = BSL.fromStrict . T.encodeUtf8
#endif

readError :: String -> Txt -> a
readError ty t = error ("Atomic.FromTxt: FromTxt failed to read an " ++ ty ++ " when given: " ++ show t)

instance FromTxt Int where
  {-# INLINE fromTxt #-}
#ifdef __GHCJS__
  fromTxt t = fromMaybe (readError "Int" t) (T.readIntMaybe t)
#else
  fromTxt t = either (readError "Int" t) fst (T.signed t)
#endif

instance FromTxt Double where
  {-# INLINE fromTxt #-}
#ifdef __GHCJS__
  fromTxt t = fromMaybe (readError "Double" t) (T.readDoubleMaybe t)
#else
  fromTxt t = either (readError "Double" t) fst (T.double t)
#endif

instance FromTxt Integer where
  {-# INLINE fromTxt #-}
#ifdef __GHCJS__
  fromTxt t = fromMaybe (readError "Integer" t) (T.readIntegerMaybe t)
#else
  fromTxt t = either (readError "Integer" t) fst (T.signed t)
#endif

instance FromTxt a => FromTxt (Maybe a) where
  fromTxt x = if x == mempty then Nothing else Just (fromTxt x)
