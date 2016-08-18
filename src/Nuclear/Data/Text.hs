{-# language OverloadedStrings #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
module Nuclear.Data.Text
  ( Text
  , FromText (..)
  , fromText
  , fromTextError
  , takeLowerText
  , takeText
  , ToText   (..)
  , toTextCI
  , showText
  ) where

import           Data.Attoparsec.Text              (Parser)
import qualified Data.Attoparsec.Text              as A
import           Data.ByteString                   (ByteString)
import qualified Data.ByteString.Char8             as BS8
import           Data.CaseInsensitive              (CI)
import qualified Data.CaseInsensitive              as CI
import           Data.Int
import           Data.Monoid
import           Data.Scientific
import           Data.Text                         (Text)
import qualified Data.Text                         as Text
import qualified Data.Text.Encoding                as Text
import qualified Data.Text.Lazy                    as LText
import           Data.Text.Lazy.Builder            (Builder)
import qualified Data.Text.Lazy.Builder            as Build
import qualified Data.Text.Lazy.Builder.Int        as Build
import qualified Data.Text.Lazy.Builder.Scientific as Build
import           Numeric
import           Numeric.Natural

-- | Fail parsing with a 'Text' error.
--
-- Constrained to the actual attoparsec monad to avoid
-- exposing 'fail' usage directly.
fromTextError :: Text -> Parser a
fromTextError = fail . Text.unpack

fromText :: FromText a => Text -> Either String a
fromText = A.parseOnly parser

takeLowerText :: Parser Text
takeLowerText = Text.toLower <$> A.takeText

takeText :: Parser Text
takeText = A.takeText

class FromText a where
    parser :: Parser a

instance FromText Text where
    parser = A.takeText

instance FromText ByteString where
    parser = Text.encodeUtf8 <$> A.takeText

instance FromText Char where
    parser = A.anyChar <* A.endOfInput

instance FromText Int where
    parser = A.signed A.decimal <* A.endOfInput

instance FromText Integer where
    parser = A.signed A.decimal <* A.endOfInput

instance FromText Scientific where
    parser = A.signed A.scientific <* A.endOfInput

instance FromText Natural where
    parser = A.decimal <* A.endOfInput

instance FromText Double where
    parser = A.signed A.rational <* A.endOfInput

instance FromText Bool where
    parser = takeLowerText >>= \case
        "true"  -> pure True
        "false" -> pure False
        e       -> fromTextError $ "Failure parsing Bool from '" <> e <> "'."

showText :: ToText a => a -> String
showText = Text.unpack . toText

class ToText a where
    toText :: a -> Text

instance ToText a => ToText (CI a) where
    toText = toText . CI.original

instance ToText Text       where toText = id
instance ToText ByteString where toText = Text.decodeUtf8
instance ToText Char       where toText = Text.singleton
instance ToText String     where toText = Text.pack
instance ToText Int        where toText = shortText . Build.decimal
instance ToText Int64      where toText = shortText . Build.decimal
instance ToText Integer    where toText = shortText . Build.decimal
instance ToText Natural    where toText = shortText . Build.decimal
instance ToText Scientific where toText = shortText . Build.scientificBuilder
instance ToText Double     where toText = toText . ($ "") . showFFloat Nothing

instance ToText Bool where
    toText True  = "true"
    toText False = "false"

shortText :: Builder -> Text
shortText = LText.toStrict . Build.toLazyTextWith 32

toTextCI :: ToText a => a -> CI Text
toTextCI = CI.mk . toText
