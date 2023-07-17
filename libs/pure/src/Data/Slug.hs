{-# language DerivingVia #-}
module Data.Slug (Slug()) where

import Data.Char
import Data.Hashable
import Data.JSON
import Data.Router
import Data.String
import Data.Txt as Txt

newtype Slug = Slug Txt
  deriving (Eq,Ord,ToJSON,FromJSON,Hashable) via Txt

instance ToTxt Slug where
  toTxt (Slug x) = x

instance FromTxt Slug where
  fromTxt = toSlug

instance IsString Slug where
  fromString = toSlug . fromTxt . toTxt

-- Idempotent.
--
-- prop> \(x :: String) -> toSlug (toTxt (toSlug (toTxt x))) == toSlug (toTxt x)
-- 
toSlug :: Txt -> Slug
toSlug = Slug . Txt.dropWhileEnd (Prelude.not . isAlphaNum) . Txt.dropWhile (Prelude.not . isAlphaNum) . process . Txt.take 255
    where 
      safe c = c == '.' || isAscii c && isAlphaNum c

      process :: Txt -> Txt
      process = go False
        where
          go dropping x =
            case Txt.uncons x of
              Just (c,rest) -> 
                let 
                  d = isAscii c && Prelude.not (isAlphaNum c) 
                  l | dropping && d = id
                    | safe c        = Txt.cons (Data.Char.toLower c)
                    | otherwise     = Txt.cons '-'
                in
                  l (go d rest)

              _ -> x
