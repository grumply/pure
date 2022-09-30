module Pure.Conjurer.Slug (Slug()) where

import Pure.Conjurer.Pathable

import Data.JSON
import Data.Txt as Txt
import Data.Router

import Data.Hashable

import Data.Char

newtype Slug x = Slug Txt
  deriving (Eq,Ord,ToJSON,FromJSON,Hashable) via Txt
type role Slug nominal

instance ToTxt (Slug x) where
  toTxt (Slug x) = x

instance FromTxt (Slug x) where
  fromTxt = toSlug

instance Pathable (Slug a) where
  toPath (Slug s) = "/" <> s
  fromPath = path' "/:slug" "slug"

-- Idempotent.
--
-- prop> \(x :: String) -> toSlug (toTxt (toSlug (toTxt x))) == toSlug (toTxt x)
-- 
toSlug :: ToTxt a => a -> Slug b
toSlug = Slug . Txt.dropWhileEnd (Prelude.not . isAlphaNum) . Txt.dropWhile (Prelude.not . isAlphaNum) . process . Txt.take 255 . toTxt
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
