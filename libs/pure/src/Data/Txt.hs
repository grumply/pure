{-# LANGUAGE CPP, PatternSynonyms, ViewPatterns, TypeSynonymInstances, OverloadedStrings, QuasiQuotes #-}
module Data.Txt 
  ( module Data.Txt 
  , module Export
#ifdef __GHCJS__
  , JSString(..)
#endif
  ) where

import Data.Char
import Data.Default
import Data.List as List
import Data.String

#ifdef __GHCJS__
import Data.Txt.GHCJS as Export
#else
import Data.Txt.GHC as Export
#endif

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as BSLC

import qualified Data.Text.Encoding as T
import qualified Data.Text.Lazy.Encoding as TL

{-
It is important to note that ToTxt/FromTxt can throw exceptions!

If you don't know where a value came from, be careful!

In general, for pure, this isn't much of an issue as the values we deal with
were either generated locally or have been produced over a websocket and thus
have already been safely utf-8 decoded.

These instances of FromTxt (Txt -> a) should be safe:

* String
* Text
* Lazy Text
* ByteString
* Lazy ByteString

These instances of ToTxt (a -> Txt) are unsafe:

* ByteString
* Lazy ByteString

-}

{-
i :: QuasiQuoter
i = QuasiQuoter {
    quoteExp  = \t -> [|fromTxt $(quoteExp I.i t)|]
  , quotePat  = err "pattern"
  , quoteType = err "type"
  , quoteDec  = err "declaration"
  }
  where
    err name  = error ("Data.Txt.i: This QuasiQuoter can not be used as a " ++ name)
-}
instance Default Txt where
  def = mempty

instance ToTxt a => ToTxt (Maybe a) where
  toTxt (Just a) = toTxt a
  toTxt Nothing = mempty

instance FromTxt a => FromTxt (Maybe a) where
  fromTxt x = if x == mempty then Nothing else Just (fromTxt x)

instance ToTxt BSLC.ByteString where
  toTxt = toTxt . TL.decodeUtf8

instance ToTxt BC.ByteString where
  toTxt = toTxt . T.decodeUtf8

instance ToTxt Txt where
  toTxt = id

unindent :: Txt -> Txt
unindent = Export.concat . removeIndentation . trimLastLine . removeLeadingEmptyLine . lines_
  where
    isEmptyLine :: Txt -> Bool
    isEmptyLine = Export.all isSpace

    lines_ :: Txt -> [Txt]
    lines_ s =
      if Export.null s
      then []
      else
        case Export.span (/= '\n') s of
          (first, rest) ->
            case Export.uncons rest of
              Just ('\n', more) -> (first <> "\n") : lines_ more
              _ -> first : lines_ rest


    removeLeadingEmptyLine :: [Txt] -> [Txt]
    removeLeadingEmptyLine xs = case xs of
      y:ys | isEmptyLine y -> ys
      _ -> xs

    trimLastLine :: [Txt] -> [Txt]
    trimLastLine (a : b : r) = a : trimLastLine (b : r)
    trimLastLine [a] = if Export.all (== ' ') a
      then []
      else [a]
    trimLastLine [] = []

    removeIndentation :: [Txt] -> [Txt]
    removeIndentation ys = List.map (dropSpaces indentation) ys
      where
        dropSpaces 0 s = s
        dropSpaces n s =
          case Export.uncons s of
            Just (' ',r) -> dropSpaces (n - 1) r
            _ -> s

        indentation = minimalIndentation ys

        minimalIndentation =
            safeMinimum 0
          . List.map (Export.length . Export.takeWhile (== ' '))
          . removeEmptyLines

        removeEmptyLines = List.filter (not . isEmptyLine)

        safeMinimum :: Ord a => a -> [a] -> a
        safeMinimum x xs = case xs of
          [] -> x
          _ -> List.minimum xs
