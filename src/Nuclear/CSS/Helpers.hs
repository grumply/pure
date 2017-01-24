{-# language OverloadedStrings #-}
{-# language CPP #-}
module Nuclear.CSS.Helpers where

import Nuclear.Cond

import Control.Arrow
import Data.Char
import Data.Functor.Identity
import Data.List as List
import Data.String
import Data.Monoid

import Data.JSText
#ifdef __GHCJS__
import Data.JSString as JSText
#else
import Data.Text as JSText
#endif

instance Cond JSText where
  nil = ""

instance IsString (JSText,Bool) where
  fromString [] = (pack [],True)
  fromString str = (pack str,True)

instance Monoid (JSText,Bool) where
  mempty = fromString []
  mappend (s1,b1) (s2,b2) = (s1 <> s2,b1 && b2)

infixr 6 <&>>
(<&>>) :: JSText -> JSText -> JSText
(<&>>) x y = x <> ", " <> y

infixr 6 <+>
(<+>) :: JSText -> JSText -> JSText
(<+>) x y = x <> "+" <> y

infixr 6 <<+>>
(<<+>>) :: JSText -> JSText -> JSText
(<<+>>) x y = x <> " + " <> y

infixr 6 <<>>
(<<>>) :: JSText -> JSText -> JSText
(<<>>) x y = x <> " " <> y

unindent :: JSText -> JSText
unindent =
      lines_
  >>> removeLeadingEmptyLine
  >>> trimLastLine
  >>> removeIndentation
  >>> JSText.concat
  where
    isEmptyLine :: JSText -> Bool
    isEmptyLine = JSText.all isSpace

    lines_ :: JSText -> [JSText]
    lines_ s =
      if JSText.null s
      then []
      else
        case JSText.span (/= '\n') s of
          (first, rest) ->
            case JSText.uncons rest of
              Just ('\n', more) -> (first <> JSText.pack "\n") : lines_ rest
              _ -> first : lines_ rest

    removeLeadingEmptyLine :: [JSText] -> [JSText]
    removeLeadingEmptyLine xs = case xs of
      y:ys | isEmptyLine y -> ys
      _ -> xs

    trimLastLine :: [JSText] -> [JSText]
    trimLastLine (a : b : r) = a : trimLastLine (b : r)
    trimLastLine [a] = if JSText.all (== ' ') a
      then []
      else [a]
    trimLastLine [] = []

    removeIndentation :: [JSText] -> [JSText]
    removeIndentation ys = List.map (dropSpaces indentation) ys
      where
        dropSpaces 0 s = s
        dropSpaces n s =
          case JSText.uncons s of
            Just (' ',r) -> dropSpaces (n - 1) r
            _ -> s
        indentation = minimalIndentation ys
        minimalIndentation =
            safeMinimum 0
          . List.map (JSText.length . JSText.takeWhile (== ' '))
          . removeEmptyLines
        removeEmptyLines = List.filter (not . isEmptyLine)

        safeMinimum :: Ord a => a -> [a] -> a
        safeMinimum x xs = case xs of
          [] -> x
          _ -> List.minimum xs
