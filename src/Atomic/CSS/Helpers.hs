{-# language OverloadedStrings #-}
{-# language CPP #-}
module Atomic.CSS.Helpers where

import Atomic.Cond

import Control.Arrow
import Data.Char
import Data.Functor.Identity
import Data.List as List
import Data.String
import Data.Monoid

import Data.Txt
#ifdef __GHCJS__
import Data.JSString as Txt
#else
import Data.Text as Txt
#endif

instance Cond Txt where
  nil = ""

instance IsString (Txt,Bool) where
  fromString [] = (pack [],True)
  fromString str = (pack str,True)

instance Monoid (Txt,Bool) where
  mempty = fromString []
  mappend (s1,b1) (s2,b2) = (s1 <> s2,b1 && b2)

infixr 6 <&>>
(<&>>) :: Txt -> Txt -> Txt
(<&>>) x y = x <> ", " <> y

infixr 6 <+>
(<+>) :: Txt -> Txt -> Txt
(<+>) x y = x <> "+" <> y

infixr 6 <<+>>
(<<+>>) :: Txt -> Txt -> Txt
(<<+>>) x y = x <> " + " <> y

infixr 6 <<>>
(<<>>) :: Txt -> Txt -> Txt
(<<>>) x y = x <> " " <> y

infixr 6 <.>
(<.>) :: Txt -> Txt -> Txt
(<.>) x y = x <> "." <> y

infixr 6 <<.>
(<<.>) :: Txt -> Txt -> Txt
(<<.>) x y = x <> " ." <> y

infixr 6 <.>>
(<.>>) :: Txt -> Txt -> Txt
(<.>>) x y = x <> ". " <> y

unindent :: Txt -> Txt
unindent =
      lines_
  >>> removeLeadingEmptyLine
  >>> trimLastLine
  >>> removeIndentation
  >>> Txt.concat
  where
    isEmptyLine :: Txt -> Bool
    isEmptyLine = Txt.all isSpace

    lines_ :: Txt -> [Txt]
    lines_ s =
      if Txt.null s
      then []
      else
        case Txt.span (/= '\n') s of
          (first, rest) ->
            case Txt.uncons rest of
              Just ('\n', more) -> (first <> Txt.pack "\n") : lines_ rest
              _ -> first : lines_ rest

    removeLeadingEmptyLine :: [Txt] -> [Txt]
    removeLeadingEmptyLine xs = case xs of
      y:ys | isEmptyLine y -> ys
      _ -> xs

    trimLastLine :: [Txt] -> [Txt]
    trimLastLine (a : b : r) = a : trimLastLine (b : r)
    trimLastLine [a] = if Txt.all (== ' ') a
      then []
      else [a]
    trimLastLine [] = []

    removeIndentation :: [Txt] -> [Txt]
    removeIndentation ys = List.map (dropSpaces indentation) ys
      where
        dropSpaces 0 s = s
        dropSpaces n s =
          case Txt.uncons s of
            Just (' ',r) -> dropSpaces (n - 1) r
            _ -> s
        indentation = minimalIndentation ys
        minimalIndentation =
            safeMinimum 0
          . List.map (Txt.length . Txt.takeWhile (== ' '))
          . removeEmptyLines
        removeEmptyLines = List.filter (not . isEmptyLine)

        safeMinimum :: Ord a => a -> [a] -> a
        safeMinimum x xs = case xs of
          [] -> x
          _ -> List.minimum xs
