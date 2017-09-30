{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
module Pure.CSS (module Pure.CSS, module Export) where

import Ef

import Pure.Data as Export hiding (wrap,wait,end,from,to)
import Pure.Styles as Export

import Pure.Data.Txt as Txt
import Pure.Types.Feature
import Pure.Types.View

import Data.Char

import Control.Category as C
import Data.Bifunctor
import Data.Foldable as F
import Data.Functor.Identity
import qualified Data.List as List
import Data.Monoid
import Data.String
import Data.Traversable

import qualified Data.Map.Strict as M

import Unsafe.Coerce

import qualified Language.Haskell.TH.Syntax as TH

import Prelude hiding ((.),id)

data Styles_ k where
  Style_ :: Txt -> Txt -> k -> Styles_ k
  deriving Functor

type Styles = Narrative Styles_ Identity

infixr 5 =:
(=:) :: Txt -> Txt -> Styles Txt
(=:) nm val = send (Style_ nm val val)

comment :: Txt -> Styles Txt
comment com = (=:) ("//" <> com) ""

classify :: Txt -> Txt
classify = ("." <>) . toTxt

important :: Styles a -> Styles a
important s = buildn $ \r l d ->
  let go (Style_ k v x) = d (Style_ k (v <> " !important") x)
  in foldn r l go s

-- useful for debugging; keep the styles on the page, but block them with //
-- Can be used to comment out one or more styles
--
-- > ignore $ backgroundColor =: hideousOrange
--
-- > ignore $ do
-- >   backgroundColor =: blue
-- >   fontSize =: ems 1.3
ignore :: Styles a -> Styles a
ignore s = buildn $ \r l d ->
  let go (Style_ k v x) = d (Style_ ("// " <> k) v x)
  in foldn r l go s

renderStyles :: Bool -> Styles a -> ([Txt],a)
renderStyles b = first Prelude.reverse . runIdentity . flip (thread go) []
  where
    go (Style_ k v r) acc =
       r (((if b then "\t\t" else "\t") <> k <> ": " <> v):acc)

styled :: Styles a -> Feature ms
styled = StyleList . fst . runIdentity . flip (thread collect) mempty
  where
    collect (Style_ k v r) acc = r $ M.insert k v acc

getStyles :: [Feature ms] -> Maybe (Styles (),[Feature ms])
getStyles = go (return ()) []
  where
    go ss fs [] =
      case ss of
        Return _ -> Nothing
        _ -> Just (ss,Prelude.reverse fs)
    go ss fs ((StyleList styles):rest) =
      go (ss >> sequence_ (M.mapWithKey (=:) styles)) fs rest
    go ss fs (x:rest) = go ss (x:fs) rest

data CSS_ k where
  CSS_ :: Txt -> Styles a -> (a -> k) -> CSS_ k
  CSS3_ :: Txt -> Txt -> CSS a -> (a -> k) -> CSS_ k

instance Functor CSS_ where
  fmap f (CSS_ t ss ak) = CSS_ t ss (fmap f ak)
  fmap f (CSS3_ n l cs ak) = CSS3_ n l cs (fmap f ak)

type CSS = Narrative CSS_ Identity

selector :: Txt -> Styles a -> CSS a
selector sel ss = send (CSS_ sel ss id)

apply :: Styles a -> CSS a
apply = selector ""

infixr 1 .>
(.>) :: (CSS a -> CSS b) -> Styles a -> CSS b
(.>) f decls = f $ apply decls

reusable :: Monad m => m a -> m (m a,a)
reusable ma = do
  a <- ma
  return (ma,a)

newtype Composable a = Composable { composes :: CSS a }
composable :: CSS a -> CSS (Composable a)
composable css = css >> return (Composable css)

newtype Extendable a = Extendable { extends :: Styles a }
extendable :: Styles a -> Styles (Extendable a)
extendable ss = ss >> return (Extendable ss)

select :: Txt -> CSS a -> CSS a
select sel s = buildn $ \r l d ->
  let go (CSS_ suf ss rest) =
        d (CSS_ (sel <> suf) ss rest)

      go (CSS3_ at rule css rest) =
        d (CSS3_ at rule (select sel css) rest)

  in foldn r l go s

any :: Txt
any = "*"

before :: Txt
before = ":before"

after :: Txt
after = ":after"

active :: Txt
active = ":active"

hovered :: Txt
hovered = ":hover"

focused :: Txt
focused = ":focus"

disabled :: Txt
disabled = "[disabled]"

is :: Txt -> CSS a -> CSS a
is = select

-- equivalent to id; purely for scanning purposes
and :: (Txt -> CSS a -> CSS a) -> Txt -> CSS a -> CSS a
and f sel css = f sel css

or :: (Txt -> CSS a -> CSS a) -> Txt -> CSS a -> CSS a
or f sel css = f (", " <> sel) css

isn't :: Txt -> CSS a -> CSS a
isn't sel = select (":not(" <> sel <> ")")

compose :: (Category cat, Foldable t) => t (cat a a) -> cat a a
compose = F.foldr (>>>) id

use :: (Traversable t, Applicative f) => t (a -> f b) -> a -> f (t b)
use fs x = for fs ($ x)

pseudo :: Txt -> CSS a -> CSS a
pseudo sel = select (":" <> sel)

attr :: Txt -> CSS a -> CSS a
attr sel = select ("[" <> sel <> "]")

child :: Txt -> CSS a -> CSS a
child sel = select (" > " <> sel)

has :: Txt -> CSS a -> CSS a
has sel = select (" " <> sel)

next :: Txt -> CSS a -> CSS a
next sel = select (" + " <> sel)

nexts :: Txt -> CSS a -> CSS a
nexts sel = select (" ~ " <> sel)

atCharset :: Txt -> CSS ()
atCharset cs = send (CSS3_ "@charset " cs (return ()) id)

utf8Charset :: CSS ()
utf8Charset = atCharset "UTF-8"

iso885915Charset :: CSS ()
iso885915Charset = atCharset "iso-8859-15"

atImport :: Txt -> CSS ()
atImport i = send (CSS3_ "@import " i (Return ()) id)

data Namespace = XHTMLNS | SVGNS

atNamespace :: Namespace -> Maybe Txt -> CSS ()
atNamespace ns mnsv = send (CSS3_ namespace_ ns_ (Return ()) id)
  where
    ns_ =
      case ns of
        XHTMLNS -> "url(http://www.w3.org/1999/xhtml)"
        SVGNS   -> "url(http://www.w3.org/2000/svg)"

    namespace_ =
      maybe "@namespace" ("@namespace " <>) mnsv

atMedia :: Txt -> CSS a -> CSS a
atMedia med c = send (CSS3_ "@media " med c id)

atPage :: Txt -> CSS a -> CSS a
atPage pgsel rls = send (CSS3_ "@page " pgsel rls id)

atFontFace :: Txt -> CSS a -> CSS a
atFontFace ff rls = send (CSS3_ "@font-face " ff rls id)

atWebkitKeyframes :: Txt -> CSS a -> CSS a
atWebkitKeyframes nm kfs = send (CSS3_ "@-webkit-keyframes" nm kfs id)

atKeyframes :: Txt -> CSS a -> CSS a
atKeyframes nm kfs = send (CSS3_ "@keyframes " nm kfs id)

-- data CSSError = InvalidCSSSyntax Txt deriving (Show)
-- instance Exception CSSError

data StaticCSS = StaticCSS { cssText :: {-# UNPACK #-} !Txt } deriving (Eq,Ord)
instance ToTxt StaticCSS where
  toTxt (StaticCSS csst) = csst
instance FromTxt StaticCSS where
  fromTxt = StaticCSS
instance Monoid StaticCSS where
  mempty = fromTxt mempty
  mappend csst1 csst2 = fromTxt $ toTxt csst1 <> "\n" <> toTxt csst2
instance TH.Lift StaticCSS where
  lift (StaticCSS csst) = [| StaticCSS csst |]

instance ToTxt (CSS a) where
  toTxt = fst . go "\n" False
    where
      go acc b (Return a) = (acc,a)
      go acc b (Lift s) = go acc b (runIdentity s)
      go acc b (Do msg) =
        case msg of
          CSS3_ atRule sel css k ->
            case css of
              Return a ->
                 go (acc <> atRule <> sel <> ";\n") False (k a)
              _ ->
                let (b,a) = go mempty True (unsafeCoerce css)
                in go (acc <> atRule <> sel <> " {\n" <> b <> "\n}\n\n") False (unsafeCoerce k a)
          CSS_ sel ss k ->
            let (s,a) = renderStyles b (unsafeCoerce ss)
                t = sel <> " {\n" <> Txt.intercalate ";\n" s <> if b then "\n\t}\n\n" else "\n}\n\n"
            in go (acc <> if b then "\t" <> t else t) b (unsafeCoerce k a)

staticCSS :: CSS a -> StaticCSS
staticCSS = fromTxt . toTxt

-- rudimentary styled components; no CSS3
pattern Styled :: ([Feature ms] -> [View ms] -> View ms) -> Styles () -> [Feature ms] -> [View ms] -> View ms
pattern Styled f ss fs vs <- (styledView -> Just (f,ss,fs,vs)) where
  Styled f ss fs vs = f (styled ss : fs) vs

styledView :: View ms -> Maybe ([Feature ms] -> [View ms] -> View ms,Styles (),[Feature ms],[View ms])
styledView (HTMLView _ tag fs vs) =
  case getStyles fs of
    Just (ss,rest) -> Just (HTMLView Nothing tag,ss,rest,vs)
    _ -> Nothing
styledView (SVGView _ tag fs vs) =
  case getStyles fs of
    Just (ss,rest) -> Just (SVGView Nothing tag,ss,rest,vs)
    _ -> Nothing

unindent :: Txt -> Txt
unindent =
  Txt.concat .
  removeIndentation .
  trimLastLine .
  removeLeadingEmptyLine .
  lines_
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

css :: Narrative CSS_ Identity a -> View e
css = css' False

css' :: forall a e. Bool -> Narrative CSS_ Identity a -> View e
css' b = mkHTML "style" [ Property "type" "text/css", Property "scoped" (if b then "true" else "") ] . ((text "\n"):) . fst . go False []
  where
    go :: forall a. Bool -> [View e] -> Narrative CSS_ Identity a -> ([View e],a)
    go b acc (Return a) = (acc,a)
    go b acc (Lift s) = go b acc (runIdentity s)
    go b acc c@(Do msg) =
      case msg of
        CSS3_ atRule sel css k ->
          case css of
            Return a ->
              go False (acc ++ [ text (atRule <> sel <> ";\n") ]) (k a)
            _ ->
              let (c,a) = go True [] css
              in go False (acc ++ ( text (atRule <> sel <> " {\n") : c) ++ [ text "\n}\n\n" ]) (k a)
        CSS_ sel ss r ->
          let (s,a) = renderStyles b ss
          in
            go b  ( acc ++ [ text ( (if b then "\t" else mempty)
                                      <> sel
                                      <> " {\n"
                                      <> (Txt.intercalate (if b then ";\n\t" else ";\n") s)
                                      <> (if b then "\n\t}\n\n" else "\n}\n\n")
                                  )
                           ]
                  ) (r a)

scss :: StaticCSS -> View e
scss = scss' False

scss' :: Bool -> StaticCSS -> View e
scss' b = raw (mkHTML "style") [ Property "type" "text/css", Property "scoped" (if b then "true" else "") ] . cssText

inlineCSS :: Narrative CSS_ Identity a -> View e
inlineCSS = css' True . classify
  where
    classify :: forall a. Narrative CSS_ Identity a -> Narrative CSS_ Identity a
    classify (Return r) = Return r
    classify (Lift sup) = Lift (fmap classify sup)
    classify (Do e) =
      case e of
        CSS_ sel ss k ->
          Do (CSS_ (Txt.cons '.' sel) ss (classify . k))
        CSS3_ at sel css k ->
          Do (CSS3_ at sel (classify css) (classify . k))

