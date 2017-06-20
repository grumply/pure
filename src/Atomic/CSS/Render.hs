{-# language OverloadedStrings #-}
{-# language CPP #-}
{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
module Atomic.CSS.Render where

import Ef.Base

import Data.Txt

import Data.Foldable as F
import Control.Category as C

import Atomic.Attribute
import Atomic.ToTxt
import Atomic.FromTxt
import Atomic.CSS.Helpers
import Atomic.Cond

import Data.Functor.Identity
import Data.Monoid
import Data.Traversable

import Data.String

import qualified Data.List as L

import Unsafe.Coerce

#ifdef __GHCJS__
import Data.JSString as Txt
#else
import Data.Text as Txt
#endif

import Language.Haskell.TH.Syntax

import Prelude hiding ((.),id)

data Styles_ k where
  Style_ :: Txt -> Txt -> k -> Styles_ k
  deriving Functor

type Styles = Ef '[Styles_] Identity

infixr 5 =:
(=:) :: Txt -> Txt -> Styles Txt
(=:) nm val = Send (Style_ nm val (Return val))

comment :: Txt -> Styles Txt
comment com = (=:) ("//" <> com) ""

classify :: Txt -> Txt
classify = ("." <>) . toTxt

important :: Styles a -> Styles a
important = go
  where
    go (Return r) = Return r
    go (Lift s) = go (runIdentity s)
    go (Do msg) =
      case prj msg of
        ~(Just (Style_ k v r)) ->
          Do (inj (Style_ k (v <> " !important") (go r)))

-- useful for debugging; keep the styles on the page, but block them with //
-- Can be used to comment out one or more styles
--
-- > ignore $ backgroundColor =: hideousOrange
--
-- > ignore $ do
-- >   backgroundColor =: blue
-- >   fontSize =: ems 1.3
ignore :: Styles a -> Styles a
ignore = go
  where
    go (Return r) = Return r
    go (Lift s) = go (runIdentity s)
    go (Do msg) =
      case prj msg of
        ~(Just (Style_ k v r)) -> Do (inj (Style_ ("//" <> k) v (go r)))

renderStyles :: Bool -> Styles a -> ([Txt],a)
renderStyles b = go []
  where
    go acc (Return a) = (L.reverse acc,a)
    go acc (Lift s) = go acc (runIdentity s)
    go acc (Do msg) =
      case prj msg of
        ~(Just (Style_ k v r)) ->
          go (((if b then "\t\t" else "\t") <> k <> ": " <> v):acc) r

styled :: Styles a -> Feature ms
styled ss = StyleF $! go ss
  where
    go (Return _) = []
    go (Lift s) = go (runIdentity s)
    go (Do msg) =
      case prj msg of
        ~(Just (Style_ k v r)) -> (k,v) : go r

data CSS_ k where
  CSS_ :: Txt -> Styles a -> (a -> k) -> CSS_ k
  CSS3_ :: Txt -> Txt -> CSS a -> (a -> k) -> CSS_ k

instance Functor CSS_ where
  fmap f (CSS_ t ss ak) = CSS_ t ss (fmap f ak)
  fmap f (CSS3_ n l cs ak) = CSS3_ n l cs (fmap f ak)

type CSS = Ef '[CSS_] Identity

selector :: Txt -> Styles a -> CSS a
selector sel ss = Send (CSS_ sel ss Return)

apply :: Styles a -> CSS a
apply = selector ""

infixr 1 .>
(.>) :: (CSS a -> CSS a) -> Styles a -> CSS a
(.>) f decls = f $ apply decls

reusable :: Monad m => m a -> m (m a,a)
reusable ma = do
  a <- ma
  return (ma,a)

newtype Composable a = Composable { composes :: CSS a }

composable :: CSS a -> CSS (Composable a)
composable css = do
  css
  return (Composable css)

newtype Extendable a = Extendable { extends :: Styles a }

extendable :: Styles a -> Styles (Extendable a)
extendable ss = do
  ss
  return (Extendable ss)

select :: Txt -> CSS a -> CSS a
select sel = go
  where
    go (Return r) = Return r
    go (Lift s) = go (runIdentity s)
    go (Do msg) =
      case prj msg of

        Just (CSS_ suf ss rest) ->
          Send (CSS_ (sel <> suf) ss (go . rest))

        Just (CSS3_ at rule css rest) ->
          Send (CSS3_ at rule (go (unsafeCoerce css)) (go . (unsafeCoerce rest)))

any :: Txt
any = "*"

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
atCharset cs = Send (CSS3_ "@charset " cs (Return ()) Return)

utf8Charset :: CSS ()
utf8Charset = atCharset "UTF-8"

iso885915Charset :: CSS ()
iso885915Charset = atCharset "iso-8859-15"

atImport :: Txt -> CSS ()
atImport i = Send (CSS3_ "@import " i (Return ()) Return)

data Namespace = XHTML | SVG

atNamespace :: Namespace -> Maybe Txt -> CSS ()
atNamespace ns mnsv = Send (CSS3_ namespace_ ns_ (Return ()) Return)
  where
    ns_ =
      case ns of
        XHTML -> "url(http://www.w3.org/1999/xhtml)"
        SVG   -> "url(http://www.w3.org/2000/svg)"

    namespace_ =
      maybe "@namespace" ("@namespace " <>) mnsv

atMedia :: Txt -> CSS a -> CSS a
atMedia med c = Send (CSS3_ "@media " med c Return)

atPage :: Txt -> CSS a -> CSS a
atPage pgsel rls = Send (CSS3_ "@page " pgsel rls Return)

atFontFace :: Txt -> CSS a -> CSS a
atFontFace ff rls = Send (CSS3_ "@font-face " ff rls Return)

atWebkitKeyframes :: Txt -> CSS a -> CSS a
atWebkitKeyframes nm kfs = Send (CSS3_ "@-webkit-keyframes" nm kfs Return)

atKeyframes :: Txt -> CSS a -> CSS a
atKeyframes nm kfs = Send (CSS3_ "@keyframes " nm kfs Return)

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
instance Lift StaticCSS where
  lift (StaticCSS csst) = [| StaticCSS csst |]

instance ToTxt (CSS a) where
  toTxt t = fst $ go "\n" False t
    where
      go acc b (Return a) = (acc,a)
      go acc b (Lift s) = go acc b (runIdentity s)
      go acc b (Do msg) =
        case prj msg of
          Just (CSS3_ atRule sel css k) ->
            case css of
              Return a ->
                 go (acc <> atRule <> sel <> ";\n") False (k a)
              _ ->
                let (b,a) = go mempty True (unsafeCoerce css)
                in go (acc <> atRule <> sel <> " {\n" <> b <> "\n}\n\n") False (unsafeCoerce k a)
          Just (CSS_ sel ss k) ->
            let (s,a) = renderStyles b (unsafeCoerce ss)
                t = sel <> " {\n" <> Txt.intercalate ";\n" s <> if b then "\n\t}\n\n" else "\n}\n\n"
            in go (acc <> if b then "\t" <> t else t) b (unsafeCoerce k a)

staticCSS :: CSS a -> StaticCSS
staticCSS = fromTxt . toTxt
