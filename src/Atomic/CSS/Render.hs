{-# language OverloadedStrings #-}
{-# language CPP #-}
{-# language TemplateHaskell #-}
{-# language ViewPatterns #-}
module Atomic.CSS.Render where

import Ef.Base

import Data.Txt

import Atomic.Attribute
import Atomic.ToTxt
import Atomic.FromTxt
import Atomic.CSS.Helpers

import Data.Functor.Identity
import Data.Monoid

#ifdef __GHCJS__
import Data.JSString as Txt
#else
import Data.Text as Txt
#endif

import Language.Haskell.TH.Syntax

data Styles_ k where
  Style_ :: Txt -> Txt -> k -> Styles_ k
  deriving Functor

type Styles = Ef '[Styles_] Identity ()

infixr 5 =:
(=:) :: Txt -> Txt -> Styles
(=:) nm val = Send (Style_ nm val (Return ()))

comment :: Txt -> Styles
comment com = (=:) ("//" <> com) ""

classify :: Txt -> Txt
classify = ("." <>) . toTxt

important :: Styles -> Styles
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
ignore :: Styles -> Styles
ignore = go
  where
    go (Return r) = Return r
    go (Lift s) = go (runIdentity s)
    go (Do msg) =
      case prj msg of
        ~(Just (Style_ k v r)) -> Do (inj (Style_ ("//" <> k) v (go r)))

renderStyles :: Bool -> Styles -> [Txt]
renderStyles b = go
  where
    go (Return _) = []
    go (Lift s) = go (runIdentity s)
    go (Do msg) =
      case prj msg of
        ~(Just (Style_ k v r)) ->
          ((if b then "\t\t" else "\t") <> k <> ": " <> v)
          : go r

styled :: Styles -> Feature ms
styled ss = StyleF $! go ss
  where
    go (Return _) = []
    go (Lift s) = go (runIdentity s)
    go (Do msg) =
      case prj msg of
        ~(Just (Style_ k v r)) -> (k,v) : go r

data CSS_ k where
  CSS_ :: Txt -> Styles -> k -> CSS_ k
  CSS3_ :: Txt -> Txt -> Maybe CSS -> k -> CSS_ k
  deriving Functor

type CSS = Ef '[CSS_] Identity ()

select :: Txt -> Styles -> CSS
select sel ss = Send (CSS_ sel ss (Return ()))

selects :: [Txt] -> Styles -> CSS
selects sels ss = sequence_ $ Prelude.map (flip select ss) sels 

nested :: Txt -> CSS -> CSS
nested sel = go
  where
    go (Return r) = Return r
    go (Lift s) = go (runIdentity s)
    go (Do msg) =
      case prj msg of

        Just (CSS_ suf ss rest) ->
          if Txt.null suf then
            Send (CSS_ sel ss (go rest))
          else
            Send (CSS_ (sel <> suf) ss (go rest))

        Just (CSS3_ at rule mcss rest) ->
          Send (CSS3_ at rule (fmap go mcss) (go rest))

atCharset :: Txt -> CSS
atCharset cs = Send (CSS3_ "@charset " cs Nothing (Return ()))

utf8Charset = atCharset "UTF-8"

iso885915Charset = atCharset "iso-8859-15"

atImport :: Txt -> CSS
atImport i = Send (CSS3_ "@import " i Nothing (Return ()))

data Namespace = XHTML | SVG

atNamespace :: Namespace -> Maybe Txt -> CSS
atNamespace ns mnsv = Send (CSS3_ namespace_ ns_ Nothing (Return ()))
  where
    ns_ =
      case ns of
        XHTML -> "url(http://www.w3.org/1999/xhtml)"
        SVG   -> "url(http://www.w3.org/2000/svg)"

    namespace_ =
      maybe "@namespace" ("@namespace " <>) mnsv

atMedia :: Txt -> CSS -> CSS
atMedia med c = Send (CSS3_ "@media " med (Just c) (Return ()))

atPage :: Txt -> CSS -> CSS
atPage pgsel rls = Send (CSS3_ "@page " pgsel (Just rls) (Return ()))

atFontFace :: Txt -> CSS -> CSS
atFontFace ff rls = Send (CSS3_ "@font-face " ff (Just rls) (Return ()))

atKeyframes :: Txt -> CSS -> CSS
atKeyframes nm kfs = Send (CSS3_ "@keyframes " nm (Just kfs) (Return ()))

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

instance ToTxt CSS where
  toTxt = ((Txt.singleton '\n') <>) . go False
    where
      go b (Return _) = mempty
      go b (Lift s) = go b (runIdentity s)
      go b (Do msg) =
        case prj msg of
          Just (CSS3_ atRule sel mCSS k) ->
            case mCSS of
              Nothing ->
                atRule <> sel <> ";\n" <> go False k
              Just c' ->
                atRule <> sel <> " {\n" <> go True c' <> "\n}\n\n" <> go False k
          Just (CSS_ sel ss k) ->
            let t = sel <> " {\n" <> Txt.intercalate ";\n" (renderStyles b ss) <> if b then "\n\t}\n\n" else "\n}\n\n"
            in (if b then "\t" <> t else t) <> go b k
          _ -> mempty

staticCSS :: CSS -> StaticCSS
staticCSS = fromTxt . toTxt
