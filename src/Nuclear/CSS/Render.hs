{-# language OverloadedStrings #-}
{-# language CPP #-}
{-# language TemplateHaskell #-}
module Nuclear.CSS.Render where

import Ef.Base

import Data.JSText

import Nuclear.Attribute
import Nuclear.ToText
import Nuclear.FromText
import Nuclear.CSS.Helpers

import Data.Functor.Identity
import Data.Monoid

#ifdef __GHCJS__
import Data.JSString as JSText
#else
import Data.Text as JSText
#endif

import Language.Haskell.TH.Syntax

data Styles_ k where
  Style_ :: JSText -> JSText -> k -> Styles_ k
  deriving Functor

type Styles = Code '[Styles_] Identity ()

instance ToText (Feature e) where
  toText NullFeature          = mempty

  toText (Attribute attr val) =
    case val of
      Left b  -> attr <> "=" <> if b then "true" else "false"
      Right v -> attr <> "=\"" <> v <> "\""

  toText (Style pairs) =
    "style=\""
      <> JSText.intercalate
           (JSText.singleton ';')
           (renderStyles False (mapM_ (uncurry (=:)) pairs))
      <> "\""

  toText (CurrentValue _) = mempty

  toText (On _ _ _)       = mempty

  toText (On' _ _ _ _)    = mempty

  toText (Link href _)    = "href=\"" <> href <> "\""

instance ToText [Feature e] where
  toText fs =
    JSText.intercalate
     (JSText.singleton ' ')
     (Prelude.filter (not . JSText.null) $ Prelude.map toText fs)

infixr 5 =:
(=:) :: JSText -> JSText -> Styles
(=:) nm val = Send (Style_ nm val (Return ()))

comment :: JSText -> Styles
comment com = (=:) ("//" <> com) ""

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

renderStyles :: Bool -> Styles -> [JSText]
renderStyles b = go
  where
    go (Return _) = []
    go (Lift s) = go (runIdentity s)
    go (Do msg) =
      case prj msg of
        ~(Just (Style_ k v r)) ->
          ((if b then "\t\t" else "\t") <> k <> ": " <> v)
          : go r

styled :: Styles -> Attribute msg
styled ss = Style $! go ss
  where
    go (Return _) = []
    go (Lift s) = go (runIdentity s)
    go (Do msg) =
      case prj msg of
        ~(Just (Style_ k v r)) -> (k,v) : go r

data CSS_ k where
  CSS_ :: JSText -> Styles -> k -> CSS_ k
  CSS3_ :: JSText -> JSText -> Maybe CSS -> k -> CSS_ k
  deriving Functor

type CSS = Code '[CSS_] Identity ()

select :: JSText -> Styles -> CSS
select sel ss = Send (CSS_ sel ss (Return ()))

atCharset :: JSText -> CSS
atCharset cs = Send (CSS3_ "@charset " cs Nothing (Return ()))

utf8Charset = atCharset "UTF-8"

iso885915Charset = atCharset "iso-8859-15"

atImport :: JSText -> CSS
atImport i = Send (CSS3_ "@import " i Nothing (Return ()))

data Namespace = XHTML | SVG

atNamespace :: Namespace -> Maybe JSText -> CSS
atNamespace ns mnsv = Send (CSS3_ namespace_ ns_ Nothing (Return ()))
  where
    ns_ =
      case ns of
        XHTML -> "url(http://www.w3.org/1999/xhtml)"
        SVG   -> "url(http://www.w3.org/2000/svg)"

    namespace_ =
      maybe "@namespace" ("@namespace " <>) mnsv

atMedia :: JSText -> CSS -> CSS
atMedia med c = Send (CSS3_ "@media " med (Just c) (Return ()))

atPage :: JSText -> CSS -> CSS
atPage pgsel rls = Send (CSS3_ "@page " pgsel (Just rls) (Return ()))

atFontFace :: JSText -> CSS -> CSS
atFontFace ff rls = Send (CSS3_ "@font-face " ff (Just rls) (Return ()))

atKeyframes :: JSText -> CSS -> CSS
atKeyframes nm kfs = Send (CSS3_ "@keyframes " nm (Just kfs) (Return ()))

-- data CSSError = InvalidCSSSyntax JSText deriving (Show)
-- instance Exception CSSError

newtype CSSText = CSSText { cssText :: JSText } deriving (Eq,Ord)
instance ToText CSSText where
  toText (CSSText csst) = csst
instance FromText CSSText where
  fromText = CSSText
instance Monoid CSSText where
  mempty = fromText mempty
  mappend csst1 csst2 = fromText $ toText csst1 <> "\n" <> toText csst2
instance Lift CSSText where
  lift (CSSText csst) = [| CSSText csst |]

staticCSS :: CSS -> CSSText
staticCSS = CSSText . go False
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
          let t = sel <> " {\n" <> JSText.intercalate (if b then ";\n\t" else ";\n") (renderStyles b ss) <> if b then "\n\t}\n\n" else "\n}\n\n"
          in (if b then "\t" <> t else t) <> go b k
        _ -> mempty
