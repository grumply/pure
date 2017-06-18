{-# language OverloadedStrings #-}
{-# language CPP #-}
{-# language TemplateHaskell #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
module Atomic.CSS.Render where

import Ef.Base

import Data.Txt

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

select :: Txt -> Styles a -> CSS a
select sel ss = Send (CSS_ sel ss Return)

selects :: [Txt] -> Styles a -> CSS [a]
selects sels ss = sequence $ Prelude.map (flip select ss) sels

newtype Block = Block Txt
  deriving (IsString,ToTxt,FromTxt,Show,Eq)

newtype Element = Element Txt
  deriving (IsString,ToTxt,FromTxt,Show,Eq)

newtype Modifier = Modifier Txt
  deriving (IsString,ToTxt,FromTxt,Show,Eq)

newtype Utility = Utility Txt
  deriving (IsString,ToTxt,FromTxt,Show,Eq)

styles :: Styles a -> CSS a
styles = select ""

b :: Block -> Txt
b = toTxt

be :: Block -> Element -> Txt
be (Block b) (Element e) = b <> "__" <> e

bm :: Block -> Modifier -> Txt
bm (Block b) (Modifier m) = b <> "--" <> m

bem :: Block -> Element -> Modifier -> Txt
bem (Block b) (Element e) (Modifier m) = b <> "__" <> e <> "--" <> m

u :: Utility -> Txt
u (Utility u) = "u-" <> u

block :: Block -> CSS a -> CSS a
block (Block b) = nested ("." <> b)

blocks :: [Block] -> CSS a -> CSS [a]
blocks bs css = for bs $ \b -> block b css

element :: Element -> CSS a -> CSS a
element (Element e) = nested ("__" <> e)

elements :: [Element] -> CSS a -> CSS [a]
elements es css = for es $ \e -> element e css

modifier :: Modifier -> Styles a -> CSS a
modifier (Modifier m) = select ("--" <> m)

modifiers :: [Modifier] -> Styles a -> CSS [a]
modifiers ms css = for ms $ \m -> modifier m css

utility :: Utility -> Styles a -> CSS a
utility (Utility u) = select (".u-" <> u) . important

utilities :: [Utility] -> Styles a -> CSS [a]
utilities us css = for us $ \u -> utility u css

getClass :: Txt -> Maybe Txt
getClass c =
  if Txt.length c > 0 && Txt.head c == '.' then
    Just (Txt.tail c)
  else
    Nothing

pattern Class c <- (getClass -> Just c) where
  Class c = Txt.cons '.' c

getB :: Txt -> Maybe Block
getB t
  | isNil t            = Nothing
  | Txt.isInfixOf "__" t = Nothing
  | Txt.isInfixOf "--" t = Nothing
  | otherwise = Just (Block t)

pattern B :: Block -> Txt
pattern B b <- (getB -> Just b) where
  B (Block b) = b

splitBE :: Txt -> Maybe (Block,Element)
splitBE t
  | Txt.isInfixOf "--" t = Nothing
  | otherwise =
      let (b,em) = Txt.breakOn "__" t
          (e,_) = Txt.breakOn "--" (Txt.drop 2 em)
      in
        if notNil b && notNil e then
          Just (Block b,Element e)
        else
          Nothing

pattern BE :: Block -> Element -> Txt
pattern BE b e <- (splitBE -> Just (b,e)) where
  BE b e = be b e

splitBM :: Txt -> Maybe (Block,Modifier)
splitBM t
  | Txt.isInfixOf "__" t       = Nothing
  | not (Txt.isInfixOf "--" t) = Nothing
  | otherwise =
      let (b,Txt.drop 2 -> m) = Txt.breakOn "--" t
      in if notNil b && notNil m then
          Just (Block b,Modifier m)
        else
          Nothing

pattern BM :: Block -> Modifier -> Txt
pattern BM b m <- (splitBM -> Just (b,m)) where
  BM b m = bm b m

splitBEM :: Txt -> Maybe (Block,Element,Modifier)
splitBEM t
  | not (Txt.isInfixOf "--" t) = Nothing
  | not (Txt.isInfixOf "__" t) = Nothing
  | otherwise =
      let (b,Txt.drop 2 -> em) = Txt.breakOn "__" t
          (e,Txt.drop 2 -> m) = Txt.breakOn "--" em
      in
        if notNil b && notNil e && notNil m then
          Just (Block b,Element e,Modifier m)
        else
          Nothing

pattern BEM :: Block -> Element -> Modifier -> Txt
pattern BEM b e m <- (splitBEM -> Just (b,e,m)) where
  BEM b e m = bem b e m

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

nested :: Txt -> CSS a -> CSS a
nested sel = go
  where
    go (Return r) = Return r
    go (Lift s) = go (runIdentity s)
    go (Do msg) =
      case prj msg of

        Just (CSS_ suf ss rest) ->
          Send (CSS_ (sel <> suf) ss (go . rest))

        Just (CSS3_ at rule css rest) ->
          Send (CSS3_ at rule (go (unsafeCoerce css)) (go . (unsafeCoerce rest)))

and :: Txt -> CSS a -> CSS a
and sel = nested sel

or :: Txt -> CSS a -> CSS a
or sel = nested (", " <> sel)

nest :: Txt -> CSS a -> CSS a
nest sel = nested (" " <> sel)

child :: Txt -> CSS a -> CSS a
child sel = nested (" > " <> sel)

predecessor :: Txt -> CSS a -> CSS a
predecessor sel = nested (" + " <> sel)

successor :: Txt -> CSS a -> CSS a
successor sel = nested (" ~ " <> sel)

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
