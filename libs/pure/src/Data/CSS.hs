{-# LANGUAGE CPP,TypeSynonymInstances, ScopedTypeVariables, FlexibleInstances, OverloadedStrings, FlexibleContexts, PatternSynonyms, DeriveFunctor, ViewPatterns, RankNTypes, DataKinds, GADTs, CPP, TypeApplications #-}
module Data.CSS where

import Data.View ( View, txt, (<|), (|>), pattern Property, pattern SimpleHTML)
import Ef ( send, Narrative(..) )
import Data.Txt as Txt ( intercalate, replicate, null, toTxt, Txt )

import Data.Functor.Identity ( Identity(runIdentity) )
import Control.Monad ( void )
import Data.Foldable (for_)
import Data.String (IsString(..))
import Data.Traversable (for)

-- NOTE: The current design does not allow for deduplication of style blocks 
--       by way of selector grouping. This implementation is verbose in its
--       produced rules. An intermediate representation would be necessary to
--       achieve deduplication and post-processing, but that would slow down the
--       rendering algorithm. Since this approach is used to produce dynamic
--       styles, it must be reasonably performant. The implementation as-is 
--       is a trade-off that performs reasonably well at being both performant
--       and expressive.
--       
-- TODO: Statically prevent tag selectors nested within class selectors.

data CSS_ k where
  Raw_    :: Txt -> k -> CSS_ k
  Scope_  :: (Txt -> k) -> CSS_ k
  Rescope_ :: Txt -> CSS a -> (a -> k) -> CSS_ k
  Selection_ :: Txt -> CSS a -> (a -> k) -> CSS_ k
  Wrap_   :: Txt -> CSS a -> (a -> k) -> CSS_ k
  Style_  :: Txt -> Txt -> k -> CSS_ k

instance Functor CSS_ where
  fmap f (Raw_ r k) = Raw_ r (f k)
  fmap f (Scope_ g) = Scope_ (f . g)
  fmap f (Rescope_ sel scoped ak) = Rescope_ sel scoped (f . ak)
  fmap f (Selection_ sel scoped ak) = Selection_ sel scoped (f . ak)
  fmap f (Wrap_ rule scoped ak) = Wrap_ rule scoped (f . ak)
  fmap f (Style_ k v a) = Style_ k v (f a)

type CSS = Narrative CSS_ Identity

instance IsString (CSS Txt) where
  fromString = pure . toTxt

-- * Commands

scope :: CSS Txt
scope = send (Scope_ id)

rawCSS :: Txt -> CSS ()
rawCSS r = send (Raw_ r ())

infixr 0 =:
(=:) :: Txt -> Txt -> CSS Txt
(=:) nm val = send (Style_ nm val val)

infixr 0 =*
(=*) :: Txt -> [Txt] -> CSS Txt
(=*) nm (Txt.intercalate " " -> val) = nm =: val

infixr 0 =!
(=!) :: Txt -> Txt -> CSS Txt
(=!) nm val = nm =: (val <> " !important")

infixr 0 =&
(=&) :: Txt -> [Txt] -> CSS Txt
(=&) nm (Txt.intercalate ", " -> val) = nm =: val

-- designed to maintain the old API
important :: CSS a -> CSS a
important (Return a) = Return a
important (Lift l) = Lift (fmap important l)
important (Do msg) =
  case msg of
    Raw_ r k -> Do (Raw_ r k)
    Scope_ g -> Do (Scope_ (fmap important g))
    Selection_ sel scoped ak -> Do (fmap important (Selection_ sel (important scoped) ak))
    Wrap_ rule scoped ak -> Do (fmap important (Wrap_ rule (important scoped) ak))
    Style_ k v a -> Do (fmap important (Style_ k (v <> " !important") a))

-- | The core mechanism used to implement basic CSS selectors. See `is`, `has`,
-- `next`, `child`.
--
-- > select ".btn" do
-- >   ...
--
-- Produces:
--
-- > .btn {
-- >   ...
-- > }
--
select :: Txt -> CSS a -> CSS a
select sel scoped = send (Selection_ sel scoped id)

-- | `rescope` allows for locally modifying the current scope; this combinator
-- can be useful in cases where a utility class must be applied to a parent selector
-- but keeping the parent selector nested within the child selector context makes
-- more sense from a comprehensibility standpoint. 
--
-- Consider this case where a parent that has a `Disabled` class. Without `rescope`,
-- multiple context blocks are required to style with and without the parent's flag.
--
-- > is ".btn"
-- >   ...
-- >
-- > is ".disabled" do
-- >   has ".btn" do
-- >     ...
-- 
-- With rescope, we can write combinators like `within`:
--
-- > -- convert a root `is` selector to a `has` selector and prepend a parent context.
-- > within parent block = do
-- >   s <- scope
-- >   rescope (parent <> " " <> s) block
--
-- and achieve the more desirable:
--
-- > is ".btn" do
-- >   ...
-- >   within ".disabled" do
-- >     ...
-- 
-- Note: While this combinator is exported, it is not used locally. I preferred to
--       create the above `within` in pure-theme.
rescope :: Txt -> CSS a -> CSS a
rescope sel scoped = send (Rescope_ sel scoped id)

-- | `wrap` allows the implementation of @_ queries by re-wrapping
-- the current scope at the root level, making it possible to
-- wrap a scope block with a CSS3 selector:
--
-- > at @SomeClass do
-- >   wrap "@media screen and (min-width: 400px)" do
-- >     ...
--
-- Produces:
--
-- > @media screen and (min-width: 400px) {
-- >   .SomeClass {
-- >     ...
-- >   }
-- > }
wrap :: Txt -> CSS a -> CSS a
wrap rule scoped = send (Wrap_ rule scoped id)

-- | Variablize a keyword.
--
-- The CSS DSL in this module subsumes CSS variables; if possible, use the 
-- variable binding nature of the DSL rather than CSS variables, or simply 
-- import necessary variables from a shared module.
--
-- In general, CSS variables are useful when defining a global color scheme or
-- default fonts, making it easy to change the variable for all instances while 
-- viewing your application in a live environment.
-- 
-- Keep in mind that it is necessary to define variables before they are used.
-- In pure-theme, this can be accomplished by guaranteeing that the variables
-- are defined in an ancestor view. If you're defining a global theme, add a 
-- `:root { }` css block to your application's theme root and define variables
-- there.
--
-- Use is relatively simple, as defvar just prefixes the variable name with `--`.
--
-- > defvar dark =: rgb(33,33,33)
--
-- The variable can then be used as:
--
-- > background-color =: usevar dark
--
defvar :: Txt -> Txt
defvar = ("--" <>)

-- | Use a keyword variable. See `defvar`.
--
-- > color =: usevar dark
--
usevar :: Txt -> Txt
usevar x = "var(--" <> x <> ")"

{-# DEPRECATED apply, (.>), (..>) "apply, (.>), and (..>) are no longer necessary to introduce a styling scope as the styling scope and selection scopes have been merged." #-}
apply :: CSS a -> CSS a
apply = id
infixr 0 .>
(.>) = ($)
infixr 0 ..>
(..>) = ($)


-- * Renderers

stylesheet :: CSS a -> Txt
stylesheet = start
  where
    start :: CSS x -> Txt
    start (Return _) = ""
    start (Lift i) = start (runIdentity i)
    start (Do msg) =
      case msg of
        Raw_ r k -> r <> "\n" <> start k
        
        Scope_ f -> start (f "")
        
        -- Couldn't figure a way to disallow this case; it is an artifact of 
        -- the unification of the css and style DSLs.
        Style_ _ _ k -> start k

        Selection_ sel scoped k -> 
          let (res,a) = selecting 0 sel "" "" scoped 
           in res <> start (k a)

        Rescope_ sel scoped k -> 
          let (res,a) = selecting 0 sel "" "" scoped
          in res <> start (k a)

        Wrap_ rule scoped k ->
          let (res,a) = selecting 1 "" "" "" scoped
           in rule <> " {\n" <> res <> "}\n\n" <> start (k a)


    selecting :: Int -> Txt -> Txt -> Txt -> CSS a -> (Txt,a)
    selecting depth sel acc rest (Return a) = 
      case (Txt.null acc,Txt.null rest) of
        (True,True) -> (mempty,a)
        (True,_   ) -> (rest,a)
        _           -> (Txt.replicate depth "\t" <> sel <> " {\n" <> acc <> Txt.replicate depth "\t" <> "}\n\n" <> rest,a)
    selecting depth sel acc rest (Lift l) = selecting depth sel acc rest (runIdentity l)
    selecting depth sel acc rest (Do msg) =
      case msg of
        Scope_ f -> selecting depth sel acc rest (f sel)
        Raw_ r k -> selecting depth sel acc (rest <> r <> "\n") k
        Style_ k v cont -> selecting depth sel (acc <> (Txt.replicate (depth + 1) "\t") <> k <> ": " <> v <> ";\n") rest cont
        Rescope_ sel' scoped k ->
          let (res,a) = selecting depth sel' "" "" scoped
          in selecting depth sel acc (rest <> res) (k a)
        Selection_ sel' scoped k -> 
          let (res,a) = selecting depth (sel <> sel') "" "" scoped
           in selecting depth sel acc (rest <> res) (k a)
        Wrap_ rule scoped k ->
          let (res,a) = selecting (depth + 1) sel "" "" scoped
           in selecting depth sel acc (rest <> Txt.replicate depth "\t" <> rule <> " {\n" <> res <> Txt.replicate depth "\t" <> "}\n\n" ) (k a)

css :: CSS a -> View
css = css' False

css' :: Bool -> CSS a -> View
css' scoped sheet = 
  SimpleHTML "style" <| Property "type" "text/css" . Property "scoped" (if scoped then "true" else "") |>
    [ txt (stylesheet sheet) ]

is :: Txt -> CSS a -> CSS ()
is s c = void (select s c)

is' :: Txt -> CSS a -> CSS a
is' = select

-- This might not do what you expect!
--
-- > is ".btn" do
-- >   is ".warn" do 
-- >     or is ".bold" $ do
-- >       font-weight =: 700
--
-- Produces:
--
-- .btn.warn, .bold {
--     font-weight: 700;
-- }
-- 
-- `or` simply inserts a comma before the supplied selector, it doesn't
-- do any re-scoping.
-- 
-- Instead, what you may be wanting is this:
--
-- > let bolded = font-weight =: 700
-- > is ".btn" do
-- >   is ".warn" bolded
-- >   is ".bold" bolded
--
-- Or, use the `using` combinator:
--
-- > is ".btn" do
-- >   using [is ".warn",is ".bold"] do
-- >     font-weight =: 700
--
-- If you're choosing this combinator for performance reasons, you're
-- optimizing the wrong thing! 
or :: (Txt -> CSS a -> CSS b) -> Txt -> CSS a -> CSS ()
or f sel block = void (or' f sel block)

or' :: (Txt -> CSS a -> CSS b) -> Txt -> CSS a -> CSS b
or' f sel block = is' ", " (f sel block)

-- Achieve a similar effect to `or`, but instead of creating a 
-- CSS selector alternative, it simply duplicates the rule block
-- for each selector.
-- 
-- Compare:
--
-- > is ".btn" . is ".warn" . or is ".btn" . is ".bold" $ do
-- >   font-weight =: 700
-- 
-- > is ".btn" do
-- >   using [is ".warn",is ".bold"] do
-- >     font-weight =: 700
-- 
-- They achieve the same result, but the first produces a more compact result.
--
-- The first produces:
--
-- .btn.warn, .btn.bold {
--     font-weight: 700;
-- }
-- 
-- The second produces:
-- 
-- .btn.warn {
--     font-weight: 700;
-- }
-- .btn.bold {
--     font-weight: 700;
-- }
-- 
-- Choose whichever you find easier to read. Personally, for such a small
-- set of rules, I choose the approach I find to be most readable:
--
-- > is ".btn" do
-- >
-- >   is ".warn" do
-- >     font-weight =: 700
-- >
-- >   is ".bold" do
-- >     font-weight =: 700
-- 
-- As a bonus, this simple approach allows for easy extension of each of
-- the separate blocks. And sharing styles can be done by let-lifting.
-- 
-- using :: [CSS a -> CSS b] -> CSS a -> CSS ()
-- using = use

isn't :: Txt -> CSS a -> CSS ()
isn't sel = void . isn't' sel

isn't' :: Txt -> CSS a -> CSS a
isn't' sel = select (":not(" <> sel <> ")")

lang :: Txt -> CSS a -> CSS ()
lang sel = void . lang' sel

lang' :: Txt -> CSS a -> CSS a
lang' sel = select (":lang(" <> sel <> ")")

nthChild :: Txt -> CSS a -> CSS ()
nthChild i = void . nthChild' i

nthChild' :: Txt -> CSS a -> CSS a
nthChild' i = select (":nth-child(" <> i <> ")")

nthLastChild :: Txt -> CSS a -> CSS ()
nthLastChild i = void . nthLastChild' i

nthLastChild' :: Txt -> CSS a -> CSS a
nthLastChild' i = select (":nth-last-child(" <> i <> ")")

nthOfType :: Txt -> CSS a -> CSS ()
nthOfType i = void . nthOfType' i

nthOfType' :: Txt -> CSS a -> CSS a
nthOfType' i = select (":nth-of-type(" <> i <> ")")

nthLastOfType :: Txt -> CSS a -> CSS ()
nthLastOfType i = void . nthLastOfType' i

nthLastOfType' :: Txt -> CSS a -> CSS a
nthLastOfType' i = select (":nth-last-of-type(" <> i <> ")")

-- Conveniently compose some combinators:
--
-- > use [or ".bold", or ".warn"] do
-- >   font-weight =: 700
--
-- Wasn't sure of a good name for this one.
use :: (Traversable t, Applicative f) => t (a -> f b) -> a -> f ()
use fs x = for_ fs ($ x)

pseudo :: Txt -> CSS a -> CSS ()
pseudo sel = void . pseudo' sel

pseudo' :: Txt -> CSS a -> CSS a
pseudo' sel = select (":" <> sel)

attr :: Txt -> CSS a -> CSS ()
attr sel = void . attr' sel

attr' :: Txt -> CSS a -> CSS a
attr' sel = select ("[" <> sel <> "]")

child :: Txt -> CSS a -> CSS ()
child sel = void . child' sel

child' :: Txt -> CSS a -> CSS a
child' sel = select (" > " <> sel)

has :: Txt -> CSS a -> CSS ()
has sel = void . has' sel

has' :: Txt -> CSS a -> CSS a
has' sel = select (" " <> sel)

next :: Txt -> CSS a -> CSS ()
next sel = void . next' sel

next' :: Txt -> CSS a -> CSS a
next' sel = select (" + " <> sel)

nexts :: Txt -> CSS a -> CSS ()
nexts sel = void . nexts' sel

nexts' :: Txt -> CSS a -> CSS a
nexts' sel = select (" ~ " <> sel)

atCharset :: Txt -> CSS ()
atCharset cs = rawCSS ("@charset " <> cs)

utf8Charset :: CSS ()
utf8Charset = atCharset "\"UTF-8\";"

iso885915Charset :: CSS ()
iso885915Charset = atCharset "\"iso-8859-15\";"

atImport :: Txt -> CSS ()
atImport i = wrap ("@import " <> i) (pure ())

any :: CSS a -> CSS a
any = is' "*"

active :: CSS a -> CSS a
active = is' ":active"

visited :: CSS a -> CSS a
visited = is' ":visited"

hover :: CSS a -> CSS a
hover = is' ":hover"

focus :: CSS a -> CSS a
focus = is' ":focus"

disabled :: CSS a -> CSS a
disabled = is' ":disabled"

link :: CSS a -> CSS a
link = is' ":link"

empty :: CSS a -> CSS a
empty = is' ":empty"

checked :: CSS a -> CSS a
checked = is' ":checked"

enabled :: CSS a -> CSS a
enabled = is' ":enabled"

firstChild :: CSS a -> CSS a
firstChild = is' ":first-child"

firstOfType :: CSS a -> CSS a
firstOfType = is' ":first-of-type"

inRange :: CSS a -> CSS a
inRange = is' ":in-range"

invalid :: CSS a -> CSS a
invalid = is' ":invalid"

lastChild :: CSS a -> CSS a
lastChild = is' ":last-child"

onlyOfType :: CSS a -> CSS a
onlyOfType = is' ":only-of-type"

onlyChild :: CSS a -> CSS a
onlyChild = is' ":only-child"

optional :: CSS a -> CSS a
optional = is' ":optional"

outOfRange :: CSS a -> CSS a
outOfRange = is' ":out-of-range"

readOnly :: CSS a -> CSS a
readOnly = is' ":read-only"

readWrite :: CSS a -> CSS a
readWrite = is' ":read-write"

required :: CSS a -> CSS a
required = is' ":required"

root :: CSS a -> CSS a
root = is' ":root"

target :: CSS a -> CSS a
target = is' ":target"

valid :: CSS a -> CSS a
valid = is' ":valid"

before :: CSS a -> CSS a
before = is' "::before"

after :: CSS a -> CSS a
after = is' "::after"

firstLetter :: CSS a -> CSS a
firstLetter = is' "::first-letter"

firstLine :: CSS a -> CSS a
firstLine = is' "::first-line"

selection :: CSS a -> CSS a
selection = is' "::selection"

atMedia :: Txt -> CSS a -> CSS ()
atMedia med = void . atMedia' med

atMedia' :: Txt -> CSS a -> CSS a
atMedia' med = wrap ("@media " <> med)

atPage :: Txt -> CSS a -> CSS ()
atPage pg = void . atPage' pg

atPage' :: Txt -> CSS a -> CSS a
atPage' pg = wrap ("@page " <> pg)

atFontFace :: Txt -> CSS a -> CSS ()
atFontFace ff = void . atFontFace' ff

atFontFace' :: Txt -> CSS a -> CSS a
atFontFace' ff = wrap ("@font-face " <> ff)

atKeyframes :: Txt -> CSS a -> CSS ()
atKeyframes nm = void . atKeyframes' nm

atKeyframes' :: Txt -> CSS a -> CSS a
atKeyframes' nm = wrap ("@keyframes " <> nm)

data Namespace = XHTMLNS | SVGNS

atNamespace :: Namespace -> Maybe Txt -> CSS ()
atNamespace ns mnsv = wrap (namespace_ <> ns_) (pure ())
  where
    ns_ =
      case ns of
        XHTMLNS -> "url(http://www.w3.org/1999/xhtml)"
        SVGNS   -> "url(http://www.w3.org/2000/svg)"

    namespace_ = 
      let n = "@namespace " in maybe n (n <>) mnsv

