{-# language PatternSynonyms, RankNTypes, TypeApplications, AllowAmbiguousTypes, CPP, DefaultSignatures, ScopedTypeVariables, ViewPatterns, RoleAnnotations, ExistentialQuantification, FlexibleContexts, OverloadedStrings, PolyKinds, FlexibleInstances, BangPatterns #-}
module Data.Theme
  ( Theme(..)
  , pattern Themed
  , pattern Customized
  , hasTheme
  , themed
  , themedWith
  , rep
  , subtheme
  , and, and'
  , at, at'
  , nest, nest'
  , Data.Theme.within, within'
  , embed
  , addTheme
  , addThemeClass
  , addSomeThemeClass
  , removeThemeClass
  , removeSomeThemeClass
  , Custom(..)
  , SomeTheme(..)
  , mkSomeTheme
  , someThemed
  , (&)
  , (^.)
  , (^-)
  , (^&)
  , (^>)
  , (^+)
  , (^*)
  , (^^)
  , (^$)
  , (^%)
  , (^#)
  ) where

import Data.View
import Data.View.Build (inject)
import Data.CSS hiding (Namespace,empty,select,wrap,checked)
import Data.DOM as Lifted (JSV,Node,head)
import Data.Styles
import Data.Txt as Txt

import Control.Arrow ((&&&))
import Control.Monad
import Data.Coerce
import Data.Foldable hiding (and,or)
import Data.Function ((&))
import Data.Traversable
import Data.List as List hiding (and,or)
import Data.IORef
import Data.Monoid
import Data.Typeable
import Data.Unique
import System.IO.Unsafe

import Data.Hashable
import Data.Map as Map

import Prelude hiding (and,or,(^^))

{-# NOINLINE activeThemes #-}
activeThemes :: IORef (Map Txt ())
activeThemes = unsafePerformIO $ newIORef Map.empty

class Typeable a => ThemeName a where
  {-# NOINLINE rep #-}
  rep :: Txt
  rep = Txt.filter nonQuote (Txt.intercalate "_" [go (typeRep (Proxy :: Proxy a)), toTxt th])
    where
      nonQuote x = x /= '\'' && x /= '\"'
      th = abs (hash (typeRep (Proxy :: Proxy a)))
      go tr =
        let tc = toTxt (show (typeRepTyCon tr))
            trs = typeRepArgs tr
        in Txt.intercalate "_" (tc : fmap go trs)

instance Typeable a => ThemeName a

newtype Namespace t = Namespace Txt
type role Namespace nominal

class Theme t where
  {-# NOINLINE namespace #-}
  namespace :: Namespace t
  default namespace :: Typeable t => Namespace t
  namespace = Namespace $! rep @t

  theme :: forall t. Txt -> CSS ()
  default theme :: Txt -> CSS ()
  theme _ = return ()

{-# NOINLINE addThemeUnsafe #-}
addThemeUnsafe :: forall t. Theme t => ()
addThemeUnsafe = unsafePerformIO (addTheme @t)

addTheme :: forall t. Theme t => IO ()
addTheme = do
  let 
    Namespace pre = namespace @t
    p = "." <> pre
  tw <- atomicModifyIORef' activeThemes $ \trie ->
          if Map.lookup pre trie == Just ()
            then (trie,True)
            else (Map.insert pre () trie,False)
  unless tw $
    case css (theme @t p) of
      Children cs _ 
        -- Some themes are simply meant to be type-safe tags for other themes.
        -- We try to avoid cluttering <head> with empty style elements. 
        -- This can probably be simplifed to just the first case?
        | [ ReactiveView _ (TextView _ "") ] <- cs -> pure ()
        | [ TextView _ "" ] <- cs                  -> pure ()
        | [] <- cs                                 -> pure ()

      content -> inject Lifted.head (Attribute "data-pure-theme" pre content)

hasTheme :: forall t b. (Theme t) => View -> Bool
hasTheme (Classes cs b) = let Namespace t = namespace @t in t `elem` cs

themedWith :: forall t. (Theme t) => Namespace t -> View -> View
themedWith _ = Themed @t

themed :: forall t. (Theme t) => t -> View -> View
themed _ = Themed @t

pattern Themed :: forall t. (Theme t) => View -> View
pattern Themed b <- (hasTheme @t &&& id -> (True,b)) where
  Themed b =
    let Namespace pre = namespace @t
    in addThemeUnsafe @t `seq` Class pre b

include :: forall a. Theme a => CSS ()
include = theme @a ""

subtheme :: forall t. Theme t => Txt
subtheme = let Namespace t = namespace @t in "." <> t

(^.) :: forall t a. Theme t => CSS a -> CSS ()
(^.) = and @t

(^-) :: forall t a. Theme t => CSS a -> CSS ()
(^-) = isn't (subtheme @t)

(^&) :: forall t a. Theme t => CSS a -> CSS ()
(^&) = at @t

(^>) :: forall t a. Theme t => CSS a -> CSS ()
(^>) = nest @t

(^+) :: forall t a. Theme t => CSS a -> CSS ()
(^+) = next (subtheme @t)

(^*) :: forall t a. Theme t => CSS a -> CSS ()
(^*) = nexts (subtheme @t)

(^^) :: forall t a. Theme t => CSS a -> CSS ()
(^^) = Data.Theme.within @t

(^$) :: Txt -> CSS a -> CSS ()
(^$) = atMedia

(^%) :: Txt -> CSS a -> CSS ()
(^%) = atKeyframes

(^#) :: Txt -> CSS a -> CSS ()
(^#) = atFontFace

and :: forall t a. Theme t => CSS a -> CSS ()
and = void . and' @t

and' :: forall t a. Theme t => CSS a -> CSS a
and' = is' (subtheme @t)

at :: forall t a. Theme t => CSS a -> CSS ()
at = void . at' @t

at' :: forall t a. Theme t => CSS a -> CSS a
at' = is' (subtheme @t)

nest :: forall t a. Theme t => CSS a -> CSS ()
nest = void . nest' @t

nest' :: forall t a. Theme t => CSS a -> CSS a
nest' = child' (subtheme @t)

within :: forall t a. Theme t => CSS a -> CSS ()
within = void . within' @t

within' :: forall t a. Theme t => CSS a -> CSS a
within' block = do
  s <- scope
  rescope (subtheme @t <> " " <> s) block

embed :: forall sub. Theme sub => CSS ()
embed = let Namespace ns = namespace @sub in theme @sub ns

data SomeTheme = forall t. Theme t => SomeTheme (Namespace t)

mkSomeTheme :: forall t. Theme t => SomeTheme
mkSomeTheme = SomeTheme (namespace @t)

someThemed :: SomeTheme -> View -> View
someThemed (SomeTheme ns) = themedWith ns

data Custom a

pattern Customized :: forall t. (Theme t, Theme (Custom t)) => View -> View
pattern Customized b <- (((&&) <$> hasTheme @(Custom t) <*> hasTheme @t) &&& id -> (True,b)) where
  Customized b =
    let Namespace t = namespace @t
        Namespace e = namespace @(Custom t)
    in addThemeUnsafe @t `seq` 
       addThemeUnsafe @(Custom t) `seq` 
       Class e (Class t b)

-- Adding and removing themes is designed to work with multiple theme sources.
-- That is, two components manually adding themes to a node can coexist without
-- removing each other's themes from the node. But, these themes are not managed
-- as themes applied with Themed; it is the user's responsibility to manage when
-- the theme is added and removed.

addSomeThemeClass :: SomeTheme -> Node -> IO ()
addSomeThemeClass st n = 
  case st of
    SomeTheme ns@(Namespace c) -> do
      addThemeFromNamespace ns
      Data.Theme.addClass n c
  where
    addThemeFromNamespace :: forall t. Theme t => Namespace t -> IO ()
    addThemeFromNamespace _ = addTheme @t

removeSomeThemeClass :: SomeTheme -> Node -> IO ()
removeSomeThemeClass (SomeTheme (Namespace c)) n = removeClass n c

addThemeClass :: forall t. Theme t => Node -> IO ()
addThemeClass n = addTheme @t >> Data.Theme.addClass n (Txt.tail (subtheme @t))

removeThemeClass :: forall t. Theme t => Node -> IO ()
removeThemeClass n = removeClass n (Txt.tail (subtheme @t))

#ifdef __GHCJS__
foreign import javascript unsafe
    "if ($1) { $1['data-pure-theme-list'] = ($1['data-pure-theme-list'] || '')['concat'](' ',$2); $1['classList']['add']($2) }" add_class_js :: Node -> Txt -> IO ()
#endif

addClass :: Node -> Txt -> IO ()
addClass n c =
#ifdef __GHCJS__
    add_class_js n c
#else
    return ()
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
    "if ($1) { var l = ($1['data-pure-theme-list'] || '')['trim']()['split'](' '); var idx = l['findIndex'](function (p) { return p === $2; }); if (idx >= 0) { l.splice(idx,1) }; $1['data-pure-theme-list'] = l.join(' '); if (l['findIndex'](function (p) { return p === $2; }) < 0) { $1['classList']['remove']($2); } }" remove_class_js :: Node -> Txt -> IO ()
#endif

removeClass :: Node -> Txt -> IO ()
removeClass n c =
#ifdef __GHCJS__
    remove_class_js n c
#else
    return ()
#endif
