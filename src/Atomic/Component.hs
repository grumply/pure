{-# language UndecidableInstances #-}
{-# language FunctionalDependencies #-}
{-# language DeriveDataTypeable #-}
{-# language StandaloneDeriving #-}
{-# language OverloadedStrings #-}
{-# language PatternSynonyms #-}
{-# language TemplateHaskell #-}
{-# language DeriveFunctor #-}
{-# language ViewPatterns #-}
{-# language MagicHash #-}
{-# language CPP #-}
module Atomic.Component (module Atomic.Component, ENode, TNode, NNode, Win, Doc, Loc) where

import Ef.Base hiding (Object,Client,After,Before,current,Lazy,Eager,construct,Index,observe,uncons,distribute,embed)
import qualified Ef.Base

import Data.Txt as Txt hiding (replace,map,head,filter)
import Data.JSON

import Atomic.Attribute
import Atomic.Cond
import Atomic.CSS
import Atomic.Default
-- import Atomic.Dict
import Atomic.Key
import Atomic.Vault
import Atomic.ToTxt
import Atomic.FromTxt
import Atomic.Observable
import Atomic.UnsafeEq

#ifdef __GHCJS__
import qualified GHCJS.Types as T
import qualified GHCJS.Marshal as M
import qualified GHCJS.Marshal.Pure as M

import GHCJS.Foreign.Callback

import qualified JavaScript.Object.Internal as O

import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.Element as E
import qualified GHCJS.DOM.EventM as Ev
import qualified GHCJS.DOM.EventTargetClosures as Ev
import qualified GHCJS.DOM.Document as D
import qualified GHCJS.DOM.History as H
import qualified GHCJS.DOM.Location as L
import qualified GHCJS.DOM.Node as N
import qualified GHCJS.DOM.NodeList as N
import qualified GHCJS.DOM.Types as T
import qualified GHCJS.DOM.Window as W

import GHCJS.DOM.RequestAnimationFrameCallback
import GHCJS.DOM.Window (requestAnimationFrame)
#else
import Data.Aeson (Value(..))
#endif

import Control.Concurrent
import Data.Bifunctor
import Data.Char
import Data.Data hiding (Constr)
import Data.Either
import Data.Foldable
import Data.Functor.Identity
import Data.Hashable
import Data.IORef
import Data.List as List hiding (delete,head)
import Data.Maybe
import Data.String
import Data.Traversable
import Data.Typeable
import Data.Void
import Data.Unique
import GHC.Prim
import System.Mem.StableName

import qualified Data.HashMap.Strict as Map

import Prelude hiding (div,head,span)
import Language.Haskell.TH hiding (Loc)
import Language.Haskell.TH.Syntax hiding (Loc)

import System.IO.Unsafe
import Unsafe.Coerce

import Control.Lens (Iso,iso,makePrisms,makeLenses,preview,review)
import Control.Lens.Plated (Plated(..))
import Control.Lens.At
import Control.Lens.Prism
import Control.Lens.Setter hiding ((.=))

import qualified GHC.Exts

#ifdef __GHCJS__
foreign import javascript unsafe
  "$r = $2.parentNode == $1;"
  is_already_embedded_js :: E.Element -> E.Element -> IO Bool

foreign import javascript unsafe
  "$r = $1.parentNode == $2;"
  is_already_embedded_text_js :: T.Text -> E.Element -> IO Bool

foreign import javascript unsafe
  "$1.parentNode.replaceChild($2,$1);"
  swap_js :: N.Node -> N.Node -> IO ()

foreign import javascript unsafe
  "$1.insertBefore($3,$1.childNodes[$2]);"
  insert_at_js :: E.Element -> Int -> N.Node -> IO ()

foreign import javascript unsafe
  "$1[\"parentNode\"][\"replaceChild\"]($2,$1);"
  swap_content_js :: T.Text -> E.Element -> IO ()

foreign import javascript unsafe
  "$1.nodeValue=$2;"
  changeText_js :: T.Text -> Txt -> IO ()

foreign import javascript unsafe
  "for (var property in $2) { $1.style[property] = $2[property]; }"
  setStyle_js :: E.Element -> O.Object -> IO ()

foreign import javascript unsafe
  "$1[$2] = null;"
  set_property_null_js :: O.Object -> Txt -> IO ()

foreign import javascript unsafe
  "$1[$2] = null;"
  set_element_property_null_js :: E.Element -> Txt -> IO ()

foreign import javascript unsafe
  "for (var property in $2) { $1.style[property] = null; }"
  clearStyle_js :: E.Element -> O.Object -> IO ()

foreign import javascript unsafe
  "$1.remove();"
  delete_js :: N.Node -> IO ()

foreign import javascript unsafe
  "var pse = new PopStateEvent('popstate',{state: 0});dispatchEvent(pse);"
  triggerPopstate_js :: IO ()

foreign import javascript unsafe
  "$1.value = $2;"
  set_value_js :: E.Element -> Txt -> IO ()

foreign import javascript unsafe
  "$1.appendChild($2);"
  append_child_js :: E.Element -> N.Node -> IO ()

foreign import javascript unsafe
  "$1.innerHTML = '';" clear_node_js :: N.Node -> IO ()

foreign import javascript unsafe
  "$1[$2] = $3" set_property_js :: E.Element -> Txt -> Txt -> IO ()
#endif

data HTML (e :: [* -> *]) where
  -- NullHTML must have a presence on the page for proper diffing
  NullHTML
    :: { _node :: !(Maybe ENode)
       } -> HTML e

  Text
    ::  { _tnode      :: !(Maybe TNode)
        , _content    :: !Txt
        } -> HTML e

  Raw
    :: { _node        :: !(Maybe ENode)
       , _tag         :: !Txt
       , _attributes  :: ![Feature e]
       , _content     :: !Txt
       } -> HTML e

  HTML
    ::  { _node       :: !(Maybe ENode)
        , _tag        :: !Txt
        , _attributes :: ![Feature e]
        , _atoms      :: ![Atom e]
        } -> HTML e
  KHTML
    ::  { _node       :: !(Maybe ENode)
        , _tag        :: !Txt
        , _attributes :: ![Feature e]
        , _keyed      :: ![(Int,Atom e)]
        } -> HTML e

  STHTML
    :: Component Atom x =>
       { _stmodel  :: !(model)
       , _stid     :: !Int
       , _ststate  :: !st
       , _strecord :: !(Maybe (IORef (st,st -> ((st -> st) -> IO () -> IO ()) -> Atom x,HTML x,Atom x)))
       , _stview   :: !(st -> ((st -> st) -> IO () -> IO ()) -> Atom x)
       , _stupdate :: !((st -> st) -> IO () -> IO ())
       } -> HTML e

  -- TODO: SVG keyed node
  SVGHTML
    ::  { _node       :: !(Maybe ENode)
        , _tag        :: !Txt
        , _attributes :: ![Feature e]
        , _atoms      :: ![Atom e]
        } -> HTML e

  KSVGHTML
    ::  { _node       :: !(Maybe ENode)
        , _tag        :: !Txt
        , _attributes :: ![Feature e]
        , _keyed      :: ![(Int,Atom e)]
        } -> HTML e

  Managed
    ::  { _node       :: !(Maybe ENode)
        , _tag        :: !Txt
        , _attributes :: ![Feature e]
        , _constr     :: !Constr
        } -> HTML e

mapComponent :: (Component a ms, Component a' ms) => (a ms -> a' ms) -> Atom ms -> Atom ms
mapComponent f sa =
  case fromAtom sa of
    Just a  -> toAtom (f a)
    Nothing -> sa

mappedComponent :: (Component a ms, Component a' ms) => Setter (Atom ms) (Atom ms) (a ms) (a' ms)
mappedComponent = sets mapComponent

component :: Component a ms => Prism' (Atom ms) (a ms)
component = prism' toAtom fromAtom

pattern Match a <- (preview component -> Just a) where
  Match a = review component a

data Mapping ms = forall a a'. (Component a ms, Component a' ms) => Mapping (a ms -> a' ms)

maps :: Atom ms -> [Mapping ms] -> Atom ms
maps a mappings = Prelude.foldr tryMap a mappings
  where
    tryMap (Mapping m) res =
      case fromAtom a of
        Just a -> toAtom $ m a
        Nothing -> res

data Atom (ms :: [* -> *]) = forall a. (Typeable (a ms), Typeable ms, Component a ms) => Atom (a ms)

class (Typeable a, Typeable ms) => Component (a :: [* -> *] -> *) ms where
  toAtom :: a ms -> Atom ms
  toAtom = Atom
  fromAtom :: Atom ms -> Maybe (a ms)
  fromAtom (Atom a) = cast a
  construct :: a ms -> HTML ms

instance Typeable ms => Component Atom ms where
  toAtom = id
  fromAtom = Just
  construct (Atom a) = construct a

instance Typeable ms => Component HTML ms where
  construct = id

instance Typeable ms => Default (Atom ms) where
  def = toAtom (nil :: HTML ms)

instance Typeable ms => Cond (Atom ms) where
  nil = toAtom (nil :: HTML ms)

html :: Typeable ms => Atom ms -> HTML ms
html = fromMaybe nil . fromAtom

-- instance Plated (HTML e) where
--   plate f (STHTML _ _ st iorec v _) =
--     case iorec of
--       Nothing -> plate f $ v st (\_ _ -> return ())
--       Just ref -> plate f $ v ((\(a,_,_,_) -> a) $ unsafePerformIO (readIORef ref)) (\_ _ -> return ())
--   plate f (KHTML n t as ks) = KHTML n t as <$> traverse (\(i,k) -> fmap (\k' -> (i,k')) (f k)) ks
--   plate f (HTML n t as cs) = HTML n t as <$> traverse f cs
--   plate f (SVGHTML n t as cs) = SVGHTML n t as <$> traverse f cs
--   plate f (KSVGHTML n t as ks) = KSVGHTML n t as <$> traverse (\(i,k) -> fmap (\k' -> (i,k')) (f k)) ks
--   plate _ a = pure a -- ? :(

-- type instance Index (HTML e) = Int
-- type instance IxValue (HTML e) = HTML e

-- instance Ixed (HTML e) where
--   ix k f (STHTML _ _ st iorec v _) =
--     case iorec of
--       Nothing -> ix k f $ v st (\_ _ -> return ())
--       Just ref -> ix k f $ v ((\(a,_,_,_) -> a) $ unsafePerformIO (readIORef ref)) (\_ _ -> return ())
--   ix k f (HTML n t as cs) = HTML n t as <$> ix k f cs
--   ix k f (KHTML n t as ks) = KHTML n t as <$> go ks k
--     where
--       go [] _ = pure []
--       go ((i,a):ias) 0 = fmap (:ias) (fmap (\a' -> (i,a')) (f a))
--       go (ia:ias) n = (ia:) <$> (go ias $! n - 1)
--   ix k f (SVGHTML n t as cs) = SVGHTML n t as <$> ix k f cs
--   ix k f (KSVGHTML n t as ks) = KSVGHTML n t as <$> go ks k
--     where
--       go [] _ = pure []
--       go ((i,a):ias) 0 = fmap (:ias) (fmap (\a' -> (i,a')) (f a))
--       go (ia:ias) n = (ia:) <$> (go ias $! n - 1)
--   ix k f a = f a -- ? :(

instance Typeable e => ToJSON (HTML e) where
  toJSON a =
#ifdef __GHCJS__
    objectValue $
#endif
      go a
    where
      go (STHTML _ _ st iorec v _) =
        case iorec of
          Nothing -> go $ construct $ (unsafeCoerce :: forall x. x -> Atom e) $ v st (\_ _ -> return ())
          Just ref -> go $ construct $ (unsafeCoerce :: forall x. x -> Atom e) $ v ((\(a,_,_,_) -> a) $ unsafePerformIO (readIORef ref)) (\_ _ -> return ())
      go (Text _ c) = object [ "type" .= ("text" :: Txt), "content" .= c]
      go (Raw _ t as c) = object [ "type" .= ("raw" :: Txt), "tag" .= t, "attrs" .= toJSON as, "content" .= c ]
      go (KHTML _ t as ks) = object [ "type" .= ("keyed" :: Txt), "tag" .= t, "attrs" .= toJSON as, "keyed" .= toJSON (map (fmap construct) ks) ]
      go (HTML _ t as cs) = object [ "type" .= ("atom" :: Txt), "tag" .= t, "attrs" .= toJSON as, "children" .= toJSON (map construct cs) ]
      go (KSVGHTML _ t as ks) = object [ "type" .= ("keyedsvg" :: Txt), "tag" .= t, "attrs" .= toJSON as, "keyed" .= toJSON (map (fmap construct) ks)]
      go (SVGHTML _ t as cs) = object [ "type" .= ("svg" :: Txt), "tag" .= t, "attrs" .= toJSON as, "children" .= toJSON (map construct cs) ]
      go _ = object [ "type" .= ("null" :: Txt) ]

instance Typeable e => FromJSON (HTML e) where
  parseJSON o0 = do
#ifdef __GHCJS__
    flip (withObject "obj") o0 $ \o -> do
#else
      let (Object o) = o0
#endif
      t <- o .: "type"
      case t :: Txt of
        "text" -> do
          c <- o .: "content"
          pure $ Text Nothing c
        "raw" -> do
          t <- o .: "tag"
          as <- o .: "attrs"
          c <- o .: "content"
          pure $ Raw Nothing t as c
        "keyed" -> do
          t <- o .: "tag"
          as <- o .: "attrs"
          ks <- o .: "keyed"
          pure $ KHTML Nothing t as (map (fmap (toAtom :: HTML e -> Atom e)) ks)
        "atom" -> do
          t <- o .: "tag"
          as <- o .: "attrs"
          cs <- o .: "children"
          pure $ HTML Nothing t as (map (toAtom :: HTML e -> Atom e) cs)
        "keyedsvg" -> do
          t <- o .: "tag"
          as <- o .: "attrs"
          ks <- o .: "keyed"
          pure $ KSVGHTML Nothing t as (map (fmap (toAtom :: HTML e -> Atom e)) ks)
        "svg" -> do
          t <- o .: "tag"
          as <- o .: "attrs"
          cs <- o .: "children"
          pure $ SVGHTML Nothing t as (map (toAtom :: HTML e -> Atom e) cs)
        "null" -> pure $ NullHTML Nothing
        _ -> Ef.Base.empty

instance Eq (HTML e) where
  (==) (NullHTML _) (NullHTML _) =
    True

  (==) (Text _ t) (Text _ t') =
    prettyUnsafeEq t t'

  (==) (Raw _ t fs c) (Raw _ t' fs' c') =
    prettyUnsafeEq t t' && prettyUnsafeEq fs fs' && prettyUnsafeEq c c'

  (==) (KHTML _ t fs ks) (KHTML _ t' fs' ks') =
    prettyUnsafeEq t t' && prettyUnsafeEq fs fs' && reallyUnsafeEq ks ks'

  (==) (HTML _ t fs cs) (HTML _ t' fs' cs') =
    prettyUnsafeEq t t' && prettyUnsafeEq fs fs' && reallyUnsafeEq cs cs'

  (==) (STHTML m k st _ v _) (STHTML m' k' st' _ v' _) =
    k == k' && reallyVeryUnsafeEq m m' && reallyVeryUnsafeEq st st' && reallyVeryUnsafeEq v v'

  (==) (KSVGHTML _ t fs ks) (KSVGHTML _ t' fs' ks') =
    prettyUnsafeEq t t' && prettyUnsafeEq fs fs' && reallyUnsafeEq ks ks'

  (==) (SVGHTML _ t fs cs) (SVGHTML _ t' fs' cs') =
    prettyUnsafeEq t t' && prettyUnsafeEq fs fs' && reallyUnsafeEq cs cs'

  (==) (Managed _ t fs c) (Managed _ t' fs' c') =
    prettyUnsafeEq t t' && prettyUnsafeEq fs fs' && prettyUnsafeEq c c'

  (==) _ _ =
    False

instance Cond (HTML e) where
  nil = NullHTML Nothing

instance Typeable e => IsString (Atom e) where
  fromString = text . fromString

instance Typeable e => FromTxt (Atom e) where
  fromTxt = text

instance {-# OVERLAPS #-} Typeable e => IsString [Atom e] where
  fromString s = [fromString s]

instance Typeable e => FromTxt [Atom e] where
  fromTxt t = [fromTxt t]

-- _atom :: Typeable e => ([Feature e] -> [Atom e] -> Atom e) -> Prism (Atom e) (Atom e) ([Feature e],[Atom e]) ([Feature e],[Atom e])
-- _atom x = prism (uncurry x) $ \a ->
--   case fromAtom $ x [] [] of
--     Just (HTML _ t _ _) ->
--       case a of
--         HTML _ t' fs as ->
--           if t == t' then
--             Right (fs,as)
--           else
--             Left a
--         _ -> Left a
--     _ -> Left a

-- _svg :: Typeable e => ([Feature e] -> [Atom e] -> Atom e) -> Prism (Atom e) (Atom e) ([Feature e],[Atom e]) ([Feature e],[Atom e])
-- _svg x = prism (uncurry x) $ \a ->
--   case fromAtom $ x [] [] of
--     Just (SVGHTML _ t _ _) ->
--       case a of
--         SVGHTML _ t' fs as ->
--           if t == t' then
--             Right (fs,as)
--           else
--             Left a
--         _ -> Left a
--     _ -> Left a

-- _list :: Typeable e => ([Feature e] -> [(Int,Atom e)] -> Atom e) -> Prism (Atom e) (Atom e) ([Feature e],[(Int,Atom e)]) ([Feature e],[(Int,Atom e)])
-- _list x = prism (uncurry x) $ \a ->
--   case fromAtom $ x [] [] of
--     Just (KHTML _ t _ _) ->
--       case a of
--         KHTML _ t' fs ks ->
--           if t == t' then
--             Right (fs,ks)
--           else
--             Left a
--         _ -> Left a
--     _ -> Left a

-- _svgList :: Typeable e => ([Feature e] -> [(Int,Atom e)] -> Atom e) -> Prism (Atom e) (Atom e) ([Feature e],[(Int,Atom e)]) ([Feature e],[(Int,Atom e)])
-- _svgList x = prism (uncurry x) $ \a ->
--   case fromAtom $ x [] [] of
--     Just (KSVGHTML _ t _ _) ->
--       case a of
--         KSVGHTML _ t' fs as ->
--           if t == t' then
--             Right (fs,as)
--           else
--             Left a
--         _ -> Left a
--     _ -> Left a

-- _raw :: Typeable e => ([Feature e] -> [Atom e] -> Atom e) -> Prism (Atom e) (Atom e) ([Feature e],Txt) ([Feature e],Txt)
-- _raw x = prism (uncurry (raw x)) $ \a ->
--   case fromAtom $ x [] [] of
--     Just (Raw _ t _ _) ->
--       case a of
--         Raw _ t' fs c ->
--           if t == t' then
--             Right (fs,c)
--           else
--             Left a
--         _ -> Left a
--     _ -> Left a

-- _nil :: Typeable e => Prism (Atom e) (Atom e) () ()
-- _nil = prism (const (NullHTML Nothing)) $ \a ->
--   case fromAtom a of
--     Just (NullHTML _) -> Right ()
--     _ -> Left a

-- _text :: Typeable e => Prism (Atom e) (Atom e) Txt Txt
-- _text = prism text $ \a ->
--   case fromAtom a of
--     Just (Text _ t) -> Right t
--     _ -> Left a

-- -- TODO: _st

-- _context :: Typeable e => ([Feature e] -> [Atom e] -> Atom e) -> Prism (Atom e) (Atom e) ([Feature e],Constr) ([Feature e],Constr)
-- _context x = prism build $ \a ->
--   case fromAtom $ context x [] (undefined :: Context '[] ()) of
--     Just (Managed _ t _ _) ->
--       case a of
--         Managed _ t' fs c ->
--           if t == t' then
--             Right (fs,c)
--           else
--             Left a
--     _ -> Left a
--   where
--     build (fs,c) =
--       case context x [] (undefined :: Context '[] ()) of
--         Managed _ t _ _ ->
--           Managed Nothing t fs c

mkAtom :: Typeable e => Txt -> [Feature e] -> [Atom e] -> Atom e
mkAtom _tag _attributes _atoms =
  let _node = Nothing
  in toAtom HTML {..}

mkSVGAtom :: Typeable e => Txt -> [Feature e] -> [Atom e] -> Atom e
mkSVGAtom _tag _attributes _atoms =
  let _node = Nothing
  in toAtom SVGHTML {..}

text :: Typeable e => Txt -> Atom e
text _content =
  let _tnode = Nothing
  in toAtom Text {..}

raw :: Typeable e => ([Feature e] -> [Atom e] -> Atom e) -> [Feature e] -> Txt -> Atom e
raw x _attributes _content =
  case fromAtom $ x [] [] of
    Just (HTML _ _tag _ _) ->
      let _node = Nothing
      in toAtom Raw {..}
    Just (SVGHTML _ _tag _ _) ->
      let _node = Nothing
      in toAtom Raw {..}
    _ -> error "HTMLic.Context.raw: raw atoms may only be built from HTMLs and SVGHTMLs"

list :: Typeable e => ([Feature e] -> [Atom e] -> Atom e) -> [Feature e] -> [(Int,Atom e)] -> Atom e
list x _attributes _keyed =
  case fromAtom $ x [] [] of
    Just (HTML _ _tag _ _) ->
      let
        _node = Nothing
      in
        toAtom KHTML {..}
    Just (SVGHTML _ _tag _ _) ->
      let
        _node = Nothing
      in
        toAtom KSVGHTML {..}
    _ -> error "HTMLic.Context.list: lists may only be built from HTMLs and SVGHTMLs"

-- The hacks used to implement this atom type are somewhat finicky. The model tracks variables
-- for changes; if any of the variables within the model are updated or moved, a diff will be
-- performed. This is how changes external to an `st` are injected into an `st`; if an `st` uses
-- state from a `Context`s model and they aren't tracked via the `st` model, those changes will
-- not be updated when the `Context`s model updates. The same rules apply to nested/inheriting
-- `st` atoms.
--
-- The major purposes for this atome type are:
--   1. The implementation of extensible and highly configurable UI frameworks with the
--      aim of reducing configuration burdens by avoiding carrying framework state within
--      user-created contexts.
--   2. Stateful subviews a la form inputs, local animations, etc....
--
-- Major caveat: If the list of elements holding a st atom changes such that the diff algorithm
--               must recreate the st element, it will be reset to its original state. This would
--               happen if the position of the st element within the list changes. If a variable
--               length list of children is required, either careful placement for the st element,
--               or the use of NullHTMLs as placeholders, or some uses of keyed atoms can overcome
--               this problem.
st :: forall model st e. Typeable e => Int -> model -> st -> (st -> ((st -> st) -> IO () -> IO ()) -> Atom e) -> Atom e
st k watch_model initial_st view = toAtom $ STHTML watch_model k initial_st Nothing view (\_ _ -> return ())

constant :: Typeable e => Atom e -> Atom e
constant a = st 0 () () $ \_ _ -> a

context :: (Typeable e)
          => ([Feature e] -> [Atom e] -> Atom e)
          -> (forall ts' ms' m. (Typeable ms', IsContext' ts' ms' m) => [Feature e] -> Context' ts' ms' m -> Atom e)
context f = \as c ->
  case fromAtom $ f [] [] of
    Just (HTML _ t _ _) -> toAtom $ Managed Nothing t as (Context' c)
    _ -> error "Incorrect usage of construct; Contexts may only be embedded in plain html HTMLs."

hashed :: Typeable e => Hashable a => ([Feature e] -> [Atom e] -> Atom e) -> [Feature e] -> [(a,Atom e)] -> Atom e
hashed x _attributes _keyed0 = list x _attributes (map (first hash) _keyed0)

css :: Typeable e => CSS -> Atom e
css = css' False

css' :: forall e. Typeable e => Bool -> CSS -> Atom e
css' b = mkAtom "style" [ Property "type" "text/css", Property "scoped" (if b then "true" else "") ] . ((text "\n"):) . go False
  where
    go :: Bool -> CSS -> [Atom e]
    go b (Return _) = []
    go b (Lift s) = go b (runIdentity s)
    go b c@(Do msg) =
      case prj msg of
        Just (CSS3_ atRule sel mCSS k) ->
          case mCSS of
            Nothing ->
              text (atRule <> sel <> ";\n")
              : go False k
            Just c' ->
              ( text (atRule <> sel <> " {\n")
              : go True c'
              ) ++ ( text "\n}\n\n"
                   : go False k
                   )
        Just (CSS_ sel ss r) ->
          ( text ( (if b then "\t" else mempty)
                     <> sel
                     <> " {\n"
                     <> (Txt.intercalate (if b then ";\n\t" else ";\n") $ renderStyles b ss)
                     <> (if b then "\n\t}\n\n" else "\n}\n\n")
                )
          : go b r
          )
        _ -> []

scss :: Typeable e => StaticCSS -> Atom e
scss = scss' False

scss' :: Typeable e => Bool -> StaticCSS -> Atom e
scss' b = raw (mkAtom "style") [ Property "type" "text/css", Property "scoped" (if b then "true" else "") ] . cssText

styles :: Typeable e => CSS -> Atom e
styles = css' True . classify
  where
    classify (Return r) = Return r
    classify (Lift sup) = Lift (fmap classify sup)
    classify (Do e) =
      case prj e of
        Just (CSS_ sel ss k) ->
          Do (inj (CSS_ (Txt.cons '.' sel) ss (classify k)))
        Just (CSS3_ at sel mcss k) ->
          Do (inj (CSS3_ at sel (fmap classify mcss) (classify k))) 
        _ -> error "impossible"


-- -- Useful for standalone contexts without a Component root.
-- renderContext' :: IsContext' ts ms m => Context' ts ms m -> ENode -> HTML (Code ms IO ()) -> IO (HTML (Code ms IO ()))
-- renderContext' a parent html = do
--   let f e = void $ with a e
--   doc <- getDocument
--   html' <- buildHTML doc f html
--   embed_ parent html'
--   return html'

-- rebuild finds managed nodes and re-embeds them in case they were
-- removed for other uses
rebuild :: forall e. Typeable e => HTML e -> IO ()
rebuild h =
#ifndef __GHCJS__
    return ()
#else
    go h
  where
    go :: HTML e -> IO ()
    go STHTML {..}  =
      forM_ _strecord $ \ref -> do
        (_,_,a,_) <- readIORef ref
        rebuild (unsafeCoerce a :: HTML e)
    go HTML {..}    = mapM_ (go . fromJust . fromAtom) _atoms
    go SVGHTML {..} = mapM_ (go . fromJust . fromAtom) _atoms
    go KHTML {..}   = mapM_ (go . fromJust . fromAtom . snd) _keyed
    go KSVGHTML {..}  = mapM_ (go . fromJust . fromAtom . snd) _keyed
    go m@Managed {..} =
      case _constr of
        Context' c -> do
          mi_ <- lookupContext (key c)
          forM_ mi_ $ \ContextRecord {..} -> do
            ContextView {..} <- readIORef crView
            rebuild cvCurrentLive
            forM_ _node $ \node ->
              embed_ node cvCurrentLive
    go _ =
      return ()
#endif


triggerBackground :: forall m e. (MonadIO m, Typeable e) => HTML e -> m ()
triggerBackground = go
  where
    bg Context {..} = do
      mc <- lookupContext key
      case mc of
        Nothing -> return ()
        Just ContextRecord {..} -> do
          let ContextHooks _ bg _= crHooks
          publish bg ()
          ContextView {..} <- liftIO $ readIORef crView
          go $ unsafeCoerce cvCurrentLive

    go :: HTML e -> m ()
    go STHTML {..}  =
      forM_ _strecord $ \ref -> do
        (_,_,a,_) <- liftIO $ readIORef ref
        go (unsafeCoerce a)
    go HTML {..}    = mapM_ (go . fromJust . fromAtom) _atoms
    go SVGHTML {..} = mapM_ (go . fromJust . fromAtom) _atoms
    go KHTML {..}   = mapM_ (go . fromJust . fromAtom . snd) _keyed
    go KSVGHTML {..} = mapM_ (go . fromJust . fromAtom . snd) _keyed
    go m@Managed {..} = case _constr of Context' c -> bg (unsafeCoerce c)
    go _ = return ()

triggerForeground :: forall m e. (MonadIO m, Typeable e) => HTML e -> m ()
triggerForeground = go
  where
    fg Context {..} = do
      mc <- lookupContext key
      case mc of
        Nothing -> return ()
        Just ContextRecord {..} -> do
          let ContextHooks _ _ fg = crHooks
          publish fg ()
          ContextView {..} <- liftIO $ readIORef crView
          go (unsafeCoerce cvCurrentLive)

    go :: HTML e -> m ()
    go STHTML {..}  =
      forM_ _strecord $ \ref -> do
        (_,_,a,_) <- liftIO $ readIORef ref
        go (unsafeCoerce a)
    go HTML {..}    = mapM_ (go . fromJust . fromAtom) _atoms
    go SVGHTML {..} = mapM_ (go . fromJust . fromAtom) _atoms
    go KHTML {..}   = mapM_ (go . fromJust . fromAtom . snd) _keyed
    go KSVGHTML {..} = mapM_ (go . fromJust . fromAtom . snd) _keyed
    go m@Managed {..} = case _constr of Context' c -> fg (unsafeCoerce c)
    go _ = return ()

onForeground :: ( MonadIO c, MonadIO c'
                , '[Revent] <: ms
                , '[State () ContextHooks] <: ms'
                , With w (Narrative (Messages ms') c') IO
                )
             => w -> Code '[Event ()] (Code ms c) () -> Code ms c (Promise (IO ()))
onForeground c f = do
  connectWith c (get >>= \(ContextHooks _ _ fg) -> return fg) $ \_ -> f

onBackground :: ( MonadIO c, MonadIO c'
                , '[Revent] <: ms
                , '[State () ContextHooks] <: ms'
                , With w (Narrative (Messages ms') c') IO
                )
             => w -> Code '[Event ()] (Code ms c) () -> Code ms c (Promise (IO ()))
onBackground c f = do
  connectWith c (get >>= \(ContextHooks _ bg _) -> return bg) $ \_ -> f

reflect :: forall ts ms m c.
           ( IsContext' ts ms m
           , MonadIO c
           )
        => Context' ts ms m
        -> c (Promise (HTML ms))
reflect c =
  with c $ do
    ContextState {..} :: ContextState m <- get
    ContextView {..} <- liftIO $ readIORef asLive
    return (unsafeCoerce cvCurrentLive)

data DiffStrategy = Eager | Manual deriving (Eq)

type Differ ms m =
    forall a. Component a ms =>
       (m ms -> a ms)
    -> IO ()
    -> (Code ms IO () -> IO ())
    -> ContextState m
    -> Code ms IO ()

data AState m =
  AState
    { as_live :: forall ms. IORef (ContextView ms m)
    , as_model :: forall ms. m ms
    }

data ContextPatch m =
  forall ms a. Component a ms =>
  APatch
      -- only modify ap_AState with atomicModifyIORef
    { ap_send         :: Code ms IO () -> IO ()
    , ap_AState       :: IORef (Maybe (AState m),Bool) -- an AState record for manipulation; nullable by context to stop a patch.
    , ap_patchView    :: (m ms -> a ms)
    , ap_viewTrigger  :: IO ()
    , ap_hooks        :: ContextHooks
    }

type IsContext' ts ms m = (Typeable ms, Base m <: ms, Base m <. ts, Delta (Modules ts) (Messages ms))
type IsContext ms m = IsContext' ms ms m

data ContextHooks = ContextHooks
  { chViews      :: Syndicate ()
  , chForeground :: Syndicate ()
  , chBackground :: Syndicate ()
  }

data ContextView ms m = ContextView
  { cvCurrent     :: Atom ms
  , cvCurrentLive :: HTML ms
  , cvModel       :: m ms
  , cvForeground  :: Bool
  }

data ContextRecord ms m = ContextRecord
  { crAsContext :: As (Code ms IO)
  , crView        :: IORef (ContextView ms m)
  , crHooks       :: ContextHooks
  }

data ContextState (m :: [* -> *] -> *) where
  ContextState ::
    { asPatch        :: Maybe (ContextPatch m)
    , asDiffer       :: ContextState m -> Code ms IO ()
    , asDiffStrategy :: DiffStrategy
    , asUpdates      :: Syndicate (m ms)
    , asModel        :: m ms
    , asLive         :: IORef (ContextView ms m)
    } -> ContextState m

type Base (m :: [* -> *] -> *)
  = '[ State () (ContextState m)
     , State () ContextHooks
     , State () Shutdown
     , Revent
     ]

data Constr where
  Context' :: (IsContext' ts ms m, Typeable ms) => Context' ts ms m -> Constr
instance Eq Constr where
 (==) (Context' c) (Context' c') =
  let Key k1 :: Key GHC.Prim.Any = unsafeCoerce (key c)
      Key k2 :: Key GHC.Prim.Any = unsafeCoerce (key c')
  in prettyUnsafeEq k1 k2

instance ToTxt (Feature e) where
  toTxt NullFeature          = mempty

  toTxt (Attribute attr val) =
    if Txt.null val then
      attr
    else
      attr <> "=\"" <> val <> "\""

  toTxt (Property prop val) =
    prop <> "=\"" <> val <> "\""

  toTxt (StyleF pairs) =
    "style=\""
      <> Txt.intercalate
           (Txt.singleton ';')
           (renderStyles False (mapM_ (uncurry (=:)) pairs))
      <> "\""

  toTxt (LinkTo href _)    = "href=\"" <> href <> "\""

  toTxt (SVGLinkTo href _) = "xlink:href=\"" <> href <> "\""

  toTxt (XLink xl v)     = xl <> "=\"" <> v <> "\""

  toTxt _ = mempty

instance ToTxt [Feature e] where
  toTxt fs =
    Txt.intercalate
     (Txt.singleton ' ')
     (Prelude.filter (not . Txt.null) $ Prelude.map toTxt fs)

type ContextKey ms m = Key (ContextRecord (Appended ms (Base m)) m)
type ContextBuilder ts m = Modules (Base m) (Action (Appended ts (Base m)) IO) -> IO (Modules (Appended ts (Base m)) (Action (Appended ts (Base m)) IO))
type ContextPrimer ms m = Code (Appended ms (Base m)) IO ()

data Context' ts ms (m :: [* -> *] -> *)
  = forall a. Component a ms => Context
      { key       :: !(Key (ContextRecord ms m))
      , build     :: !(Modules (Base m) (Action ts IO) -> IO (Modules ts (Action ts IO)))
      , prime     :: !(Code ms IO ())
      , model     :: !(m ms)
      , render    :: !(m ms -> a ms)
      }
type Context ms m = Context' (Appended ms (Base m)) (Appended ms (Base m)) m

instance ToTxt (Context' ts ms m) where
  toTxt = toTxt . key

instance Eq (Context' ts ms m) where
  (==) (Context k _ _ _ _) (Context k' _ _ _ _) =
    let Key k1 = k
        Key k2 = k'
    in prettyUnsafeEq k1 k2

instance Ord (Context' ts ms m) where
  compare (Context (Key k) _ _ _ _) (Context (Key k') _ _ _ _) = compare k k'

instance IsContext' ts ms m
  => With (Context' ts ms m)
          (Code ms IO)
          IO
  where
    using_ c = do
      -- FIXME: likely a bug here with double initialization in multithreaded contex-ts!
      mi_ <- lookupContext (key c)
      case mi_ of
        Just (ContextRecord {..}) -> return (runAs crAsContext)
        Nothing -> do
          mkContext BuildOnly c
          using_ c
    with_ c m = do
      run <- using_ c
      run m
    shutdown_ c = do
      -- this method should 1. destroy the view 2. syndicate a shutdown event 3. poison the context
      -- so that unmount events that call with on the context do not fail
      miohhm <- lookupContext (key c)
      case miohhm of
        Just ContextRecord {..} -> do
          ContextView {..} <- liftIO $ readIORef crView
          cleanup (void . with c) [cvCurrentLive]
          delete cvCurrentLive
          void $ runAs crAsContext $ do
            buf <- get
            Shutdown sdn <- get
            publish sdn ()
            -- this is where things get iffy... what should this look like?
            delay 0 $ do
              deleteContext (key c)
              liftIO $ do
                killBuffer buf
                myThreadId >>= killThread
        _ -> return ()

{-# NOINLINE constructShutdownSyndicate #-}
constructShutdownSyndicate :: Syndicate ()
constructShutdownSyndicate = unsafePerformIO syndicate

{-# NOINLINE constructVault__ #-}
constructVault__ :: Vault
constructVault__ = Vault (unsafePerformIO (newMVar Map.empty))

lookupContext :: (MonadIO c) => Key phantom -> c (Maybe phantom)
lookupContext = vaultLookup constructVault__

getContextName :: IsContext' ts ms m => Context' ts ms m -> Txt
getContextName = toTxt . key

addContext :: (MonadIO c) => Key phantom -> phantom -> c ()
addContext = vaultAdd constructVault__

deleteContext :: (MonadIO c) => Key phantom -> c ()
deleteContext = vaultDelete constructVault__

data MkContextAction
  = ClearAndAppend ENode
  | forall e. Replace (HTML e)
  | Append ENode
  | BuildOnly

mkContext :: forall ms ts m.
          ( IsContext' ts ms m
          , Base m <: ms
          )
       => MkContextAction
       -> Context' ts ms m
       -> IO (ContextRecord ms m)
mkContext mkContextAction c@Context {..} = do
  let !m = model
      !raw = toAtom $ render m
  doc <- getDocument
  buf <- newEvQueue
  ch  <- ContextHooks <$> syndicate <*> syndicate <*> syndicate
  us  <- syndicate
  sdn <- Shutdown <$> syndicate
  as  <- unsafeConstructAs buf
  let sendEv = void . runAs as
  (i,l) <- case mkContextAction of
            ClearAndAppend n -> do
              i <- buildAndEmbedMaybe sendEv doc ch True Nothing raw
              clearNode (Just $ toNode n)
              mn <- getNode i
              forM_ mn (appendChild n)
              return (i,True)
            Replace as -> do
              i <- buildAndEmbedMaybe sendEv doc ch True Nothing raw
              replace as i
              return (i,True)
            Append en -> do
              i <- buildAndEmbedMaybe sendEv doc ch True (Just en) raw
              return (i,True)
            BuildOnly -> do
              i <- buildAndEmbedMaybe sendEv doc ch False Nothing raw
              return (i,False)
  cr <- ContextRecord <$> pure as <*> newIORef (ContextView raw i m l) <*> pure ch
  -- keep out of forkIO to prevent double-initialization
  addContext key cr
  forkIO $ do
    built <- build $ state (ContextState
                                Nothing
                                (differ render (publish (chViews ch) ()) sendEv)
                                Eager
                                us
                                model
                                (crView cr)
                            )
                    *:* state ch
                    *:* state sdn
                    *:* state buf
                    *:* Empty
    (obj',_) <- Ef.Base.Object built Ef.Base.! do
      connect constructShutdownSyndicate $ const (Ef.Base.lift shutdownSelf)
      prime
#if (defined __GHCJS__) || (defined DEVEL)
    driverPrintExceptions (show key ++ " - context exception: ")
#else
    driver
#endif
        buf obj'
  return cr

diff :: forall m ms. (Base m <: ms) => Proxy m -> Code ms IO ()
diff _ = do
  as@ContextState {..} :: ContextState m <- get
  unsafeCoerce (asDiffer as)

setEagerDiff :: forall m ms. ('[State () (ContextState m)] <: ms)
             => Proxy m -> Code ms IO ()
setEagerDiff _ = do
  ContextState {..} :: ContextState m <- get
  put ContextState { asDiffStrategy = Eager, .. }

setManualDiff :: forall m ms. ('[State () (ContextState m)] <: ms)
              => Proxy m -> Code ms IO ()
setManualDiff _ = do
  ContextState {..} :: ContextState m <- get
  put ContextState { asDiffStrategy = Manual, .. }

currentView :: forall ts ms c m.
               ( IsContext' ts ms m
               , MonadIO c
               )
            => Context' ts ms m
            -> c (Promise (HTML ms))
currentView c = with c $ ownView c

ownView :: forall ts ms c m.
           ( IsContext' ts ms m
           , MonadIO c
           , Base m <: ms
           )
        => Context' ts ms m
        -> Code ms c (HTML ms)
ownView _ = do
  ContextState {..} :: ContextState m <- get
  ContextView {..} <- liftIO $ readIORef asLive
  return (unsafeCoerce cvCurrent)

onModelChange :: forall ts ms ms' m c.
                ( IsContext' ts ms m
                , MonadIO c
                , Base m <: ms
                , '[Revent] <: ms'
                )
              => Context' ts ms m
              -> (m ms -> Code '[Event (m ms)] (Code ms' c) ())
              -> Code ms' c (Promise (IO ()))
onModelChange c f = do
  buf <- get
  with c $ do
    ContextState {..} :: ContextState m <- get
    sub <- subscribe (unsafeCoerce asUpdates) (return buf)
    bhv <- listen sub f
    return (stop bhv >> leaveSyndicate (unsafeCoerce asUpdates) sub)

onOwnModelChange :: forall ts ms ms' m c.
                    ( IsContext' ts ms m
                    , MonadIO c
                    , Base m <: ms
                    )
                  => Context' ts ms m
                  -> (m ms -> Code '[Event (m ms)] (Code ms c) ())
                  -> Code ms c (IO ())
onOwnModelChange _ f = do
  buf <- get
  pr  <- promise
  ContextState {..} :: ContextState m <- get
  sub <- subscribe (unsafeCoerce asUpdates) (return buf)
  bhv <- listen sub f
  return (stop bhv >> leaveSyndicate (unsafeCoerce asUpdates) sub)

onOwnModelChangeByProxy :: forall ms m c.
                           ( MonadIO c
                           , Base m <: ms
                           )
                         => Proxy m
                         -> (m ms -> Code '[Event (m ms)] (Code ms c) ())
                         -> Code ms c (IO ())
onOwnModelChangeByProxy _ f = do
  buf <- get
  pr  <- promise
  ContextState {..} :: ContextState m <- get
  sub <- subscribe (unsafeCoerce asUpdates) (return buf)
  bhv <- listen sub f
  return (stop bhv >> leaveSyndicate (unsafeCoerce asUpdates) sub)

{-# INLINE getModel #-}
getModel :: forall m ms. ('[State () (ContextState m)] <: ms) => Code ms IO (m ms)
getModel = do
  ContextState {..} :: ContextState m <- get
  return $ unsafeCoerce asModel

{-# INLINE putModel #-}
putModel :: forall ms m.
        ( Base m <: ms
        )
     => m ms -> Code ms IO ()
putModel !new = do
  (ContextState {..},(old,cmp')) <- modify $ \(ContextState {..} :: ContextState m) ->
    let !old = unsafeCoerce asModel
        cmp' = ContextState { asModel = unsafeCoerce new, .. }
    in (cmp',(old,cmp'))
  publish (unsafeCoerce asUpdates) new
  let d :: ContextState m -> Code ms IO ()
      d = unsafeCoerce asDiffer
#ifdef __GHCJS__
  case reallyUnsafePtrEquality# old new of
    1# -> return ()
    _  ->
      case asDiffStrategy of
        Eager  -> d cmp'
        Manual -> return ()
#else
  d cmp'
#endif

{-# INLINE modifyModel #-}
modifyModel :: forall e ms m.
           ( Base m <: ms
           , e ~ Code ms IO ()
           )
        => (m ms -> m ms)
        -> Code ms IO ()
modifyModel f = do
  (ContextState {..},(old,!new,cmp')) <- modify $ \ContextState {..} ->
    let !old = unsafeCoerce asModel
        !new = f old
        cmp' = ContextState { asModel = unsafeCoerce new, ..  }
    in (cmp',(old,new,cmp'))
  publish (unsafeCoerce asUpdates) new
  let d :: ContextState m -> Code ms IO ()
      d = unsafeCoerce asDiffer
#ifdef __GHCJS__
  case reallyUnsafePtrEquality# old new of
    1# -> return ()
    _  ->
      case asDiffStrategy of
        Eager  -> d cmp'
        Manual -> return ()
#else
  d cmp'
#endif

differ :: (Base m <: ms) => Differ ms m
differ render trig sendEv ContextState {..} = do
#ifdef __GHCJS__
  ch <- get
  let setupDiff = do
        let !new_as = AState (unsafeCoerce asLive) (unsafeCoerce asModel)
        new_ap_AState <- liftIO $ newIORef (Just new_as,False)
        let !aPatch = APatch sendEv new_ap_AState render trig ch
        put ContextState { asPatch = Just aPatch, .. }
        liftIO $ diff_ aPatch
        return ()
  case asPatch of
    -- no current patch awaiting diff
    -- if this strategy doesn't work, use the return value
    -- from diff_ to cancel the animation frame event instead
    Nothing ->
      setupDiff
    Just APatch {..} -> do
        shouldSetupDiff <- liftIO $ atomicModifyIORef' ap_AState $ \mpatch ->
          case mpatch of
            (Just cs,False) ->
              ((Just cs { as_model = unsafeCoerce asModel },False),False)
            _ -> (mpatch,True)
        when shouldSetupDiff $ do
          setupDiff
  return ()
#else
  let v = render asModel
  liftIO $ do
    ContextView _ _ _ isFG <- liftIO $ readIORef asLive
    writeIORef asLive $ unsafeCoerce $ ContextView v v asModel isFG
#endif

#ifdef __GHCJS__
toNode :: T.IsNode n => n -> NNode
toNode = T.castToNode
#else
toNode :: n -> NNode
toNode _ = ()
#endif

createElement :: Doc -> Txt -> IO (Maybe ENode)
createElement doc tag =
#ifdef __GHCJS__
  D.createElement doc (Just tag)
#else
  return (Just ())
#endif

createTextNode :: Doc -> Txt -> IO (Maybe TNode)
createTextNode doc c =
#ifdef __GHCJS__
  D.createTextNode doc c
#else
  return (Just ())
#endif

createElementNS :: Doc -> Txt -> Txt -> IO (Maybe ENode)
createElementNS doc ns tag =
#ifdef __GHCJS__
  D.createElementNS doc (Just ns) (Just tag)
#else
  return (Just ())
#endif

clearNode :: Maybe NNode -> IO ()
clearNode mnode =
#ifdef __GHCJS__
  forM_ mnode clear_node_js
#else
  return ()
#endif

#ifdef __GHCJS__
appendChild :: T.IsNode n => ENode -> n -> IO ()
appendChild parent child =
  append_child_js parent (toNode child)
#else
appendChild :: ENode -> n -> IO ()
appendChild _ _ =
  return ()
#endif

setInnerHTML :: ENode -> Txt -> IO ()
setInnerHTML el r =
#ifdef __GHCJS__
  E.setInnerHTML el (Just r)
#else
  return ()
#endif

isAlreadyEmbedded :: ENode -> ENode -> IO Bool
isAlreadyEmbedded target elem =
#ifdef __GHCJS__
  is_already_embedded_js target elem
#else
  return True
#endif

isAlreadyEmbeddedText :: TNode -> ENode -> IO Bool
isAlreadyEmbeddedText target txt =
#ifdef __GHCJS__
  is_already_embedded_text_js target txt
#else
  return True
#endif

changeText :: TNode -> Txt -> IO ()
changeText t cnt' =
#ifdef __GHCJS__
  changeText_js t cnt'
#else
  return ()
#endif

swapContent :: TNode -> ENode -> IO ()
swapContent t e =
#ifdef __GHCJS__
  swap_content_js t e
#else
  return ()
#endif

{-# NOINLINE embed_ #-}
embed_ :: forall e. ENode -> HTML e -> IO ()
embed_ parent STHTML {..} = do
  forM_ _strecord $ \ref -> do
    (_,_,a,_) <- readIORef ref
    embed_ parent (unsafeCoerce a :: HTML e)
embed_ parent Text {..} =
  forM_ _tnode $ \node -> do
    ae <- isAlreadyEmbeddedText node parent
    unless ae (void $ appendChild parent node)
embed_ parent n =
  forM_ (_node n) $ \node -> do
    ae <- isAlreadyEmbedded node parent
    unless ae (void $ appendChild parent node)

{-# NOINLINE embedMany_ #-}
embedMany_ parent children = do
  forM_ children $ \child -> do
    embed_ parent child

setAttributes :: [Feature e] -> (Code e IO () -> IO ()) -> Bool -> ENode -> IO ([Feature e],IO ())
setAttributes as f diffing el = do
#ifdef __GHCJS__
  didMount_ <- newIORef (return ())
  attrs <- go didMount_ as
  dm <- readIORef didMount_
  return (attrs,dm)
  where
    go _ [] = return []
    go didMount_ (a:as) = do
      dm <- readIORef didMount_
      (a',dm') <- setAttribute_ f diffing el a dm
      writeIORef didMount_ dm'
      res <- go didMount_ as
      return (a':res)
#else
  return (as,return ())
#endif

{-# NOINLINE buildAndEmbedMaybe #-}
buildAndEmbedMaybe :: forall e. Typeable e => (Code e IO () -> IO ()) -> Doc -> ContextHooks -> Bool -> Maybe ENode -> Atom e -> IO (HTML e)
buildAndEmbedMaybe f doc ch isFG mn = go mn . construct
  where
    go :: Maybe ENode -> HTML e -> IO (HTML e)
    go mparent nn@NullHTML {..} = do
      _cond@(Just el) <- createElement doc "template"
      forM_ mparent (flip appendChild el)
      return $ NullHTML _cond

    go mparent Raw {..} = do
      _node@(Just el) <- createElement doc _tag
      (_attributes,didMount) <- setAttributes _attributes f False el
      setInnerHTML el _content
      didMount
      forM_ mparent $ \parent -> appendChild parent el
      return $ Raw _node _tag _attributes _content

    go mparent HTML {..} = do
      _node@(Just el) <- createElement doc _tag
      (_attributes,didMount) <- setAttributes _attributes f False el
      _atoms <- mapM (fmap toAtom . go (Just el) . construct) _atoms
      didMount
      forM_ mparent $ \parent -> appendChild parent el
      return $ HTML _node _tag _attributes _atoms

    go mparent STHTML {..} = do
      strec <- newIORef (_ststate,_stview,nil,nil)
      let upd g cb = void $ do
            -- this won't work properly on GHC; look into using onFPS or something to
            -- replicate the rAF approach
#ifdef __GHCJS__
            rafCallback <- newRequestAnimationFrameCallback $ \_ -> do
#endif
              (st,sv,old,mid) <- readIORef strec
              let st' = g st
                  new_mid = unsafeCoerce $ sv st' upd
              new <- diffHelper f doc ch isFG old mid new_mid
              writeIORef strec (st',sv,new,new_mid)
              cb
#ifdef __GHCJS__
            win <- getWindow
            requestAnimationFrame win (Just rafCallback)
#endif
      let mid = _stview _ststate upd
      new <- go mparent (unsafeCoerce $ construct mid)
      writeIORef strec (_ststate,_stview,new,unsafeCoerce mid)
      return $ STHTML _stmodel _stid _ststate (Just $ unsafeCoerce strec) _stview upd

    go mparent SVGHTML {..} = do
      _node@(Just el) <- createElementNS doc "http://www.w3.org/2000/svg" _tag
      (_attributes,didMount) <- setAttributes _attributes f False el
      _atoms <- mapM (fmap toAtom . go (Just el) . construct) _atoms
      didMount
      forM_ mparent $ \parent -> appendChild parent el
      return $ SVGHTML _node _tag _attributes _atoms

    go mparent KHTML {..} = do
      _node@(Just el) <- createElement doc _tag
      (_attributes,didMount) <- setAttributes _attributes f False el
      _keyed <- mapM (\(k,x) -> go (Just el) (construct x) >>= \y -> return (k,toAtom y)) _keyed
      didMount
      forM_ mparent $ \parent -> appendChild parent el
      return $ KHTML _node _tag _attributes _keyed

    go mparent KSVGHTML {..} = do
      _node@(Just el) <- createElementNS doc "http://www.w3.org/2000/svg" _tag
      (_attributes,didMount) <- setAttributes _attributes f False el
      _keyed <- mapM (\(k,x) -> go (Just el) (construct x) >>= \y -> return (k,toAtom y)) _keyed
      didMount
      forM_ mparent $ \parent -> appendChild parent el
      return $ KSVGHTML _node _tag _attributes _keyed

    go mparent Text {..} = do
      _tnode@(Just el) <- createTextNode doc _content
      forM_ mparent (flip appendChild el)
      return $ Text _tnode _content

    go mparent m@Managed {..} =
      case _constr of
        Context' a -> do
          case _node of
            Nothing -> do
              _node@(Just el) <- createElement doc _tag
              (_attributes,didMount) <- setAttributes _attributes f False el
              didMount
              mi_ <- lookupContext (key a)
              case mi_ of
                Nothing -> do
                  -- never built before; make and embed
                  ContextRecord {..} <- mkContext BuildOnly a
                  ContextView {..} <- liftIO $ readIORef crView
                  forM_ mparent $ \parent -> do
                    when isFG (triggerForeground m)
                    embed_ parent Managed {..}
                  embed_ el cvCurrentLive
                  return Managed {..}
                Just ContextRecord {..} -> do
                  ContextView {..} <- liftIO $ readIORef crView
                  rebuild Managed {..}
                  when isFG (triggerForeground m)
                  embed_ el cvCurrentLive
                  forM_ mparent $ \parent -> embed_ parent Managed {..}
                  return Managed {..}

            Just e -> do
              mi_ <- lookupContext (key a)
              case mi_ of
                Nothing -> do
                  -- shut down?
                  ContextRecord {..} <- mkContext BuildOnly a
                  ContextView {..} <- liftIO $ readIORef crView
                  forM_ mparent $ \parent -> do
                    when isFG (triggerForeground m)
                    embed_ parent Managed {..}
                  embed_ e cvCurrentLive
                  return Managed {..}
                Just ContextRecord {..} -> do
                  ContextView {..} <- liftIO $ readIORef crView
                  rebuild m
                  when isFG (triggerForeground m)
                  embed_ e cvCurrentLive
                  return m

{-# NOINLINE buildHTML #-}
buildHTML :: (Typeable e, Component a e) => Doc -> ContextHooks -> Bool -> (Code e IO () -> IO ()) -> a e -> IO (HTML e)
buildHTML doc ch isFG f = buildAndEmbedMaybe f doc ch isFG Nothing . toAtom

getElement :: forall e. HTML e -> IO (Maybe ENode)
getElement Text {} = return Nothing
getElement STHTML {..} =
  case _strecord of
    Nothing -> return Nothing
    Just ref -> do
      (_,_,a,_) <- readIORef ref
      getElement (unsafeCoerce a :: HTML e)
getElement n = return $ _node n

getNode :: forall e. HTML e -> IO (Maybe NNode)
getNode Text {..} = return $ fmap toNode _tnode
getNode STHTML {..} =
  case _strecord of
    Nothing -> return Nothing
    Just ref -> do
      (_,_,a,_) <- readIORef ref
      getNode (unsafeCoerce a :: HTML e)
getNode n = return $ fmap toNode $ _node n


diff_ :: ContextPatch m -> IO ()
diff_ APatch {..} = do
#ifdef __GHCJS__
  -- made a choice here to do all the diffing in the animation frame; this way we
  -- can avoid recalculating changes multiple times during a frame. No matter how
  -- many changes occur in any context, the diff is only calculated once per frame.
  rafCallback <- newRequestAnimationFrameCallback $ \_ -> do
    (mcs,b) <- atomicModifyIORef' ap_AState $ \(mcs,b) -> ((Nothing,True),(mcs,b))
    case mcs of
      Nothing -> return ()
      Just (AState as_live !as_model) -> do
        doc <- getDocument
        ContextView !raw_html !live_html live_m isFG <- readIORef as_live
        let !new_html = toAtom $ ap_patchView $ unsafeCoerce as_model
        new_live_html <- diffHelper ap_send doc ap_hooks isFG live_html raw_html new_html
        writeIORef as_live $ ContextView new_html new_live_html as_model isFG
        ap_viewTrigger
  win <- getWindow
  requestAnimationFrame win (Just rafCallback)
  return ()
  -- void $ forkIO $ takeMVar mv >> releaseCallback (unsafeCoerce rafCallback :: Callback (T.JSVal -> IO ()))
#else
  (mcs,b) <- atomicModifyIORef' ap_AState $ \(mcs,b) -> ((Nothing,True),(mcs,b))
  case mcs of
    Nothing -> return ()
    Just (AState as_live !as_model) -> do
      doc <- getDocument
      ContextView !raw_html !live_html live_m isFG <- readIORef as_live
      let !new_html = toAtom $ ap_patchView as_model
      new_live_html <- diffHelper ap_send doc ap_hooks isFG live_html raw_html new_html
      writeIORef as_live $ ContextView new_html new_live_html as_model isFG
      ap_viewTrigger
#endif

{-# NOINLINE replace #-}
replace :: HTML e -> HTML e' -> IO ()
#ifndef __GHCJS__
replace _ _ = return ()
#else
replace old@STHTML {} new@STHTML {} =
  case (old,new) of
    (STHTML _ _ _ (Just r) _ _,STHTML _ _ _ (Just r') _ _) -> do
      (_,_,a,_) <- readIORef r
      (_,_,b,_) <- readIORef r'
      replace a b
    _ -> return ()

replace STHTML {..} new = do
  case _strecord of
    Nothing -> return ()
    Just ref -> do
      (_,_,a,_) <- readIORef ref
      replace a new

replace old STHTML {..} = do
  case _strecord of
    Nothing -> return ()
    Just ref -> do
      (_,_,a,_) <- readIORef ref
      replace old a

replace old@Text {} new@Text {} = do
  forM_ (_tnode old) $ \o ->
    forM_ (_tnode new) $ \n ->
      swap_js (toNode o) (toNode n)

replace old Text {..} = do
  forM_ (_node old) $ \o ->
    forM_ _tnode $ \n ->
      swap_js (toNode o) (toNode n)

replace Text {..} new = do
  forM_ _tnode $ \o ->
    forM_ (_node new) $ \n ->
      swap_js (toNode o) (toNode n)

replace old new = do
  forM_ (_node old) $ \o ->
    forM_ (_node new) $ \n ->
      swap_js (toNode o) (toNode n)
#endif

{-# NOINLINE delete #-}
delete :: HTML e -> IO ()
#ifndef __GHCJS__
delete _ = return ()
#else
delete STHTML {..} = do
  case _strecord of
    Nothing -> return ()
    Just ref -> do
      (_,_,a,_) <- readIORef ref
      delete a
delete Text {..} = forM_ _tnode (delete_js . toNode)
delete n = forM_ (_node n) (delete_js . toNode)
#endif

-- f node feature io -> io
{-# NOINLINE cleanup #-}
cleanup :: Typeable e => (Code e IO () -> IO ()) -> [HTML e] -> IO (IO ())
#ifndef __GHCJS__
cleanup _ _ = return (return ())
#else
cleanup f = go (return ())
  where
    go didUnmount (STHTML{..}:rest) = do
      didUnmount' <- case _strecord of
                       Nothing -> return (return ())
                       Just ref -> do
                         (_,_,a,_) <- readIORef ref
                         cleanup f [unsafeCoerce a]
      go (didUnmount' >> didUnmount) rest
    go didUnmount (NullHTML{}:rest) = go didUnmount rest
    go didUnmount (r@Raw {..}:rest) = do
      en <- getElement r
      du <- case en of
              Nothing -> return (return ())
              Just n  -> foldM (flip (cleanupAttr f n)) (return ()) _attributes
      go (du >> didUnmount) rest
    go didUnmount (a@HTML {..}:rest) = do
      en <- getElement a
      du <- case en of
              Nothing -> return (return ())
              Just n -> foldM (flip (cleanupAttr f n)) (return ()) _attributes
      unmounts' <- cleanup f (map (fromJust . fromAtom) _atoms)
      go (unmounts' >> du >> didUnmount) rest
    go didUnmount (a@SVGHTML {..}:rest) = do
      en <- getElement a
      du <- case en of
              Nothing -> return (return ())
              Just n -> foldM (flip (cleanupAttr f n)) (return ()) _attributes
      unmounts' <- cleanup f (map (fromJust . fromAtom) _atoms)
      go (unmounts' >> du >> didUnmount) rest
    go didUnmount (a@KHTML {..}:rest) = do
      en <- getElement a
      du <- case en of
              Nothing -> return (return ())
              Just n -> foldM (flip (cleanupAttr f n)) (return ()) _attributes
      unmounts' <- cleanup f (map (fromJust . fromAtom . snd) _keyed)
      go (unmounts' >> du >> didUnmount) rest
    go didUnmount (a@KSVGHTML {..}:rest) = do
      en <- getElement a
      du <- case en of
              Nothing -> return (return ())
              Just n -> foldM (flip (cleanupAttr f n)) (return ()) _attributes
      unmounts' <- cleanup f (map (fromJust . fromAtom . snd) _keyed)
      go (unmounts' >> du >> didUnmount) rest
    go didUnmount (a@Managed {..}:rest) = do
      en <- getElement a
      du <- case en of
              Nothing -> return (return ())
              Just n -> foldM (flip (cleanupAttr f n)) (return ()) _attributes
      go (du >> didUnmount) rest
    go didUnmount _ = return didUnmount
#endif

{-# NOINLINE insertAt #-}
insertAt :: ENode -> Int -> HTML e -> IO ()
#ifndef __GHCJS__
insertAt _ _ _ = return ()
#else
insertAt parent ind STHTML {..} = do
  forM_ _strecord $ \ref -> do
    (_,_,a,_) <- readIORef ref
    insertAt parent ind a
insertAt parent ind Text {..} = forM_ _tnode $ insert_at_js parent ind . toNode
insertAt parent ind n = forM_ (_node n) $ insert_at_js parent ind . toNode
#endif

{-# NOINLINE insertBefore_ #-}
insertBefore_ :: forall e. ENode -> HTML e -> HTML e -> IO ()
#ifndef __GHCJS__
insertBefore_ _ _ _ = return ()
#else
insertBefore_ parent child@(STHTML{}) new@(STHTML{}) = do
  case (child,new) of
    (STHTML _ _ _ (Just r) _ _, STHTML _ _ _ (Just r') _ _) -> do
      (_,_,a,_) <- readIORef r
      (_,_,b,_) <- readIORef r'
      insertBefore_ parent (unsafeCoerce a :: HTML e) (unsafeCoerce b :: HTML e)
    _ -> return ()
insertBefore_ parent STHTML {..} new = do
  forM_ _strecord $ \ref -> do
    (_,_,a,_) <- readIORef ref
    insertBefore_ parent (unsafeCoerce a :: HTML e) new
insertBefore_ parent child STHTML {..} = do
  forM_ _strecord $ \ref -> do
    (_,_,a,_) <- readIORef ref
    insertBefore_ parent child (unsafeCoerce a :: HTML e)
insertBefore_ parent child@(Text {}) new@(Text{}) = void $ N.insertBefore parent (_tnode new) (_tnode child)
insertBefore_ parent child new@(Text{}) = void $ N.insertBefore parent (_tnode new) (_node child)
insertBefore_ parent child@(Text {}) new = void $ N.insertBefore parent (_node new) (_tnode child)
insertBefore_ parent child new = void $ N.insertBefore parent (_node new) (_node child)
#endif

diffHelper :: forall e. Typeable e => (Code e IO () -> IO ()) -> Doc -> ContextHooks -> Bool -> HTML e -> Atom e -> Atom e -> IO (HTML e)
diffHelper f doc ch isFG =
#ifdef __GHCJS__
    go
#else
    \_ _ n -> return n
#endif
  where

    go :: HTML e -> Atom e -> Atom e -> IO (HTML e)
    go old mid new =
      if reallyUnsafeEq mid new then do
        return old
      else
        go' old mid new

    go' :: HTML e -> Atom e -> Atom e -> IO (HTML e)
    go' old@NullHTML{} _ new = do
      case construct new of
        NullHTML _ -> return old
        _          -> do
          new' <- buildHTML doc ch isFG f new
          replace old new'
          didUnmount <- cleanup f [old]
          delete old
          didUnmount
          return new'

    go' old _ (construct -> new@NullHTML{}) = do
      new' <- buildHTML doc ch isFG f new
      replace old new'
      didUnmount <- cleanup f [old]
      delete old
      didUnmount
      return new'

    go' old@HTML {} (construct -> mid@HTML {}) (construct -> new@HTML {}) =
      if prettyUnsafeEq (_tag old) (_tag new)
      then do
        let Just n = _node old
        (a',didMount) <-
              if reallyUnsafeEq (_attributes mid) (_attributes new) then do
                return (_attributes old,return ())
              else
                runElementDiff f n (_attributes old) (_attributes mid) (_attributes new)
        c' <- if reallyUnsafeEq (_atoms mid) (_atoms new) then do
                return (_atoms old)
              else
                diffChildren n (map (fromJust . fromAtom) $ _atoms old) (_atoms mid) (_atoms new)
        didMount
        return $ HTML (_node old) (_tag old) a' c'
      else do new' <- buildHTML doc ch isFG f new
              replace old new'
              didUnmount <- cleanup f [old]
              delete old
              didUnmount
              return new'

    go' old@STHTML {} _ (construct -> new@STHTML {}) = do
      case (old,new) of
        (STHTML m k s ~(Just r) v u,STHTML m' k' s' _ v' _) -> do
          if prettyUnsafeEq k k' then
            if reallyVeryUnsafeEq m m' then do
              return old
            else do
              (st,sv,old,mid) <- readIORef r
              writeIORef r (st,unsafeCoerce v',old,mid)
              u (const (unsafeCoerce s')) (return ())
              return $ STHTML m' k' s' (Just $ unsafeCoerce r) v' (unsafeCoerce u)
          else do
            (_,_,a,_) <- readIORef r
            new' <- buildHTML doc ch isFG f new
            replace a new'
            didUnmount <- cleanup f [unsafeCoerce a]
            delete a
            didUnmount
            return new'

    go' old@SVGHTML {} (construct -> mid@SVGHTML {}) (construct -> new@SVGHTML {}) =
      if prettyUnsafeEq (_tag old) (_tag new)
      then do
        let Just n = _node old
        (a',didMount) <-
              if reallyUnsafeEq (_attributes mid) (_attributes new) then do
                return (_attributes old,return ())
              else do
                runElementDiff f n (_attributes old) (_attributes mid) (_attributes new)
        c' <- if reallyUnsafeEq (_atoms mid) (_atoms new) then do
                return (_atoms old)
              else do
                diffChildren n (map (fromJust . fromAtom) $ _atoms old) (_atoms mid) (_atoms new)
        didMount
        return $ SVGHTML (_node old) (_tag old) a' c'
      else do new' <- buildHTML doc ch isFG f new
              replace old new'
              didUnmount <- cleanup f [old]
              delete old
              didUnmount
              return new'

    go' old@(KHTML old_node old_tag old_attributes old_keyed)
      (construct -> mid@(KHTML midAnode _ midAattributes midAkeyed))
      (construct -> new@(KHTML _ new_tag new_attributes new_keyed)) =
      if prettyUnsafeEq old_tag new_tag
      then do
        let Just n = _node old
        (a',didMount) <-
              if reallyUnsafeEq midAattributes new_attributes then
                return (old_attributes,return ())
              else
                runElementDiff f n old_attributes midAattributes new_attributes
        c' <- if reallyUnsafeEq midAkeyed new_keyed then return old_keyed else
                diffKeyedChildren n (map (fmap (fromJust . fromAtom)) old_keyed) midAkeyed new_keyed
        didMount
        return $ KHTML old_node old_tag a' c'
      else do new' <- buildHTML doc ch isFG f new
              replace old new'
              didUnmount <- cleanup f [old]
              delete old
              didUnmount
              return new'

    go' old@(KSVGHTML old_node old_tag old_attributes old_keyed)
      (construct -> mid@(KSVGHTML midAnode _ midAattributes midAkeyed))
      (construct -> new@(KSVGHTML _ new_tag new_attributes new_keyed)) =
      if prettyUnsafeEq old_tag new_tag
      then do
        let Just n = _node old
        (a',didMount) <-
              if reallyUnsafeEq midAattributes new_attributes then
                return (old_attributes,return ())
              else
                runElementDiff f n old_attributes midAattributes new_attributes
        c' <- if reallyUnsafeEq midAkeyed new_keyed then return old_keyed else
                diffKeyedChildren n (map (fmap (fromJust . fromAtom)) old_keyed) midAkeyed new_keyed
        didMount
        return $ KSVGHTML old_node old_tag a' c'
      else do new' <- buildHTML doc ch isFG f new
              replace old new'
              didUnmount <- cleanup f [old]
              delete old
              didUnmount
              return new'

    go' txt@(Text (Just t) cnt) (construct -> mid@(Text _ mcnt)) (construct -> new@(Text _ cnt')) =
      if prettyUnsafeEq mcnt cnt' then do
        return txt
      else do
        changeText t cnt'
        return $ Text (Just t) cnt'

    go' old@(Raw {}) (construct -> mid@(Raw {})) (construct -> new@(Raw {})) =
      if prettyUnsafeEq (_tag old) (_tag new) then do
        let Just n = _node old
        (a',didMount) <-
                if reallyUnsafeEq (_attributes mid) (_attributes new) then
                  return (_attributes old,return ())
                else
                  runElementDiff f n (_attributes old) (_attributes mid) (_attributes new)
        if prettyUnsafeEq (_content mid) (_content new) then do
          didMount
          return $ Raw (_node old) (_tag old) a' (_content old)
        else do
          setInnerHTML n (_content new)
          didMount
          return $ Raw (_node old) (_tag old) a' (_content new)
      else do new' <- buildHTML doc ch isFG f new
              replace old new'
              didUnmount <- cleanup f [old]
              delete old
              didUnmount
              return new'

    go' old@(Managed {}) (construct -> mid) new@(construct -> newc@(Managed {})) =
      if    (_constr old) == (_constr newc)
        && prettyUnsafeEq (_tag old) (_tag newc)
      then do
        let Just n = _node old
        (a',didMount) <-
                if reallyUnsafeEq (_attributes mid) (_attributes newc) then
                  return (_attributes old,return ())
                else
                  runElementDiff f n (_attributes old) (_attributes mid) (_attributes newc)
        didMount
        return $ Managed (_node old) (_tag old) a' (_constr old)
      else do
        when isFG (triggerBackground old)
        new' <- buildAndEmbedMaybe f doc ch isFG Nothing new
        replace old new'
        when isFG (triggerForeground new')
        didUnmount <- cleanup f [old]
        delete old
        didUnmount
        return new'

    go' old _ n = do
      n' <- buildAndEmbedMaybe f doc ch isFG Nothing n
      case old of
        t@(Text (Just o) _) ->
          forM_ (_node n') (swapContent o)
        _ -> do
          replace old n'
          didUnmount <- cleanup f [old]
          delete old
          didUnmount
      return n'

    diffChildren :: ENode -> [HTML e] -> [Atom e] -> [Atom e] -> IO [Atom e]
    diffChildren n olds mids news = do
      withLatest olds mids news
      where

        withLatest :: [HTML e] -> [Atom e] -> [Atom e] -> IO [Atom e]
        withLatest = go_
          where

            go_ :: [HTML e] -> [Atom e] -> [Atom e] -> IO [Atom e]
            go_ [] _ news =
              mapM (fmap toAtom . buildAndEmbedMaybe f doc ch isFG (Just n)) news

            go_ olds _ [] = do
              didUnmount <- cleanup f olds
              mapM_ delete olds
              didUnmount
              return []

            go_ (old:olds) (mid:mids) (new:news) =
              let
                remove = do
                  didUnmount <- cleanup f [old]
                  delete old
                  didUnmount

                continue :: HTML e -> IO [Atom e]
                continue up = do
                  upds <-
                    if reallyUnsafeEq mids news then return (map toAtom olds) else
                      withLatest olds mids news
                  return ((toAtom up):upds)

              in
                if reallyUnsafeEq mid new then continue old else
                  case (construct mid,construct new) of
                    (NullHTML {},NullHTML {}) ->
                      continue old

                    (_,NullHTML {}) -> do
                      new' <- buildHTML doc ch isFG f new
                      replace old new'
                      didUnmount <- cleanup f [old]
                      delete old
                      didUnmount
                      continue new'

                    (NullHTML{},_) -> do
                      new' <- buildHTML doc ch isFG f new
                      replace old new'
                      didUnmount <- cleanup f [old]
                      delete old
                      didUnmount
                      continue new'

                    (m,n) -> do
                      new <- go old mid new
                      continue new

    -- note that keyed nodes are filtered for NullHTMLs during construction
    diffKeyedChildren :: ENode -> [(Int,HTML e)] -> [(Int,Atom e)] -> [(Int,Atom e)] -> IO [(Int,Atom e)]
    diffKeyedChildren n = go_ 0
      where

        go_ :: Int -> [(Int,HTML e)] -> [(Int,Atom e)] -> [(Int,Atom e)] -> IO [(Int,Atom e)]
        go_ i a m b = do
          if reallyUnsafeEq m b then do
            return (fmap (fmap toAtom) a)
          else
            go__ i a m b
          where

            go__ :: Int -> [(Int,HTML e)] -> [(Int,Atom e)] -> [(Int,Atom e)] -> IO [(Int,Atom e)]
            go__ _ [] _ news = do
              forM news $ \(bkey,b) -> do
                new <- buildAndEmbedMaybe f doc ch isFG (Just n) b
                return (bkey,toAtom new)

            go__ _ olds _ [] = do
              forM_ olds $ \(_,a) -> do
                didUnmount <- cleanup f [a]
                delete a
                didUnmount
              return []

            go__ i old@((akey,a):as) mid@((mkey,m):ms) new@((bkey,b):bs)
              | prettyUnsafeEq akey bkey = do
                  new <- go' a m b
                  let !i' = i + 1
                  rest <- go_ i' as ms bs
                  return $ (akey,toAtom new):rest

              | otherwise =
                  case (as,ms,bs) of
                    ((akey',a'):as',(mkey',m'):ms',(bkey',b'):bs')
                      -- swap 2
                      | prettyUnsafeEq bkey akey' && prettyUnsafeEq akey bkey' -> do
                          new1 <- go' a m b'
                          new2 <- go' a' m' b
                          let !i' = i + 2
                          rest <- go_ i' as' ms' bs'
                          return $ (akey',toAtom new1):(akey,toAtom new2):rest

                      -- insert
                      | prettyUnsafeEq akey bkey' -> do
                          new <- buildAndEmbedMaybe f doc ch isFG Nothing b
                          insertAt n i new
                          let !i' = i + 1
                          rest <- go_ i' old mid bs
                          return $ (bkey,toAtom new):rest

                      -- delete
                      | otherwise -> do
                          didUnmount <- cleanup f [a]
                          delete a
                          didUnmount
                          if prettyUnsafeEq bkey akey' then
                            go_ i ((akey',a'):as) ((mkey',m'):ms) new
                          else do
                            -- replace
                            new <- buildAndEmbedMaybe f doc ch isFG Nothing b
                            insertAt n i new
                            let !i' = i + 1
                            rest <- go_ i' as ms bs
                            return $ (bkey,toAtom new):rest

                    _ | prettyUnsafeEq akey bkey -> do
                          new <- go a m b
                          let !i' = i + 1
                          rest <- go_ i' as ms bs
                          return $ (akey,toAtom new):rest

                      | otherwise ->
                          case (old,new) of
                            ([_],(_:(bkey',b'):_)) ->
                              if prettyUnsafeEq akey bkey' then do
                                new <- buildAndEmbedMaybe f doc ch isFG Nothing b
                                insertAt n i new
                                let !i' = i + 1
                                rest <- go_ i' old mid bs
                                return $ (bkey,toAtom new):rest
                              else do
                                didUnmount <- cleanup f [a]
                                delete a
                                didUnmount
                                go_ i as ms ((bkey,b):bs)
                            _ -> do
                              didUnmount <- cleanup f [a]
                              delete a
                              didUnmount
                              go_ i as ms ((bkey,b):bs)

{-# NOINLINE applyStyleDiffs #-}
applyStyleDiffs :: ENode -> [(Txt,Txt)] -> [(Txt,Txt)] -> IO [(Txt,Txt)]
applyStyleDiffs el olds0 news0 = do
#ifndef __GHCJS__
  return news0
#else
  obj <- O.create
  res <- go obj olds0 news0
  setStyle_js el obj
  return res
  where
    go obj = go'
      where
        go' [] news =
          mapM (\new@(nm,val) -> O.setProp nm (M.pToJSVal val) obj >> return new) news

        go' olds [] =
          mapM (\old@(nm,_) -> set_property_null_js obj nm >> return old) olds

        go' (old@(oname,oval):olds) (new@(nname,nval):news) =
          let
            remove =
              set_property_null_js obj oname

            set =
              O.setProp nname (M.pToJSVal nval) obj

            goRest =
              go' olds news

            continue up = do
              upds <- if reallyUnsafeEq olds news then return olds else goRest
              return (up:upds)

            update = do
              set
              continue new

            replace = do
              remove
              update

          in
            if reallyUnsafeEq old new then
                continue old
            else
              if prettyUnsafeEq oname nname then
                update
              else do
                replace
#endif

{-# NOINLINE runElementDiff #-}
runElementDiff :: (Code e IO () -> IO ()) -> ENode -> [Feature e] -> [Feature e] -> [Feature e] -> IO ([Feature e],IO ())
runElementDiff f el os0 ms0 ns0 = do
#ifndef __GHCJS__
    return (ns0,return ())
#else
    dm_ <- newIORef (return ())
    fs <- go dm_ os0 ms0 ns0
    dm <- readIORef dm_
    return (fs,dm)
  where

    go dm_ [] [] news = do
      dm <- readIORef dm_
      go' news
      where
        go' [] = return []
        go' (n:ns) = do
          dm <- readIORef dm_
          (f,dm') <- setAttribute_ f True el n dm
          writeIORef dm_ dm'
          fs <- go' ns
          return (f:fs)

    go dm_ olds _ [] =
      mapM (\old -> removeAttribute_ el old >> return NullFeature) olds

    go dm_ (old:olds) (mid:mids) (new:news) =
      let
        remove =
          removeAttribute_ el old

        set = do
          dm <- readIORef dm_
          (f,dm') <- setAttribute_ f True el new dm
          writeIORef dm_ dm'
          return f

        goRest =
          go dm_ olds mids news

        continue up = do
          upds <- if reallyUnsafeEq mids news then do
                    return olds
                  else do
                    goRest
          return (up:upds)

        update = do
          new' <- set
          continue new'

        replace = do
          remove
          update

      in
        if reallyUnsafeEq mid new then do
          continue old
        else
          case (mid,new) of
            (_,NullFeature) -> do
              remove
              continue new

            (NullFeature,_) ->
              update

            (Property nm oldV,Property nm' newV) ->
              if prettyUnsafeEq nm nm' then
                if prettyUnsafeEq oldV newV then
                  continue old
                else
                  update
              else
                replace

            (StyleF oldS,StyleF newS) -> do
              -- we know /something/ changed
              applyStyleDiffs el oldS newS
              continue new

            (Attribute nm val,Attribute nm' val') ->
              if prettyUnsafeEq nm nm' then
                if prettyUnsafeEq val val' then do
                  continue old
                else do
                  update
              else
                replace

            (OnE en e os g _,OnE en' e' os' g' _) ->
              if Txt.null en && Txt.null en' && prettyUnsafeEq e e' && prettyUnsafeEq os os' && reallyUnsafeEq g g' || not (Txt.null en) && not (Txt.null en') && prettyUnsafeEq en en' then
                continue old
              else
                replace

            (OnDoc en e os g _,OnDoc en' e' os' g' _) ->
              if Txt.null en && Txt.null en' && prettyUnsafeEq e e' && prettyUnsafeEq os os' && reallyUnsafeEq g g' || not (Txt.null en) && not (Txt.null en') && prettyUnsafeEq en en' then
                continue old
              else
                replace

            (OnWin en e os g _,OnWin en' e' os' g' _) ->
              if Txt.null en && Txt.null en' && prettyUnsafeEq e e' && prettyUnsafeEq os os' && reallyUnsafeEq g g' || not (Txt.null en) && not (Txt.null en') && prettyUnsafeEq en en' then
                continue old
              else do
                replace

            (OnBuild en e,OnBuild en' e') ->
              if Txt.null en && Txt.null en' && reallyUnsafeEq e e' || not (Txt.null en) && not (Txt.null en') && prettyUnsafeEq en en' then
                continue old
              else do
                f e'
                replace

            (OnDestroy en e,OnDestroy en' e') ->
              if Txt.null en && Txt.null en' && reallyUnsafeEq e e' || not (Txt.null en) && not (Txt.null en') && prettyUnsafeEq en en' then
                continue old
              else do
                f e
                replace

            (OnWillMount en g,OnWillMount en' g') ->
              if Txt.null en && Txt.null en' && reallyUnsafeEq g g' || not (Txt.null en) && not (Txt.null en') && prettyUnsafeEq en en' then
                continue old
              else
                replace

            (OnDidMount en g,OnDidMount en' g') ->
              if Txt.null en && Txt.null en' && reallyUnsafeEq g g' || not (Txt.null en) && not (Txt.null en') && prettyUnsafeEq en en' then
                continue old
              else
                replace

            (OnUpdate en m g,OnUpdate en' m' g') ->
              if Txt.null en && Txt.null en' && reallyVeryUnsafeEq g g' && reallyVeryUnsafeEq m m' || not (Txt.null en) && not (Txt.null en') && prettyUnsafeEq en en' then
                continue old
              else do
                g' m' el
                replace

            (OnModel en m g,OnModel en' m' g') ->
              if Txt.null en && Txt.null en' && reallyVeryUnsafeEq g g' && reallyVeryUnsafeEq m m' || not (Txt.null en) && not (Txt.null en') && prettyUnsafeEq en en' then
                continue old
              else do
                f (g' m' el)
                replace

            (OnWillUnmount en g,OnWillUnmount en' g') ->
              if Txt.null en && Txt.null en' && reallyUnsafeEq g g' || not (Txt.null en) && not (Txt.null en') && prettyUnsafeEq en en' then
                continue old
              else
                replace

            (OnDidUnmount en g,OnDidUnmount en' g') ->
              if Txt.null en && Txt.null en' && reallyUnsafeEq g g' || not (Txt.null en) && not (Txt.null en') && prettyUnsafeEq en en' then
                continue old
              else
                replace

            (LinkTo olda oldv, LinkTo newa newv) ->
              if prettyUnsafeEq olda newa && reallyUnsafeEq oldv newv then
                continue old
              else
                replace

            (SVGLinkTo olda oldv, SVGLinkTo newa newv) ->
              if prettyUnsafeEq olda newa && reallyUnsafeEq oldv newv then
                continue old
              else
                replace

            (XLink olda oldv,XLink newa newv) ->
              if prettyUnsafeEq olda newa then
                if prettyUnsafeEq oldv newv then
                  continue old
                else
                  update
              else
                replace

            _ ->
              replace
#endif

{-# NOINLINE removeAttribute_ #-}
removeAttribute_ :: ENode -> Feature e -> IO ()
removeAttribute_ element attr =
#ifndef __GHCJS__
  return ()
#else
  case attr of
    Property nm _ ->
      set_element_property_null_js element nm

    Attribute nm _ ->
      E.removeAttribute element nm

    LinkTo _ unreg -> do
      forM_ unreg id
      E.removeAttribute element ("href" :: Txt)

    OnE _ _ _ _ unreg ->
      forM_ unreg id

    OnDoc _ _ _ _ unreg ->
      forM_ unreg id

    OnWin _ _ _ _ unreg ->
      forM_ unreg id

    StyleF styles -> do
      obj <- O.create
      forM_ styles $ \(nm,val) -> O.unsafeSetProp nm (M.pToJSVal val) obj
      clearStyle_js element obj

    SVGLinkTo _ unreg -> do
      forM_ unreg id
      E.removeAttributeNS element (Just ("http://www.w3.org/1999/xlink" :: Txt)) ("xlink:href" :: Txt)

    XLink nm _ ->
      E.removeAttributeNS element (Just ("http://www.w3.org/1999/xlink" :: Txt)) nm

    _ -> return ()
#endif

onRaw :: ENode -> Txt -> Atomic.Attribute.Options -> (IO () -> Obj -> IO ()) -> IO (IO ())
onRaw el nm os f = do
  stopper <- newIORef undefined
  stopListener <- Ev.on el (Ev.unsafeEventName nm :: Ev.EventName E.Element T.CustomEvent) $ do
    ce <- Ev.event
    when (_preventDef os) Ev.preventDefault
    when (_stopProp os) Ev.stopPropagation
    stop <- liftIO $ readIORef stopper
    liftIO $ f stop (unsafeCoerce ce)
  writeIORef stopper stopListener
  return stopListener

{-# NOINLINE setAttribute_ #-}
setAttribute_ :: (Code e IO () -> IO ()) -> Bool -> ENode -> Feature e -> IO () -> IO (Feature e,IO ())
setAttribute_ c diffing element attr didMount =
#ifndef __GHCJS__
  return (attr,return ())
#else
  case attr of
    NullFeature ->
      return (NullFeature,didMount)

    Property nm v -> do
      set_property_js element nm v
      return (attr,didMount)

    -- optimize this; we're doing a little more work than necessary!
    Attribute nm val -> do
      E.setAttribute element nm val
      return (attr,didMount)

    LinkTo href _ -> do
      E.setAttribute element ("href" :: Txt) href
      stopListener <-
        Ev.on
          element
          (Ev.unsafeEventName "click" :: Ev.EventName E.Element T.MouseEvent)
            $ do Ev.preventDefault
                 liftIO $ do
                   win <- getWindow
                   Just hist <- W.getHistory win
                   H.pushState hist (M.pToJSVal (0 :: Int)) ("" :: Txt) href
                   triggerPopstate_js
                   scrollToTop
      return (LinkTo href (Just stopListener),didMount)

    OnE en ev os f _ -> do
      stopper <- newIORef undefined
      stopListener <-
        Ev.on
          element
          (Ev.unsafeEventName ev :: Ev.EventName E.Element T.CustomEvent) -- for the type checking; actually just an object
            $ do ce <- Ev.event
                 when (_preventDef os) Ev.preventDefault
                 when (_stopProp os) Ev.stopPropagation
                 stop <- liftIO $ readIORef stopper
                 liftIO $ f stop element (unsafeCoerce ce) >>= mapM_ c
                 return ()
      writeIORef stopper stopListener
      return (OnE en ev os f (Just stopListener),didMount)

    OnDoc en ev os f _ -> do
      stopper <- newIORef undefined
      doc <- getDocument
      stopListener <-
        Ev.on
          doc
          (Ev.unsafeEventName ev :: Ev.EventName Doc T.CustomEvent) -- for the type checking; actually just an object
            $ do ce <- Ev.event
                 when (_preventDef os) Ev.preventDefault
                 when (_stopProp os) Ev.stopPropagation
                 stop <- liftIO $ readIORef stopper
                 liftIO $ f stop element doc (unsafeCoerce ce) >>= mapM_ c
                 return ()
      writeIORef stopper stopListener
      return (OnDoc en ev os f (Just stopListener),didMount)

    OnWin en ev os f _ -> do
      stopper <- newIORef undefined
      win <- getWindow
      stopListener <-
        Ev.on
          win
          (Ev.unsafeEventName ev :: Ev.EventName Win T.CustomEvent) -- for the type checking; actually just an object
            $ do ce <- Ev.event
                 when (_preventDef os) Ev.preventDefault
                 when (_stopProp os) Ev.stopPropagation
                 stop <- liftIO $ readIORef stopper
                 liftIO $ f stop element win (unsafeCoerce ce) >>= mapM_ c
                 return ()
      writeIORef stopper stopListener
      return (OnWin en ev os f (Just stopListener),didMount)

    OnBuild _ e -> do
      c e
      return (attr,didMount)

    OnWillMount _ f -> do
      f element
      return (attr,didMount)

    OnDidMount _ f -> do
      return (attr,if diffing then didMount else f element >> didMount)

    StyleF styles -> do
      obj <- O.create
      forM_ styles $ \(nm,val) -> O.unsafeSetProp nm (M.pToJSVal val) obj
      setStyle_js element obj
      return (attr,didMount)

    SVGLinkTo href _ -> do
      E.setAttributeNS element (Just ("http://www.w3.org/1999/xlink" :: Txt)) ("xlink:href" :: Txt) href
      stopListener <-
        Ev.on
          element
          (Ev.unsafeEventName "click" :: Ev.EventName E.Element T.MouseEvent)
            $ do Ev.preventDefault
                 liftIO $ do
                   win <- getWindow
                   Just hist <- W.getHistory win
                   H.pushState hist (M.pToJSVal (0 :: Int)) ("" :: Txt) href
                   triggerPopstate_js
                   scrollToTop
      return (SVGLinkTo href (Just stopListener),didMount)

    XLink nm val -> do
      E.setAttributeNS element (Just ("http://www.w3.org/1999/xlink" :: Txt)) nm val
      return (attr,didMount)

    _ -> return (attr,didMount)
#endif

{-# NOINLINE cleanupAttr #-}
cleanupAttr :: (Code e IO () -> IO ()) -> ENode -> Feature e -> IO () -> IO (IO ())
cleanupAttr f element attr didUnmount =
#ifndef __GHCJS__
  return didUnmount
#else
  case attr of
    SVGLinkTo _ unreg -> do
      forM_ unreg id
      return didUnmount
    LinkTo _ unreg -> do
      forM_ unreg id
      return didUnmount
    OnE _ _ _ _ unreg -> do
      forM_ unreg id
      return didUnmount
    OnDoc _ _ _ _ unreg -> do
      forM_ unreg id
      return didUnmount
    OnWin _ _ _ _ unreg -> do
      forM_ unreg id
      return didUnmount
    OnDestroy _ e -> do
      f e
      return didUnmount
    OnWillUnmount _ g -> do
      g element
      return didUnmount
    OnDidUnmount _ g -> return (didUnmount >> g element)
    _ -> return didUnmount
#endif

getWindow :: MonadIO c => c Win
getWindow =
#ifdef __GHCJS__
  fromJust <$> liftIO DOM.currentWindow
#else
  return ()
#endif

scrollToTop :: MonadIO c => c ()
scrollToTop = do
  win <- getWindow
#ifdef __GHCJS__
  W.scrollTo win 0 0
#else
  return win
#endif

getDocument :: (MonadIO c) => c Doc
getDocument = do
#ifdef __GHCJS__
    win <- getWindow
    Just doc <- liftIO $ W.getDocument win
#else
    let doc = ()
#endif
    return doc

getFirstElementByTagName :: MonadIO c => Txt -> c ENode
getFirstElementByTagName nm = do
  doc     <- getDocument
#ifdef __GHCJS__
  Just nl <- D.getElementsByTagName doc nm
  Just b  <- N.item nl 0
  return $ T.castToElement b
#else
  return doc
#endif

getLocation :: (MonadIO c) => c Loc
getLocation = do
  win <- getWindow
#ifdef __GHCJS__
  Just loc <- W.getLocation win
#else
  let loc = ()
#endif
  return loc

redirect :: MonadIO c => Txt -> c ()
redirect redir = do
  loc <- getLocation
#ifdef __GHCJS__
  L.assign loc redir
#else
  return loc
#endif

makePrisms ''HTML
makeLenses ''HTML
