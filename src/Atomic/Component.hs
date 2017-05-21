{-# language UndecidableInstances #-}
{-# language DeriveDataTypeable #-}
{-# language StandaloneDeriving #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language DeriveFunctor #-}
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

import Control.Lens (Iso,iso,makePrisms,makeLenses)
import Control.Lens.Plated (Plated(..))
import Control.Lens.At
import Control.Lens.Prism

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

data SomeAtom = SomeAtom (forall e. Atom e)

data SomeView st where
  SomeView :: (st -> ((st -> st) -> IO () -> IO ()) -> Atom e) -> SomeView st

data Atom e where
  -- NullAtom must have a presence on the page for proper diffing
  NullAtom
    :: { _node :: !(Maybe ENode)
       } -> Atom e

  Text
    ::  { _tnode      :: !(Maybe TNode)
        , _content    :: !Txt
        } -> Atom e

  Raw
    :: { _node        :: !(Maybe ENode)
       , _tag         :: !Txt
       , _attributes  :: ![Feature e]
       , _content     :: !Txt
       } -> Atom e

  Atom
    ::  { _node       :: !(Maybe ENode)
        , _tag        :: !Txt
        , _attributes :: ![Feature e]
        , _atoms      :: ![Atom e]
        } -> Atom e
  KAtom
    ::  { _node       :: !(Maybe ENode)
        , _tag        :: !Txt
        , _attributes :: ![Feature e]
        , _keyed      :: ![(Int,Atom e)]
        } -> Atom e

  STAtom
    :: { _stmodel  :: !(model)
       , _stid     :: !Int
       , _ststate  :: !st
       , _strecord :: !(Maybe (IORef (st,SomeView st,SomeAtom,SomeAtom)))
       , _stview   :: !(st -> ((st -> st) -> IO () -> IO ()) -> Atom e)
       , _stupdate :: !((st -> st) -> IO () -> IO ())
       } -> Atom e

  -- TODO: SVG keyed node
  SVGAtom
    ::  { _node       :: !(Maybe ENode)
        , _tag        :: !Txt
        , _attributes :: ![Feature e]
        , _atoms      :: ![Atom e]
        } -> Atom e

  KSVGAtom
    ::  { _node       :: !(Maybe ENode)
        , _tag        :: !Txt
        , _attributes :: ![Feature e]
        , _keyed      :: ![(Int,Atom e)]
        } -> Atom e

  Managed
    ::  { _node       :: !(Maybe ENode)
        , _tag        :: !Txt
        , _attributes :: ![Feature e]
        , _constr     :: !Constr
        } -> Atom e

deriving instance Functor Atom

instance Default (Atom a) where
  def = nil

instance Plated (Atom e) where
  plate f (STAtom _ _ st iorec v _) =
    case iorec of
      Nothing -> plate f $ v st (\_ _ -> return ())
      Just ref -> plate f $ v ((\(a,_,_,_) -> a) $ unsafePerformIO (readIORef ref)) (\_ _ -> return ())
  plate f (KAtom n t as ks) = KAtom n t as <$> traverse (\(i,k) -> fmap (\k' -> (i,k')) (f k)) ks
  plate f (Atom n t as cs) = Atom n t as <$> traverse f cs
  plate f (SVGAtom n t as cs) = SVGAtom n t as <$> traverse f cs
  plate f (KSVGAtom n t as ks) = KSVGAtom n t as <$> traverse (\(i,k) -> fmap (\k' -> (i,k')) (f k)) ks
  plate _ a = pure a -- ? :(

type instance Index (Atom e) = Int
type instance IxValue (Atom e) = Atom e

instance Ixed (Atom e) where
  ix k f (STAtom _ _ st iorec v _) =
    case iorec of
      Nothing -> ix k f $ v st (\_ _ -> return ())
      Just ref -> ix k f $ v ((\(a,_,_,_) -> a) $ unsafePerformIO (readIORef ref)) (\_ _ -> return ())
  ix k f (Atom n t as cs) = Atom n t as <$> ix k f cs
  ix k f (KAtom n t as ks) = KAtom n t as <$> go ks k
    where
      go [] _ = pure []
      go ((i,a):ias) 0 = fmap (:ias) (fmap (\a' -> (i,a')) (f a))
      go (ia:ias) n = (ia:) <$> (go ias $! n - 1)
  ix k f (SVGAtom n t as cs) = SVGAtom n t as <$> ix k f cs
  ix k f (KSVGAtom n t as ks) = KSVGAtom n t as <$> go ks k
    where
      go [] _ = pure []
      go ((i,a):ias) 0 = fmap (:ias) (fmap (\a' -> (i,a')) (f a))
      go (ia:ias) n = (ia:) <$> (go ias $! n - 1)
  ix k f a = f a -- ? :(

instance ToJSON (Atom e) where
  toJSON a =
#ifdef __GHCJS__
    objectValue $
#endif
      go a
    where
      go (STAtom _ _ st iorec v _) =
        case iorec of
          Nothing -> go $ v st (\_ _ -> return ())
          Just ref -> go $ v ((\(a,_,_,_) -> a) $ unsafePerformIO (readIORef ref)) (\_ _ -> return ())
      go (Text _ c) = object [ "type" .= ("text" :: Txt), "content" .= c]
      go (Raw _ t as c) = object [ "type" .= ("raw" :: Txt), "tag" .= t, "attrs" .= toJSON as, "content" .= c ]
      go (KAtom _ t as ks) = object [ "type" .= ("keyed" :: Txt), "tag" .= t, "attrs" .= toJSON as, "keyed" .= toJSON ks ]
      go (Atom _ t as cs) = object [ "type" .= ("atom" :: Txt), "tag" .= t, "attrs" .= toJSON as, "children" .= toJSON cs ]
      go (KSVGAtom _ t as ks) = object [ "type" .= ("keyedsvg" :: Txt), "tag" .= t, "attrs" .= toJSON as, "keyed" .= toJSON ks]
      go (SVGAtom _ t as cs) = object [ "type" .= ("svg" :: Txt), "tag" .= t, "attrs" .= toJSON as, "children" .= toJSON cs ]
      go _ = object [ "type" .= ("null" :: Txt) ]

instance FromJSON (Atom e) where
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
          pure $ KAtom Nothing t as ks
        "atom" -> do
          t <- o .: "tag"
          as <- o .: "attrs"
          cs <- o .: "children"
          pure $ Atom Nothing t as cs
        "keyedsvg" -> do
          t <- o .: "tag"
          as <- o .: "attrs"
          ks <- o .: "keyed"
          pure $ KSVGAtom Nothing t as ks
        "svg" -> do
          t <- o .: "tag"
          as <- o .: "attrs"
          cs <- o .: "children"
          pure $ SVGAtom Nothing t as cs
        "null" -> pure $ NullAtom Nothing
        _ -> Ef.Base.empty

instance Eq (Atom e) where
  (==) (NullAtom _) (NullAtom _) =
    True

  (==) (Text _ t) (Text _ t') =
    prettyUnsafeEq t t'

  (==) (Raw _ t fs c) (Raw _ t' fs' c') =
    prettyUnsafeEq t t' && prettyUnsafeEq fs fs' && prettyUnsafeEq c c'

  (==) (KAtom _ t fs ks) (KAtom _ t' fs' ks') =
    prettyUnsafeEq t t' && prettyUnsafeEq fs fs' && prettyUnsafeEq ks ks'

  (==) (Atom _ t fs cs) (Atom _ t' fs' cs') =
    prettyUnsafeEq t t' && prettyUnsafeEq fs fs' && prettyUnsafeEq cs cs'

  (==) (STAtom m k st _ v _) (STAtom m' k' st' _ v' _) =
    k == k' && reallyVeryUnsafeEq m m' && reallyVeryUnsafeEq st st' && reallyVeryUnsafeEq v v'

  (==) (KSVGAtom _ t fs ks) (KSVGAtom _ t' fs' ks') =
    prettyUnsafeEq t t' && prettyUnsafeEq fs fs' && prettyUnsafeEq ks ks'

  (==) (SVGAtom _ t fs cs) (SVGAtom _ t' fs' cs') =
    prettyUnsafeEq t t' && prettyUnsafeEq fs fs' && prettyUnsafeEq cs cs'

  (==) (Managed _ t fs c) (Managed _ t' fs' c') =
    prettyUnsafeEq t t' && prettyUnsafeEq fs fs' && prettyUnsafeEq c c'

  (==) _ _ =
    False

instance Cond (Atom e) where
  nil = NullAtom Nothing

instance IsString (Atom e) where
  fromString = text . fromString

instance FromTxt (Atom e) where
  fromTxt = text

instance {-# OVERLAPS #-} IsString [Atom e] where
  fromString s = [fromString s]

instance FromTxt [Atom e] where
  fromTxt t = [fromTxt t]

_atom :: ([Feature e] -> [Atom e] -> Atom e) -> Prism (Atom e) (Atom e) ([Feature e],[Atom e]) ([Feature e],[Atom e])
_atom x = prism (uncurry x) $ \a ->
  case x [] [] of
    Atom _ t _ _ ->
      case a of
        Atom _ t' fs as ->
          if t == t' then
            Right (fs,as)
          else
            Left a
        _ -> Left a
    _ -> Left a

_svg :: ([Feature e] -> [Atom e] -> Atom e) -> Prism (Atom e) (Atom e) ([Feature e],[Atom e]) ([Feature e],[Atom e])
_svg x = prism (uncurry x) $ \a ->
  case x [] [] of
    SVGAtom _ t _ _ ->
      case a of
        SVGAtom _ t' fs as ->
          if t == t' then
            Right (fs,as)
          else
            Left a
        _ -> Left a
    _ -> Left a

_list :: ([Feature e] -> [(Int,Atom e)] -> Atom e) -> Prism (Atom e) (Atom e) ([Feature e],[(Int,Atom e)]) ([Feature e],[(Int,Atom e)])
_list x = prism (uncurry x) $ \a ->
  case x [] [] of
    KAtom _ t _ _ ->
      case a of
        KAtom _ t' fs ks ->
          if t == t' then
            Right (fs,ks)
          else
            Left a
        _ -> Left a
    _ -> Left a

_svgList :: ([Feature e] -> [(Int,Atom e)] -> Atom e) -> Prism (Atom e) (Atom e) ([Feature e],[(Int,Atom e)]) ([Feature e],[(Int,Atom e)])
_svgList x = prism (uncurry x) $ \a ->
  case x [] [] of
    KSVGAtom _ t _ _ ->
      case a of
        KSVGAtom _ t' fs as ->
          if t == t' then
            Right (fs,as)
          else
            Left a
        _ -> Left a
    _ -> Left a

_raw :: ([Feature e] -> [Atom e] -> Atom e) -> Prism (Atom e) (Atom e) ([Feature e],Txt) ([Feature e],Txt)
_raw x = prism (uncurry (raw x)) $ \a ->
  case x [] [] of
    Raw _ t _ _ ->
      case a of
        Raw _ t' fs c ->
          if t == t' then
            Right (fs,c)
          else
            Left a
        _ -> Left a
    _ -> Left a

_nil :: Prism (Atom e) (Atom e) () ()
_nil = prism (const (NullAtom Nothing)) $ \a ->
  case a of
    NullAtom _ -> Right ()
    _ -> Left a

_text :: Prism (Atom e) (Atom e) Txt Txt
_text = prism text $ \a ->
  case a of
    Text _ t -> Right t
    _ -> Left a

-- TODO: _st

_component :: ([Feature e] -> [Atom e] -> Atom e) -> Prism (Atom e) (Atom e) ([Feature e],Constr) ([Feature e],Constr)
_component x = prism build $ \a ->
  case component x [] (undefined :: Component '[] ()) of
    Managed _ t _ _ ->
      case a of
        Managed _ t' fs c ->
          if t == t' then
            Right (fs,c)
          else
            Left a
    _ -> Left a
  where
    build (fs,c) =
      case component x [] (undefined :: Component '[] ()) of
        Managed _ t _ _ ->
          Managed Nothing t fs c

witness :: Atom Void -> Atom a
witness = vacuous

mkAtom :: Txt -> [Feature e] -> [Atom e] -> Atom e
mkAtom _tag _attributes _atoms =
  let _node = Nothing
  in Atom {..}

mkSVGAtom :: Txt -> [Feature e] -> [Atom e] -> Atom e
mkSVGAtom _tag _attributes _atoms =
  let _node = Nothing
  in SVGAtom {..}

text :: Txt -> Atom e
text _content =
  let _tnode = Nothing
  in Text {..}

raw :: ([Feature e] -> [Atom e] -> Atom e) -> [Feature e] -> Txt -> Atom e
raw x _attributes _content =
  case x [] [] of
    Atom _ _tag _ _ ->
      let _node = Nothing
      in Raw {..}
    SVGAtom _ _tag _ _ ->
      let _node = Nothing
      in Raw {..}

list :: ([Feature e] -> [Atom e] -> Atom e) -> [Feature e] -> [(Int,Atom e)] -> Atom e
list x _attributes _keyed =
  case x [] [] of
    Atom _ _tag _ _ ->
      let
        _node = Nothing
      in
        KAtom {..}
    SVGAtom _ _tag _ _ ->
      let
        _node = Nothing
      in
        KSVGAtom {..}

-- The hacks used to implement this atom type are somewhat finicky. The model tracks variables
-- for changes; if any of the variables within the model are updated or moved, a diff will be
-- performed. This is how changes external to an `st` are injected into an `st`; if an `st` uses
-- state from a `Component`s model and they aren't tracked via the `st` model, those changes will
-- not be updated when the `Component`s model updates. The same rules apply to nested/inheriting
-- `st` atoms.
--
-- The major purposes for this atome type are:
--   1. The implementation of extensible and highly configurable UI frameworks with the
--      aim of reducing configuration burdens by avoiding carrying framework state within
--      user-created components.
--   2. Stateful subviews a la form inputs, local animations, etc....
--
-- Major caveat: If the list of elements holding a st atom changes such that the diff algorithm
--               must recreate the st element, it will be reset to its original state. This would
--               happen if the position of the st element within the list changes. If a variable
--               length list of children is required, either careful placement for the st element,
--               or the use of NullAtoms as placeholders, or some uses of keyed atoms can overcome
--               this problem.
st :: forall model st e. Int -> model -> st -> (st -> ((st -> st) -> IO () -> IO ()) -> Atom e) -> Atom e
st k watch_model initial_st view = STAtom watch_model k initial_st Nothing view (\_ _ -> return ())

constant :: Atom e -> Atom e
constant a = st 0 () () $ \_ _ -> a

component :: ([Feature e] -> [Atom e] -> Atom e)
          -> (forall ts' ms' m. IsComponent' ts' ms' m => [Feature e] -> Component' ts' ms' m -> Atom e)
component f = \as c ->
  case f [] [] of
    Atom _ t _ _ -> Managed Nothing t as (Component' c)
    _ -> error "Incorrect usage of construct; Components may only be embedded in plain html Atoms."

hashed :: Hashable a => ([Feature e] -> [Atom e] -> Atom e) -> [Feature e] -> [(a,Atom e)] -> Atom e
hashed x _attributes _keyed0 = list x _attributes (map (first hash) _keyed0)

css :: CSS -> Atom e
css = css' False

css' :: Bool -> CSS -> Atom e
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

scss :: StaticCSS -> Atom e
scss = scss' False

scss' :: Bool -> StaticCSS -> Atom e
scss' b = raw (mkAtom "style") [ Property "type" "text/css", Property "scoped" (if b then "true" else "") ] . cssText

styles :: CSS -> Atom e
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


-- -- Useful for standalone components without a Atomic root.
-- renderComponent' :: IsComponent' ts ms m => Component' ts ms m -> ENode -> Atom (Code ms IO ()) -> IO (Atom (Code ms IO ()))
-- renderComponent' a parent html = do
--   let f e = void $ with a e
--   doc <- getDocument
--   html' <- buildHTML doc f html
--   embed_ parent html'
--   return html'

-- rebuild finds managed nodes and re-embeds them in case they were
-- removed for other uses
rebuild :: Atom e -> IO ()
rebuild h =
#ifndef __GHCJS__
    return ()
#else
    go h
  where
    go STAtom {..}  =
      forM_ _strecord $ \ref -> do
        (_,_,SomeAtom e,_) <- readIORef ref
        rebuild e
    go Atom {..}    = mapM_ go _atoms
    go SVGAtom {..} = mapM_ go _atoms
    go KAtom {..}   = mapM_ (go . snd) _keyed
    go m@Managed {..} =
      case _constr of
        Component' c -> do
          mi_ <- lookupComponent (key c)
          forM_ mi_ $ \ComponentRecord {..} -> do
            ComponentView {..} <- readIORef crView
            rebuild cvCurrentLive
            forM_ _node $ \node ->
              embed_ node cvCurrentLive
    go _ =
      return ()
#endif


triggerBackground :: MonadIO m => Atom e -> m ()
triggerBackground = go
  where
    bg Component {..} = do
      mc <- lookupComponent key
      case mc of
        Nothing -> return ()
        Just ComponentRecord {..} -> do
          let ComponentHooks _ bg _= crHooks
          publish bg ()
          ComponentView {..} <- liftIO $ readIORef crView
          go $ unsafeCoerce cvCurrentLive

    go STAtom {..}  =
      forM_ _strecord $ \ref -> do
        (_,_,SomeAtom e,_) <- liftIO $ readIORef ref
        go (unsafeCoerce e)
    go Atom {..}    = mapM_ go _atoms
    go SVGAtom {..} = mapM_ go _atoms
    go KAtom {..}   = mapM_ (go . snd) _keyed
    go KSVGAtom {..} = mapM_ (go . snd) _keyed
    go m@Managed {..} = case _constr of Component' c -> bg (unsafeCoerce c)
    go _ = return ()

triggerForeground :: MonadIO m => Atom e -> m ()
triggerForeground = go
  where
    fg Component {..} = do
      mc <- lookupComponent key
      case mc of
        Nothing -> return ()
        Just ComponentRecord {..} -> do
          let ComponentHooks _ _ fg = crHooks
          publish fg ()
          ComponentView {..} <- liftIO $ readIORef crView
          go (unsafeCoerce cvCurrentLive)

    go STAtom {..}  =
      forM_ _strecord $ \ref -> do
        (_,_,SomeAtom e,_) <- liftIO $ readIORef ref
        go (unsafeCoerce e)
    go Atom {..}    = mapM_ go _atoms
    go SVGAtom {..} = mapM_ go _atoms
    go KAtom {..}   = mapM_ (go . snd) _keyed
    go KSVGAtom {..} = mapM_ (go . snd) _keyed
    go m@Managed {..} = case _constr of Component' c -> fg (unsafeCoerce c)
    go _ = return ()

onForeground :: ( MonadIO c, MonadIO c'
                , '[Revent] <: ms
                , '[State () ComponentHooks] <: ms'
                , With w (Narrative (Messages ms') c') IO
                )
             => w -> Code '[Event ()] (Code ms c) () -> Code ms c (Promise (IO ()))
onForeground c f = do
  connectWith c (get >>= \(ComponentHooks _ _ fg) -> return fg) $ \_ -> f

onBackground :: ( MonadIO c, MonadIO c'
                , '[Revent] <: ms
                , '[State () ComponentHooks] <: ms'
                , With w (Narrative (Messages ms') c') IO
                )
             => w -> Code '[Event ()] (Code ms c) () -> Code ms c (Promise (IO ()))
onBackground c f = do
  connectWith c (get >>= \(ComponentHooks _ bg _) -> return bg) $ \_ -> f

reflect :: forall ts ms m c.
           ( IsComponent' ts ms m
           , MonadIO c
           )
        => Component' ts ms m
        -> c (Promise (Atom (Code ms IO ())))
reflect c =
  with c $ do
    ComponentState {..} :: ComponentState m <- get
    ComponentView {..} <- liftIO $ readIORef asLive
    return (unsafeCoerce cvCurrentLive)

data DiffStrategy = Eager | Manual deriving (Eq)

type Differ ms m =
       (m -> Atom (Code ms IO ()))
    -> IO ()
    -> (Code ms IO () -> IO ())
    -> ComponentState m
    -> Code ms IO ()

data AState m = AState
  { as_live :: forall ms. IORef (ComponentView ms m)
  , as_model :: m
  }

data ComponentPatch m where
  APatch ::
      -- only modify ap_AState with atomicModifyIORef
    { ap_send         :: Code ms IO () -> IO ()
    , ap_AState       :: IORef (Maybe (AState m),Bool) -- an AState record for manipulation; nullable by component to stop a patch.
    , ap_patchView    :: (m -> Atom (Code ms IO ()))
    , ap_viewTrigger  :: IO ()
    , ap_hooks        :: ComponentHooks
    } -> ComponentPatch m

type IsComponent' ts ms m = (Base m <: ms, Base m <. ts, Delta (Modules ts) (Messages ms))
type IsComponent ms m = IsComponent' ms ms m

data ComponentHooks = ComponentHooks
  { chViews      :: Syndicate ()
  , chForeground :: Syndicate ()
  , chBackground :: Syndicate ()
  }

data ComponentView ms m = ComponentView
  { cvCurrent     :: Atom (Code ms IO ()) -- un-rendered
  , cvCurrentLive :: Atom (Code ms IO ())
  , cvModel       :: m
  , cvForeground  :: Bool
  }

data ComponentRecord ms m = ComponentRecord
  { crAsComponent :: As (Code ms IO)
  , crView        :: IORef (ComponentView ms m)
  , crHooks       :: ComponentHooks
  }

data ComponentState m where
  ComponentState ::
    { asPatch        :: Maybe (ComponentPatch m)
    , asDiffer       :: ComponentState m -> Code ms IO ()
    , asDiffStrategy :: DiffStrategy
    , asUpdates      :: Syndicate m
    , asModel        :: m
    , asLive         :: IORef (ComponentView ms m)
    } -> ComponentState m

type Base m
  = '[ State () (ComponentState m)
     , State () ComponentHooks
     , State () Shutdown
     , Revent
     ]

data Constr where
  Component' :: IsComponent' ts ms m => Component' ts ms m -> Constr
instance Eq Constr where
 (==) (Component' c) (Component' c') =
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

  toTxt (Style pairs) =
    "style=\""
      <> Txt.intercalate
           (Txt.singleton ';')
           (renderStyles False (mapM_ (uncurry (=:)) pairs))
      <> "\""

  toTxt (Link href _)    = "href=\"" <> href <> "\""

  toTxt (SVGLink href _) = "xlink:href=\"" <> href <> "\""

  toTxt (XLink xl v)     = xl <> "=\"" <> v <> "\""

  toTxt _ = mempty

instance ToTxt [Feature e] where
  toTxt fs =
    Txt.intercalate
     (Txt.singleton ' ')
     (Prelude.filter (not . Txt.null) $ Prelude.map toTxt fs)

type ComponentKey ms m = Key (ComponentRecord (Appended ms (Base m)) m)
type ComponentBuilder ts m = Modules (Base m) (Action (Appended ts (Base m)) IO) -> IO (Modules (Appended ts (Base m)) (Action (Appended ts (Base m)) IO))
type ComponentPrimer ms m = Code (Appended ms (Base m)) IO ()

data Component' ts ms m
  = Component
      { key       :: !(Key (ComponentRecord ms m))
      , build     :: !(Modules (Base m) (Action ts IO) -> IO (Modules ts (Action ts IO)))
      , prime     :: !(Code ms IO ())
      , model     :: !(m)
      , render    :: !(m -> Atom (Code ms IO ()))
      }
type Component ms m = Component' (Appended ms (Base m)) (Appended ms (Base m)) m

instance ToTxt (Component' ts ms m) where
  toTxt = toTxt . key

instance Eq (Component' ts ms m) where
  (==) (Component k _ _ _ _) (Component k' _ _ _ _) =
    let Key k1 = k
        Key k2 = k'
    in prettyUnsafeEq k1 k2

instance Ord (Component' ts ms m) where
  compare (Component (Key k) _ _ _ _) (Component (Key k') _ _ _ _) = compare k k'

instance IsComponent' ts ms m
  => With (Component' ts ms m)
          (Code ms IO)
          IO
  where
    using_ c = do
      -- FIXME: likely a bug here with double initialization in multithreaded contexts!
      mi_ <- lookupComponent (key c)
      case mi_ of
        Just (ComponentRecord {..}) -> return (runAs crAsComponent)
        Nothing -> do
          mkComponent BuildOnly c
          using_ c
    with_ c m = do
      run <- using_ c
      run m
    shutdown_ c = do
      -- this method should 1. destroy the view 2. syndicate a shutdown event 3. poison the component
      -- so that unmount events that call with on the component do not fail
      miohhm <- lookupComponent (key c)
      case miohhm of
        Just ComponentRecord {..} -> do
          ComponentView {..} <- liftIO $ readIORef crView
          cleanup (void . with c) [cvCurrentLive]
          delete cvCurrentLive
          void $ runAs crAsComponent $ do
            buf <- get
            Shutdown sdn <- get
            publish sdn ()
            -- this is where things get iffy... what should this look like?
            delay 0 $ do
              deleteComponent (key c)
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

lookupComponent :: (MonadIO c) => Key phantom -> c (Maybe phantom)
lookupComponent = vaultLookup constructVault__

getComponentName :: IsComponent' ts ms m => Component' ts ms m -> Txt
getComponentName = toTxt . key

addComponent :: (MonadIO c) => Key phantom -> phantom -> c ()
addComponent = vaultAdd constructVault__

deleteComponent :: (MonadIO c) => Key phantom -> c ()
deleteComponent = vaultDelete constructVault__

data MkComponentAction
  = ClearAndAppend ENode
  | forall e. Replace (Atom e)
  | Append ENode
  | BuildOnly

mkComponent :: forall ms ts m.
          ( IsComponent' ts ms m
          , Base m <: ms
          )
       => MkComponentAction
       -> Component' ts ms m
       -> IO (ComponentRecord ms m)
mkComponent mkComponentAction c@Component {..} = do
  let !m = model
      !raw = render m
  doc <- getDocument
  buf <- newEvQueue
  ch  <- ComponentHooks <$> syndicate <*> syndicate <*> syndicate
  us  <- syndicate
  sdn <- Shutdown <$> syndicate
  as  <- unsafeConstructAs buf
  let sendEv = void . runAs as
  (i,l) <- case mkComponentAction of
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
  cr <- ComponentRecord <$> pure as <*> newIORef (ComponentView raw i m l) <*> pure ch
  -- keep out of forkIO to prevent double-initialization
  addComponent key cr
  forkIO $ do
    built <- build $ state (ComponentState
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
    driverPrintExceptions (show key ++ " - component exception: ")
#else
    driver
#endif
        buf obj'
  return cr

diff :: forall m ms. (Base m <: ms) => Proxy m -> Code ms IO ()
diff _ = do
  as@ComponentState {..} :: ComponentState m <- get
  unsafeCoerce (asDiffer as)

setEagerDiff :: forall m ms. ('[State () (ComponentState m)] <: ms)
             => Proxy m -> Code ms IO ()
setEagerDiff _ = do
  ComponentState {..} :: ComponentState m <- get
  put ComponentState { asDiffStrategy = Eager, .. }

setManualDiff :: forall m ms. ('[State () (ComponentState m)] <: ms)
              => Proxy m -> Code ms IO ()
setManualDiff _ = do
  ComponentState {..} :: ComponentState m <- get
  put ComponentState { asDiffStrategy = Manual, .. }

currentView :: forall ts ms c m.
               ( IsComponent' ts ms m
               , MonadIO c
               )
            => Component' ts ms m
            -> c (Promise (Atom (Code ms IO ())))
currentView c = with c $ ownView c

ownView :: forall ts ms c m.
           ( IsComponent' ts ms m
           , MonadIO c
           , Base m <: ms
           )
        => Component' ts ms m
        -> Code ms c (Atom (Code ms IO ()))
ownView _ = do
  ComponentState {..} :: ComponentState m <- get
  ComponentView {..} <- liftIO $ readIORef asLive
  return (unsafeCoerce cvCurrent)

onModelChange :: forall ts ms ms' m c.
                ( IsComponent' ts ms m
                , MonadIO c
                , Base m <: ms
                , '[Revent] <: ms'
                )
              => Component' ts ms m
              -> (m -> Code '[Event m] (Code ms' c) ())
              -> Code ms' c (Promise (IO ()))
onModelChange c f = do
  buf <- get
  with c $ do
    ComponentState {..} :: ComponentState m <- get
    sub <- subscribe asUpdates (return buf)
    bhv <- listen sub f
    return (stop bhv >> leaveSyndicate asUpdates sub)

onOwnModelChange :: forall ts ms ms' m c.
                    ( IsComponent' ts ms m
                    , MonadIO c
                    , Base m <: ms
                    )
                  => Component' ts ms m
                  -> (m -> Code '[Event m] (Code ms c) ())
                  -> Code ms c (IO ())
onOwnModelChange _ f = do
  buf <- get
  pr  <- promise
  ComponentState {..} :: ComponentState m <- get
  sub <- subscribe asUpdates (return buf)
  bhv <- listen sub f
  return (stop bhv >> leaveSyndicate asUpdates sub)

onOwnModelChangeByProxy :: forall ms m c.
                           ( MonadIO c
                           , Base m <: ms
                           )
                         => Proxy m
                         -> (m -> Code '[Event m] (Code ms c) ())
                         -> Code ms c (IO ())
onOwnModelChangeByProxy _ f = do
  buf <- get
  pr  <- promise
  ComponentState {..} :: ComponentState m <- get
  sub <- subscribe asUpdates (return buf)
  bhv <- listen sub f
  return (stop bhv >> leaveSyndicate asUpdates sub)

{-# INLINE getModel #-}
getModel :: ('[State () (ComponentState m)] <: ms) => Code ms IO m
getModel = do
  ComponentState {..} <- get
  return asModel

{-# INLINE putModel #-}
putModel :: forall ms m.
        ( Base m <: ms
        )
     => m -> Code ms IO ()
putModel !new = do
  (ComponentState {..},(old,cmp')) <- modify $ \(ComponentState {..} :: ComponentState m) ->
    let !old = asModel
        cmp' = ComponentState { asModel = new, .. }
    in (cmp',(old,cmp'))
  publish asUpdates new
  let d :: ComponentState m -> Code ms IO ()
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
modifyModel :: forall ms m.
           ( Base m <: ms
           )
        => (m -> m)
        -> Code ms IO ()
modifyModel f = do
  (ComponentState {..},(old,!new,cmp')) <- modify $ \ComponentState {..} ->
    let !old = asModel
        !new = f old
        cmp' = ComponentState { asModel = new, ..  }
    in (cmp',(old,new,cmp'))
  publish asUpdates new
  let d :: ComponentState m -> Code ms IO ()
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
differ render trig sendEv ComponentState {..} = do
#ifdef __GHCJS__
  ch <- get
  let setupDiff = do
        let !new_as = AState (unsafeCoerce asLive) asModel
        new_ap_AState <- liftIO $ newIORef (Just new_as,False)
        let !aPatch = APatch sendEv new_ap_AState render trig ch
        put ComponentState { asPatch = Just aPatch, .. }
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
              ((Just cs { as_model = asModel },False),False)
            _ -> (mpatch,True)
        when shouldSetupDiff $ do
          setupDiff
  return ()
#else
  let v = render asModel
  liftIO $ do
    ComponentView _ _ _ isFG <- liftIO $ readIORef asLive
    writeIORef asLive $ unsafeCoerce $ ComponentView v v asModel isFG
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
embed_ :: ENode -> Atom e -> IO ()
embed_ parent STAtom {..} = do
  forM_ _strecord $ \ref -> do
    (_,_,SomeAtom a,_) <- readIORef ref
    embed_ parent a
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

setAttributes :: [Feature e] -> (e -> IO ()) -> Bool -> ENode -> IO ([Feature e],IO ())
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
buildAndEmbedMaybe :: (e -> IO ()) -> Doc -> ComponentHooks -> Bool -> Maybe ENode -> Atom e -> IO (Atom e)
buildAndEmbedMaybe f doc ch isFG = go
  where
    go mparent nn@NullAtom {..} = do
      _cond@(Just el) <- createElement doc "template"
      forM_ mparent (flip appendChild el)
      return $ NullAtom _cond

    go mparent Raw {..} = do
      _node@(Just el) <- createElement doc _tag
      (_attributes,didMount) <- setAttributes _attributes f False el
      setInnerHTML el _content
      didMount
      forM_ mparent $ \parent -> appendChild parent el
      return $ Raw _node _tag _attributes _content

    go mparent Atom {..} = do
      _node@(Just el) <- createElement doc _tag
      (_attributes,didMount) <- setAttributes _attributes f False el
      _atoms <- mapM (go (Just el)) _atoms
      didMount
      forM_ mparent $ \parent -> appendChild parent el
      return $ Atom _node _tag _attributes _atoms

    go mparent STAtom {..} = do
      strec <- newIORef (_ststate,SomeView (unsafeCoerce _stview),SomeAtom nil,SomeAtom nil)
      let upd g cb = void $ do
            -- this won't work properly on GHC; look into using onFPS or something to
            -- replicate the rAF approach
#ifdef __GHCJS__
            rafCallback <- newRequestAnimationFrameCallback $ \_ -> do
#endif
              (st,SomeView sv,SomeAtom old,SomeAtom mid) <- readIORef strec
              let st' = g st
                  new_mid = unsafeCoerce $ sv st' upd
              new <- diffHelper f doc ch isFG old mid new_mid
              writeIORef strec (st',SomeView sv,SomeAtom (unsafeCoerce new),SomeAtom (unsafeCoerce new_mid))
              cb
#ifdef __GHCJS__
            win <- getWindow
            requestAnimationFrame win (Just rafCallback)
#endif
      let mid = _stview _ststate upd
      new <- go mparent mid
      writeIORef strec (_ststate,SomeView (unsafeCoerce _stview),SomeAtom (unsafeCoerce new),SomeAtom (unsafeCoerce mid))
      return $ STAtom _stmodel _stid _ststate (Just strec) _stview upd

    go mparent SVGAtom {..} = do
      _node@(Just el) <- createElementNS doc "http://www.w3.org/2000/svg" _tag
      (_attributes,didMount) <- setAttributes _attributes f False el
      _atoms <- mapM (go (Just el)) _atoms
      didMount
      forM_ mparent $ \parent -> appendChild parent el
      return $ SVGAtom _node _tag _attributes _atoms

    go mparent KAtom {..} = do
      _node@(Just el) <- createElement doc _tag
      (_attributes,didMount) <- setAttributes _attributes f False el
      _keyed <- mapM (\(k,x) -> go (Just el) x >>= \y -> return (k,y)) _keyed
      didMount
      forM_ mparent $ \parent -> appendChild parent el
      return $ KAtom _node _tag _attributes _keyed

    go mparent KSVGAtom {..} = do
      _node@(Just el) <- createElementNS doc "http://www.w3.org/2000/svg" _tag
      (_attributes,didMount) <- setAttributes _attributes f False el
      _keyed <- mapM (\(k,x) -> go (Just el) x >>= \y -> return (k,y)) _keyed
      didMount
      forM_ mparent $ \parent -> appendChild parent el
      return $ KSVGAtom _node _tag _attributes _keyed

    go mparent Text {..} = do
      _tnode@(Just el) <- createTextNode doc _content
      forM_ mparent (flip appendChild el)
      return $ Text _tnode _content

    go mparent m@Managed {..} =
      case _constr of
        Component' a -> do
          case _node of
            Nothing -> do
              _node@(Just el) <- createElement doc _tag
              (_attributes,didMount) <- setAttributes _attributes f False el
              didMount
              mi_ <- lookupComponent (key a)
              case mi_ of
                Nothing -> do
                  -- never built before; make and embed
                  ComponentRecord {..} <- mkComponent BuildOnly a
                  ComponentView {..} <- liftIO $ readIORef crView
                  forM_ mparent $ \parent -> do
                    when isFG (triggerForeground m)
                    embed_ parent Managed {..}
                  embed_ el cvCurrentLive
                  return Managed {..}
                Just ComponentRecord {..} -> do
                  ComponentView {..} <- liftIO $ readIORef crView
                  rebuild Managed {..}
                  when isFG (triggerForeground m)
                  embed_ el cvCurrentLive
                  forM_ mparent $ \parent -> embed_ parent Managed {..}
                  return Managed {..}

            Just e -> do
              mi_ <- lookupComponent (key a)
              case mi_ of
                Nothing -> do
                  -- shut down?
                  ComponentRecord {..} <- mkComponent BuildOnly a
                  ComponentView {..} <- liftIO $ readIORef crView
                  forM_ mparent $ \parent -> do
                    when isFG (triggerForeground m)
                    embed_ parent Managed {..}
                  embed_ e cvCurrentLive
                  return Managed {..}
                Just ComponentRecord {..} -> do
                  ComponentView {..} <- liftIO $ readIORef crView
                  rebuild m
                  when isFG (triggerForeground m)
                  embed_ e cvCurrentLive
                  return m

{-# NOINLINE buildHTML #-}
buildHTML :: Doc -> ComponentHooks -> Bool -> (e -> IO ()) -> Atom e -> IO (Atom e)
buildHTML doc ch isFG f = buildAndEmbedMaybe f doc ch isFG Nothing

getElement Text {} = return Nothing
getElement STAtom {..} =
  case _strecord of
    Nothing -> return Nothing
    Just ref -> do
      (_,_,SomeAtom a,_) <- readIORef ref
      getElement a
getElement n = return $ _node n

getNode Text {..} = return $ fmap toNode _tnode
getNode STAtom {..} =
  case _strecord of
    Nothing -> return Nothing
    Just ref -> do
      (_,_,SomeAtom a,_) <- readIORef ref
      getNode a
getNode n = return $ fmap toNode $ _node n


diff_ :: ComponentPatch m -> IO ()
diff_ APatch {..} = do
#ifdef __GHCJS__
  -- made a choice here to do all the diffing in the animation frame; this way we
  -- can avoid recalculating changes multiple times during a frame. No matter how
  -- many changes occur in any component, the diff is only calculated once per frame.
  rafCallback <- newRequestAnimationFrameCallback $ \_ -> do
    (mcs,b) <- atomicModifyIORef' ap_AState $ \(mcs,b) -> ((Nothing,True),(mcs,b))
    case mcs of
      Nothing -> return ()
      Just (AState as_live !as_model) -> do
        doc <- getDocument
        ComponentView !raw_html !live_html live_m isFG <- readIORef as_live
        let !new_html = ap_patchView as_model
        new_live_html <- diffHelper ap_send doc ap_hooks isFG live_html raw_html new_html
        writeIORef as_live $ ComponentView new_html new_live_html as_model isFG
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
      ComponentView !raw_html !live_html live_m isFG <- readIORef as_live
      let !new_html = ap_patchView as_model
      new_live_html <- diffHelper ap_send doc ap_hooks isFG live_html raw_html new_html
      writeIORef as_live $ ComponentView new_html new_live_html as_model isFG
      ap_viewTrigger
#endif

{-# NOINLINE replace #-}
replace :: Atom e -> Atom e' -> IO ()
#ifndef __GHCJS__
replace _ _ = return ()
#else
replace old@STAtom {} new@STAtom {} =
  case (old,new) of
    (STAtom _ _ _ (Just r) _ _,STAtom _ _ _ (Just r') _ _) -> do
      (_,_,SomeAtom a,_) <- readIORef r
      (_,_,SomeAtom b,_) <- readIORef r'
      replace a b
    _ -> return ()

replace STAtom {..} new = do
  case _strecord of
    Nothing -> return ()
    Just ref -> do
      (_,_,SomeAtom a,_) <- readIORef ref
      replace a new

replace old STAtom {..} = do
  case _strecord of
    Nothing -> return ()
    Just ref -> do
      (_,_,SomeAtom a,_) <- readIORef ref
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
delete :: Atom e -> IO ()
#ifndef __GHCJS__
delete _ = return ()
#else
delete STAtom {..} = do
  case _strecord of
    Nothing -> return ()
    Just ref -> do
      (_,_,SomeAtom a,_) <- readIORef ref
      delete a
delete Text {..} = forM_ _tnode (delete_js . toNode)
delete n = forM_ (_node n) (delete_js . toNode)
#endif

-- f node feature io -> io
{-# NOINLINE cleanup #-}
cleanup :: (e -> IO ()) -> [Atom e] -> IO (IO ())
#ifndef __GHCJS__
cleanup _ _ = return (return ())
#else
cleanup f = go (return ())
  where
    go didUnmount (STAtom{..}:rest) = do
      didUnmount' <- case _strecord of
                       Nothing -> return (return ())
                       Just ref -> do
                         (_,_,SomeAtom a,_) <- readIORef ref
                         cleanup f [a]
      go (didUnmount' >> didUnmount) rest
    go didUnmount (NullAtom{}:rest) = go didUnmount rest
    go didUnmount (r@Raw {..}:rest) = do
      en <- getElement r
      du <- case en of
              Nothing -> return (return ())
              Just n  -> foldM (flip (cleanupAttr f n)) (return ()) _attributes
      go (du >> didUnmount) rest
    go didUnmount (a@Atom {..}:rest) = do
      en <- getElement a
      du <- case en of
              Nothing -> return (return ())
              Just n -> foldM (flip (cleanupAttr f n)) (return ()) _attributes
      unmounts' <- cleanup f _atoms
      go (unmounts' >> du >> didUnmount) rest
    go didUnmount (a@SVGAtom {..}:rest) = do
      en <- getElement a
      du <- case en of
              Nothing -> return (return ())
              Just n -> foldM (flip (cleanupAttr f n)) (return ()) _attributes
      unmounts' <- cleanup f _atoms
      go (unmounts' >> du >> didUnmount) rest
    go didUnmount (a@KAtom {..}:rest) = do
      en <- getElement a
      du <- case en of
              Nothing -> return (return ())
              Just n -> foldM (flip (cleanupAttr f n)) (return ()) _attributes
      unmounts' <- cleanup f (map snd _keyed)
      go (unmounts' >> du >> didUnmount) rest
    go didUnmount (a@KSVGAtom {..}:rest) = do
      en <- getElement a
      du <- case en of
              Nothing -> return (return ())
              Just n -> foldM (flip (cleanupAttr f n)) (return ()) _attributes
      unmounts' <- cleanup f (map snd _keyed)
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
insertAt :: ENode -> Int -> Atom e -> IO ()
#ifndef __GHCJS__
insertAt _ _ _ = return ()
#else
insertAt parent ind STAtom {..} = do
  forM_ _strecord $ \ref -> do
    (_,_,SomeAtom a,_) <- readIORef ref
    insertAt parent ind a
insertAt parent ind Text {..} = forM_ _tnode $ insert_at_js parent ind . toNode
insertAt parent ind n = forM_ (_node n) $ insert_at_js parent ind . toNode
#endif

{-# NOINLINE insertBefore_ #-}
insertBefore_ :: ENode -> Atom e -> Atom e -> IO ()
#ifndef __GHCJS__
insertBefore_ _ _ _ = return ()
#else
insertBefore_ parent child@(STAtom{}) new@(STAtom{}) = do
  case (child,new) of
    (STAtom _ _ _ (Just r) _ _, STAtom _ _ _ (Just r') _ _) -> do
      (_,_,SomeAtom a,_) <- readIORef r
      (_,_,SomeAtom b,_) <- readIORef r'
      insertBefore_ parent a b
    _ -> return ()
insertBefore_ parent STAtom {..} new = do
  forM_ _strecord $ \ref -> do
    (_,_,SomeAtom a,_) <- readIORef ref
    insertBefore_ parent a new
insertBefore_ parent child STAtom {..} = do
  forM_ _strecord $ \ref -> do
    (_,_,SomeAtom a,_) <- readIORef ref
    insertBefore_ parent child a
insertBefore_ parent child@(Text {}) new@(Text{}) = void $ N.insertBefore parent (_tnode new) (_tnode child)
insertBefore_ parent child new@(Text{}) = void $ N.insertBefore parent (_tnode new) (_node child)
insertBefore_ parent child@(Text {}) new = void $ N.insertBefore parent (_node new) (_tnode child)
insertBefore_ parent child new = void $ N.insertBefore parent (_node new) (_node child)
#endif

diffHelper :: (e -> IO ()) -> Doc -> ComponentHooks -> Bool -> Atom e -> Atom e -> Atom e -> IO (Atom e)
diffHelper f doc ch isFG =
#ifdef __GHCJS__
    go
#else
    \_ _ n -> return n
#endif
  where

    go old mid new =
      if reallyUnsafeEq mid new then do
        return old
      else
        go' old mid new

    go' old@NullAtom{} _ new = do
      case new of
        NullAtom _ -> return old
        _          -> do
          new' <- buildHTML doc ch isFG f new
          replace old new'
          didUnmount <- cleanup f [old]
          delete old
          didUnmount
          return new'

    go' old _ new@NullAtom{} = do
      new' <- buildHTML doc ch isFG f new
      replace old new'
      didUnmount <- cleanup f [old]
      delete old
      didUnmount
      return new'

    go' old@Atom {} mid@Atom {} new@Atom {} =
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
                diffChildren n (_atoms old) (_atoms mid) (_atoms new)
        didMount
        return $ Atom (_node old) (_tag old) a' c'
      else do new' <- buildHTML doc ch isFG f new
              replace old new'
              didUnmount <- cleanup f [old]
              delete old
              didUnmount
              return new'

    go' old@STAtom {} _ new@STAtom {} = do
      case (old,new) of
        (STAtom m k s ~(Just r) v u,STAtom m' k' s' _ v' _) -> do
          if prettyUnsafeEq k k' then
            if reallyVeryUnsafeEq m m' then do
              return old
            else do
              (st,sv,old,mid) <- readIORef r
              writeIORef r (st,SomeView (unsafeCoerce v'),old,mid)
              u (const (unsafeCoerce s')) (return ())
              return $ STAtom m' k' s' (Just $ unsafeCoerce r) v' (unsafeCoerce u)
          else do
            (_,_,SomeAtom a,_) <- readIORef r
            new' <- buildHTML doc ch isFG f new
            replace a new'
            didUnmount <- cleanup f [a]
            delete a
            didUnmount
            return new'

    go' old@SVGAtom {} mid@SVGAtom {} new@SVGAtom {} =
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
                diffChildren n (_atoms old) (_atoms mid) (_atoms new)
        didMount
        return $ SVGAtom (_node old) (_tag old) a' c'
      else do new' <- buildHTML doc ch isFG f new
              replace old new'
              didUnmount <- cleanup f [old]
              delete old
              didUnmount
              return new'

    go' old@(KAtom old_node old_tag old_attributes old_keyed)
      mid@(KAtom midAnode _ midAattributes midAkeyed)
      new@(KAtom _ new_tag new_attributes new_keyed) =
      if prettyUnsafeEq old_tag new_tag
      then do
        let Just n = _node old
        (a',didMount) <-
              if reallyUnsafeEq midAattributes new_attributes then
                return (old_attributes,return ())
              else
                runElementDiff f n old_attributes midAattributes new_attributes
        c' <- if reallyUnsafeEq midAkeyed new_keyed then return old_keyed else
                diffKeyedChildren n old_keyed midAkeyed new_keyed
        didMount
        return $ KAtom old_node old_tag a' c'
      else do new' <- buildHTML doc ch isFG f new
              replace old new'
              didUnmount <- cleanup f [old]
              delete old
              didUnmount
              return new'

    go' old@(KSVGAtom old_node old_tag old_attributes old_keyed)
      mid@(KSVGAtom midAnode _ midAattributes midAkeyed)
      new@(KSVGAtom _ new_tag new_attributes new_keyed) =
      if prettyUnsafeEq old_tag new_tag
      then do
        let Just n = _node old
        (a',didMount) <-
              if reallyUnsafeEq midAattributes new_attributes then
                return (old_attributes,return ())
              else
                runElementDiff f n old_attributes midAattributes new_attributes
        c' <- if reallyUnsafeEq midAkeyed new_keyed then return old_keyed else
                diffKeyedChildren n old_keyed midAkeyed new_keyed
        didMount
        return $ KSVGAtom old_node old_tag a' c'
      else do new' <- buildHTML doc ch isFG f new
              replace old new'
              didUnmount <- cleanup f [old]
              delete old
              didUnmount
              return new'

    go' txt@(Text (Just t) cnt) mid@(Text _ mcnt) new@(Text _ cnt') =
      if prettyUnsafeEq mcnt cnt' then do
        return txt
      else do
        changeText t cnt'
        return $ Text (Just t) cnt'

    go' old@(Raw {}) mid@(Raw {}) new@(Raw {}) =
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

    go' old@(Managed {}) mid new@(Managed {}) =
      if    (_constr old) == (_constr new)
        && prettyUnsafeEq (_tag old) (_tag new)
      then do
        let Just n = _node old
        (a',didMount) <-
                if reallyUnsafeEq (_attributes mid) (_attributes new) then
                  return (_attributes old,return ())
                else
                  runElementDiff f n (_attributes old) (_attributes mid) (_attributes new)
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

    diffChildren n olds mids news = do
      withLatest Nothing olds mids news
      where

        withLatest ml = go_
          where

            go_ [] _ news =
              mapM (buildAndEmbedMaybe f doc ch isFG (Just n)) news

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

                continue up = do
                  upds <-
                    if reallyUnsafeEq mids news then return olds else
                      withLatest (Just up) olds mids news
                  return (up:upds)

              in
                if reallyUnsafeEq mid new then continue old else
                  case (mid,new) of
                    (NullAtom {},NullAtom {}) ->
                      continue old

                    (_,NullAtom {}) -> do
                      new' <- buildHTML doc ch isFG f new
                      replace old new'
                      didUnmount <- cleanup f [old]
                      delete old
                      didUnmount
                      continue new'

                    (NullAtom{},_) -> do
                      new' <- buildHTML doc ch isFG f new
                      replace old new'
                      didUnmount <- cleanup f [old]
                      delete old
                      didUnmount
                      continue new'

                    (m,n) -> do
                      new <- go old mid new
                      continue new

    -- note that keyed nodes are filtered for NullAtoms during construction
    diffKeyedChildren n = go_ 0
      where

        go_ i a m b = do
          if reallyUnsafeEq m b then do
            return a
          else
            go__ i a m b
          where

            go__ _ [] _ news = do
              forM (Prelude.zip [i..] news) $ \(i,(bkey,b)) -> do
                new <- buildAndEmbedMaybe f doc ch isFG (Just n) b
                return (bkey,new)

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
                  return $ (akey,new):rest

              | otherwise =
                  case (as,ms,bs) of
                    ((akey',a'):as',(mkey',m'):ms',(bkey',b'):bs')
                      -- swap 2
                      | prettyUnsafeEq bkey akey' && prettyUnsafeEq akey bkey' -> do
                          new1 <- go' a m b'
                          new2 <- go' a' m' b
                          let !i' = i + 2
                          rest <- go_ i' as' ms' bs'
                          return $ (akey',new1):(akey,new2):rest

                      -- insert
                      | prettyUnsafeEq akey bkey' -> do
                          new <- buildAndEmbedMaybe f doc ch isFG Nothing b
                          insertAt n i new
                          let !i' = i + 1
                          rest <- go_ i' old mid bs
                          return $ (bkey,new):rest

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
                            return $ (bkey,new):rest

                    _ | prettyUnsafeEq akey bkey -> do
                          new <- go a m b
                          let !i' = i + 1
                          rest <- go_ i' as ms bs
                          return $ (akey,new):rest

                      | otherwise ->
                          case (old,new) of
                            ([_],(_:(bkey',b'):_)) ->
                              if prettyUnsafeEq akey bkey' then do
                                new <- buildAndEmbedMaybe f doc ch isFG Nothing b
                                insertAt n i new
                                let !i' = i + 1
                                rest <- go_ i' old mid bs
                                return $ (bkey,new):rest
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
runElementDiff :: (e -> IO ()) -> ENode -> [Feature e] -> [Feature e] -> [Feature e] -> IO ([Feature e],IO ())
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

            (Style oldS,Style newS) -> do
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

            (On en e os g _,On en' e' os' g' _) ->
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

            (Link olda oldv, Link newa newv) ->
              if prettyUnsafeEq olda newa && reallyUnsafeEq oldv newv then
                continue old
              else
                replace

            (SVGLink olda oldv, SVGLink newa newv) ->
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

    Link _ unreg -> do
      forM_ unreg id
      E.removeAttribute element ("href" :: Txt)

    On _ _ _ _ unreg ->
      forM_ unreg id

    OnDoc _ _ _ _ unreg ->
      forM_ unreg id

    OnWin _ _ _ _ unreg ->
      forM_ unreg id

    Style styles -> do
      obj <- O.create
      forM_ styles $ \(nm,val) -> O.unsafeSetProp nm (M.pToJSVal val) obj
      clearStyle_js element obj

    SVGLink _ unreg -> do
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
setAttribute_ :: (e -> IO ()) -> Bool -> ENode -> Feature e -> IO () -> IO (Feature e,IO ())
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

    Link href _ -> do
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
      return (Link href (Just stopListener),didMount)

    On en ev os f _ -> do
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
      return (On en ev os f (Just stopListener),didMount)

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

    Style styles -> do
      obj <- O.create
      forM_ styles $ \(nm,val) -> O.unsafeSetProp nm (M.pToJSVal val) obj
      setStyle_js element obj
      return (attr,didMount)

    SVGLink href _ -> do
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
      return (SVGLink href (Just stopListener),didMount)

    XLink nm val -> do
      E.setAttributeNS element (Just ("http://www.w3.org/1999/xlink" :: Txt)) nm val
      return (attr,didMount)

    _ -> return (attr,didMount)
#endif

{-# NOINLINE cleanupAttr #-}
cleanupAttr :: (e -> IO ()) -> ENode -> Feature e -> IO () -> IO (IO ())
cleanupAttr f element attr didUnmount =
#ifndef __GHCJS__
  return didUnmount
#else
  case attr of
    SVGLink _ unreg -> do
      forM_ unreg id
      return didUnmount
    Link _ unreg -> do
      forM_ unreg id
      return didUnmount
    On _ _ _ _ unreg -> do
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

makePrisms ''Atom
makeLenses ''Atom
