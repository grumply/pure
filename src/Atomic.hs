{-# language CPP #-}
{-# language TemplateHaskell #-}
{-# language OverloadedStrings #-}
{-# language ImplicitParams #-}
{-# language ViewPatterns #-}
{-# language PatternSynonyms #-}
{-# language UndecidableInstances #-}
module Atomic
  ( module Atomic
  , module Export
  , HTML(..)
  , html
  , Atom
  , mkAtom, mkSVGAtom, text, unindent, raw, keyed, hashed, st
  , Component(..)
  , component
  , css, css', scss, scss', styles
  , diff, setManualDiff, setEagerDiff
  , getModel, putModel, modifyModel
  , onModelChange, onOwnModelChange, onOwnModelChangeByProxy, ownView, currentView
  , ContextHooks(..)
  , ContextRecord(..)
  , ContextView(..)
  , LazyByteString
  ) where

import Ef.Base as Export hiding (As,Index,transform,observe,uncons,distribute,embed,construct,Nat(..),End)

#if MIN_VERSION_hashable(1,2,5)
import Data.Hashable as Export hiding (hashed)
#else
import Data.Hashable as Export
#endif

#if MIN_VERSION_aeson(1,0,0)
import qualified Data.Aeson.Encoding as AE
#endif

import Data.Typeable as Export
import GHC.Generics as Export (Generic)

import Atomic.Component
import Atomic.Service hiding (Base)

import Control.Lens as Export hiding
  (lazy,Empty,none,(<~),(.=),(<.>),Context,Context',Reversed)
import Control.Lens.Extras as Export

import Data.Function as Export hiding (on)
import Data.Bifunctor as Export
import Data.Bool as Export
import Data.Maybe as Export
import Data.Void as Export

import qualified Data.Txt as Export (Txt(..))
import Data.JSON         as Export hiding (defaultOptions,Options,(!),Alt)
import Data.Millis       as Export
import Data.Micros       as Export
import Atomic.API        as Export hiding (Index)
import Atomic.Attribute  as Export
import Atomic.CSS        as Export
import Atomic.Cond       as Export
import Atomic.Debounce   as Export
import Atomic.Default    as Export
import Atomic.Dispatch   as Export
import Atomic.Dict       as Export
import Atomic.Ease       as Export
import Atomic.Endpoint   as Export
import Atomic.FromBS     as Export
import Atomic.FromTxt    as Export
import Atomic.Grid       as Export
import Atomic.HTML       as Export
import Atomic.Identify   as Export
import Atomic.Key        as Export
import Atomic.Lazy       as Export
import Atomic.Limit      as Export
import Atomic.Memo       as Export
import Atomic.Message    as Export
import Atomic.Normalize  as Export
import Atomic.Observable as Export
import Atomic.Request    as Export
import Atomic.Route      as Export hiding (route)
import Atomic.Router     as Export
import Atomic.Services   as Export
import Atomic.Signals    as Export
import Atomic.Throttle   as Export
import Atomic.ToBS       as Export
import Atomic.ToTxt      as Export
import Atomic.Try        as Export
import Atomic.TypeRep    as Export
import Atomic.UnsafeEq   as Export
import Atomic.Vault      as Export
#ifdef __GHCJS__
import Atomic.WebSocket  as Export
#else
import Atomic.WebSocket  as Export hiding (accept)
#endif

import Data.ByteString as Export (ByteString)

import Prelude as Export hiding (all,exponent,tan,lookup,reverse)
import qualified Prelude
import Data.Monoid as Export hiding (Alt)
import Data.Bifunctor as Export

import Data.HashMap.Strict as Map hiding (null,map,update)
import Data.Tree as Tree

import Data.Txt as Txt hiding (map,null,Alt)
import qualified Data.Txt as Txt hiding (Alt)
import Data.JSON as JSON hiding (Alt)

import Language.Haskell.TH
import Language.Haskell.TH.Syntax

import Data.IORef

-- for printAny/traceAny and STAtom
import System.IO.Unsafe
import Unsafe.Coerce
#ifdef __GHCJS__
import qualified GHCJS.Types as T
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "console.log($1);console.log($2);"
  printAny_js :: Txt -> T.JSVal -> IO ()

printAny :: (MonadIO c) => Txt -> a -> c ()
printAny label a = liftIO $ printAny_js label (unsafeCoerce a)

traceAny :: Txt -> a -> b -> b
traceAny label a b =
  let prnt = unsafePerformIO (printAny_js label (unsafeCoerce a))
  in prnt `seq` b
#endif

instance FromMillis Micros where
 --  fromMillis :: Millis -> Micros
  fromMillis jt = Micros $ (getMillis jt) * 1000

instance FromMicros Millis where
  -- fromMicrotime :: Micros -> Millis
  -- truncate rather than round
  fromMicros mt = Millis $ (getMicros mt) `Prelude.div` 1000

#if !MIN_VERSION_aeson(1,0,0)
instance {-# OVERLAPS #-} (ToJSON v,ToTxt k) => ToJSON (HashMap k v) where
  {-# INLINE toJSON #-}
  toJSON =
#ifdef __GHCJS__
    objectValue .
#endif
      JSON.object . Prelude.map (\(k,v) -> (toTxt k,toJSON v)) . Map.toList

instance {-# OVERLAPS #-} (FromJSON v,Hashable k,Eq k,FromTxt k) => FromJSON (HashMap k v) where
  parseJSON x = do
    o <- parseJSON x
    kvs <- flip mapM (Map.toList o) $ \(k,v) -> do
      v' <- parseJSON v
      pure (fromTxt k,v')
    pure $ Map.fromList kvs
#else

-- instance (ToTxt k,ToJSON k) => ToJSONKey k where
--   toJSONKey = toJSONKeyText toTxt

-- instance (FromTxt k,FromJSON k) => FromJSONKey k where
--   fromJSONKey = FromJSONKeyText fromTxt

#endif

#ifdef __GHCJS__
instance (ToJSON v) => ToJSON (Tree v) where
  toJSON (Node root branches) = toJSON (root,branches)

instance (FromJSON v) => FromJSON (Tree v) where
  parseJSON j = uncurry Node <$> parseJSON j
#endif

instance Identify Txt where
  type I Txt = Txt
  identify = id

ghc :: Monad m => m () -> m ()
ghc =
#ifndef __GHCJS__
  id
#else
  const (return ())
#endif

ghcjs :: Monad m => m () -> m ()
ghcjs =
#ifdef __GHCJS__
  id
#else
  const (return ())
#endif

type TxtLike as = Constrain '[FromTxt,ToTxt] as
type JSON as = Constrain '[ToJSON,FromJSON,Typeable] as

-- Build a Getter through an intermediate type from an Iso.
-- This is equivalent to writing the often impossible combinator
--
-- > 'l . from l'
--
-- which, in general, produces a function equivalent to 'id'.
--
-- This is especially useful when l is overloaded via a typeclass
-- as is the case in 'txt'. Thus, a common use is:
--
-- > via txt
--
-- to go through a textual intermediary to produce a result, like when
-- turning some identifier (w) witnessing ToTxt into a Key for a context:
--
-- > key = w ^. via txt
via :: (Functor f, Profunctor p, Contravariant f) => Iso s a i i -> Optic' p f s a
via f = to (withIso f $ \f t -> t . f)

-- translate is equivalent to 'via txt'
translated :: (Functor f, Profunctor p, FromTxt a, ToTxt s, Contravariant f)
           => Optic' p f s a
translated = via txt

identified :: (Functor f, Identify a, Profunctor p, Contravariant f) => Optic' p f a (I a)
identified = to identify

named :: (Functor f, Identify a, Profunctor p, Contravariant f) => Optic' p f a (I a)
named = to identify

renamed :: (Functor f, Identify a, FromTxt x, Profunctor p, ToTxt (I a), Contravariant f)
        => p x (f x) -> p a (f a)
renamed = named . translated

txt :: (FromTxt t, ToTxt a) => Iso a t Txt Txt
txt = iso toTxt fromTxt

pattern Txt a <- (view txt -> a) where
  Txt a = review txt a

scoped :: (FromTxt x) => (?scope :: Txt) => Txt -> x
scoped t = fromTxt (?scope <> t)

this :: Q Exp
this = do
  md <- fmap loc_module qLocation
  let t = dash $ Txt.pack md
  [| fromTxt t |]

dash :: Txt -> Txt
dash = Txt.map (\x -> if x == '.' then '-' else x)

-- Look up an optional `Txt` from the `Dict` and call `fromTxt` on it. If
-- no value exists, call `fromTxt` on the empty `Txt`.
(?:|) :: FromTxt a => Dict -> Txt -> a
(?:|) (Dict st _) k = fromTxt $
  -- the stored value is optional; avoid the tracing in (?|)
  case Map.lookup k st of
    Just (Val s) ->
      case cast s of
        Just t   -> t
        Nothing  -> ""
    _            -> ""

-- Look up an optional `HTML` from the `Dict`, on failure try to get a `Txt`
-- value and call `fromTxt` on it. If that fails, return a nil node.
(?^|) :: Typeable a => Dict -> Txt -> Atom a
(?^|) (Dict st _) k =
  -- the stored value is optional; avoid the tracing in (?|)
  case Map.lookup k st of
    Just (Val s) -> case cast s of
      Just a     -> a
      Nothing    -> case cast s of
        Just t   -> fromTxt t
        _        -> nil
    _            -> nil

-- Look up an optional `Value` from the `Dict`. On failure, return `Null`.
(?*|) :: Dict -> Txt -> Value
(?*|) (Dict st _) k =
  case Map.lookup k st of
    Just (Val s) ->
      case cast s of
        Just v   -> v
#ifdef __GHCJS__
        Nothing  -> nullValue
    _            -> nullValue
#else
        Nothing  -> Null
    _            -> Null
#endif

newtype Controlling m e = Controlling m
type Controller m = Context '[] (Controlling m)
-- controller :: ContextKey '[] m -> m -> (m -> HTML '[] m) -> Controller m
controller :: Typeable m => ContextKey '[] (Controlling m) -> m -> (m -> Atom (Base (Controlling m))) -> Controller m
controller key0 model0 render0 = Context {..}
  where
    key = key0
    build = return
    prime = return ()
    model = Controlling model0
    render (Controlling m) = render0 m

type Static = Context '[] (Const ())
-- static :: ContextKey '[] () -> HTML [] () -> Static
static :: ContextKey '[] (Const ()) -> Atom (Base (Const ())) -> Static
static key0 render0 = Context {..}
  where
    key = key0
    build = return
    prime = return ()
    model = Const ()
    render _ = render0

type Observatory m = Service '[Observable m]
observatory :: ServiceKey '[Observable m] -> m -> Observatory m
observatory key initial = Service {..}
  where
    build base = do
      o <- observable initial
      return (o *:* base)
    prime = return ()

looks :: (MonadIO c, Typeable m) => Observatory m -> c m
looks o = do
  Right r <- demand =<< with o getO
  return r

change :: (MonadIO c, Typeable m) => Observatory m -> m -> c ()
change o m = void $ with o (setO m)

newtype Observing m e = Observing (Maybe m)
type Observer m = Context '[] (Observing m)
-- observer :: Observatory m -> ContextKey '[] (Maybe m) -> (m -> HTML '[] m) -> Observer m
observer :: forall m ms a w.
            ( Typeable m
            , With w (Code ms IO) (Code (Base (Observing m)) IO)
            , With w (Code ms IO) IO
            , '[Observable m] <: ms
            , Component a (Base (Observing m))
            )
         => w -> ContextKey '[] (Observing m) -> (m -> a (Base (Observing m))) -> Observer m
observer s key0 render0 = Context {..}
  where
    key = key0
    build = return
    prime = void $ observe' s $ \(m :: m) -> putModel (Observing $ Just m)
    model = Observing Nothing
    render (Observing Nothing) = nil :: HTML (Base (Observing m))
    render (Observing (Just m)) = construct $ render0 m

-- specialized to Observatory to avoid inline type signatures
observes :: (Typeable m, MonadIO c, '[Revent] <: ms) => Observatory m -> (m -> Code ms c ()) -> Code ms c (Promise (IO ()))
observes o f = observe o (Export.lift . f)

-- specialized to Observatory to avoid inline type signatures
observes' :: (Typeable m, MonadIO c, '[Revent] <: ms) => Observatory m -> (m -> Code ms c ()) -> Code ms c (Promise (IO ()))
observes' = observe'

newtype StaticHTML = StaticHTML { htmlText :: Txt } deriving (Eq,Ord)
instance ToTxt StaticHTML where
  toTxt (StaticHTML htmlt) = htmlt
instance FromTxt StaticHTML where
  fromTxt = StaticHTML
instance Monoid StaticHTML where
  mempty = fromTxt mempty
  mappend htmlt1 htmlt2 = fromTxt $ toTxt htmlt1 <> toTxt htmlt2
instance Lift StaticHTML where
  lift (StaticHTML htmlt) = [| StaticHTML htmlt |]

staticHTML :: Typeable e => HTML e -> StaticHTML
staticHTML = fromTxt . toTxt

shtml :: Typeable e => Txt -> [Feature e] -> StaticHTML -> Atom e
shtml _tag _attributes = raw (mkAtom _tag) _attributes . toTxt

selfClosing tag = tag `elem` selfclosing
  where
    selfclosing =
      ["area","base","br","col","frame","command"
      ,"embed","hr","img","input","keygen","link"
      ,"meta","param","source","track","wbr"
      ]

instance Typeable e => ToTxt (HTML e) where
  toTxt NullHTML {} = mempty

  toTxt Text {..} = _content

  toTxt Raw {..} =
    "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes)) <>
      ">" <> _content <> "</" <> _tag <> ">"

  toTxt KHTML {..} =
    "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes)) <>
      if selfClosing _tag then
        "/>"
      else
        ">" <> Txt.concat (map (toTxt . construct . snd) _keyed) <> "</" <> _tag <> ">"

  toTxt HTML {..} =
    "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes)) <>
      if selfClosing _tag then
        "/>"
      else
        ">" <> Txt.concat (map (toTxt . construct) _atoms) <> "</" <> _tag <> ">"

  toTxt STHTML {..} = toTxt $ construct (_stview _ststate (\_ _ -> return ()))

  toTxt SVGHTML {..} =
    "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes)) <>
      if selfClosing _tag then
        "/>"
      else
        ">" <> Txt.concat (map (toTxt . construct) _atoms) <> "</" <> _tag <> ">"

  toTxt KSVGHTML {..} =
    "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes)) <>
      if selfClosing _tag then
        "/>"
      else
        ">" <> Txt.concat (map (toTxt . construct . snd) _keyed) <> "</" <> _tag <> ">"

  toTxt Managed {..} =
    case _constr of
      Context' Context {..} ->
        "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes)) <>
          ">"  <> toTxt (construct $ render model) <> "</" <> _tag <> ">"

instance Typeable e => ToTxt [HTML e] where
  toTxt = mconcat . map toTxt

data System
  = System
    { getHead :: Constr
    , getContent :: Constr
    }
  | Subsystem
    { getContent :: Constr
    }
  deriving Eq

page :: ( IsContext' ts ms m
        , IsContext' ts' ms' m'
        )
     => Context' ts ms m
     -> Context' ts' ms' m'
     -> System
page h b = System (Context' h) (Context' b)

partial :: IsContext' ts ms m
        => Context' ts ms m
        -> System
partial = Subsystem . Context'

renderSystem :: System -> Txt
renderSystem (System h c) =
  "<!DOCTYPE html>" <>
    case h of
      Context' Context {..} ->
        let htm :: Atom '[]
            htm =
              Html []
                [ unsafeCoerce $ toAtom $ construct $ render model
                , Body []
                    [ case c of
                        Context' a@Context {} -> context Div [ Id "atomic" ] a
                    ]
                ]
        in
          toTxt (construct htm)
renderSystem (Subsystem c) =
  ("<!DOCTYPE html>" <>) $
    let htm :: Atom '[]
        htm =
          Html []
            [ Head [] []
            , Body []
                [ case c of
                    Context' a@Context {} -> context Div [ Id "atomic" ] a
                ]
            ]
    in
      toTxt (construct htm)

renderSystemBootstrap :: System -> Txt -> Txt
renderSystemBootstrap (System h c) mainScript =
  "<!DOCTYPE html>" <>
    case h of
      Context' Context {..} ->
        let htm :: Atom '[]
            htm =
              Html []
                [ Head [] []
                , Body []
                    [ case c of
                        Context' a@Context {} -> context Div [ Id "atomic" ] a
                    , Script [ Src mainScript, Defer True ] []
                    ]
                ]
        in
          toTxt (construct htm)
renderSystemBootstrap (Subsystem c) mainScript =
  "<!DOCTYPE html>" <>
    case c of
      Context' a@Context {} ->
        let htm :: Atom '[]
            htm =
              Html []
                [ Head [] []
                , Body []
                    [ context Div [ Id "atomic" ] a
                    , Script [ Src mainScript, Defer True ] []
                    ]
                ]
        in
          toTxt (construct htm)

renderDynamicSystem :: System -> IO Txt
renderDynamicSystem (System (Context' h) (Context' c)) = do
  let dt = "<!DOCTYPE html><html>"
  Just h_ <- demandMaybe =<< currentView h
  head_html <- renderDynamicHTML h_
  let bdy :: Atom '[]
      bdy = Body [] [ context Div [ Id "atomic" ] c]
  body_html <- renderDynamicHTML (construct bdy)
  return $ dt <> head_html <> body_html <> "</html>"
renderDynamicSystem (Subsystem (Context' c)) = do
  let dt = "<!DOCTYPE html><head></head>"
  Just c_ <- demandMaybe =<< currentView c
  let bdy :: Atom '[]
      bdy = Body [] [ context Div [ Id "atomic" ] c ]
  body_html <- renderDynamicHTML (construct bdy)
  return $ dt <> body_html <> "</html>"

renderDynamicSystemBootstrap :: System -> Txt -> IO Txt
renderDynamicSystemBootstrap (System (Context' h) (Context' c)) mainScript = do
  let dt = "<!DOCTYPE html><html>"
  Just h_ <- demandMaybe =<< currentView h
  head_html <- renderDynamicHTML h_
  let bdy :: Atom '[]
      bdy = Body [] [ context Div [ Id "atomic" ] c, Script [ Src mainScript, Defer True ] [] ]
  body_html <- renderDynamicHTML (construct bdy)
  return $ dt <> head_html <> body_html <> "</html>"
renderDynamicSystemBootstrap (Subsystem (Context' c)) mainScript = do
  let dt = "<!DOCTYPE html><html><head></head>"
  let bdy :: Atom '[]
      bdy = Body [] [ context Div [ Id "atomic" ] c, Script [ Src mainScript, Defer True ] [] ]
  body_html <- renderDynamicHTML (construct bdy)
  return $ dt <> body_html <> "</html>"

renderDynamicHTML :: forall e. Typeable e => HTML e -> IO Txt
renderDynamicHTML h =
  case h of
    NullHTML {} -> return mempty

    Text {..} -> return _content

    Raw {..} ->
      return $
        "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes))
          <> ">"<> _content <> "</" <> _tag <> ">"

    KHTML {..} -> do
      cs <- mapM (\(_,c) -> renderDynamicHTML (fromJust $ fromAtom c)) _keyed
      return $
        "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes))
          <> if selfClosing _tag then
               "/>"
             else
               ">" <> Txt.concat cs <> "</" <> _tag <> ">"

    HTML {..} -> do
      cs <- mapM renderDynamicHTML (map (fromJust . fromAtom ) _atoms)
      return $
        "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes))
          <> if selfClosing _tag then
              "/>"
            else
              ">" <> Txt.concat cs <> "</" <> _tag <> ">"

    STHTML {..} -> do
      case _strecord of
        Nothing ->
          return $ toTxt $ construct $ _stview _ststate (\_ _ -> return ())
        Just str -> do
          (_,_,a,_) <- readIORef str
          renderDynamicHTML (unsafeCoerce a :: HTML e)

    SVGHTML {..} -> do
      cs <- mapM renderDynamicHTML (map (fromJust . fromAtom) _atoms)
      return $
        "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes))
          <> if selfClosing _tag then
               "/>"
             else
               ">" <> Txt.concat cs <> "</" <> _tag <> ">"

    KSVGHTML {..} -> do
      cs <- mapM (\(_,c) -> renderDynamicHTML (fromJust $ fromAtom c)) _keyed
      return $
        "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes))
          <> if selfClosing _tag then
               "/>"
             else
               ">" <> Txt.concat cs <> "</" <> _tag <> ">"



    Managed {..} ->
      case _constr of
        Context' a@Context {..} -> do
          Just v <- demandMaybe =<< currentView a
          inner <- renderDynamicHTML v
          return $
            "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes))
              <> ">"  <> inner <> "</" <> _tag <> ">"
