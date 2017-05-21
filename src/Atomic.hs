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
  , Atom(..)
  , mkAtom, mkSVGAtom, text, unindent, raw, keyed, hashed, component, st
  , css, css', scss, scss', styles
  , diff, setManualDiff, setEagerDiff
  , getModel, putModel, modifyModel
  , onModelChange, onOwnModelChange, onOwnModelChangeByProxy, ownView, currentView
  , ComponentHooks(..)
  , ComponentRecord(..)
  , ComponentView(..)
  , LazyByteString
  ) where

import Ef.Base as Export hiding (As,Index,transform,observe,uncons,distribute,embed)

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
  (lazy,Empty,none,(<~),(.=),(<.>))
import Control.Lens.Extras as Export

import Data.Function as Export hiding (on)
import Data.Bifunctor as Export
import Data.Bool as Export
import Data.Maybe as Export
import Data.Void as Export

import qualified Data.Txt as Export (Txt(..))
import Data.JSON         as Export hiding (defaultOptions,Options,(!))
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

import Prelude as Export hiding (all,exponent,div,head,span,tan,lookup,reverse)
import qualified Prelude
import Data.Monoid as Export
import Data.Bifunctor as Export

import Data.HashMap.Strict as Map hiding (map,null,update)
import Data.Tree as Tree

import Data.Txt as Txt hiding (head,map,null)
import qualified Data.Txt as Txt
import Data.JSON as JSON

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
-- turning some identifier (w) witnessing ToTxt into a Key for a component:
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

-- Look up an optional `Atom` from the `Dict`, on failure try to get a `Txt`
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

type Controller m = Component '[] m
-- controller :: ComponentKey '[] m -> m -> (m -> HTML '[] m) -> Controller m
controller :: ComponentKey '[] m -> m -> (m -> Atom (Code (Base m) IO ())) -> Controller m
controller key0 model0 render0 = Component {..}
  where
    key = key0
    build = return
    prime = return ()
    model = model0
    render = render0

type Static = Component '[] ()
-- static :: ComponentKey '[] () -> HTML [] () -> Static
static :: ComponentKey '[] () -> Atom (Code (Base ()) IO ()) -> Static
static key0 render0 = Component {..}
  where
    key = key0
    build = return
    prime = return ()
    model = ()
    render _ = render0

type Observatory m = Service '[Observable m]
observatory :: ServiceKey '[Observable m] -> m -> Observatory m
observatory key initial = Service {..}
  where
    build base = do
      o <- observable initial
      return (o *:* base)
    prime = return ()

looks :: MonadIO c => Observatory m -> c m
looks o = do
  Right r <- demand =<< with o getO
  return r

change :: MonadIO c => Observatory m -> m -> c ()
change o m = void $ with o (setO m)

type Observer m = Component '[] (Maybe m)
-- observer :: Observatory m -> ComponentKey '[] (Maybe m) -> (m -> HTML '[] m) -> Observer m
observer :: forall m ms w. (Eq m, With w (Code ms IO) (Code (Base (Maybe m)) IO), With w (Code ms IO) IO, '[Observable m] <: ms)
         => w -> ComponentKey '[] (Maybe m) -> (m -> Atom (Code (Base (Maybe m)) IO ())) -> Observer m
observer s key0 render0 = Component {..}
  where
    key = key0
    build = return
    prime = void $ observe' s $ \(m :: m) -> putModel (Just m)
    model = Nothing
    render Nothing = nil
    render (Just m) = render0 m

-- specialized to Observatory to avoid inline type signatures
observes :: (MonadIO c, '[Revent] <: ms) => Observatory m -> (m -> Code ms c ()) -> Code ms c (Promise (IO ()))
observes o f = observe o (Export.lift . f)

-- specialized to Observatory to avoid inline type signatures
observes' :: (MonadIO c, '[Revent] <: ms) => Observatory m -> (m -> Code ms c ()) -> Code ms c (Promise (IO ()))
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

staticHTML :: Atom e -> StaticHTML
staticHTML = fromTxt . toTxt

shtml :: Txt -> [Feature e] -> StaticHTML -> Atom e
shtml _tag _attributes = raw (mkAtom _tag) _attributes . toTxt

{-
Try this in 8.0

type family HTML ms m = h | h -> ms, h -> m where
  HTML ms m = Atom (C ms m ())

type family Attribute ms m = a | a -> ms, a -> m where
  Attribute ms m = Feature (C ms m ())

type family C ms m a = c | c -> ms, c -> m, c -> a where
  C ms m a = Code (Appended ms (Base m)) IO a
-}

-- These will have to suffice for now with 7.10
type HTML ms m = Atom (Code (Appended ms (Base m)) IO ())
type Attribute ms m = Feature (Code (Appended ms (Base m)) IO ())
type C ms m a = Code (Appended ms (Base m)) IO a

selfClosing tag = tag `elem` selfclosing
  where
    selfclosing =
      ["area","base","br","col","frame","command"
      ,"embed","hr","img","input","keygen","link"
      ,"meta","param","source","track","wbr"
      ]

instance ToTxt (Atom e) where
  toTxt NullAtom {} = mempty

  toTxt Text {..} = _content

  toTxt Raw {..} =
    "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes)) <>
      ">" <> _content <> "</" <> _tag <> ">"

  toTxt KAtom {..} =
    "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes)) <>
      if selfClosing _tag then
        "/>"
      else
        ">" <> Txt.concat (map (toTxt . snd) _keyed) <> "</" <> _tag <> ">"

  toTxt Atom {..} =
    "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes)) <>
      if selfClosing _tag then
        "/>"
      else
        ">" <> Txt.concat (map toTxt _atoms) <> "</" <> _tag <> ">"

  toTxt STAtom {..} = toTxt (_stview _ststate (\_ _ -> return ()))

  toTxt SVGAtom {..} =
    "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes)) <>
      if selfClosing _tag then
        "/>"
      else
        ">" <> Txt.concat (map toTxt _atoms) <> "</" <> _tag <> ">"

  toTxt KSVGAtom {..} =
    "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes)) <>
      if selfClosing _tag then
        "/>"
      else
        ">" <> Txt.concat (map (toTxt . snd) _keyed) <> "</" <> _tag <> ">"

  toTxt Managed {..} =
    case _constr of
      Component' Component {..} ->
        "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes)) <>
          ">"  <> toTxt (render model) <> "</" <> _tag <> ">"

instance ToTxt [Atom e] where
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

page :: ( IsComponent' ts ms m
        , IsComponent' ts' ms' m'
        )
     => Component' ts ms m
     -> Component' ts' ms' m'
     -> System
page h b = System (Component' h) (Component' b)

partial :: IsComponent' ts ms m
        => Component' ts ms m
        -> System
partial = Subsystem . Component'

renderSystem :: System -> Txt
renderSystem (System h c) =
  "<!DOCTYPE html>" <>
    case h of
      Component' Component {..} ->
        toTxt $
          htmlE []
            [ render model
            , body []
                [ case c of
                    Component' a@Component {} -> component div [ idA "atomic" ] a
                ]
            ]
renderSystem (Subsystem c) =
  ("<!DOCTYPE html>" <>) $
    toTxt $
      htmlE []
        [ head []
        , body []
            [ case c of
                Component' a@Component {} -> component div [ idA "atomic" ] a
            ]
        ]

renderSystemBootstrap :: System -> Txt -> Txt
renderSystemBootstrap (System h c) mainScript =
  "<!DOCTYPE html>" <>
    case h of
      Component' Component {..} ->
        toTxt $
          htmlE []
            [ head []
            , body []
                [ case c of
                    Component' a@Component {} -> component div [ idA "atomic" ] a
                , script [ src mainScript, defer True ] []
                ]
            ]
renderSystemBootstrap (Subsystem c) mainScript =
  "<!DOCTYPE html>" <>
    case c of
      Component' a@Component {} ->
        toTxt $
          htmlE []
            [ head []
            , body []
                [ component div [ idA "atomic" ] a
                , script [ src mainScript, defer True ] []
                ]
            ]

renderDynamicSystem :: System -> IO Txt
renderDynamicSystem (System (Component' h) (Component' c)) = do
  let dt = "<!DOCTYPE html><html>"
  Just h_ <- demandMaybe =<< currentView h
  head_html <- renderDynamicHTML h_
  body_html <- renderDynamicHTML (body [] [ component div [ idA "atomic" ] c])
  return $ dt <> head_html <> body_html <> "</html>"
renderDynamicSystem (Subsystem (Component' c)) = do
  let dt = "<!DOCTYPE html><head></head>"
  Just c_ <- demandMaybe =<< currentView c
  body_html <- renderDynamicHTML (body [] [ component div [ idA "atomic" ] c ])
  return $ dt <> body_html <> "</html>"

renderDynamicSystemBootstrap :: System -> Txt -> IO Txt
renderDynamicSystemBootstrap (System (Component' h) (Component' c)) mainScript = do
  let dt = "<!DOCTYPE html><html>"
  Just h_ <- demandMaybe =<< currentView h
  head_html <- renderDynamicHTML h_
  body_html <- renderDynamicHTML (body [] [ component div [ idA "atomic" ] c, script [ src mainScript, defer True ] [] ])
  return $ dt <> head_html <> body_html <> "</html>"
renderDynamicSystemBootstrap (Subsystem (Component' c)) mainScript = do
  let dt = "<!DOCTYPE html><html><head></head>"
  body_html <- renderDynamicHTML (body [] [ component div [ idA "atomic" ] c, script [ src mainScript, defer True ] [] ])
  return $ dt <> body_html <> "</html>"

renderDynamicHTML :: Atom e -> IO Txt
renderDynamicHTML h =
  case h of
    NullAtom {} -> return mempty

    Text {..} -> return _content

    Raw {..} ->
      return $
        "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes))
          <> ">"<> _content <> "</" <> _tag <> ">"

    KAtom {..} -> do
      cs <- mapM (\(_,c) -> renderDynamicHTML c) _keyed
      return $
        "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes))
          <> if selfClosing _tag then
               "/>"
             else
               ">" <> Txt.concat cs <> "</" <> _tag <> ">"

    Atom {..} -> do
      cs <- mapM renderDynamicHTML _atoms
      return $
        "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes))
          <> if selfClosing _tag then
              "/>"
            else
              ">" <> Txt.concat cs <> "</" <> _tag <> ">"

    STAtom {..} -> do
      case _strecord of
        Nothing -> return $ toTxt (_stview _ststate (\_ _ -> return ()))
        Just str -> do
          (_,_,SomeAtom a,_) <- readIORef str
          renderDynamicHTML a

    SVGAtom {..} -> do
      cs <- mapM renderDynamicHTML _atoms
      return $
        "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes))
          <> if selfClosing _tag then
               "/>"
             else
               ">" <> Txt.concat cs <> "</" <> _tag <> ">"

    KSVGAtom {..} -> do
      cs <- mapM (\(_,c) -> renderDynamicHTML c) _keyed
      return $
        "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes))
          <> if selfClosing _tag then
               "/>"
             else
               ">" <> Txt.concat cs <> "</" <> _tag <> ">"



    Managed {..} ->
      case _constr of
        Component' a@Component {..} -> do
          Just v <- demandMaybe =<< currentView a
          inner <- renderDynamicHTML v
          return $
            "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes))
              <> ">"  <> inner <> "</" <> _tag <> ">"
