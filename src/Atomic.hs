{-# language CPP #-}
{-# language TemplateHaskell #-}
{-# language OverloadedStrings #-}
module Atomic
  ( module Atomic
  , module Export
  , Atom(..)
  , html, jss, ujss, raw
  , cnode, keyed, svgHTML, construct, tagged, hashed
  , css, css', scss, scss', styles
  , diff, setManualDiff, setEagerDiff, setLazyDiff
  , observe, set, update
  , onModelChange, onViewChange, ownView, currentView
  ) where

import Ef.Base as Export hiding (watch,transform,construct)

import Data.Hashable as Export
import Data.Typeable as Export
import GHC.Generics as Export (Generic)

import Atomic.Construct
import Atomic.Mediator

import qualified Data.Txt as Export (Txt(..))
import Data.JSON         as Export hiding (defaultOptions,Options,(!))
import Data.Millis       as Export
import Data.Micros       as Export
import Atomic.API        as Export
import Atomic.Attribute  as Export
import Atomic.CSS        as Export
import Atomic.Cond       as Export
import Atomic.Debounce   as Export
import Atomic.Default    as Export
import Atomic.Dispatch   as Export
import Atomic.Ease       as Export
import Atomic.Endpoint   as Export
import Atomic.FromBS     as Export
import Atomic.FromTxt    as Export
import Atomic.HTML       as Export
import Atomic.Indexed    as Export
import Atomic.Key        as Export
import Atomic.Lazy       as Export
import Atomic.Limit      as Export
import Atomic.Memo       as Export
import Atomic.Message    as Export
import Atomic.Normalize  as Export
import Atomic.Observable as Export
import Atomic.Render     as Export
import Atomic.Request    as Export
import Atomic.Revent     as Export
import Atomic.Route      as Export hiding (route)
import Atomic.Router     as Export
import Atomic.Mediators  as Export
import Atomic.Signals    as Export
import Atomic.Strict     as Export
import Atomic.Throttle   as Export
import Atomic.ToBS       as Export
import Atomic.ToTxt      as Export
import Atomic.Try        as Export
import Atomic.TypeRep    as Export
import Atomic.UnsafeEq   as Export
import Atomic.Vault      as Export
#ifdef __GHCJS__
import Atomic.WebSocket  as Export hiding (LazyByteString)
#else
import Atomic.WebSocket  as Export hiding (LazyByteString,accept)
#endif
import Atomic.With       as Export

import Data.ByteString as Export (ByteString)

import Prelude as Export hiding (all,exponent,div,head,span,tan,lookup,reverse)
import qualified Prelude
import Data.Monoid as Export
import Data.Bifunctor as Export

import Data.HashMap.Strict as Map hiding (map,null,update)

import Data.Txt as Txt hiding (head,map,null)
import qualified Data.Txt as Txt
import Data.JSON as JSON

import Language.Haskell.TH.Syntax

instance FromMillis Micros where
 --  fromMillis :: Millis -> Micros
  fromMillis jt = Micros $ (getMillis jt) * 1000

instance FromMicros Millis where
  -- fromMicrotime :: Micros -> Millis
  -- truncate rather than round
  fromMicros mt = Millis $ (getMicros mt) `Prelude.div` 1000

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

type Controller m = Construct '[] m
-- controller :: ConstructKey '[] m -> m -> (m -> HTML '[] m) -> Controller m
controller :: ConstructKey '[] m -> m -> (m -> Atom (Code (ConstructBase m) IO ())) -> Controller m
controller key0 model0 view0 = Construct {..}
  where
    key = key0
    build = return
    prime = return ()
    model = model0
    view = view0

type Static = Construct '[] ()
-- static :: ConstructKey '[] () -> HTML [] () -> Static
static :: ConstructKey '[] () -> Atom (Code (ConstructBase ()) IO ()) -> Static
static key0 view0 = Construct {..}
  where
    key = key0
    build = return
    prime = return ()
    model = ()
    view _ = view0

type Store m = Mediator '[Observable m]
store :: MediatorKey '[Observable m] -> m -> Store m
store key initial = Mediator {..}
  where
    build base = do
      o <- observable initial
      return (o *:* base)
    prime = return ()

type Observer m = Construct '[] (Maybe m)
-- observer :: Store m -> ConstructKey '[] (Maybe m) -> (m -> HTML '[] m) -> Observer m
observer :: forall m ms w. (Eq m, With w (Code ms IO) (Code (ConstructBase (Maybe m)) IO), With w (Code ms IO) IO, '[Observable m] <: ms)
         => w -> ConstructKey '[] (Maybe m) -> (m -> Atom (Code (ConstructBase (Maybe m)) IO ())) -> Observer m
observer s key0 view0 = Construct {..}
  where
    key = key0
    build = return
    prime = void $ watch' s (set . Just :: m -> Code (ConstructBase (Maybe m)) IO ())
    model = Nothing
    view Nothing = nil
    view (Just m) = view0 m

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
staticHTML = render

shtml :: Txt -> [Feature e] -> StaticHTML -> Atom e
shtml _tag _attributes = raw _tag _attributes . toTxt

type HTML ms m = Atom (Code (Appended (ConstructBase m) ms) IO ())
type Attribute ms m = Atom (Code (Appended (ConstructBase m) ms) IO ())

instance ToTxt (Feature e) where
  toTxt NullFeature          = mempty

  toTxt (Attribute attr val) =
    case val of
      Left b  -> attr <> "=" <> if b then "true" else "false"
      Right v -> attr <> "=\"" <> v <> "\""

  toTxt (Style pairs) =
    "style=\""
      <> Txt.intercalate
           (Txt.singleton ';')
           (renderStyles False (mapM_ (uncurry (=:)) pairs))
      <> "\""

  toTxt (CurrentValue _) = mempty

  toTxt (On _ _ _)       = mempty

  toTxt (On' _ _ _ _)    = mempty

  toTxt (Link href _)    = "href=\"" <> href <> "\""

instance ToTxt [Feature e] where
  toTxt fs =
    Txt.intercalate
     (Txt.singleton ' ')
     (Prelude.filter (not . Txt.null) $ Prelude.map toTxt fs)

selfClosing tag =
  case tag of
    "area"    -> True
    "base"    -> True
    "br"      -> True
    "col"     -> True
    "command" -> True
    "embed"   -> True
    "hr"      -> True
    "img"     -> True
    "input"   -> True
    "keygen"  -> True
    "link"    -> True
    "meta"    -> True
    "param"   -> True
    "source"  -> True
    "track"   -> True
    "wbr"     -> True
    _         -> False

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
        ">" <> Txt.concat (map toTxt _children) <> "</" <> _tag <> ">"
  toTxt SVGAtom {..} =
    "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes)) <>
      if selfClosing _tag then
        "/>"
      else
        ">" <> Txt.concat (map toTxt _children) <> "</" <> _tag <> ">"
  toTxt Managed {..} =
    case _constr of
      Construct' Construct {..} ->
        "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes)) <>
          ">"  <> toTxt (view model) <> "</" <> _tag <> ">"

data System
  = System
    { getHead :: Constr
    , getContent :: Constr
    }
  | Subsystem
    { getContent :: Constr
    }
  deriving Eq

page :: ( IsConstruct' ts ms m
        , IsConstruct' ts' ms' m'
        )
     => Construct' ts ms m
     -> Construct' ts' ms' m'
     -> System
page h b = System (Construct' h) (Construct' b)

partial :: IsConstruct' ts ms m
        => Construct' ts ms m
        -> System
partial = Subsystem . Construct'

renderSystem :: System -> Txt
renderSystem (System h c) =
  "<!DOCTYPE html>" <>
    case h of
      Construct' Construct {..} ->
        toTxt $
          html_ []
            [ view model
            , body []
                [ case c of
                    Construct' a@Construct {} -> construct div [ idA "atomic" ] a
                ]
            ]
renderSystem (Subsystem c) =
  ("<!DOCTYPE html>" <>) $
    toTxt $
      html_ []
        [ head []
        , body []
            [ case c of
                Construct' a@Construct {} -> construct div [ idA "atomic" ] a
            ]
        ]

renderSystemBootstrap :: System -> Txt -> Txt
renderSystemBootstrap (System h c) mainScript =
  "<!DOCTYPE html>" <>
    case h of
      Construct' Construct {..} ->
        toTxt $
          html_ []
            [ view model
            , body []
                [ case c of
                    Construct' a@Construct {} -> construct div [ idA "atomic" ] a
                , script [ src mainScript, defer "defer" ] []
                ]
            ]
renderSystemBootstrap (Subsystem c) mainScript =
  "<!DOCTYPE html>" <>
    case c of
      Construct' a@Construct {} ->
        toTxt $
          html_ []
            [ head []
            , body []
                [ construct div [ idA "atomic" ] a
                , script [ src mainScript, defer "defer" ] []
                ]
            ]

renderDynamicSystem :: System -> IO Txt
renderDynamicSystem (System (Construct' h) (Construct' c)) = do
  let dt = "<!DOCTYPE html>"
  Just h_ <- demandMaybe =<< currentView h
  Just c_ <- demandMaybe =<< currentView c
  head_html <- renderDynamicHTML h_
  body_html <- renderDynamicHTML c_
  return $ (dt <>) $ toTxt $
    html_ []
      [ raw "head" [] head_html
      , body []
          [ raw "div" [ idA "atomic" ] body_html ]
      ]
renderDynamicSystem (Subsystem (Construct' c)) = do
  let dt = "<!DOCTYPE html>"
  Just c_ <- demandMaybe =<< currentView c
  body_html <- renderDynamicHTML c_
  return $ (dt <>) $ toTxt $
    html_ []
      [ head []
      , body []
          [ raw "div" [ idA "atomic" ] body_html ]
      ]

renderDynamicSystemBootstrap :: System -> Txt -> IO Txt
renderDynamicSystemBootstrap (System (Construct' h) (Construct' c)) mainScript = do
  let dt = "<!DOCTYPE html>"
  Just h_ <- demandMaybe =<< currentView h
  Just c_ <- demandMaybe =<< currentView c
  head_html <- renderDynamicHTML h_
  body_html <- renderDynamicHTML c_
  return $ (dt <>) $ toTxt $
    html_ []
      [ raw "head" [] head_html
      , body []
          [ raw "div" [ idA "atomic" ] body_html
          , script [ src mainScript, defer "defer" ] []
          ]
      ]
renderDynamicSystemBootstrap (Subsystem (Construct' c)) mainScript = do
  let dt = "<!DOCTYPE html>"
  Just c_ <- demandMaybe =<< currentView c
  body_html <- renderDynamicHTML c_
  return $ (dt <>) $ toTxt $
    html_ []
      [ head []
      , body []
          [ raw "div" [ idA "atomic" ] body_html
          , script [ src mainScript, defer "defer" ] []
          ]
      ]

renderDynamicHTML :: Atom e -> IO Txt
renderDynamicHTML h =
  case h of
    NullAtom {} -> return mempty

    Text {..} -> return _content

    Raw {..} ->
      return $
        "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes))
          <> ">"<> _content <> "</" <> _tag <> ">"

    KAtom {..} ->
      return $
        "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes))
          <> if selfClosing _tag then
               "/>"
             else
               ">" <> Txt.concat (map (toTxt . snd) _keyed) <> "</" <> _tag <> ">"

    Atom {..} ->
      return $
        "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes))
          <> if selfClosing _tag then
               "/>"
             else
               ">" <> Txt.concat (map toTxt _children) <> "</" <> _tag <> ">"

    SVGAtom {..} ->
      return $
        "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes))
          <> if selfClosing _tag then
               "/>"
             else
               ">" <> Txt.concat (map toTxt _children) <> "</" <> _tag <> ">"

    Managed {..} ->
      case _constr of
        Construct' a@Construct {..} -> do
          Just v <- demandMaybe =<< currentView a
          inner <- renderDynamicHTML v
          return $
            "<" <> _tag <> (if null _attributes then "" else " " <> Txt.intercalate " " (map toTxt _attributes))
              <> ">"  <> inner <> "</" <> _tag <> ">"

