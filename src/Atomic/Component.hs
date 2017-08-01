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

{-# language ImplicitParams #-}
module Atomic.Component (module Atomic.Component, ENode, TNode, NNode, Win, Doc, Loc) where

import Ef.Base hiding (Object,Client,After,Before,child,current,Lazy,Eager,construct,Index,observe,uncons,distribute,embed,initialize)
import qualified Ef.Base

import qualified Data.Foldable as F

import Data.Txt as Txt hiding (replace,map,head,filter)
import Data.JSON hiding (Result)

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
import qualified GHCJS.DOM.JSFFI.Generated.EventTarget as Ev
import qualified GHCJS.DOM.JSFFI.Generated.Event as Event
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
import GHC.Generics
import GHC.Prim

import qualified Data.Function as F

import qualified Data.IntMap.Strict as IM

import qualified Data.HashMap.Strict as Map

import Prelude
import Language.Haskell.TH hiding (Loc)
import Language.Haskell.TH.Syntax hiding (Loc)

import System.IO.Unsafe
import Unsafe.Coerce

-- import Control.Lens (Iso,iso,makePrisms,makeLenses,preview,review)
-- import Control.Lens.Plated (Plated(..))
-- import Control.Lens.At
-- import Control.Lens.Prism
-- import Control.Lens.Setter hiding ((.=))

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

newtype Old a = Old { getOld :: a }
newtype Props props = Props { getP :: props }
newtype St state = St { getS :: state }

askState ::
  ( ?state :: Proxy state
  , ms <: '[Reader () (St state)]
  , Monad c
  ) => Ef ms c state
askState = asks getS

askOldState ::
  ( ?state :: Proxy state
  , ms <: '[Reader () (Old (St state))]
  , Monad c
  ) => Ef ms c state
askOldState = asks (getS . getOld)

getState ::
  ( ?state :: Proxy state
  , ms <: '[State () (St state)]
  , Monad c
  ) => Ef ms c state
getState = getS <$> get

setState ::
  ( ?state :: Proxy state
  , ms <: '[State () (St state)]
  , Monad c
  ) => state
    -> Ef ms c ()
setState = put . St

modifyState ::
  ( ms <: '[State () (St state)]
  , Monad c
  ) => (state -> state)
    -> Ef ms c ()
modifyState f = void $ modify $ \(St s) -> (St (f s),())

props ::
  ( ms <: '[Reader () (Props props)]
  , Monad c
  ) => Ef ms c props
props = asks getP

oldProps ::
  ( ms <: '[Reader () (Old (Props props))]
  , Monad c
  ) => Ef ms c props
oldProps = asks (getP . getOld)

getDOMTree :: ( ms <: '[Reader () (View parent)]
              , parent <: '[]
              , ms <: '[]
              ) => Ef ms IO (View parent)
getDOMTree = ask

data Component (parent :: [* -> *]) (props :: *) (state :: *) =
    ( Comment "For each method, props are available via ask, but are wrapped in"
          ::: "Old and New in willReceiveProps and shouldUpdate."
          ::: "State is available via get/put with previous state available via"
          ::: "ask in shouldUpdate. Current state is only available via ask in"
          ::: "willUnmount."
    )
  => Component
    { constructor ::
        ( Method "constructor"
            '[ Description '[ "One-time initialization with access to props." ]
             , Use '[ "Use constructor to initialize state defaults." ]
             , Errors '[ "State update in constructor is an error." ]
             ]
        ) => Ef '[ Reader () (Props props) ] IO state

    , initialize ::
        ( Method "initialize"
            '[ Description '[ "A version of willMount that only runs on the server." ]
             , Use '[ "Use initialize for synchronous component seeding." ]
             , Errors '[ "State update in initialize is an error." ]
             ]
        ) => Ef '[ Reader () (Props props)
                , State () (St state)
                ] IO ()

    , willMount ::
        (

        ) => Ef '[ Reader () (Props props)
                 , Reader () (St state)
                 ] IO ()

    , didMount ::
        (

        ) => Ef '[ Reader () (Props props)
                 , Reader () (St state)
                 , Reader () (View parent)
                 ] IO ()

    , willReceiveProps ::
        (

        ) => Ef '[ Reader () (Old (Props props))
                 , Reader () (Props props)
                 , State () (St state)
                 , Reader () (View parent)
                 ] IO ()

    , shouldUpdate ::
        (

        ) => Ef '[ Reader () (Old (Props props))
                 , Reader () (Props props)
                 , Reader () (Old (St state))
                 , Reader () (St state)
                 , Reader () (View parent)
                 ] IO Bool

    , willUpdate ::
        (

        ) => Ef '[ Reader () (Props props)
                 , Reader () (St state)
                 , Reader () (View parent)
                 ] IO ()

    , didUpdate ::
        (

        ) => Ef '[ Reader () (Props props)
                 , Reader () (St state)
                 , Reader () (View parent)
                 ] IO ()

    , willUnmount ::
        (

        ) => Ef '[ Reader () (Props props)
                 , Reader () (St state)
                 ] IO ()

    , renderer ::
        (

        ) => props -> state -> StateUpdate props state -> View parent

    }

instance Default (Component parent props state) where
  def =
    Component
      { renderer = \_ _ _ -> nil
      , willUnmount = def
      , didUpdate = def
      , willUpdate = def
      , shouldUpdate = return True
      , willReceiveProps = def
      , didMount = def
      , willMount = def
      , initialize = def
      , constructor = error "Component.constructor: state not initialized."
      }

runConstructor
  :: ( Typeable parent, Typeable props, Typeable state
     , parent <: '[]
     , component ~ Component parent props state
     ) => props -> component -> IO state
runConstructor props comp = do
  let obj = Ef.Base.Object $
              reader (Props props)
              *:* Ef.Base.Empty
  (_,state) <- obj Ef.Base.! constructor comp
  return state

runInitialize
  :: ( Typeable parent, Typeable props, Typeable state
     , parent <: '[]
     , component ~ Component parent props state
     ) => props -> state -> component -> IO state
runInitialize props state comp = do
  let obj = Ef.Base.Object $
              reader (Props props)
              *:* Ef.Base.state (St state)
              *:* Ef.Base.Empty
  (_,st) <- obj Ef.Base.! do
    initialize comp
    getS <$> get
  return st

runWillMount
  :: ( Typeable parent, Typeable props, Typeable state
     , parent <: '[]
     , component ~ Component parent props state
     ) => props -> state -> component -> IO ()
runWillMount props state comp = do
  let obj = Ef.Base.Object $
              reader (Props props)
              *:* reader (St state)
              *:* Ef.Base.Empty
  _ <- obj Ef.Base.! willMount comp
  return ()

runDidMount
  :: ( Typeable parent, Typeable props, Typeable state
     , parent <: '[]
     , component ~ Component parent props state
     ) => props -> state -> component -> View parent -> IO ()
runDidMount props state comp view = do
  let obj = Ef.Base.Object $
              reader (Props props)
              *:* reader (St state)
              *:* reader view
              *:* Ef.Base.Empty
  _ <- obj Ef.Base.! didMount comp
  return ()

runRenderer :: props -> state -> Component parent props state -> StateUpdate props state -> View parent
runRenderer props state Component {..} upd = renderer props state upd

runWillReceiveProps
  :: ( Typeable parent, Typeable props, Typeable state
     , parent <: '[]
     , component ~ Component parent props state
     , ?state :: Proxy state
     ) => props -> props -> state -> component -> View parent -> IO state
runWillReceiveProps old_props new_props state comp view = do
  let obj = Ef.Base.Object $
              reader (Old (Props old_props))
              *:* reader (Props new_props)
              *:* Ef.Base.state (St state)
              *:* reader view
              *:* Ef.Base.Empty
  (_,st) <- obj Ef.Base.! do
    willReceiveProps comp
    getS <$> get
  return st

-- One too many traits for GHC again?
runShouldUpdate
  :: forall parent props state component.
     ( Typeable parent, Typeable props, Typeable state
     , parent <: '[]
     , component ~ Component parent props state
     ) => props -> props -> state -> state -> component -> View parent -> IO Bool
runShouldUpdate old_props new_props old_state new_state comp view = do
  let obj = Ef.Base.Object $
              reader (Old (Props old_props))
              *:* reader (Props new_props)
              *:* reader (Old (St old_state))
              *:* reader (St new_state)
              *:* reader view
              *:* Ef.Base.Empty
  (_,shouldUpd) <- obj Ef.Base.! shouldUpdate comp
  return shouldUpd

runWillUpdate
  :: ( Typeable parent, Typeable props, Typeable state
     , parent <: '[]
     , component ~ Component parent props state
     ) => props -> state -> component -> View parent -> IO ()
runWillUpdate props state comp view = do
  let obj = Ef.Base.Object $
              reader (Props props)
              *:* reader (St state)
              *:* reader view
              *:* Ef.Base.Empty
  _ <- obj Ef.Base.! willUpdate comp
  return ()

runDidUpdate
  :: ( Typeable parent, Typeable props, Typeable state
     , parent <: '[]
     , component ~ Component parent props state
     ) => props -> state -> component -> View parent -> IO ()
runDidUpdate props state comp view = do
  let obj = Ef.Base.Object $
              reader (Props props)
              *:* reader (St state)
              *:* reader view
              *:* Ef.Base.Empty
  _ <- obj Ef.Base.! didUpdate comp
  return ()

runWillUnmount :: ( Typeable props
                  , Typeable state
                  , component ~ Component parent props state
                  )
               => props -> state -> component -> IO ()
runWillUnmount props state comp = do
  let obj = Ef.Base.Object $
              reader (Props props)
              *:* reader (St state)
              *:* Ef.Base.Empty
  _ <- obj Ef.Base.! (willUnmount comp)
  return ()

type Lifter parent = Ef parent IO () -> IO ()
type StateUpdate props state = (props -> state -> IO (Maybe state,IO ())) -> IO ()
type PropsUpdate props = props -> IO ()
type UnmountAction = IO ()

data View e where
  -- NullView must have a presence on the page for proper diffing
  NullView
    :: { _node :: (Maybe ENode)
       } -> View e

  TextView
    ::  { _tnode      :: (Maybe TNode)
        , _content    :: Txt
        } -> View e

  RawView
    :: { _node        :: (Maybe ENode)
       , _tag         :: Txt
       , _attributes  :: [Feature e]
       , _content     :: Txt
       } -> View e

  HTML
    ::  { _node       :: (Maybe ENode)
        , _tag        :: Txt
        , _attributes :: [Feature e]
        , _atoms      :: [View e]
        } -> View e
  KHTML
    ::  { _node       :: (Maybe ENode)
        , _tag        :: Txt
        , _attributes :: [Feature e]
        , _keyed      :: [(Int,View e)]
        } -> View e

  STView
    :: (Typeable props,Typeable st) =>
       { _stprops  :: props
       , _stid     :: Int
       , _strecord :: (Maybe (MVar (props,st,Component e props st,View x,View x)))
       , _stview   :: Lifter e -> StateUpdate props st -> Component e props st
       , _stupdate :: StateUpdate props st
       , _psupdate :: PropsUpdate props
       , _unmount  :: UnmountAction
       , _stateProxy :: Proxy st
       } -> View e

  SVG
    ::  { _node       :: (Maybe ENode)
        , _tag        :: Txt
        , _attributes :: [Feature e]
        , _atoms      :: [View e]
        } -> View e

  KSVG
    ::  { _node       :: (Maybe ENode)
        , _tag        :: Txt
        , _attributes :: [Feature e]
        , _keyed      :: [(Int,View e)]
        } -> View e

  Managed
    ::  { _node       :: (Maybe ENode)
        , _tag        :: Txt
        , _attributes :: [Feature e]
        , _constr     :: Constr
        } -> View e

  DiffView
    :: (Typeable model)
    => { _diff_model :: model
       , _diff_view :: View e
       } -> View e

  DiffEqView
    :: (Typeable model, Eq model)
    => { _diffEq_model :: model
       , _diffEq_view :: View e
       } -> View e

  View
    :: (Renderable a e, Typeable a, Typeable e) => { renderable :: a e } -> View e

pattern Rendered :: (Renderable a e, Typeable a, Typeable e) => a e -> View e
pattern Rendered ams <- (View (cast -> Just ams)) where
  Rendered ams = View ams

class Renderable (a :: [* -> *] -> *) (ms :: [* -> *]) where
  -- TODO:
  --   build :: a ms -> IO (View ms)
  --   diff :: (Ef ms IO () -> IO ()) -> ENode -> View ms -> a ms -> a ms -> IO (View ms)
  -- With build and diff the only primitive view elements would be HTML, SVG, Managed, and View.
  -- Great avenue for extensibility and modularity, but I don't see that the expressivity gained
  -- would currently justify the work; it's mostly just a refactoring, but it is a major refactoring.
  render :: a ms -> View ms
  default render :: (Generic (a ms), GRenderable (Rep (a ms)) ms) => a ms -> View ms
  render = grender . from

instance Renderable View ms where
  render (View a) = render a
  render a = a

class GRenderable a ms where
  grender :: a x -> View ms

instance GRenderable GHC.Generics.U1 ms where
  grender GHC.Generics.U1 = nil

instance (Renderable a ms) => GRenderable (GHC.Generics.K1 i (a ms)) ms where
  grender (GHC.Generics.K1 k) = render k

instance (GRenderable a ms, GRenderable b ms) => GRenderable (a :*: b) ms where
  grender (a :*: b) = mkHTML "div" [ ] [ grender a, grender b ]

instance (GRenderable a ms, GRenderable b ms) => GRenderable (a :+: b) ms where
  grender (L1 a) = grender a
  grender (R1 b) = grender b

mapRenderable :: (Typeable a, Typeable a', Typeable ms, Renderable a ms, Renderable a' ms) => (a ms -> a' ms) -> View ms -> View ms
mapRenderable f sa =
  case sa of
    Rendered a -> Rendered (f a)
    _ -> sa

forRenderable :: (Typeable a, Typeable a', Typeable ms, Renderable a ms, Renderable a' ms)
             => View ms -> (a ms -> a' ms) -> View ms
forRenderable = flip mapRenderable

infixl 9 %
(%) :: (Typeable a, Typeable a', Typeable ms, Renderable a ms, Renderable a' ms) => View ms -> (a ms -> a' ms) -> View ms
(%) = forRenderable

mapRenderables :: (Typeable a, Typeable a', Typeable ms, Renderable a ms, Renderable a' ms)
              => (a ms -> a' ms) -> [View ms] -> [View ms]
mapRenderables f as = map (mapRenderable f) as

forRenderables :: (Typeable a, Typeable a', Typeable ms, Renderable a ms, Renderable a' ms)
              => [View ms] -> (a ms -> a' ms) -> [View ms]
forRenderables = flip mapRenderables

data Mapping ms = forall a a'. (Typeable a, Typeable a', Typeable ms, Renderable a ms, Renderable a' ms)
                => Mapping (a ms -> a' ms)

maps :: View ms -> [Mapping ms] -> View ms
maps a mappings = Prelude.foldr tryMap a mappings
  where
    tryMap (Mapping m) res =
      case a of
        Rendered a -> Rendered (m a)
        _ -> res

forceToFromTxt :: (ToTxt t, FromTxt t) => Txt -> t
forceToFromTxt = fromTxt

witness :: View '[] -> View ms
witness = unsafeCoerce

witnesses :: [View '[]] -> [View ms]
witnesses = unsafeCoerce

styledRenderable :: View ms -> Maybe ([Feature ms] -> [View ms] -> View ms,Styles (),[Feature ms],[View ms])
styledRenderable (HTML _ tag fs vs) =
  case getStyles fs of
    Just (ss,rest) -> Just (HTML Nothing tag,ss,rest,vs)
    _ -> Nothing
styledRenderable (SVG _ tag fs vs) =
  case getStyles fs of
    Just (ss,rest) -> Just (SVG Nothing tag,ss,rest,vs)
    _ -> Nothing

getStyles :: [Feature ms] -> Maybe (Styles (),[Feature ms])
getStyles = go (return ()) []
  where
    go ss fs [] =
      case ss of
        Return _ -> Nothing
        _ -> Just (ss,Prelude.reverse fs)
    go ss fs ((StyleList styles):rest) =
      go (ss >> mapM_ (uncurry (=:)) styles) fs rest
    go ss fs (x:rest) = go ss (x:fs) rest

-- rudimentary; no CSS3
pattern Styled :: ([Feature ms] -> [View ms] -> View ms) -> Styles () -> [Feature ms] -> [View ms] -> View ms
pattern Styled f ss fs vs <- (styledRenderable -> Just (f,ss,fs,vs)) where
  Styled f ss fs vs = f (styled ss : fs) vs

pattern Null :: Typeable ms => View ms
pattern Null <- (NullView _) where
  Null = NullView Nothing

pattern Raw :: Txt -> [Feature ms] -> Txt -> View ms
pattern Raw t fs c <- (RawView _ t fs c) where
  Raw t fs c = RawView Nothing t fs c

pattern Translated :: (ToTxt t, FromTxt t, ToTxt f, FromTxt f) => f -> t
pattern Translated t <- (fromTxt . toTxt -> t) where
  Translated f = fromTxt $ toTxt f

-- Specialized to avoid type signatures.
pattern Text :: (ToTxt t, FromTxt t) => t -> Txt
pattern Text t = Translated t

pattern String :: (ToTxt t, FromTxt t) => t -> View ms
pattern String t <- (TextView _ (fromTxt -> t)) where
  String t = TextView Nothing (toTxt t)

pattern ST p v <- STView p 0 _ v _ _ _ _ where
  ST p v = STView p 0 Nothing v (\_ -> return ()) (\_ -> return ()) def Proxy

weakRender (View a) = weakRender (render a)
weakRender a = a

addClass c = go False
  where
    go added [] = if added then [] else [ ClassList [ c ] ]
    go added ((ClassList cs) : fs) = ClassList (c : cs) : go True fs
    go added (f : fs) = f : go added fs

updateFeatures f v =
  case v of
    HTML     {..} -> HTML     { _attributes = f _attributes, .. }
    RawView  {..} -> RawView  { _attributes = f _attributes, .. }
    KHTML    {..} -> KHTML    { _attributes = f _attributes, .. }
    SVG  {..} -> SVG  { _attributes = f _attributes, .. }
    KSVG {..} -> KSVG { _attributes = f _attributes, .. }
    Managed  {..} -> Managed  { _attributes = f _attributes, .. }
    _             -> v

instance Default (View ms) where
  def = nil

-- toJSON for View will server-side render components, but not controllers.
instance (e <: '[]) => ToJSON (View e) where
  toJSON a =
#ifdef __GHCJS__
    objectValue $
#endif
      go a
    where
      go (STView props _ iorec c _ _ _ sp) =
          unsafeCoerce go $
            case iorec of
              Nothing -> unsafePerformIO $ do
                let comp = let ?state = sp in c (\_ -> return ()) (\_ -> return ())
                state <- runConstructor props comp
                state' <- runInitialize props state comp
                return $ runRenderer props (state' `asTypeOf` state) comp

              Just ref ->
                let (_,_,_,cur,_) = unsafePerformIO (readMVar ref)
                in unsafeCoerce cur

      go (View v) = go (render v)
      go (TextView _ c) = object [ "type" .= ("text" :: Txt), "content" .= c]
      go (RawView _ t as c) = object [ "type" .= ("raw" :: Txt), "tag" .= t, "attrs" .= toJSON as, "content" .= c ]
      go (KHTML _ t as ks) = object [ "type" .= ("keyed" :: Txt), "tag" .= t, "attrs" .= toJSON as, "keyed" .= toJSON (map (fmap render) ks) ]
      go (HTML _ t as cs) = object [ "type" .= ("atom" :: Txt), "tag" .= t, "attrs" .= toJSON as, "children" .= toJSON (map render cs) ]
      go (KSVG _ t as ks) = object [ "type" .= ("keyedsvg" :: Txt), "tag" .= t, "attrs" .= toJSON as, "keyed" .= toJSON (map (fmap render) ks)]
      go (SVG _ t as cs) = object [ "type" .= ("svg" :: Txt), "tag" .= t, "attrs" .= toJSON as, "children" .= toJSON (map render cs) ]
      -- go (Component r) = go (render r)
      go (DiffView _ v) = go v
      go (DiffEqView _ v) = go v

      -- Need a better approach here.
      go (Managed mn t as (Controller' c)) =
        let !v = unsafePerformIO $ do
                  with c (return ())
                  Just (ControllerRecord {..}) <- lookupController (key c)
                  v <- readIORef crView
                  shutdown c
                  return $ cvCurrent v
        in go (HTML mn t as [unsafeCoerce v])

      go _ = object [ "type" .= ("null" :: Txt) ]

instance Typeable e => FromJSON (View e) where
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
          pure $ TextView Nothing c
        "raw" -> do
          t <- o .: "tag"
          as <- o .: "attrs"
          c <- o .: "content"
          pure $ RawView Nothing t as c
        "keyed" -> do
          t <- o .: "tag"
          as <- o .: "attrs"
          ks <- o .: "keyed"
          pure $ KHTML Nothing t as ks
        "atom" -> do
          t <- o .: "tag"
          as <- o .: "attrs"
          cs <- o .: "children"
          pure $ HTML Nothing t as cs
        "keyedsvg" -> do
          t <- o .: "tag"
          as <- o .: "attrs"
          ks <- o .: "keyed"
          pure $ KSVG Nothing t as ks
        "svg" -> do
          t <- o .: "tag"
          as <- o .: "attrs"
          cs <- o .: "children"
          pure $ SVG Nothing t as cs
        "null" -> pure $ NullView Nothing
        _ -> Ef.Base.empty

instance Eq (View e) where
  (==) (NullView _) (NullView _) =
    True

  (==) (TextView _ t) (TextView _ t') =
    prettyUnsafeEq t t'

  (==) (RawView _ t fs c) (RawView _ t' fs' c') =
    prettyUnsafeEq t t' && prettyUnsafeEq fs fs' && prettyUnsafeEq c c'

  (==) (KHTML _ t fs ks) (KHTML _ t' fs' ks') =
    prettyUnsafeEq t t' && prettyUnsafeEq fs fs' && reallyUnsafeEq ks ks'

  (==) (HTML _ t fs cs) (HTML _ t' fs' cs') =
    prettyUnsafeEq t t' && prettyUnsafeEq fs fs' && reallyUnsafeEq cs cs'

  (==) (STView p k _ _ _ _ _ sp) (STView p' k' _ _ _ _ _ sp') =
    k == k' && reallyVeryUnsafeEq p p' && reallyVeryUnsafeEq sp sp'

  (==) (KSVG _ t fs ks) (KSVG _ t' fs' ks') =
    prettyUnsafeEq t t' && prettyUnsafeEq fs fs' && reallyUnsafeEq ks ks'

  (==) (SVG _ t fs cs) (SVG _ t' fs' cs') =
    prettyUnsafeEq t t' && prettyUnsafeEq fs fs' && reallyUnsafeEq cs cs'

  (==) (Managed _ t fs c) (Managed _ t' fs' c') =
    prettyUnsafeEq t t' && prettyUnsafeEq fs fs' && prettyUnsafeEq c c'

  (==) (DiffView m v) (DiffView m' v') =
    typeOf m == typeOf m' && reallyUnsafeEq m (unsafeCoerce m')

  (==) (DiffEqView m v) (DiffEqView m' v') =
    typeOf m == typeOf m' && prettyUnsafeEq m (unsafeCoerce m')

  (==) _ _ =
    False

instance Cond (View e) where
  nil = NullView Nothing

instance Typeable e => IsString (View e) where
  fromString = text . fromString

instance Typeable e => FromTxt (View e) where
  fromTxt = text

instance {-# OVERLAPS #-} Typeable e => IsString [View e] where
  fromString s = [fromString s]

instance Typeable e => FromTxt [View e] where
  fromTxt t = [fromTxt t]

mkHTML :: Txt -> [Feature e] -> [View e] -> View e
mkHTML _tag _attributes _atoms =
  let _node = Nothing
  in HTML {..}

mkSVG :: Txt -> [Feature e] -> [View e] -> View e
mkSVG _tag _attributes _atoms =
  let _node = Nothing
  in SVG {..}

text :: Txt -> View e
text _content =
  let _tnode = Nothing
  in TextView {..}

raw :: ([Feature e] -> [View e] -> View e) -> [Feature e] -> Txt -> View e
raw x _attributes _content =
  case x [] [] of
    HTML _ _tag _ _ ->
      let _node = Nothing
      in RawView {..}
    SVG _ _tag _ _ ->
      let _node = Nothing
      in RawView {..}
    _ -> error "HTMLic.Controller.raw: raw atoms may only be built from HTMLs and SVGs"

list :: ([Feature e] -> [View e] -> View e) -> [Feature e] -> [(Int,View e)] -> View e
list x _attributes _keyed =
  case x [] [] of
    HTML _ _tag _ _ ->
      let
        _node = Nothing
      in
        KHTML {..}
    SVG _ _tag _ _ ->
      let
        _node = Nothing
      in
        KSVG {..}
    _ -> error "HTMLic.Controller.list: lists may only be built from HTMLs and SVGs"
--
-- viewManager_ :: forall props st e. Int -> props -> st -> (props -> st -> (Ef e IO () -> IO ()) -> StateUpdate e props st -> View e) -> View e
-- viewManager_ k props initial_st view = STView props k initial_st Nothing view (\_ -> return ())
--
-- -- The hacks used to implement this atom type are somewhat finicky. The model tracks variables
-- -- for changes; if any of the variables within the model are updated, a diff will be performed.
-- -- This is how changes external to a `viewManager` are injected; if a `viewManager` uses state
-- -- from a `Controller`s model and that state is untracked in the `viewManager`, changes to the
-- -- `Controller`s model will not be injected. The same rules apply to nesting/inheriting
-- -- `viewManager` models.
-- --
-- -- Major caveat: If the list of elements holding a viewManager changes such that the diff algorithm
-- --               must recreate the element, it will be reset to its original state. This would
-- --               happen if the position of the st element within the list changes. If a variable
-- --               length list of children is required, either careful placement for the st element,
-- --               or the use of NullViews as placeholders, or some uses of keyed atoms can overcome
-- --               this problem. The solution is the good practice of keeping lists of views static
-- --               or at the very least keep extensibility at the end of a view list.
-- viewManager :: forall props st e. props -> st -> (props -> st -> (Ef e IO () -> IO ()) -> StateUpdate e props st -> View e) -> View e
-- viewManager props initial_st view = STView props 0 initial_st Nothing view (\_ -> return ())
--
-- constant :: View e -> View e
-- constant a = viewManager () () $ \_ _ _ _ -> a
--
mvc :: ([Feature e] -> [View e] -> View e)
    -> (forall ts' ms' m. (IsController' ts' ms' m) => [Feature e] -> Controller' ts' ms' m -> View e)
mvc f = \as c ->
  case f [] [] of
    HTML _ t _ _ -> Managed Nothing t as (Controller' c)
    _ -> error "Incorrect usage of construct; Controllers may only be embedded in plain html HTMLs."

diffView :: Typeable model => model -> View ms -> View ms
diffView = DiffView

diffEqView :: (Typeable model, Eq model) => model -> View ms -> View ms
diffEqView = DiffEqView

hashed :: Hashable a => ([Feature e] -> [View e] -> View e) -> [Feature e] -> [(a,View e)] -> View e
hashed x _attributes _keyed0 = list x _attributes (map (first hash) _keyed0)

css :: Ef '[CSS_] Identity a -> View e
css = css' False

css' :: forall a e. Bool -> Ef '[CSS_] Identity a -> View e
css' b = mkHTML "style" [ Property "type" "text/css", Property "scoped" (if b then "true" else "") ] . ((text "\n"):) . fst . go False []
  where
    go :: forall a. Bool -> [View e] -> Ef '[CSS_] Identity a -> ([View e],a)
    go b acc (Return a) = (acc,a)
    go b acc (Lift s) = go b acc (runIdentity s)
    go b acc c@(Do msg) =
      case prj msg of
        Just (CSS3_ atRule sel css k) ->
          case css of
            Return a ->
              go False (acc ++ [ text (atRule <> sel <> ";\n") ]) (k a)
            _ ->
              let (c,a) = go True [] css
              in go False (acc ++ ( text (atRule <> sel <> " {\n") : c) ++ [ text "\n}\n\n" ]) (k a)
        Just (CSS_ sel ss r) ->
          let (s,a) = renderStyles b ss
          in
            go b  ( acc ++ [ text ( (if b then "\t" else mempty)
                                      <> sel
                                      <> " {\n"
                                      <> (Txt.intercalate (if b then ";\n\t" else ";\n") s)
                                      <> (if b then "\n\t}\n\n" else "\n}\n\n")
                                  )
                           ]
                  ) (r a)

scss :: StaticCSS -> View e
scss = scss' False

scss' :: Bool -> StaticCSS -> View e
scss' b = raw (mkHTML "style") [ Property "type" "text/css", Property "scoped" (if b then "true" else "") ] . cssText

inlineCSS :: Ef '[CSS_] Identity a -> View e
inlineCSS = css' True . classify
  where
    classify :: forall a. Ef '[CSS_] Identity a -> Ef '[CSS_] Identity a
    classify (Return r) = Return r
    classify (Lift sup) = Lift (fmap classify sup)
    classify (Send e) =
      case e of
        CSS_ sel ss k ->
          Send (CSS_ (Txt.cons '.' sel) ss (classify . k))
        CSS3_ at sel css k ->
          Send (CSS3_ at sel (classify css) (classify . k))

-- rebuild finds managed nodes and re-embeds them in case they were
-- removed for other uses
rebuild :: forall e. View e -> IO ()
rebuild h =
#ifndef __GHCJS__
    return ()
#else
    go h
  where
    go :: View e -> IO ()
    go STView {..}  = do
      forM_ _strecord $ \ref -> do
        (_,_,_,a,_) <- readMVar ref
        rebuild (unsafeCoerce a :: View e)
    go HTML {..}    = mapM_ go _atoms
    go SVG {..} = mapM_ go _atoms
    go KHTML {..}   = mapM_ (go . snd) _keyed
    go KSVG {..}  = mapM_ (go . snd) _keyed
    go (DiffView _ v) = go v
    go (DiffEqView _ v) = go v
    go m@Managed {..} = do
      case _constr of
        Controller' c -> do
          mi_ <- lookupController (key c)
          forM_ mi_ $ \ControllerRecord {..} -> do
            ControllerView {..} <- readIORef crView
            rebuild cvCurrentLive
            forM_ _node $ \node ->
              embed_ node cvCurrentLive
    go _ =
      return ()
#endif

triggerBackground :: forall m e. (MonadIO m) => View e -> m ()
triggerBackground = go
  where
    bg Controller {..} = do
      mc <- lookupController key
      case mc of
        Nothing -> return ()
        Just ControllerRecord {..} -> do
          let ControllerHooks _ bg _= crHooks
          publish bg ()
          ControllerView {..} <- liftIO $ readIORef crView
          go $ unsafeCoerce cvCurrentLive

    go :: View e -> m ()
    go STView {..}  =
      forM_ _strecord $ \ref -> do
        (_,_,_,a,_) <- liftIO $ readMVar ref
        go (unsafeCoerce a)
    go HTML {..}    = mapM_ go _atoms
    go SVG {..} = mapM_ go _atoms
    go KHTML {..}   = mapM_ (go . snd) _keyed
    go KSVG {..} = mapM_ (go . snd) _keyed
    go (DiffView _ v) = go v
    go (DiffEqView _ v) = go v
    go m@Managed {..} = case _constr of Controller' c -> bg (unsafeCoerce c)
    go _ = return ()

triggerForeground :: forall m e. (MonadIO m) => View e -> m ()
triggerForeground = go
  where
    fg Controller {..} = do
      mc <- lookupController key
      case mc of
        Nothing -> return ()
        Just ControllerRecord {..} -> do
          let ControllerHooks _ _ fg = crHooks
          publish fg ()
          ControllerView {..} <- liftIO $ readIORef crView
          go (unsafeCoerce cvCurrentLive)

    go :: View e -> m ()
    go STView {..}  =
      forM_ _strecord $ \ref -> do
        (_,_,_,a,_) <- liftIO $ readMVar ref
        go (unsafeCoerce a)
    go HTML {..}    = mapM_ go _atoms
    go SVG {..} = mapM_ go _atoms
    go KHTML {..}   = mapM_ (go . snd) _keyed
    go KSVG {..} = mapM_ (go . snd) _keyed
    go (DiffView _ v) = go v
    go (DiffEqView _ v) = go v
    go m@Managed {..} = case _constr of Controller' c -> fg (unsafeCoerce c)
    go _ = return ()

onForeground :: ( MonadIO c, MonadIO c'
                , ms <: '[Evented]
                , ms' <: '[State () ControllerHooks]
                , With w (Narrative (Messages ms') c') IO
                )
             => w -> Ef '[Ef.Base.Event ()] (Ef ms c) () -> Ef ms c (Promise (IO ()))
onForeground c f = do
  connectWith c (get >>= \(ControllerHooks _ _ fg) -> return fg) $ \_ -> f

onBackground :: ( MonadIO c, MonadIO c'
                , ms <: '[Evented]
                , ms' <: '[State () ControllerHooks]
                , With w (Narrative (Messages ms') c') IO
                )
             => w -> Ef '[Event ()] (Ef ms c) () -> Ef ms c (Promise (IO ()))
onBackground c f = do
  connectWith c (get >>= \(ControllerHooks _ bg _) -> return bg) $ \_ -> f

reflect :: forall ts ms m c.
           ( IsController' ts ms m
           , MonadIO c
           )
        => Controller' ts ms m
        -> c (Promise (View ms))
reflect c =
  with c $ do
    ControllerState {..} :: ControllerState m <- get
    ControllerView {..} <- liftIO $ readIORef asLive
    return (unsafeCoerce cvCurrentLive)

data DiffStrategy = Eager | Manual deriving (Eq)

type Differ ms m =
    forall a. Renderable a ms =>
       (m ms -> a ms)
    -> IO ()
    -> (Ef ms IO () -> IO ())
    -> ControllerState m
    -> Ef ms IO ()

data AState m =
  AState
    { as_live :: forall ms. IORef (ControllerView ms m)
    , as_model :: forall ms. m ms
    }

data ControllerPatch m =
  forall ms a. (Renderable a ms, ms <: '[]) =>
  APatch
      -- only modify ap_AState with atomicModifyIORef
    { ap_send         :: Ef ms IO () -> IO ()
    , ap_AState       :: IORef (Maybe (AState m),Bool) -- an AState record for manipulation; nullable by context to stop a patch.
    , ap_patchRenderable    :: (m ms -> a ms)
    , ap_viewTrigger  :: IO ()
    , ap_hooks        :: ControllerHooks
    }

type IsController' ts ms m = (ms <: Base m, ts <. Base m, Delta (Modules ts) (Messages ms))
type IsController ms m = IsController' ms ms m

data ControllerHooks = ControllerHooks
  { chRenderables      :: Syndicate ()
  , chForeground :: Syndicate ()
  , chBackground :: Syndicate ()
  }

data ControllerView ms m = ControllerView
  { cvCurrent     :: View ms
  , cvCurrentLive :: View ms
  , cvModel       :: m ms
  , cvForeground  :: Bool
  }

data ControllerRecord ms m = ControllerRecord
  { crAsController :: As (Ef ms IO)
  , crView        :: IORef (ControllerView ms m)
  , crHooks       :: ControllerHooks
  }

data ControllerState (m :: [* -> *] -> *) where
  ControllerState ::
    { asPatch        :: Maybe (ControllerPatch m)
    , asDiffer       :: ControllerState m -> Ef ms IO ()
    , asDiffStrategy :: DiffStrategy
    , asUpdates      :: Syndicate (m ms)
    , asModel        :: m ms
    , asLive         :: IORef (ControllerView ms m)
    } -> ControllerState m

type MVC m ms = (ms <: Base m)
type VC ms = ms <: '[State () ControllerHooks, State () Shutdown, Evented]

type Base (m :: [* -> *] -> *)
  = '[ State () (ControllerState m)
     , State () ControllerHooks
     , State () Shutdown
     , Evented
     ]

data Constr where
  Controller' :: (IsController' ts ms m) => Controller' ts ms m -> Constr
instance Eq Constr where
 (==) (Controller' c) (Controller' c') =
  let Key k1 :: Key GHC.Prim.Any = unsafeCoerce (key c)
      Key k2 :: Key GHC.Prim.Any = unsafeCoerce (key c')
  in prettyUnsafeEq k1 k2

instance ToTxt (Feature e) where
  toTxt NullFeature          = mempty

  toTxt (DiffFeature _ f) = toTxt f

  toTxt (DiffEqFeature _ f) = toTxt f

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
           (fst $ renderStyles False (mapM_ (uncurry (=:)) pairs))
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

type ControllerKey ms m = Key (ControllerRecord (Appended ms (Base m)) m)
type ControllerBuilder ts m = Modules (Base m) (Action (Appended ts (Base m)) IO) -> IO (Modules (Appended ts (Base m)) (Action (Appended ts (Base m)) IO))
type ControllerPrimer ms m = Ef (Appended ms (Base m)) IO ()

data Controller' ts ms m = forall a. Renderable a ms => Controller
  { key       :: !(Key (ControllerRecord ms m))
  , build     :: !(Modules (Base m) (Action ts IO) -> IO (Modules ts (Action ts IO)))
  , prime     :: !(Ef ms IO ())
  , model     :: !(m ms)
  , view      :: !(m ms -> a ms)
  }
type Controller ms m = Controller' (Appended ms (Base m)) (Appended ms (Base m)) m

instance ToTxt (Controller' ts ms m) where
  toTxt = toTxt . key

instance Eq (Controller' ts ms m) where
  (==) (Controller k _ _ _ _) (Controller k' _ _ _ _) =
    let Key k1 = k
        Key k2 = k'
    in prettyUnsafeEq k1 k2

instance Ord (Controller' ts ms m) where
  compare (Controller (Key k) _ _ _ _) (Controller (Key k') _ _ _ _) = compare k k'

instance IsController' ts ms m
  => With (Controller' ts ms m)
          (Ef ms IO)
          IO
  where
    using_ c = do
      -- FIXME: likely a bug here with double initialization in multithreaded contex-ts!
      mi_ <- lookupController (key c)
      case mi_ of
        Just (ControllerRecord {..}) -> return (runAs crAsController)
        Nothing -> do
          mkController BuildOnly c
          using_ c
    with_ c m = do
      run <- using_ c
      run m
    shutdown_ c = do
      -- this method should 1. destroy the view 2. syndicate a shutdown event 3. poison the context
      -- so that unmount events that call with on the context do not fail
      miohhm <- lookupController (key c)
      case miohhm of
        Just ControllerRecord {..} -> do
          ControllerView {..} <- liftIO $ readIORef crView
          cleanup (void . with c) [cvCurrentLive]
          delete cvCurrentLive
          void $ runAs crAsController $ do
            buf <- get
            Shutdown sdn <- get
            publish sdn ()
            -- this is where things get iffy... what should this look like?
            delay 0 $ do
              deleteController (key c)
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

lookupController :: (MonadIO c) => Key phantom -> c (Maybe phantom)
lookupController = vaultLookup constructVault__

getControllerName :: IsController' ts ms m => Controller' ts ms m -> Txt
getControllerName = toTxt . key

addController :: (MonadIO c) => Key phantom -> phantom -> c ()
addController = vaultAdd constructVault__

deleteController :: (MonadIO c) => Key phantom -> c ()
deleteController = vaultDelete constructVault__

data MkControllerAction
  = ClearAndAppend ENode
  | forall e. Replace (View e)
  | Append ENode
  | BuildOnly

mkController :: forall ms ts m.
          ( IsController' ts ms m
          , ms <: Base m
          )
       => MkControllerAction
       -> Controller' ts ms m
       -> IO (ControllerRecord ms m)
mkController mkControllerAction c@Controller {..} = do
  -- TODO: simply render a nil View and then call diff to force building in rAF?
  let !raw = render $ view model
  doc <- getDocument
  buf <- newEvQueue
  ch  <- ControllerHooks <$> syndicate <*> syndicate <*> syndicate
  us  <- syndicate
  sdn <- Shutdown <$> syndicate
  as  <- unsafeConstructAs buf
  let sendEv = void . runAs as
  (i,l) <- case mkControllerAction of
            ClearAndAppend n -> do
              i <- buildAndEmbedMaybe sendEv doc ch True Nothing raw
              clearNode . Just =<< toNode n
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
  cr <- ControllerRecord <$> pure as <*> newIORef (ControllerView raw i model l) <*> pure ch
  -- keep out of forkIO to prevent double-initialization
  addController key cr
  forkIO $ do
    built <- build $ Ef.Base.state
                            (ControllerState
                                Nothing
                                (differ view (publish (chRenderables ch) ()) sendEv)
                                Eager
                                us
                                model
                                (crView cr)
                            )
                    *:* Ef.Base.state ch
                    *:* Ef.Base.state sdn
                    *:* Ef.Base.state buf
                    *:* Empty
    (obj',_) <- Ef.Base.Object built Ef.Base.! do
      connect constructShutdownSyndicate $ const (Ef.Base.lift shutdownSelf)
      prime
#if (defined __GHCJS__) || (defined DEVEL)
    driverPrintExceptions (" Controller exception (" ++ show key ++ "): ")
#else
    driver
#endif
        buf obj'
  return cr

diff :: forall m ms. ms <: Base m => Proxy m -> Ef ms IO ()
diff _ = do
  as@ControllerState {..} :: ControllerState m <- get
  unsafeCoerce (asDiffer as)

setEagerDiff :: forall m ms. ms <: '[State () (ControllerState m)] => Proxy m -> Ef ms IO ()
setEagerDiff _ = do
  ControllerState {..} :: ControllerState m <- get
  put ControllerState { asDiffStrategy = Eager, .. }

setManualDiff :: forall m ms. ms <: '[State () (ControllerState m)] => Proxy m -> Ef ms IO ()
setManualDiff _ = do
  ControllerState {..} :: ControllerState m <- get
  put ControllerState { asDiffStrategy = Manual, .. }

currentHTML :: (IsController' ts ms m, MonadIO c) => Controller' ts ms m -> c (Promise (View ms))
currentHTML c = with c $ ownHTML c

ownHTML :: forall ts ms c m.
           ( IsController' ts ms m
           , MonadIO c
           , ms <: Base m
           )
        => Controller' ts ms m
        -> Ef ms c (View ms)
ownHTML _ = do
  ControllerState {..} :: ControllerState m <- get
  ControllerView {..} <- liftIO $ readIORef asLive
  return (unsafeCoerce cvCurrent)

onModelChange :: forall ts ms ms' m c e.
                ( IsController' ts ms m
                , MonadIO c
                , ms <: Base m
                , ms' <: '[Evented]
                , e ~ Ef ms' c
                )
              => Controller' ts ms m
              -> (m ms -> Ef '[Event (m ms)] e ())
              -> e (Promise (IO ()))
onModelChange c f = do
  buf <- get
  with c $ do
    ControllerState {..} :: ControllerState m <- get
    sub <- subscribe (unsafeCoerce asUpdates) (return buf)
    bhv <- listen sub f
    return (stop bhv >> leaveSyndicate (unsafeCoerce asUpdates) sub)

onOwnModelChange :: forall ts ms ms' m c e.
                    ( IsController' ts ms m
                    , MonadIO c
                    , ms <: Base m
                    , e ~ Ef ms c
                    )
                  => Controller' ts ms m
                  -> (m ms -> Ef '[Event (m ms)] e ())
                  -> e (IO ())
onOwnModelChange _ f = do
  buf <- get
  pr  <- promise
  ControllerState {..} :: ControllerState m <- get
  sub <- subscribe (unsafeCoerce asUpdates) (return buf)
  bhv <- listen sub f
  return (stop bhv >> leaveSyndicate (unsafeCoerce asUpdates) sub)

onOwnModelChangeByProxy :: forall ms m c e. (MonadIO c, ms <: Base m, e ~ Ef ms c)
                        => Proxy m -> (m ms -> Ef '[Event (m ms)] e ()) -> e (IO ())
onOwnModelChangeByProxy _ f = do
  buf <- get
  pr  <- promise
  ControllerState {..} :: ControllerState m <- get
  sub <- subscribe (unsafeCoerce asUpdates) (return buf)
  bhv <- listen sub f
  return (stop bhv >> leaveSyndicate (unsafeCoerce asUpdates) sub)

getModel :: forall m ms. ms <: '[State () (ControllerState m)] => Ef ms IO (m ms)
getModel = do
  ControllerState {..} :: ControllerState m <- get
  return $ unsafeCoerce asModel

putModel :: forall ms m. ms <: Base m => m ms -> Ef ms IO ()
putModel !new = do
  (ControllerState {..},(old,cmp')) <- modify $ \(ControllerState {..} :: ControllerState m) ->
    let !old = unsafeCoerce asModel
        cmp' = ControllerState { asModel = unsafeCoerce new, .. }
    in (cmp',(old,cmp'))
  publish (unsafeCoerce asUpdates) new
  let d :: ControllerState m -> Ef ms IO ()
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

modifyModel :: forall e ms m. ms <: Base m => (m ms -> m ms) -> Ef ms IO ()
modifyModel f = do
  (ControllerState {..},(old,!new,cmp')) <- modify $ \ControllerState {..} ->
    let !old = unsafeCoerce asModel
        !new = f old
        cmp' = ControllerState { asModel = unsafeCoerce new, ..  }
    in (cmp',(old,new,cmp'))
  publish (unsafeCoerce asUpdates) new
  let d :: ControllerState m -> Ef ms IO ()
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

differ :: ms <: Base m => Differ ms m
differ r trig sendEv ControllerState {..} = do
#ifdef __GHCJS__
  ch <- get
  let setupDiff = do
        let !new_as = AState (unsafeCoerce asLive) (unsafeCoerce asModel)
        new_ap_AState <- liftIO $ newIORef (Just new_as,False)
        let !aPatch = APatch sendEv new_ap_AState r trig ch
        put ControllerState { asPatch = Just aPatch, .. }
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
  let v = render $ r (unsafeCoerce asModel)
  liftIO $ do
    ControllerView _ _ _ isFG <- liftIO $ readIORef asLive
    writeIORef asLive $ unsafeCoerce $ ControllerView v v (unsafeCoerce asModel) isFG
#endif

#ifdef __GHCJS__
toNode :: T.IsNode n => n -> IO NNode
toNode = T.castToNode
#else
toNode :: n -> IO NNode
toNode _ = return ()
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
  append_child_js parent =<< toNode child
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

embed_ :: forall e. ENode -> View e -> IO ()
embed_ parent STView {..} = do
  forM_ _strecord $ \ref -> do
    (_,_,_,a,_) <- readMVar ref
    embed_ parent (unsafeCoerce a :: View e)
embed_ parent TextView {..} =
  forM_ _tnode $ \node -> do
    ae <- isAlreadyEmbeddedText node parent
    unless ae (void $ appendChild parent node)
embed_ parent n =
  forM_ (_node n) $ \node -> do
    ae <- isAlreadyEmbedded node parent
    unless ae (void $ appendChild parent node)

embedMany_ parent children = do
  forM_ children $ \child -> do
    embed_ parent child

setAttributes :: [Feature e] -> (Ef e IO () -> IO ()) -> Bool -> ENode -> IO ([Feature e],IO ())
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

newtype ComponentPatchQueue e props state
  = ComponentPatchQueue (MVar [CPatch e props state])

type WillReceiveProps e props stateo =
     props ::: "New properties."
  -> Ef '[ Reader () props ::: "Current properties."
         , Reader () state ::: "Current state; writable."
         ]
         IO
         state

type ShouldUpdate e props state =
       props ::: "Next properties."
    -> state ::: "Next state."
    -> Ef '[ Reader () props ::: "Current properties."
           , Reader () state ::: "Current state; writable."
           ]
           IO
           Bool

type WillUpdate e props state =
       props ::: "Next properties to be fed to the patcher."
    -> state ::: "Next state to be fed to the patcher."
    -> Ef '[ Reader () props ::: "Current properties."
           , Reader () state ::: "Current state; writable."
           ]
           IO
           state

type DidUpdate e props state =
      props ::: "Previous properties from before patch."
   -> state ::: "Previous state from before patch."
   -> Ef '[ Reader () props ::: "Current, new properties after patch."
          , Reader () state ::: "Current, new state after patch; read-only."
          ]
          IO
          ()

data CPatch e props state = CPatch
  { cp_update :: Either props (props -> state -> IO (state,IO ()))
  , cp_willReceiveProps :: WillReceiveProps e props state
  , cp_shouldUpdate :: ShouldUpdate e props state
  , cp_willUpdate :: WillUpdate e props state
  , cp_didUpdate :: DidUpdate e props state
  }

queueComponentPatch
  :: ( patch ~ CPatch e props state
     , queue ~ ComponentPatchQueue e prosp state
     , Describe 'queueComponentPatch
         '[ Param patch
              '[ "A CPatch that contains two continuations to run before and"
               , "after the patch is applied."
               ]
          , Param queue
              '[ "An MVar containing a list of patches in reverse order."
               ]
          , About
              '[ "If no existing patch is queued, this method will fork one."
               ]
          ]
     )
  => props -> state -> patch -> queue -> IO ()
queueComponentPatch ps st p cpq@(ComponentPatchQueue q_) = do
  join . modifyMVar q_ $ \q ->
    case q of
      [] -> return ([q],void . forkIO $ runComponentPatch ps st cpq)
      ps -> return (p:ps,return ())

runComponentPatch
  :: ( queue ~ ComponentPatchQueue e props state
     , cstate ~ ComponentState e props state
     , Describe 'runComponentPatch
         '[ Param queue
              '[ "An MVar containing a list of patches in reverse order." ]
          , About
              '[ "The existing queue will be locked and reset"
               , "If executed in the browse, runComponentPatch will request a"
               , "new animation frame to perform a patch."
               , "The existing queue will be reset"]
          , About
              '[ "All willUpdate methods are chained together with their state"
               , "results being passed down the chain. After each willUpdate,"
               , "the corresponding didUpdate will be constructed from the"
               , "result and turned into an IO action and added to a queue"
               , "to be executed with (sequence_ . reverse) at the end of the"
               , "patch."
               ]
          ]
     ) => props -> state -> queue -> IO ()
runComponentPatch ps st (ComponentPatchQueue cpq) = do
  patches <- getPatches
  chained <- chain patches
  rAF $ do
    st' <- runWillUpdates chained
    
    runDiffs
    run
  where
    getPatches = reverse <$> swapMVar cpq []

    chain ps = (\(acc,ps,st) -> (reverse acc,ps,st)) <$> foldM go ([],ps,st) ps
      where
        go (acc,ps,st) CPatch {..} = do
          case cp_update of
            Left ps' -> do
              st' <- runWillReceiveProps ps ps' st cp_willReceiveProps
              su <- runShouldUpdate ps ps' st st' cp_shouldUpdate
              return $
                if su then
                  let p = Left (cp_willUpdate,cp_didUpdate,ps,ps',st,st')
                  in (p:acc,ps',st')
                else
                  (acc,ps',st')

            Right f -> do
              (st',cb) <- f ps st
              su <- runShouldUpdate ps ps st st' cp_shouldUpdate
              return $
                if su then
                  let p = Right (cp_willUpdate,cp_didUpdate,ps,st,st',cb)
                  in (p:acc,ps,st')
                else
                  (acc,ps,st')







rAF f = void $ do
#ifdef __GHCJS__
  rafCallback <- newRequestAnimationFrameCallback $ \_ -> f
  win <- getWindow
  requestAnimationFrame win (Just rafCallback)
#else
  f
#endif

buildComponent :: forall e. (Ef e IO () -> IO ()) -> Maybe ENode -> View e -> IO (View e)
buildComponent f mparent STView {..} = do
  let ?parent = Proxy :: Proxy e
  in let ?state = _stateProxy
  in do
    let rAF f = void $ do
#ifdef __GHCJS__
          rafCallback <- newRequestAnimationFrameCallback $ \_ -> f
          win <- getWindow
          requestAnimationFrame win (Just rafCallback)
#else
          f
#endif

    propUpdater <- newEmptyMVar

    strec <- newEmptyMVar

    let
      updateStateInternal should_raf g = void $ forkIO $ do
          -- putStrLn "7"
          (props,st,c,old,mid) <- takeMVar strec
          -- putStrLn "8"
          (mst,cb) <- g props st
          -- putStrLn "9"
          let
            updateState props c old mid st mst cb = do
                -- putStrLn "10"
                case mst of
                  Nothing  -> do
                    -- putStrLn "11"
                    putMVar strec (props,st,c,old,mid)
                    cb
                  Just st' -> do
                    -- putStrLn "12"
                    shouldUpd <- runShouldUpdate props props st st' c old

                    -- putStrLn "13"
                    when shouldUpd $ runWillUpdate props st' c old

                    if shouldUpd then do
                      (if should_raf then rAF else id) $ do
                        -- putStrLn "14"
                        let new_mid = runRenderer props st' c (updateStateInternal False)
                        new <- diffHelper f doc ch isFG old mid new_mid
                        -- putStrLn "15"
                        runDidUpdate props st' c new
                        putMVar strec (props,st',c,new,new_mid)
                        cb
                    else do
                      -- putStrLn "16"
                      putMVar strec (props,st',c,old,mid)
                      cb

          updateState props c old mid st mst cb


      updatePropsInternal props = forkIO $ do
        -- putStrLn "17"
        (old_props,st,c,old,mid) <- takeMVar strec
        -- putStrLn "18"
        st' <- runWillReceiveProps old_props props st c old
        -- putStrLn "19"
        shouldUpd <- runShouldUpdate old_props props st st' c old
        -- putStrLn "20"
        if shouldUpd then do
          -- putStrLn "21"
          runWillUpdate props st' c old
          let new_mid = runRenderer props st' c (updateStateInternal True)
          -- putStrLn "22"
          new <- diffHelper f doc ch isFG old mid new_mid
          -- putStrLn "23"
          mst <- runDidUpdate props st' c new
          -- putStrLn "24"
          putMVar strec (props,st',c,new,new_mid)

        else do
          -- putStrLn "25"
          putMVar strec (props,st',c,old,mid)

      unmountInternal = void $ do
        (props,st,c,old,mid) <- takeMVar strec
        runWillUnmount props st c
        return ()

    let comp = _stview f (updateStateInternal True)
    -- putStrLn "1"
    st <- runConstructor _stprops comp
    -- putStrLn "2"
    putMVar strec (_stprops,st,comp,nil,nil)
    -- putStrLn "3"
    runWillMount _stprops st comp
    let mid = runRenderer _stprops st comp (updateStateInternal False)
    -- putStrLn "4"
    new <- go mparent (render mid)
    -- putStrLn "5"
    runDidMount _stprops st comp new
    -- putStrLn "6"
    modifyMVar strec $ \(props,st,comp,_,_) -> return ((props,st,comp,new,mid),())

    return $
      STView
        _stprops
        _stid
        (Just strec)
        _stview
        (updateStateInternal False)
        updatePropsInternal
        unmountInternal
        _stateProxy


-- consider this: https://github.com/spicyj/innerhtml-vs-createelement-vs-clonenode
-- and this: https://stackoverflow.com/questions/8913419/is-chromes-appendchild-really-that-slow
-- Would a bottom-up, display='none' -> display='' solution work globally?
-- Does the fact that this runs in a rAF resolve any of this a priori?
buildAndEmbedMaybe :: forall e. (e <: '[]) => (Ef e IO () -> IO ()) -> Doc -> ControllerHooks -> Bool -> Maybe ENode -> View e -> IO (View e)
buildAndEmbedMaybe f doc ch isFG mn v = do
  go mn $ render v
  where
    go :: Maybe ENode -> View e -> IO (View e)
    go mparent (View c) = go mparent (render c)

    go mparent nn@NullView {..} = do
      _cond@(Just el) <- createElement doc "template"
      forM_ mparent (flip appendChild el)
      return $ NullView _cond

    go mparent RawView {..} = do
      _node@(Just el) <- createElement doc _tag
      (_attributes,didMount) <- setAttributes _attributes f False el
      setInnerHTML el _content
      forM_ mparent $ \parent -> appendChild parent el
      didMount
      return $ RawView _node _tag _attributes _content

    go mparent HTML {..} = do
      _node@(Just el) <- createElement doc _tag
      (_attributes,didMount) <- setAttributes _attributes f False el
      _atoms <- mapM (go (Just el)) _atoms
      forM_ mparent $ \parent -> appendChild parent el
      didMount
      return $ HTML _node _tag _attributes _atoms

    go mparent STView {..} =
      let ?parent = Proxy :: Proxy e
      in let ?state = _stateProxy
      in do
        let rAF f = void $ do
#ifdef __GHCJS__
              rafCallback <- newRequestAnimationFrameCallback $ \_ -> f
              win <- getWindow
              requestAnimationFrame win (Just rafCallback)
#else
              f
#endif

        propUpdater <- newEmptyMVar

        strec <- newEmptyMVar

        let
          updateStateInternal should_raf g = void $ forkIO $ do
              -- putStrLn "7"
              (props,st,c,old,mid) <- takeMVar strec
              -- putStrLn "8"
              (mst,cb) <- g props st
              -- putStrLn "9"
              let
                updateState props c old mid st mst cb = do
                    -- putStrLn "10"
                    case mst of
                      Nothing  -> do
                        -- putStrLn "11"
                        putMVar strec (props,st,c,old,mid)
                        cb
                      Just st' -> do
                        -- putStrLn "12"
                        shouldUpd <- runShouldUpdate props props st st' c old

                        -- putStrLn "13"
                        when shouldUpd $ runWillUpdate props st' c old

                        if shouldUpd then do
                          (if should_raf then rAF else id) $ do
                            -- putStrLn "14"
                            let new_mid = runRenderer props st' c (updateStateInternal False)
                            new <- diffHelper f doc ch isFG old mid new_mid
                            -- putStrLn "15"
                            runDidUpdate props st' c new
                            putMVar strec (props,st',c,new,new_mid)
                            cb
                        else do
                          -- putStrLn "16"
                          putMVar strec (props,st',c,old,mid)
                          cb

              updateState props c old mid st mst cb


          updatePropsInternal props = forkIO $ do
            -- putStrLn "17"
            (old_props,st,c,old,mid) <- takeMVar strec
            -- putStrLn "18"
            st' <- runWillReceiveProps old_props props st c old
            -- putStrLn "19"
            shouldUpd <- runShouldUpdate old_props props st st' c old
            -- putStrLn "20"
            if shouldUpd then do
              -- putStrLn "21"
              runWillUpdate props st' c old
              let new_mid = runRenderer props st' c (updateStateInternal True)
              -- putStrLn "22"
              new <- diffHelper f doc ch isFG old mid new_mid
              -- putStrLn "23"
              mst <- runDidUpdate props st' c new
              -- putStrLn "24"
              putMVar strec (props,st',c,new,new_mid)

            else do
              -- putStrLn "25"
              putMVar strec (props,st',c,old,mid)

          unmountInternal = void $ do
            (props,st,c,old,mid) <- takeMVar strec
            runWillUnmount props st c
            return ()

        let comp = _stview f (updateStateInternal True)
        -- putStrLn "1"
        st <- runConstructor _stprops comp
        -- putStrLn "2"
        putMVar strec (_stprops,st,comp,nil,nil)
        -- putStrLn "3"
        runWillMount _stprops st comp
        let mid = runRenderer _stprops st comp (updateStateInternal False)
        -- putStrLn "4"
        new <- go mparent (render mid)
        -- putStrLn "5"
        runDidMount _stprops st comp new
        -- putStrLn "6"
        modifyMVar strec $ \(props,st,comp,_,_) -> return ((props,st,comp,new,mid),())

        return $
          STView
            _stprops
            _stid
            (Just strec)
            _stview
            (updateStateInternal False)
            updatePropsInternal
            unmountInternal
            _stateProxy

    go mparent SVG {..} = do
      _node@(Just el) <- createElementNS doc "http://www.w3.org/2000/svg" _tag
      (_attributes,didMount) <- setAttributes _attributes f False el
      _atoms <- mapM (go (Just el)) _atoms
      forM_ mparent $ \parent -> appendChild parent el
      didMount
      return $ SVG _node _tag _attributes _atoms

    go mparent KHTML {..} = do
      _node@(Just el) <- createElement doc _tag
      (_attributes,didMount) <- setAttributes _attributes f False el
      _keyed <- mapM (\(k,x) -> go (Just el) (render x) >>= \y -> return (k,y)) _keyed
      forM_ mparent $ \parent -> appendChild parent el
      didMount
      return $ KHTML _node _tag _attributes _keyed

    go mparent KSVG {..} = do
      _node@(Just el) <- createElementNS doc "http://www.w3.org/2000/svg" _tag
      (_attributes,didMount) <- setAttributes _attributes f False el
      _keyed <- mapM (\(k,x) -> go (Just el) (render x) >>= \y -> return (k,y)) _keyed
      forM_ mparent $ \parent -> appendChild parent el
      didMount
      return $ KSVG _node _tag _attributes _keyed

    go mparent TextView {..} = do
      _tnode@(Just el) <- createTextNode doc _content
      forM_ mparent (flip appendChild el)
      return $ TextView _tnode _content

    go mparent (DiffView m v) = do
      n <- go mparent v
      return (DiffView m n)

    go mparent (DiffEqView m v) = do
      n <- go mparent v
      return (DiffEqView m n)

    go mparent m@Managed {..} =
      case _constr of
        Controller' a -> do
          case _node of
            Nothing -> do
              _node@(Just el) <- createElement doc _tag
              (_attributes,didMount) <- setAttributes _attributes f False el
              mi_ <- lookupController (key a)
              case mi_ of
                Nothing -> do
                  -- never built before; make and embed
                  ControllerRecord {..} <- mkController BuildOnly a
                  ControllerView {..} <- liftIO $ readIORef crView
                  forM_ mparent $ \parent -> do
                    when isFG (triggerForeground m)
                    embed_ parent Managed {..}
                  embed_ el cvCurrentLive
                  didMount
                  return Managed {..}
                Just ControllerRecord {..} -> do
                  ControllerView {..} <- liftIO $ readIORef crView
                  rebuild Managed {..}
                  when isFG (triggerForeground m)
                  embed_ el cvCurrentLive
                  forM_ mparent $ \parent -> embed_ parent Managed {..}
                  didMount
                  return Managed {..}

            Just e -> do
              mi_ <- lookupController (key a)
              case mi_ of
                Nothing -> do
                  -- shut down?
                  ControllerRecord {..} <- mkController BuildOnly a
                  ControllerView {..} <- liftIO $ readIORef crView
                  forM_ mparent $ \parent -> do
                    when isFG (triggerForeground m)
                    embed_ parent Managed {..}
                  embed_ e cvCurrentLive
                  return Managed {..}
                Just ControllerRecord {..} -> do
                  ControllerView {..} <- liftIO $ readIORef crView
                  rebuild m
                  when isFG (triggerForeground m)
                  embed_ e cvCurrentLive
                  return m

buildHTML :: e <: '[] => Doc -> ControllerHooks -> Bool -> (Ef e IO () -> IO ()) -> View e -> IO (View e)
buildHTML doc ch isFG f = buildAndEmbedMaybe f doc ch isFG Nothing

getElement :: forall e. View e -> IO (Maybe ENode)
getElement View {} = return Nothing
getElement TextView {} = return Nothing
getElement (DiffView _ v) = getElement v
getElement (DiffEqView _ v) = getElement v
getElement STView {..} =
  case _strecord of
    Nothing -> return Nothing
    Just ref -> do
      (_,_,_,a,_) <- readMVar ref
      getElement (unsafeCoerce a :: View e)
getElement n = return $ _node n

getNode :: forall e. View e -> IO (Maybe NNode)
getNode View {} = return Nothing
getNode (DiffView _ v) = getNode v
getNode (DiffEqView _ v) = getNode v
getNode TextView {..} = forM _tnode toNode
getNode STView {..} =
  case _strecord of
    Nothing -> return Nothing
    Just ref -> do
      (_,_,_,a,_) <- readMVar ref
      getNode (unsafeCoerce a :: View e)
getNode n = forM (_node n) toNode

getAttributes :: View e -> [Feature e]
getAttributes TextView {} = []
getAttributes STView {} = []
getAttributes View {} = []
getAttributes NullView {} = []
getAttributes (DiffView _ v) = getAttributes v
getAttributes (DiffEqView _ v) = getAttributes v
getAttributes n = _attributes n

getChildren :: forall e. View e -> IO [View e]
getChildren (DiffView _ v) = getChildren v
getChildren (DiffEqView _ v) = getChildren v
getChildren STView {..} = do
  case _strecord of
    Nothing -> return []
    Just ref -> do
      (_,_,_,a,_) <- readMVar ref
      return [unsafeCoerce a :: View e]
getChildren HTML {..} = return _atoms
getChildren SVG {..} = return _atoms
getChildren KHTML {..} = return $ map snd _keyed
getChildren KSVG {..} = return $ map snd _keyed
getChildren _ = return []

diff_ :: ControllerPatch m -> IO ()
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
        ControllerView !raw_html !live_html live_m isFG <- readIORef as_live
        let !new_html = render $ ap_patchRenderable as_model
        new_live_html <- diffHelper ap_send doc ap_hooks isFG live_html raw_html new_html
        writeIORef as_live $ ControllerView new_html new_live_html as_model isFG
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
      ControllerView !raw_html !live_html live_m isFG <- readIORef as_live
      let !new_html = render $ ap_patchRenderable as_model
      new_live_html <- diffHelper ap_send doc ap_hooks isFG live_html raw_html new_html
      writeIORef as_live $ ControllerView new_html new_live_html as_model isFG
      ap_viewTrigger
#endif

replace :: View e -> View e' -> IO ()
#ifndef __GHCJS__
replace _ _ = return ()
#else
replace old new = do
  mon <- getNode old
  mnn <- getNode new
  forM_ mon $ \on ->
    forM_ mnn $ \nn ->
      swap_js on nn
#endif

delete :: View e -> IO ()
#ifndef __GHCJS__
delete _ = return ()
#else
delete o = do
  mn <- getNode o
  forM_ mn delete_js
#endif

cleanup :: (Ef e IO () -> IO ()) -> [View e] -> IO (IO ())
#ifndef __GHCJS__
cleanup _ _ = return (return ())
#else
cleanup f = go (return ())
  where
    go didUnmount [] = return didUnmount
    go didUnmount (r:rest) = do
      me <- getElement r
      du <- case me of
              Nothing -> return (return ())
              Just e  -> foldM (flip (cleanupAttr f e)) (return ()) (getAttributes r)
      unmounts' <- cleanup f =<< getChildren r
      go (unmounts' >> du >> didUnmount) rest
#endif

insertAt :: ENode -> Int -> View e -> IO ()
#ifndef __GHCJS__
insertAt _ _ _ = return ()
#else
insertAt parent ind n = getNode n >>= \mn -> forM_ mn $ insert_at_js parent ind
#endif

insertBefore_ :: forall e. ENode -> View e -> View e -> IO ()
#ifndef __GHCJS__
insertBefore_ _ _ _ = return ()
#else
insertBefore_ parent child new = do
  mcn <- getNode child
  mnn <- getNode new
  void $ N.insertBefore parent mnn mcn
#endif

diffHelper :: forall e v. (v ~ View e, e <: '[])
           => (Ef e IO () -> IO ()) -> Doc -> ControllerHooks -> Bool -> v -> v -> v -> IO v
diffHelper f doc ch isFG =
#ifdef __GHCJS__
    go
#else
    \_ _ n -> return n
#endif
  where

    go :: v -> v -> v -> IO v
    go old mid@(View m) new@(View n) =
      if reallyVeryUnsafeEq m n then do
        return old
      else
        go old (render m) (render n)

    go old mid new = do
      if reallyUnsafeEq mid new then do
        return old
      else
        go' old (render mid) (render new)

    go' :: View e -> View e -> View e -> IO (View e)
    go' old mid new@(View n) = do
      go old (render mid) (render new)

    go' old mid@(View _) new =
      go old (render mid) (render new)

    go' old@(DiffView _ v_old) mid@(DiffView m v) new@(DiffView m' v') =
      if typeOf m == typeOf m' && reallyVeryUnsafeEq m m' then
        return old
      else do
        new <- go' v_old v v'
        return (DiffView m' new)

    go' old@(DiffEqView _ v_old) mid@(DiffEqView m v) new@(DiffEqView m' v') =
      if typeOf m == typeOf m' && prettyUnsafeEq m (unsafeCoerce m') then do
        return old
      else do
        new <- go' v_old v v'
        return (DiffEqView m' new)

    go' old@NullView{} _ new = do
      case new of
        NullView _ -> return old
        _          -> do
          new' <- buildHTML doc ch isFG f new
          replace old new'
          didUnmount <- cleanup f [old]
          delete old
          didUnmount
          return new'

    go' old _ new@NullView{} = do
      new' <- buildHTML doc ch isFG f new
      replace old new'
      didUnmount <- cleanup f [old]
      delete old
      didUnmount
      return new'

    go' old@HTML {} mid@HTML {} new@HTML {} = do
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
        return $ HTML (_node old) (_tag old) a' c'
      else do new' <- buildHTML doc ch isFG f new
              replace old new'
              didUnmount <- cleanup f [old]
              delete old
              didUnmount
              return new'

    go' old@STView {} _ new@STView {} = do
      case (old,new) of
        (STView p k ~(Just r) c upds updp unm sp,STView p' k' _ _ _ _ _ _) -> do
          if prettyUnsafeEq k k' then
            if reallyVeryUnsafeEq p p' then do
              return old
            else do
              updp (unsafeCoerce p')
              return $ STView (unsafeCoerce p') k (Just r) c upds updp unm sp
          else do
            (_,_,_,a,_) <- readMVar r
            new' <- buildHTML doc ch isFG f new
            unm
            replace a new'
            didUnmount <- cleanup f [unsafeCoerce a]
            delete a
            didUnmount
            return new'

    go' old@SVG {} mid@SVG {} new@SVG {} =
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
        return $ SVG (_node old) (_tag old) a' c'
      else do new' <- buildHTML doc ch isFG f new
              replace old new'
              didUnmount <- cleanup f [old]
              delete old
              didUnmount
              return new'

    go' old@(KHTML old_node old_tag old_attributes old_keyed)
      mid@(KHTML midAnode _ midAattributes midAkeyed)
      new@(KHTML _ new_tag new_attributes new_keyed) =
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
        return $ KHTML old_node old_tag a' c'
      else do new' <- buildHTML doc ch isFG f new
              replace old new'
              didUnmount <- cleanup f [old]
              delete old
              didUnmount
              return new'

    go' old@(KSVG old_node old_tag old_attributes old_keyed)
      mid@(KSVG midAnode _ midAattributes midAkeyed)
      new@(KSVG _ new_tag new_attributes new_keyed) =
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
        return $ KSVG old_node old_tag a' c'
      else do new' <- buildHTML doc ch isFG f new
              replace old new'
              didUnmount <- cleanup f [old]
              delete old
              didUnmount
              return new'

    go' txt@(TextView (Just t) cnt) mid@(TextView _ mcnt) new@(TextView _ cnt') =
      if prettyUnsafeEq mcnt cnt' then do
        return txt
      else do
        changeText t cnt'
        return $ TextView (Just t) cnt'

    go' old@(RawView {}) mid@(RawView {}) new@(RawView {}) =
      if prettyUnsafeEq (_tag old) (_tag new) then do
        let Just n = _node old
        (a',didMount) <-
                if reallyUnsafeEq (_attributes mid) (_attributes new) then
                  return (_attributes old,return ())
                else
                  runElementDiff f n (_attributes old) (_attributes mid) (_attributes new)
        if prettyUnsafeEq (_content mid) (_content new) then do
          didMount
          return $ RawView (_node old) (_tag old) a' (_content old)
        else do
          setInnerHTML n (_content new)
          didMount
          return $ RawView (_node old) (_tag old) a' (_content new)
      else do new' <- buildHTML doc ch isFG f new
              replace old new'
              didUnmount <- cleanup f [old]
              delete old
              didUnmount
              return new'

    go' old@(Managed {}) mid new@(newc@(Managed {})) =
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
        t@(TextView (Just o) _) ->
          forM_ (_node n') (swapContent o)
        _ -> do
          replace old n'
          didUnmount <- cleanup f [old]
          delete old
          didUnmount
      return n'

    diffChildren :: ENode -> [v] -> [v] -> [v] -> IO [v]
    diffChildren n olds mids news = do
      withLatest olds mids news
      where

        withLatest :: [v] -> [v] -> [v] -> IO [v]
        withLatest = go_
          where

            go_ :: [v] -> [v] -> [v] -> IO [v]
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

                continue :: v -> IO [v]
                continue up = do
                  upds <-
                    if reallyUnsafeEq mids news then return olds else
                      withLatest olds mids news
                  return (up:upds)

              in do
                new <- go old mid new
                continue new

    diffKeyedChildren :: ENode -> [(Int,v)] -> [(Int,v)] -> [(Int,v)] -> IO [(Int,v)]
    diffKeyedChildren n = go_ 0 IM.empty
      where

        go_ :: Int -> IM.IntMap v -> [(Int,v)] -> [(Int,v)] -> [(Int,v)] -> IO [(Int,v)]
        go_ i store a m b = do
          if reallyUnsafeEq m b then do
            mapM_ cleanupElement store
            return a
          else
            go__ i store a m b
          where

            getFromStore :: IM.IntMap v -> Int -> (Maybe v,IM.IntMap v)
            getFromStore store i = IM.updateLookupWithKey (\_ _ -> Nothing) i store

            cleanupElement :: v -> IO ()
            cleanupElement v = do
              didUnmount <- cleanup f [v]
              delete v
              didUnmount

            go__ :: Int -> IM.IntMap v -> [(Int,v)] -> [(Int,v)] -> [(Int,v)] -> IO [(Int,v)]
            go__ _ store [] _ [] = do
              mapM_ cleanupElement store
              return []

            go__ i store [] _ ((bkey,b):bs) = do

              (child,store') <-
                case getFromStore store bkey of

                  (Nothing,_) -> do
                    new <- buildAndEmbedMaybe f doc ch isFG (Just n) b
                    return (new,store)

                  (Just prebuilt_b,store') -> do
                    mn <- getNode prebuilt_b
                    forM_ mn (appendChild n)
                    return (prebuilt_b,store')

              rest <- go__ 0 store' [] [] bs
              return $ (bkey,child) : rest

            go__ _ store olds _ [] = do
              mapM_ (cleanupElement . snd) olds
              mapM_ cleanupElement store
              return []

            go__ i store old@((akey,a):as) mid@((mkey,m):ms) new@((bkey,b):bs)
              | akey == bkey = do
                  new <- go' a (render m) (render b)
                  let !i' = i + 1
                  rest <- go_ i' store as ms bs
                  return $ (akey,new):rest

              | otherwise =
                  case (as,ms,bs) of
                    ((akey',a'):as',(mkey',m'):ms',(bkey',b'):bs')

                      -- swap
                      | bkey == akey' && akey == bkey' -> do

                          -- Diff both nodes
                          new1 <- go' a (render m) (render b)
                          new2 <- go' a' (render m') (render b')

                          -- move the second before the first
                          insertAt n i new1

                          -- continue with the rest of the list
                          let !i' = i + 2
                          rest <- go_ i' store as' ms' bs'
                          return $ (akey',new1):(akey,new2):rest

                      -- insert
                      | akey == bkey' ->

                          -- check if bkey was deleted earlier for re-embedding
                          case getFromStore store bkey of

                            (Nothing,_) ->
                              -- bkey wasn't deleted, check if it exists later in the list
                              case List.lookup bkey as of

                                Nothing -> do

                                  -- bkey is not later in the list, create it
                                  new <- buildAndEmbedMaybe f doc ch isFG Nothing b
                                  insertAt n i new
                                  let !i' = i + 1
                                  rest <- go_ i' store old mid bs
                                  return $ (bkey,new):rest

                                Just prebuilt_b -> do

                                  -- bkey found later in the list, move it
                                  -- this path defeats reallyUnsafeEq
                                  insertAt n i prebuilt_b
                                  let !i' = i + 1
                                      old' = List.deleteFirstsBy ((==) `F.on` fst) old [(bkey,prebuilt_b)]
                                      mid' = List.deleteFirstsBy ((==) `F.on` fst) mid [(bkey,prebuilt_b)]
                                  rest <- go__ i' store old' mid' bs
                                  return $ (bkey,prebuilt_b):rest

                            (Just prebuilt_b,store') -> do

                              -- bkey was seen earlier, move it
                              insertAt n i prebuilt_b
                              let !i' = i + 1
                              rest <- go_ i' store' old mid bs
                              return $ (bkey,prebuilt_b):rest

                      -- delete
                      | otherwise -> do

                          -- simply add akey to the store and continue
                          let !store' = IM.insert akey a store
                              !i' = i + 1
                          go_ i' store' as ms new

                    _ | akey == bkey -> do

                          new <- go' a (render m) (render b)
                          let !i' = i + 1
                          rest <- go_ i' store as ms bs
                          return $ (akey,new):rest

                      | otherwise -> do

                          let !store' = IM.insert akey a store
                              !i' = i + 1
                          go_ i' store' as ms new

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

runElementDiff :: f ~ Feature e => (Ef e IO () -> IO ()) -> ENode -> [f] -> [f] -> [f] -> IO ([f],IO ())
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
      mapM (\old -> removeAttribute_ f el old >> return NullFeature) olds

    go dm_ (old:olds) (mid:mids) (new:news) =
      let
        remove =
          removeAttribute_ f el old

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

            (DiffFeature m ft,DiffFeature m' ft') ->
              if typeOf m == typeOf m' && reallyVeryUnsafeEq m m' then
                continue old
              else
                replace

            (DiffEqFeature m ft,DiffEqFeature m' ft') ->
              if typeOf m == typeOf m' && prettyUnsafeEq m (unsafeCoerce m') then
                continue old
              else
                replace

            (Property nm oldV,Property nm' newV) ->
              if prettyUnsafeEq nm nm' then
                if prettyUnsafeEq oldV newV then
                  continue old
                else
                  update
              else
                replace

            (DelayedProperty nm oldV,DelayedProperty nm' newV) ->
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

            (DelayedAttribute nm val,DelayedAttribute nm' val') ->
              if prettyUnsafeEq nm nm' then
                if prettyUnsafeEq val val' then do
                  continue old
                else do
                  update
              else
                replace

            (OnE e os g _,OnE e' os' g' _) ->
              if prettyUnsafeEq e e' && prettyUnsafeEq os os' && reallyUnsafeEq g g' then
                continue old
              else
                replace

            (OnDocument e os g _,OnDocument e' os' g' _) ->
              if prettyUnsafeEq e e' && prettyUnsafeEq os os' && reallyUnsafeEq g g' then
                continue old
              else
                replace

            (OnWindow e os g _,OnWindow e' os' g' _) ->
              if prettyUnsafeEq e e' && prettyUnsafeEq os os' && reallyUnsafeEq g g' then
                continue old
              else do
                replace

            (OnFeatureAdd e,OnFeatureAdd e') ->
              -- Unlike On(Will/Did)Mount, OnFeatureAdd runs any time the handler changes, like OnModelChange.
              if reallyUnsafeEq e e' then
                continue old
              else do
                f (e' el)
                replace

            (OnFeatureRemove e,OnFeatureRemove e') ->
              if reallyUnsafeEq e e' then
                continue old
              else do
                f (e el)
                replace

            (OnWillMount g,OnWillMount g') ->
              -- OnWillMount has already run, it can't run again.
              continue old

            (OnDidMount g,OnDidMount g') ->
              -- OnDidMount has already run, it can't run again.
              continue old

            (OnModelChangeIO m g,OnModelChangeIO m' g') ->
              if typeOf m == typeOf m' then
                if reallyVeryUnsafeEq m m' then
                  if reallyVeryUnsafeEq g g' then
                    continue old
                  else
                    replace
                else do
                  g' (unsafeCoerce m) m' el
                  replace
              else
                replace

            (OnModelChange m g,OnModelChange m' g') ->
              if typeOf m == typeOf m' then
                if reallyVeryUnsafeEq m m' then
                  if reallyVeryUnsafeEq g g' then
                    continue old
                  else
                    replace
                else do
                  f (g' (unsafeCoerce m) m' el)
                  replace
              else
                replace

            (OnWillUnmount g,OnWillUnmount g') ->
              if reallyUnsafeEq g g' then
                continue old
              else
                replace

            (OnDidUnmount g,OnDidUnmount g') ->
              if reallyUnsafeEq g g' then
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

removeAttribute_ :: (Ef e IO () -> IO ()) -> ENode -> Feature e -> IO ()
removeAttribute_ f element attr =
#ifndef __GHCJS__
  return ()
#else
  case attr of
    DiffFeature _ ft ->
      removeAttribute_ f element ft

    DiffEqFeature _ ft ->
      removeAttribute_ f element ft

    Property nm _ ->
      set_element_property_null_js element nm

    DelayedProperty nm _ ->
      set_element_property_null_js element nm

    Attribute nm _ ->
      E.removeAttribute element nm

    DelayedAttribute nm _ ->
      E.removeAttribute element nm

    LinkTo _ unreg -> do
      forM_ unreg id
      E.removeAttribute element ("href" :: Txt)

    OnE _ _ _ unreg ->
      forM_ unreg id

    OnDocument _ _ _ unreg ->
      forM_ unreg id

    OnWindow _ _ _ unreg ->
      forM_ unreg id

    OnFeatureRemove e ->
      f (e element)

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
#ifdef __GHCJS__
  stopper <- newIORef undefined
  stopListener <- Ev.on el (Ev.unsafeEventName nm :: Ev.EventName E.Element T.CustomEvent) $ do
    ce <- Ev.event
    when (_preventDef os) Ev.preventDefault
    when (_stopProp os) Ev.stopPropagation
    stop <- liftIO $ readIORef stopper
    liftIO $ f stop (unsafeCoerce ce)
  writeIORef stopper stopListener
  return stopListener
#else
  return (return ())
#endif

property :: ENode -> Feature e -> IO ()
#ifdef __GHCJS__
property node (Property k v) = set_property_js node k v
property node (DelayedProperty k v) = set_property_js node k v
#endif
property _ _ = return ()

attribute :: ENode -> Feature e -> IO ()
#ifdef __GHCJS__
attribute node (Attribute k v) = E.setAttribute node k v
attribute node (DelayedAttribute k v) = E.setAttribute node k v
#endif
attribute _ _ = return ()

#ifdef __GHCJS__
addEventListenerOptions :: (MonadIO c, Ev.IsEventTarget et, T.ToJSString t)
                        => et -> t -> T.EventListener -> Obj -> c ()
addEventListenerOptions self type' callback options =
  liftIO $
    js_addEventListenerOptions
      (T.toEventTarget self)
      (T.toJSString type')
      callback
      options

removeEventListenerOptions :: (MonadIO c, Ev.IsEventTarget et, T.ToJSString t)
                           => et -> t -> T.EventListener -> Obj -> c ()
removeEventListenerOptions self type' callback options =
  liftIO $
    js_removeEventListenerOptions
      (T.toEventTarget self)
      (T.toJSString type')
      callback
      options

foreign import javascript unsafe
        "$1[\"addEventListener\"]($2, $3,\n$4)" js_addEventListenerOptions
        :: T.EventTarget -> JSString -> T.EventListener -> Obj -> IO ()

foreign import javascript unsafe
        "$1[\"removeEventListener\"]($2,\n$3, $4)" js_removeEventListenerOptions
        :: T.EventTarget -> JSString -> T.EventListener -> Obj -> IO ()

onWith :: forall et e. (T.IsEventTarget et, T.IsEvent e)
       => Obj
       -> et
       -> Ev.EventName et e
       -> Ev.EventM et e ()
       -> IO (IO ())
onWith options target (Ev.EventName eventName) callback = do
  sl@(Ev.SaferEventListener l) :: Ev.SaferEventListener et e <- Ev.newListener callback
  addEventListenerOptions target eventName l options
  return (removeEventListenerOptions target eventName l options >> Ev.releaseListener sl)
#endif

setAttribute_ :: f ~ Feature e => (Ef e IO () -> IO ()) -> Bool -> ENode -> f -> IO () -> IO (f,IO ())
setAttribute_ c diffing element attr didMount =
#ifndef __GHCJS__
  return (attr,return ())
#else
  case attr of
    NullFeature ->
      return (NullFeature,didMount)

    DiffFeature m f -> do
      (f,dm) <- setAttribute_ c diffing element f didMount
      return (DiffFeature m f,dm)

    DiffEqFeature m f -> do
      (f,dm) <- setAttribute_ c diffing element f didMount
      return (DiffEqFeature m f,dm)

    Property nm v -> do
      set_property_js element nm v
      return (attr,didMount)

    DelayedProperty nm v -> do
      rafCallback <- newRequestAnimationFrameCallback $ \_ ->
        set_property_js element nm v
      win <- getWindow
      requestAnimationFrame win (Just rafCallback)
      return (attr,didMount)

    -- optimize this; we're doing a little more work than necessary!
    Attribute nm val -> do
      E.setAttribute element nm val
      return (attr,didMount)

    DelayedAttribute nm val -> do
      rafCallback <- newRequestAnimationFrameCallback $ \_ ->
        E.setAttribute element nm val
      win <- getWindow
      requestAnimationFrame win (Just rafCallback)
      return (attr,didMount)

    LinkTo href _ -> do
      E.setAttribute element ("href" :: Txt) href
      stopListener <-
#ifdef PASSIVE_LISTENERS
        -- enable by default when I have a polyfill or Edge/IE supports
        onWith
          (object (if _passive os then [ "passive" .= True ] else []))
#else
        Ev.on
#endif
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

    OnE ev os f _ -> do
      stopper <- newIORef undefined
      stopListener <-
#ifdef PASSIVE_LISTENERS
        -- enable by default when I have a polyfill or Edge/IE supports
        onWith
          (object (if _passive os then [ "passive" .= True ] else []))
#else
        Ev.on
#endif
          element
          (Ev.unsafeEventName ev :: Ev.EventName E.Element T.CustomEvent) -- for the type checking; actually just an object
            $ do ce <- Ev.event
                 when (_preventDef os) Ev.preventDefault
                 when (_stopProp os) Ev.stopPropagation
                 stop <- liftIO $ readIORef stopper
                 liftIO $ mapM_ c =<< f (Evt
                   (unsafeCoerce ce)
                   (Event.preventDefault ce)
                   (Event.stopPropagation ce)
                   (join $ readIORef stopper)
                   element
                   )
                 return ()
      writeIORef stopper stopListener
      return (OnE ev os f (Just stopListener),didMount)

    OnDocument ev os f _ -> do
      stopper <- newIORef undefined
      doc <- getDocument
      stopListener <-
#ifdef PASSIVE_LISTENERS
        -- enable by default when I have a polyfill or Edge/IE supports
        onWith
          (object (if _passive os then [ "passive" .= True ] else []))
#else
        Ev.on
#endif
          doc
          (Ev.unsafeEventName ev :: Ev.EventName Doc T.CustomEvent) -- for the type checking; actually just an object
            $ do ce <- Ev.event
                 when (_preventDef os) Ev.preventDefault
                 when (_stopProp os) Ev.stopPropagation
                 stop <- liftIO $ readIORef stopper
                 liftIO $ mapM_ c =<< f (Evt
                   (unsafeCoerce ce)
                   (Event.preventDefault ce)
                   (Event.stopPropagation ce)
                   (join $ readIORef stopper)
                   element
                   )
                 return ()
      writeIORef stopper stopListener
      return (OnDocument ev os f (Just stopListener),didMount)

    OnWindow ev os f _ -> do
      stopper <- newIORef undefined
      win <- getWindow
      stopListener <-
#ifdef PASSIVE_LISTENERS
        -- enable by default when I have a polyfill or Edge/IE supports
        onWith
          (object (if _passive os then [ "passive" .= True ] else []))
#else
        Ev.on
#endif
          win
          (Ev.unsafeEventName ev :: Ev.EventName Win T.CustomEvent) -- for the type checking; actually just an object
            $ do ce <- Ev.event
                 when (_preventDef os) Ev.preventDefault
                 when (_stopProp os) Ev.stopPropagation
                 stop <- liftIO $ readIORef stopper
                 liftIO $ mapM_ c =<< f (Evt
                   (unsafeCoerce ce)
                   (Event.preventDefault ce)
                   (Event.stopPropagation ce)
                   (join $ readIORef stopper)
                   element
                   )
                 return ()
      writeIORef stopper stopListener
      return (OnWindow ev os f (Just stopListener),didMount)

    OnFeatureAdd e -> do
      c (e element)
      return (attr,didMount)

    OnWillMount f -> do
      f element
      return (attr,didMount)

    OnDidMount f -> do
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

cleanupAttr :: (Ef e IO () -> IO ()) -> ENode -> Feature e -> IO () -> IO (IO ())
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
    DiffFeature _ ft ->
      cleanupAttr f element ft didUnmount
    DiffEqFeature _ ft ->
      cleanupAttr f element ft didUnmount
    OnE _ _ _ unreg -> do
      forM_ unreg id
      return didUnmount
    OnDocument _ _ _ unreg -> do
      forM_ unreg id
      return didUnmount
    OnWindow _ _ _ unreg -> do
      forM_ unreg id
      return didUnmount
    OnFeatureRemove e -> do
      f (e element)
      return didUnmount
    OnWillUnmount g -> do
      g element
      return didUnmount
    OnDidUnmount g -> return (didUnmount >> g element)
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
  liftIO $ T.castToElement b
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

-- makePrisms ''View
-- makeLenses ''View
