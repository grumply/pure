{-# language UndecidableInstances #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language DeriveFunctor #-}
{-# language MagicHash #-}
{-# language CPP #-}
module Atomic.Construct (module Atomic.Construct) where

import Ef.Base hiding (Client,After,Before,current,Lazy,Eager,construct)
import qualified Ef.Base

import Data.Txt as Txt hiding (replace,map,head,filter)

import Atomic.Attribute
import Atomic.Cond
import Atomic.CSS
import Atomic.Key
import Atomic.Render
import Atomic.Revent
import Atomic.UnsafeEq
import Atomic.Vault
import Atomic.With
import Atomic.ToTxt
import Atomic.FromTxt

#ifdef __GHCJS__
import qualified GHCJS.Types as T
import qualified GHCJS.Marshal as M
import qualified GHCJS.Marshal.Pure as M

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
#endif

import Control.Concurrent
import Data.Char
import Data.Foldable
import Data.Functor.Identity
import Data.Hashable
import Data.IORef
import Data.List as List hiding (delete,head)
import Data.Maybe
import Data.String
import Data.Traversable
import Data.Void
import GHC.Prim

import qualified Data.HashMap.Strict as Map

import Prelude hiding (div,head,span)
import Language.Haskell.TH hiding (Loc)
import Language.Haskell.TH.Syntax hiding (Loc)

import System.IO.Unsafe
import Unsafe.Coerce

#ifdef LENS
import Control.Lens (makePrisms,makeLenses)
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "$r = $1.parentNode == $2;"
  is_already_embedded_js :: E.Element -> E.Element -> IO Bool

foreign import javascript unsafe
  "$r = $1.parentNode == $2;"
  is_already_embedded_text_js :: T.Text -> E.Element -> IO Bool

foreign import javascript unsafe
  "$1.parentNode.replaceChild($2,$1);"
  swap_js :: N.Node -> N.Node -> IO ()

foreign import javascript unsafe
  "$1.insertBefore($3,$1.children[$2]);"
  insert_at_js :: E.Element -> Int -> N.Node -> IO ()

foreign import javascript unsafe
  "$1.parentNode.removeChild($1);"
  remove_js :: N.Node -> IO ()

foreign import javascript unsafe
  "$1.insertBefore($2,$1.children[0]);"
  prepend_child_js :: E.Element -> N.Node -> IO ()

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
  setPropertyNull :: O.Object -> Txt -> IO ()

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
#endif

type ENode =
#ifdef __GHCJS__
  T.Element
#else
  ()
#endif

type TNode =
#ifdef __GHCJS__
  T.Text
#else
  ()
#endif

type NNode =
#ifdef __GHCJS__
  T.Node
#else
  ()
#endif

data Atom e where
  -- NullAtom must have a presence on the page for proper diffing
  NullAtom
    :: { _node :: !(Maybe ENode)
       } -> Atom e

  Text
    ::  { _tnode      :: Maybe TNode
        , _content    :: Txt
        } -> Atom e

  Raw
    :: { _node        :: Maybe ENode
       , _tag         :: Txt
       , _attributes  :: [Feature e]
       , _content     :: Txt
       } -> Atom e

  KAtom
    ::  { _node       :: Maybe ENode
        , _tag        :: Txt
        , _attributes :: [Feature e]
        , _keyed      :: [(Int,Atom e)]
        } -> Atom e

  Atom
    ::  { _node       :: Maybe ENode
        , _tag        :: Txt
        , _attributes :: [Feature e]
        , _children   :: [Atom e]
        } -> Atom e

  -- TODO: SVG keyed node
  SVGAtom
    ::  { _node       :: Maybe ENode
        , _tag        :: Txt
        , _attributes :: [Feature e]
        , _children   :: [Atom e]
        } -> Atom e

  Managed
    ::  { _node       :: Maybe ENode
        , _tag        :: Txt
        , _attributes :: [Feature e]
        , _constr      :: Constr
        } -> Atom e
  deriving Functor

type HTML ms m = Atom (Code (Appended (ConstructBase m) ms) IO ())
type Attribute ms m = Atom (Code (Appended (ConstructBase m) ms) IO ())

instance ToTxt (Feature (Code ms IO ())) where
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

instance ToTxt [Feature (Code ms IO ())] where
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

instance ToTxt (Atom (Code ms IO ())) where
  toTxt NullAtom {} = mempty
  toTxt Text {..} = _content
  toTxt Raw {..} =
    "<" <> _tag <> " " <> Txt.intercalate " " (map toTxt _attributes) <>
      ">" <> _content <> "</" <> _tag <> ">"
  toTxt KAtom {..} =
    "<" <> _tag <> " " <> Txt.intercalate " " (map toTxt _attributes) <>
      if selfClosing _tag then
        "/>"
      else
        ">" <> Txt.concat (map (toTxt . snd) _keyed) <> "</" <> _tag <> ">"
  toTxt Atom {..} =
    "<" <> _tag <> " " <> Txt.intercalate " " (map toTxt _attributes) <>
      if selfClosing _tag then
        "/>"
      else
        ">" <> Txt.concat (map toTxt _children) <> "</" <> _tag <> ">"
  toTxt SVGAtom {..} =
    "<" <> _tag <> " " <> Txt.intercalate " " (map toTxt _attributes) <>
      if selfClosing _tag then
        "/>"
      else
        ">" <> Txt.concat (map toTxt _children) <> "</" <> _tag <> ">"
  toTxt Managed {..} =
    case _constr of
      Construct' Construct {..} ->
        "<" <> _tag <> " " <> Txt.intercalate " " (map toTxt _attributes) <>
          ">"  <> toTxt (view model) <> "</" <> _tag <> ">"

staticHTML :: Atom (Code ms IO ()) -> StaticHTML
staticHTML = render

shtml :: Txt -> [Feature (Code ms IO ())] -> StaticHTML -> Atom (Code ms IO ())
shtml _tag _attributes = raw _tag _attributes . toTxt

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
                    Construct' a@Construct {} -> construct div [ id_ "fusion" ] a
                ]
            ]
renderSystem (Subsystem c) =
  ("<!DOCTYPE html>" <>) $
    toTxt $
      html_ []
        [ head []
        , body []
            [ case c of
                Construct' a@Construct {} -> construct div [ id_ "fusion" ] a
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
                    Construct' a@Construct {} -> construct div [ id_ "fusion" ] a
                , script [ src mainScript, defer True ] []
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
                [ construct div [ id_ "fusion" ] a
                , script [ src mainScript, defer True ] []
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
          [ raw "div" [ id_ "fusion" ] body_html ]
      ]
renderDynamicSystem (Subsystem (Construct' c)) = do
  let dt = "<!DOCTYPE html>"
  Just c_ <- demandMaybe =<< currentView c
  body_html <- renderDynamicHTML c_
  return $ (dt <>) $ toTxt $
    html_ []
      [ head []
      , body []
          [ raw "div" [ id_ "fusion" ] body_html ]
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
          [ raw "div" [ id_ "fusion" ] body_html
          , script [ src mainScript, defer True ] []
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
          [ raw "div" [ id_ "fusion" ] body_html
          , script [ src mainScript, defer True ] []
          ]
      ]

renderDynamicHTML :: Atom (Code ms IO ()) -> IO Txt
renderDynamicHTML h =
  case h of
    NullAtom {} -> return mempty

    Text {..} -> return _content

    Raw {..} ->
      return $ "<" <> _tag <> " " <> Txt.intercalate " " (map toTxt _attributes) <>
                 ">" <> _content <> "</" <> _tag <> ">"

    KAtom {..} ->
      return $
        "<" <> _tag <> " " <> Txt.intercalate " " (map toTxt _attributes) <>
          if selfClosing _tag then
            "/>"
          else
            ">" <> Txt.concat (map (toTxt . snd) _keyed) <> "</" <> _tag <> ">"

    Atom {..} ->
      return $
        "<" <> _tag <> " " <> Txt.intercalate " " (map toTxt _attributes) <>
          if selfClosing _tag then
            "/>"
          else
            ">" <> Txt.concat (map toTxt _children) <> "</" <> _tag <> ">"

    SVGAtom {..} ->
      return $
        "<" <> _tag <> " " <> Txt.intercalate " " (map toTxt _attributes) <>
          if selfClosing _tag then
            "/>"
          else
            ">" <> Txt.concat (map toTxt _children) <> "</" <> _tag <> ">"

    Managed {..} ->
      case _constr of
        Construct' a@Construct {..} -> do
          Just v <- demandMaybe =<< currentView a
          inner <- renderDynamicHTML v
          return $
            "<" <> _tag <> " " <> Txt.intercalate " " (map toTxt _attributes) <>
              ">"  <> inner <> "</" <> _tag <> ">"

reflect :: forall ts ms m c.
           ( IsConstruct' ts ms m
           , MonadIO c
           )
        => Construct' ts ms m
        -> c (Promise (Atom (Code ms IO ())))
reflect c =
  with c $ do
    ConstructState {..} :: ConstructState m <- get
    (l,_,_) <- liftIO $ readIORef asLive
    return (unsafeCoerce l)

data DiffStrategy = Lazy | Eager | Manual deriving (Eq)

type Differ ms m =
       (m -> Atom (Code ms IO ()))
    -> ((m,Atom (Code ms IO ())) -> IO ())
    -> (Code ms IO () -> IO ())
    -> ConstructState m
    -> Code ms IO ()

data AState m = AState
  { as_live :: forall ms. IORef (Atom (Code ms IO ()), Atom (Code ms IO ()), m)
  , as_model :: m
  }

data ConstructPatch m where
  APatch ::
      -- only modify ap_AState with atomicModifyIORef
    { ap_send         :: Code ms IO () -> IO ()
    , ap_AState       :: IORef (Maybe (AState m),Bool) -- an AState record for manipulation; nullable by component to stop a patch.
    , ap_patchView    :: (m -> Atom (Code ms IO ()))
    , ap_viewTrigger  :: (m,Atom (Code ms IO ())) -> IO ()
    } -> ConstructPatch m

type IsConstruct' ts ms m =
  ( ConstructBase m <: ms
  , ConstructBase m <. ts
  , Delta (Modules ts) (Messages ms)
  , Eq m
  )
type IsConstruct ms m = IsConstruct' ms ms m

data ConstructState m where
  ConstructState ::
    { asPatch        :: Maybe (ConstructPatch m)
    , asDiffer       :: ConstructState m -> Code ms IO ()
    , asDiffStrategy :: DiffStrategy
    , asViews        :: Network (m,Atom (Code ms IO ()))
    , asUpdates      :: Network m
    , asModel        :: m
    , asLive         :: IORef (Atom (Code ms IO ()), Atom (Code ms IO ()), m)
    } -> ConstructState m

type ConstructBase m
  = '[ State () (ConstructState m)
     , State () Shutdown
     , Revent
     ]

data Constr where
  Construct' :: IsConstruct' ts ms m => Construct' ts ms m -> Constr
instance Eq Constr where
 (==) (Construct' c) (Construct' c') =
  let Key k1 :: Key GHC.Prim.Any = unsafeCoerce (key c)
      Key k2 :: Key GHC.Prim.Any = unsafeCoerce (key c')
  in prettyUnsafeEq k1 k2

type ConstructKey' ms m = Key (Code ms IO `As` IO, IORef (Atom (Code ms IO ()),Atom (Code ms IO ()),m))
type ConstructKey ms m = ConstructKey' (Appended (ConstructBase m) ms) m
type ConstructBuilder' ts m = Modules (ConstructBase m) (Action ts IO) -> IO (Modules ts (Action ts IO))
type ConstructBuilder ts m = ConstructBuilder' (Appended (ConstructBase m) ts) m
type ConstructPrimer' ms = Code ms IO ()
type ConstructPrimer ms m = ConstructPrimer' (Appended (ConstructBase m) ms)
type Construct ms m = Construct' (Appended (ConstructBase m) ms) (Appended (ConstructBase m) ms) m

data Construct' ts ms m
  = Construct
      { key       :: !(ConstructKey' ms m)
      , build     :: !(ConstructBuilder' ts m)
      , prime     :: !(ConstructPrimer' ms)
      , model     :: !(m)
      , view      :: !(m -> Atom (Code ms IO ()))
      }

instance Eq (Construct' ts ms m) where
  (==) (Construct k _ _ _ _) (Construct k' _ _ _ _) =
    let Key k1 = k
        Key k2 = k'
    in prettyUnsafeEq k1 k2

instance Ord (Construct' ts ms m) where
  compare (Construct (Key k) _ _ _ _) (Construct (Key k') _ _ _ _) = compare k k'

instance IsConstruct' ts ms m
  => With (Construct' ts ms m)
          (Code ms IO)
          IO
  where
    using_ c = do
      -- keep an eye on this; there was a bug in fission that caused multiple-instantiation
      -- (since the lookup+mk is non-atomic) but I'm not sure it will arise here.
      mi_ <- lookupConstruct (key c)
      case mi_ of
        Just (as,_) -> return (runAs as)
        Nothing -> do
          mkConstruct (Just differ) Nothing c
          using_ c
    with_ c m = do
      run <- using_ c
      run m
    shutdown_ c = do
      with_ c $ do
        buf <- getReventBuffer
        Shutdown sdn <- get
        syndicate sdn ()
        delay 0 $
          liftIO $ do
            killBuffer buf
            myThreadId >>= killThread
      miohhm <- lookupConstruct (key c)
      case miohhm of
        Just (_,iohhm) -> do
          (h,_,_) <- liftIO $ readIORef iohhm
          cleanup [h]
          delete h
        _ -> return ()
      deleteConstruct (key c)

atomVault__ :: Vault
atomVault__ = Vault (unsafePerformIO (newMVar Map.empty))

lookupConstruct :: (MonadIO c) => Key phantom -> c (Maybe phantom)
lookupConstruct = vaultLookup atomVault__

getConstructName :: IsConstruct' t ms m => Construct' ts ms m -> Txt
getConstructName = toTxt . key

addConstruct :: (MonadIO c) => Key phantom -> phantom -> c ()
addConstruct = vaultAdd atomVault__

deleteConstruct :: (MonadIO c) => Key phantom -> c ()
deleteConstruct = vaultDelete atomVault__

mkConstruct :: forall ms ts m.
          ( IsConstruct' ts ms m
          , ConstructBase m <: ms
          )
       => Maybe (Differ ms m)
       -> Maybe ENode
       -> Construct' ts ms m
       -> IO (IORef (Atom (Code ms IO ()),Atom (Code ms IO ()),m))
mkConstruct mDiffer mparent c@Construct {..} = do
  let !m = model
  sig :: Signal ms IO (Code ms IO ()) <- runner
  sigBuf <- newSignalBuffer
  updates :: Network m <- network
  views :: Network (m,Atom (Code ms IO ())) <- network
  let asComp = constructAs sigBuf sig
      sendEv :: Code ms IO () -> IO ()
      sendEv = void . runAs asComp
  let !raw = view m
  doc <- getDocument
  i <- buildAndEmbedMaybe sendEv doc mparent raw
  cs_live_ :: IORef (Atom (Code ms IO ()),Atom (Code ms IO ()),m) <- newIORef (i,raw,m)
  let cs = AState (unsafeCoerce cs_live_) m
  sdn :: Network () <- network
  addConstruct key (asComp,cs_live_)
  let trig = syndicate views
  built <- build $ state (ConstructState
                               Nothing
                               (differ view trig sendEv)
                               Lazy
                               views
                               updates
                               model
                               cs_live_
                          )
                  *:* state (Shutdown sdn)
                  *:* revent sigBuf
                  *:* Empty
  (obj',_) <- Ef.Base.Object built Ef.Base.! prime
  forkIO $ driverPrintExceptions ("Construct' (" ++ show key ++ ") exception. If this is a DriverStopped exception, this Construct' may be blocked in its event loop, likely caused by cyclic 'with' calls. Exception") sigBuf obj'
  return cs_live_

static :: ConstructKey '[] () -> HTML '[] () -> Construct '[] ()
static key view0 =
  let view _ = view0
      build = return
      prime = return ()
      model = ()
  in Construct {..}

diff :: forall m ms. ('[State () (ConstructState m)] <: ms)
     => Proxy m -> Code ms IO ()
diff _ = do
  as@ConstructState {..} :: ConstructState m <- get
  unsafeCoerce (asDiffer as)

setLazyDiff :: forall m ms. ('[State () (ConstructState m)] <: ms)
            => Proxy m -> Code ms IO ()
setLazyDiff _ = do
  ConstructState {..} :: ConstructState m <- get
  put ConstructState { asDiffStrategy = Lazy, .. }

setEagerDiff :: forall m ms. ('[State () (ConstructState m)] <: ms)
             => Proxy m -> Code ms IO ()
setEagerDiff _ = do
  ConstructState {..} :: ConstructState m <- get
  put ConstructState { asDiffStrategy = Eager, .. }

setManualDiff :: forall m ms. ('[State () (ConstructState m)] <: ms)
              => Proxy m -> Code ms IO ()
setManualDiff _ = do
  ConstructState {..} :: ConstructState m <- get
  put ConstructState { asDiffStrategy = Manual, .. }

currentView :: forall ts ms c m.
               ( IsConstruct' ts ms m
               , MonadIO c
               )
            => Construct' ts ms m
            -> c (Promise (Atom (Code ms IO ())))
currentView c = with c $ ownView c

ownView :: forall ts ms c m.
           ( IsConstruct' ts ms m
           , MonadIO c
           )
        => Construct' ts ms m
        -> Code ms c (Atom (Code ms IO ()))
ownView _ = do
  ConstructState {..} :: ConstructState m <- get
  (h,_,_) <- liftIO $ readIORef asLive
  return (unsafeCoerce h)

onViewChange :: forall ts ms ms' m c.
                ( IsConstruct' ts ms m
                , MonadIO c
                , '[Revent] <: ms'
                )
            => Construct' ts ms m
            -> ((m,Atom (Code ms IO ())) -> Code '[Event (m,Atom (Code ms IO ()))] (Code ms' c) ())
            -> Code ms' c (Subscription ms' c (m,Atom (Code ms IO ())),Periodical ms' c (m,Atom (Code ms IO ())))
onViewChange c f = do
  p <- periodical
  Just s <- subscribe p f
  buf <- getReventBuffer
  with c $ do
    ConstructState {..} :: ConstructState m <- get
    joinNetwork (unsafeCoerce asViews :: Network (m,Atom (Code ms IO ()))) p buf
  return (s,p)

onModelChange :: forall ts ms ms' m c.
                ( IsConstruct' ts ms m
                , MonadIO c
                , '[Revent] <: ms'
                )
              => Construct' ts ms m
              -> (m -> Code '[Event m] (Code ms' c) ())
              -> Code ms' c (Subscription ms' c m,Periodical ms' c m)
onModelChange c f = do
  p <- periodical
  Just s <- subscribe p f
  buf <- getReventBuffer
  with c $ do
    ConstructState {..} :: ConstructState m <- get
    joinNetwork asUpdates p buf
  return (s,p)

{-# INLINE observe #-}
observe :: ('[State () (ConstructState m)] <: ms) => Code ms IO m
observe = do
  ConstructState {..} <- get
  return asModel

{-# INLINE set #-}
set :: forall ms m.
        ( '[State () (ConstructState m)] <: ms
        , Eq m
        )
     => m -> Code ms IO ()
set !new = do
  (ConstructState {..},(old,cmp')) <- modify $ \(ConstructState {..} :: ConstructState m) ->
    let !old = asModel
        cmp' = ConstructState { asModel = new, .. }
    in (cmp',(old,cmp'))
  syndicate asUpdates new
  let d :: ConstructState m -> Code ms IO ()
      d = unsafeCoerce asDiffer
  case reallyUnsafePtrEquality# old new of
    1# -> return ()
    _  ->
      case asDiffStrategy of
        Lazy   -> unless (old == new) (d cmp')
        Eager  -> d cmp'
        Manual -> return ()

{-# INLINE update #-}
update :: forall ms m.
           ( '[State () (ConstructState m)] <: ms
           , Eq m
           )
        => (m -> m)
        -> Code ms IO ()
update f = do
  (ConstructState {..},(old,new,cmp')) <- modify $ \ConstructState {..} ->
    let !old = asModel
        !new = f old
        cmp' = ConstructState { asModel = new, ..  }
    in (cmp',(old,new,cmp'))
  syndicate asUpdates new
  let d :: ConstructState m -> Code ms IO ()
      d = unsafeCoerce asDiffer
  case reallyUnsafePtrEquality# old new of
    1# -> return ()
    _  ->
      case asDiffStrategy of
        Lazy   -> unless (old == new) (d cmp')
        Eager  -> d cmp'
        Manual -> return ()

differ :: (ConstructBase m <: ms) => Differ ms m
differ view trig sendEv ConstructState {..} = do
  let setupDiff = do
        let !new_as = AState (unsafeCoerce asLive) asModel
        new_ap_AState <- liftIO $ newIORef (Just new_as,False)
        let !aPatch = APatch sendEv new_ap_AState view trig
        put ConstructState { asPatch = Just aPatch, .. }
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
embed_ :: ENode -> Atom (Code ms IO ()) -> IO ()
embed_ parent Text {..} =
  forM_ _tnode $ \node -> do
    ae <- isAlreadyEmbeddedText node parent
    unless ae (void $ appendChild parent node)
embed_ parent n =
  forM_ (_node n) $ \node -> do
    ae <- isAlreadyEmbedded node parent
    unless ae (void $ appendChild parent node)

-- rebuild finds managed nodes and re-embeds them in case they were
-- removed for other uses
rebuild :: IsConstruct' ts ms m => Construct' ts ms m -> Maybe ENode -> Atom (Code ms IO ()) -> IO ()
rebuild c me h =
#ifndef __GHCJS__
    return ()
#else
    go me h
  where
    go mparent NullAtom {..} =
      forM_ mparent $ \parent ->
        embed_ parent NullAtom {..}

    go mparent Raw {..} =
      forM_ mparent $ \parent ->
        embed_ parent Raw {..}

    go mparent Atom {..} = do
      forM_ mparent $ \parent ->
        embed_ parent Atom {..}
      forM_ _children (go _node)

    go mparent SVGAtom {..} = do
      forM_ mparent $ \parent ->
        embed_ parent SVGAtom {..}
      forM_ _children (go _node)

    go mparent KAtom {..} = do
      forM_ mparent $ \parent ->
        embed_ parent KAtom {..}
      forM_ _keyed $ go _node . snd

    go mparent Text {..} =
      forM_ mparent $ \parent ->
        embed_ parent Text {..}

    go mparent m@Managed {..} =
      case _constr of
        Construct' c -> do
          mi_ <- lookupConstruct (key c)
          forM_ mi_ $ \(_,x_) -> do
            (h,_,_) <- readIORef x_
            rebuild c _node h
            forM_ mparent $ \parent ->
              embed_ parent h
#endif

setAttributes :: [Feature (Code ms IO ())] -> (Code ms IO () -> IO ()) -> ENode -> IO [Feature (Code ms IO ())]
setAttributes as f el =
#ifdef __GHCJS__
  forM as (setAttribute_ f el)
#else
  return as
#endif


{-# NOINLINE buildAndEmbedMaybe #-}
buildAndEmbedMaybe :: (Code ms IO () -> IO ()) -> Doc -> Maybe ENode -> Atom (Code ms IO ()) -> IO (Atom (Code ms IO ()))
buildAndEmbedMaybe f doc = go
  where
    go mparent nn@NullAtom {..} = do
      _cond@(Just el) <- createElement doc "template"
      forM_ mparent (flip appendChild el)
      return $ NullAtom _cond

    go mparent Raw {..} = do
      _node@(Just el) <- createElement doc _tag
      _attributes <- setAttributes _attributes f el
      setInnerHTML el _content
      forM_ mparent (flip appendChild el)
      return $ Raw _node _tag _attributes _content

    go mparent Atom {..} = do
      _node@(Just el) <- createElement doc _tag
      _attributes <- setAttributes _attributes f el
      forM_ mparent (flip appendChild el)
      _children <- mapM (go (Just el)) _children
      return $ Atom _node _tag _attributes _children

    go mparent SVGAtom {..} = do
      _node@(Just el) <- createElementNS doc ("http://www.w3.org/2000/svg") _tag
      _attributes <- setAttributes _attributes f el
      forM_ mparent (flip appendChild el)
      _children <- mapM (go (Just el)) _children
      return $ SVGAtom _node _tag _attributes _children

    go mparent KAtom {..} = do
      _node@(Just el) <- createElement doc _tag
      _attributes <- setAttributes _attributes f el
      forM_ mparent (flip appendChild el)
      _keyed <- mapM (\(k,x) -> go (Just el) x >>= \y -> return (k,y)) _keyed
      return $ KAtom _node _tag _attributes _keyed

    go mparent Text {..} = do
      _tnode@(Just el) <- createTextNode doc _content
      forM_ mparent (flip appendChild el)
      return $ Text _tnode _content

    go mparent Managed {..} =
      case _constr of
        Construct' a ->
          case _node of
            Nothing -> do
              _node@(Just el) <- createElement doc _tag
              _attributes <- setAttributes _attributes f el
              mi_ <- lookupConstruct (key a)
              case mi_ of
                Nothing -> do
                  -- never built before; make and embed
                  mkConstruct (Just differ) _node a
                  forM_ mparent (`embed_` Managed {..})
                  return Managed {..}
                Just (_,x_) -> do
                  -- built before but not in this context; rebuild and embed
                  (h,_,_) <- readIORef x_
                  rebuild a mparent h
--                  forM_ mparent (`embed` Managed {..})
                  return Managed {..}

            Just e -> do
              mi_ <- lookupConstruct (key a)
              forM_ mi_ $ \(_,x_) -> do
                -- build before in this context; rebuild and embed
                (h,_,_) <- readIORef x_
                rebuild a mparent (unsafeCoerce h) -- should be safe
                -- forM_ mparent (`embed` Managed {..})
              return Managed {..}

{-# NOINLINE buildHTML #-}
buildHTML :: Doc -> (Code ms IO () -> IO ()) -> Atom (Code ms IO ()) -> IO (Atom (Code ms IO ()))
buildHTML doc f = buildAndEmbedMaybe f doc Nothing

-- Useful for standalone components without a Fusion root.
renderConstruct' :: IsConstruct' ts ms m => Construct' ts ms m -> ENode -> Atom (Code ms IO ()) -> IO (Atom (Code ms IO ()))
renderConstruct' a parent html = do
  let f e = void $ with a e
  doc <- getDocument
  html' <- buildHTML doc f html
  embed_ parent html'
  return html'

getElement Text {} = Nothing
getElement n = _node n

diff_ :: ConstructPatch m -> IO Int
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
        (live_html,!raw_html,!live_m) <- readIORef as_live
        let !new_html = ap_patchView as_model
        new_live_html <- either id id <$> diffHelper ap_send doc live_html raw_html new_html
        writeIORef as_live (new_live_html,new_html,as_model)
        ap_viewTrigger (as_model,new_html)
  win <- getWindow
  requestAnimationFrame win (Just rafCallback)
#else
  (mcs,b) <- atomicModifyIORef' ap_AState $ \(mcs,b) -> ((Nothing,True),(mcs,b))
  case mcs of
    Nothing -> return ()
    Just (AState as_live !as_model) -> do
      doc <- getDocument
      (live_html,!raw_html,!live_m) <- readIORef as_live
      let !new_html = ap_patchView as_model
      new_live_html <- either id id <$> diffHelper ap_send doc live_html raw_html new_html
      writeIORef as_live (new_live_html,new_html,as_model)
      ap_viewTrigger (as_model,new_html)
  return 0
#endif

{-# NOINLINE replace #-}
replace :: Atom (Code ms0 IO ()) -> Atom (Code ms1 IO ()) -> IO ()
#ifndef __GHCJS__
replace _ _ = return ()
#else
replace old@Text {} new@Text {} = do
  forM_ (_tnode old) $ \o ->
    forM_ (_tnode new) $ \n ->
      swap_js (toNode o) (toNode n)

replace old Text {..} = do
  forM_ (_node old) $ \o ->
    forM_ _tnode $ \n ->
      swap_js (toNode o) (toNode n)

replace old new = do
  forM_ (_node old) $ \o ->
    forM_ (_node new) $ \n ->
      swap_js (toNode o) (toNode n)
#endif

{-# NOINLINE delete #-}
delete :: Atom (Code ms IO ()) -> IO ()
#ifndef __GHCJS__
delete _ = return ()
#else
delete Text {..} = forM_ _tnode (delete_js . toNode)-- this won't work, will it?
delete n = forM_ (_node n) (delete_js . toNode)
#endif

{-# NOINLINE remove #-}
remove :: Atom (Code ms IO ()) -> IO ()
#ifndef __GHCJS__
remove _ = return ()
#else
remove (Text {..}) = forM_ _tnode (remove_js . toNode)
remove n = forM_ (_node n) (remove_js . toNode)
#endif

{-# NOINLINE cleanup #-}
cleanup :: [Atom (Code ms IO ())] -> IO ()
#ifndef __GHCJS__
cleanup _ = return ()
#else
cleanup (NullAtom{}:rest) = cleanup rest
cleanup (Raw {..}:rest) = do
  forM_ _attributes cleanupAttr
  cleanup rest
cleanup (Atom {..}:rest) = do
  forM_ _attributes cleanupAttr
  cleanup _children
  cleanup rest
cleanup (SVGAtom {..}:rest) = do
  forM_ _attributes cleanupAttr
  cleanup _children
  cleanup rest
cleanup (KAtom {..}:rest) = do
  forM_ _attributes cleanupAttr
  cleanup (map snd _keyed)
  cleanup rest
cleanup (Managed {..}:rest) = do
  forM_ _attributes cleanupAttr
  cleanup rest
cleanup _ = return ()
#endif

{-# NOINLINE insertAt #-}
insertAt :: ENode -> Int -> Atom (Code ms IO ()) -> IO ()
#ifndef __GHCJS__
insertAt _ _ _ = return ()
#else
insertAt parent ind Text {..} = forM_ _tnode $ insert_at_js parent ind . toNode
insertAt parent ind n = forM_ (_node n) $ insert_at_js parent ind . toNode
#endif

{-# NOINLINE prepend #-}
prepend :: ENode -> Atom (Code ms IO ()) -> IO ()
#ifndef __GHCJS__
prepend _ _ = return ()
#else
prepend parent Text {..} = forM_ _tnode $ prepend_child_js parent . toNode
prepend parent n = forM_ (_node n) $ prepend_child_js parent . toNode
#endif

{-# NOINLINE insertBefore_ #-}
insertBefore_ :: ENode -> Atom (Code ms IO ()) -> Atom (Code ms IO ()) -> IO ()
#ifndef __GHCJS__
insertBefore_ _ _ _ = return ()
#else
insertBefore_ parent child@(Text {}) new@(Text{}) = void $ N.insertBefore parent (_tnode new) (_tnode child)
insertBefore_ parent child new@(Text{}) = void $ N.insertBefore parent (_tnode new) (_node child)
insertBefore_ parent child@(Text {}) new = void $ N.insertBefore parent (_node new) (_tnode child)
insertBefore_ parent child new = void $ N.insertBefore parent (_node new) (_node child)
#endif

diffHelper :: (Code ms IO () -> IO ()) -> Doc -> Atom (Code ms IO ()) -> Atom (Code ms IO ()) -> Atom (Code ms IO ()) -> IO (Either (Atom (Code ms IO ())) (Atom (Code ms IO ())))
diffHelper f doc = go
  where
    go old mid new =
      if reallyUnsafeEq mid new then
        return $ Right old
      else
        go' old mid new

      where
        go' old@NullAtom{} _ new = do
          case new of
            NullAtom _ -> return $ Right old
            _          -> do
              new' <- buildHTML doc f new
              replace old new'
              cleanup [old]
              delete old
              return $ Right new'

        go' old _ new@NullAtom{} = do
          new' <- buildHTML doc f new
          replace old new'
          cleanup [old]
          delete old
          return $ Right new'

        go' old@Atom {} mid@Atom {} new@Atom {} =
          if prettyUnsafeEq (_tag old) (_tag new)
          then do let Just n = _node old
                      Just m = _node mid
                  a' <- if reallyUnsafeEq (_attributes mid) (_attributes new) then do
                          return (_attributes old)
                        else
                          runElementDiff f n (_attributes old) (_attributes mid) (_attributes new)
                  c' <- if reallyUnsafeEq (_children mid) (_children new) then do
                          return (_children old)
                        else
                          diffChildren n (_children old) (_children mid) (_children new)
                  return $ Right $ Atom (_node old) (_tag old) a' c'
          else do new' <- buildHTML doc f new
                  -- shouldn't ever hit a nothing, but I stil don't like it
                  replace old new'
                  cleanup [old]
                  delete old
                  return $ Left new'

        go' old@SVGAtom {} mid@SVGAtom {} new@SVGAtom {} =
          if prettyUnsafeEq (_tag old) (_tag new)
          then do let Just n = _node old
                      Just m = _node mid
                  a' <- if reallyUnsafeEq (_attributes mid) (_attributes new) then do
                          return (_attributes old)
                        else
                          runElementDiffSVG f n (_attributes old) (_attributes mid) (_attributes new)
                  c' <- if reallyUnsafeEq (_children mid) (_children new) then do
                          return (_children old)
                        else
                          diffChildren n (_children old) (_children mid) (_children new)
                  return $ Right $ Atom (_node old) (_tag old) a' c'
          else do new' <- buildHTML doc f new
                  -- shouldn't ever hit a nothing, but I stil don't like it
                  replace old new'
                  cleanup [old]
                  delete old
                  return $ Left new'

        go' old@(KAtom old_node old_tag old_attributes old_keyed)
          mid@(KAtom mid_node _ mid_attributes mid_keyed)
          new@(KAtom _ new_tag new_attributes new_keyed) =
          if prettyUnsafeEq old_tag new_tag
          then do let Just n = old_node
                  a' <- if reallyUnsafeEq mid_attributes new_attributes then return old_attributes else
                          runElementDiff f n old_attributes mid_attributes new_attributes
                  c' <- if reallyUnsafeEq mid_keyed new_keyed then return old_keyed else
                          diffKeyedChildren n old_keyed mid_keyed new_keyed
                  return $ Right $ KAtom old_node old_tag a' c'
          else do new' <- buildHTML doc f new
                  replace old new'
                  cleanup [old]
                  delete old
                  return $ Left new'

        go' txt@(Text (Just t) cnt) mid@(Text _ mcnt) new@(Text _ cnt') =
          if reallyUnsafeEq mcnt cnt' then do
            -- putStrLn "Short circuit on text mid/text new in diffHelper.go'.Text/Text/Text"
            return $ Right txt
          else
            if prettyUnsafeEq cnt cnt' then do
              -- putStrLn "Equal text in diffHelper.go'.Text/Text/Text; short circuit failed"
              return $ Right txt
            else do
              changeText t cnt'
              return $ Right $ Text (Just t) cnt'

        go' old@(Raw {}) mid@(Raw {}) new@(Raw {}) =
          if prettyUnsafeEq (_tag old) (_tag new) then do
            let Just n = _node old
            a' <- if reallyUnsafeEq (_attributes mid) (_attributes new) then return (_attributes old) else
                    runElementDiff f n (_attributes old) (_attributes mid) (_attributes new)
            if prettyUnsafeEq (_content mid) (_content new) then
              return $ Right $ Raw (_node old) (_tag old) a' (_content old)
            else do
              setInnerHTML n (_content new)
              return $ Right $ Raw (_node old) (_tag old) a' (_content new)
          else do new' <- buildHTML doc f new
                  replace old new'
                  cleanup [old]
                  delete old
                  return $ Left new'

        go' old@(Managed {}) mid new@(Managed {}) =
          if    (_constr old) == (_constr new)
            && prettyUnsafeEq (_tag old) (_tag new)
          then do
            let Just n = _node old
            a' <- if reallyUnsafeEq (_attributes mid) (_attributes new) then return (_attributes old) else
                    runElementDiff f n (_attributes old) (_attributes mid) (_attributes new)
            return $ Right $ Managed (_node old) (_tag old) a' (_constr old)
          else do
            new' <- buildAndEmbedMaybe f doc Nothing new
            replace old new'
            cleanup [old]
            delete old
            return $ Right new'

        go' old _ n = do
          n' <- buildAndEmbedMaybe f doc Nothing n
          case old of
            t@(Text (Just o) _) ->
              forM_ (_node n) $ \n -> swapContent o n
            _ -> do
              replace old n'
              cleanup [old]
              delete old
          return $ Left n'

    diffChildren n olds mids news = do
      withLatest Nothing olds mids news
      where

        withLatest ml = go_
          where

            go_ [] _ news =
              mapM (buildAndEmbedMaybe f doc (Just n)) news

            go_ olds _ [] = do
              cleanup olds
              mapM_ delete olds
              return []

            go_ (old:olds) (mid:mids) (new:news) =
              let
                remove = do
                  cleanup [old]
                  delete old

                add = do
                  new' <- buildAndEmbedMaybe f doc Nothing new
                  case ml of
                    Nothing -> prepend n new'
                    Just l  -> insertBefore_ n old new'
                  return new'

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
                      new' <- buildHTML doc f new
                      replace old new'
                      cleanup [old]
                      delete old
                      continue new'

                    (NullAtom{},_) -> do
                      new' <- buildHTML doc f new
                      replace old new'
                      cleanup [old]
                      delete old
                      continue new'

                    (m,n) -> do
                      enew <- go old mid new
                      continue (either id id enew)

    -- note that keyed nodes are filtered for NullAtoms during construction
    diffKeyedChildren n = go_ 0
      where

        go_ _ [] _ news =
          forM news $ \(bkey,b) -> do
            new <- buildAndEmbedMaybe f doc (Just n) b
            return (bkey,new)

        go_ _ olds _ [] = do
          forM_ olds $ \(_,a) -> do
            cleanup [a]
            delete a
          return []

        go_ i a_@((akey,a):(akey',a'):as) m_@((mkey,m):(mkey',m'):ms) b_@((bkey,b):(bkey',b'):bs)
          -- heads match
          | prettyUnsafeEq akey bkey = do
            enew <- go a m b
            let new = either id id enew
            if reallyUnsafeEq ms bs then
              return $ (akey,new):as
            else do
              let !i' = i + 1
              rest <- go_ i' ((akey',a'):as) ((mkey',m'):ms) ((bkey',b'):bs)
              return $ (akey,new):rest

          -- swap 2
          | prettyUnsafeEq bkey akey' && prettyUnsafeEq akey bkey' = do
            enew1 <- go a m b'
            enew2 <- go a' m' b
            let new1 = either id id enew1
                new2 = either id id enew2
                !i' = i + 2
            if reallyUnsafeEq ms bs then
              return $ (akey',new1):(akey,new2):as
            else do
              rest <- go_ i' as ms bs
              return $ (akey',new1):(akey,new2):rest

          -- insert
          | prettyUnsafeEq akey bkey' = do
            new <- buildAndEmbedMaybe f doc Nothing b
            insertAt n i new
            let !i' = i + 1
            rest <- go_ i' a_ m_ ((bkey',b'):bs)
            return $ (bkey,new):rest

          -- delete
          | otherwise = do
            cleanup [a]
            delete a
            if prettyUnsafeEq bkey akey' then
              go_ i ((akey',a'):as) ((mkey',m'):ms) b_
            else do
              new <- buildAndEmbedMaybe f doc Nothing b
              insertAt n i new
              let !i' = i + 1
              rest <- go_ i' ((akey',a'):as) ((mkey',m'):ms) ((bkey',b'):bs)
              return $ (bkey,new):rest

        go_ i ((akey,a):as) ((mkey,m):ms) ((bkey,b):bs)
          | prettyUnsafeEq akey bkey = do
            enew <- go a m b
            let new = either id id enew
                !i' = i + 1
            rest <- go_ i' as ms bs
            return $ (akey,new):rest

          | otherwise = do
            cleanup [a]
            delete a
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
          mapM (\old@(nm,_) -> setPropertyNull obj nm >> return old) olds

        go' (old@(oname,oval):olds) (new@(nname,nval):news) =
          let
            remove =
              setPropertyNull obj oname

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
runElementDiff :: (Code ms IO () -> IO ()) -> ENode -> [Feature (Code ms IO ())] -> [Feature (Code ms IO ())] -> [Feature (Code ms IO ())] -> IO [Feature (Code ms IO ())]
runElementDiff f el os0 ms0 ns0 =
#ifndef __GHCJS__
    return ns0
#else
    go os0 ms0 ns0
  where

    go [] [] news =
      mapM (setAttribute_ f el) news

    go olds _ [] =
      mapM (\old -> removeAttribute_ el old >> return NullFeature) olds

    go (old:olds) (mid:mids) (new:news) =
      let
        remove =
          removeAttribute_ el old

        set =
          setAttribute_ f el new

        goRest =
          go olds mids news

        continue up = do
          upds <- if reallyUnsafeEq mids news then return olds else goRest
          return (up:upds)

        update = do
          new' <- set
          continue new'

        replace = do
          remove
          update

      in
        if reallyUnsafeEq mid new then
          continue old
        else
          case (mid,new) of
            (_,NullFeature) -> do
              remove
              continue new

            (NullFeature,_) ->
              update

            (CurrentValue oldV,CurrentValue newV) ->
              if prettyUnsafeEq oldV newV then
                continue old
              else
                update

            (Style oldS,Style newS) -> do
              -- we know /something/ changed
              applyStyleDiffs el oldS newS
              continue new

            (Attribute nm val,Attribute nm' val') ->
              if prettyUnsafeEq nm nm' then
                update
              else
                replace

            (On e m _,On e' m' _) ->
              if prettyUnsafeEq e e' && reallyUnsafeEq m m' then
                continue old
              else
                replace

            (On' e os g _,On' e' os' g' _) ->
              if prettyUnsafeEq e e' && prettyUnsafeEq os os' && reallyUnsafeEq g g' then
                continue old
              else
                replace

            _ ->
              replace
#endif

{-# NOINLINE runElementDiffSVG #-}
runElementDiffSVG :: (Code ms IO () -> IO ())
                  -> ENode
                  -> [Feature (Code ms IO ())]
                  -> [Feature (Code ms IO ())]
                  -> [Feature (Code ms IO ())]
                  -> IO [Feature (Code ms IO ())]
runElementDiffSVG f el os0 ms0 ns0 =
#ifndef __GHCJS__
    return ns0
#else
    go os0 ms0 ns0
  where

    go [] [] news =
      mapM (setAttributeSVG_ f el) news

    go olds _ [] =
      mapM (\old -> removeAttributeSVG_ el old >> return NullFeature) olds

    go (old:olds) (mid:mids) (new:news) =
      let
        remove =
          removeAttributeSVG_ el old

        set =
          setAttributeSVG_ f el new

        goRest =
          go olds mids news

        continue up = do
          upds <- if reallyUnsafeEq mids news then return olds else goRest
          return (up:upds)

        update = do
          new' <- set
          continue new'

        replace = do
          remove
          update

      in
        if reallyUnsafeEq mid new then
          continue old
        else
          case (mid,new) of
            (_,NullFeature) -> do
              remove
              continue new

            (NullFeature,_) ->
              update

            (CurrentValue oldV,CurrentValue newV) ->
              if prettyUnsafeEq oldV newV then
                continue old
              else
                update

            (Style oldS,Style newS) -> do
              -- we know /something/ changed
              applyStyleDiffs el oldS newS
              continue new

            (Attribute nm val,Attribute nm' val') ->
              if prettyUnsafeEq nm nm' then
                update
              else
                replace

            (On e m _,On e' m' _) ->
              if prettyUnsafeEq e e' && reallyUnsafeEq m m' then
                continue old
              else
                replace

            (On' e os g _,On' e' os' g' _) ->
              if prettyUnsafeEq e e' && prettyUnsafeEq os os' && reallyUnsafeEq g g' then
                continue old
              else
                replace

            _ ->
              replace
#endif

{-# NOINLINE removeAttribute_ #-}
removeAttribute_ :: ENode -> Feature (Code ms IO ()) -> IO ()
removeAttribute_ element attr =
#ifndef __GHCJS__
  return ()
#else
  case attr of
    NullFeature ->
      return ()

    CurrentValue _ ->
      set_value_js element ""

    Attribute nm _ ->
      E.removeAttribute element nm

    Link _ unreg -> do
      forM_ unreg id
      E.removeAttribute element ("href" :: Txt)

    On _ _ unreg ->
      forM_ unreg id

    On' ev _ _ unreg ->
      forM_ unreg id

    Style styles -> do
      obj <- O.create
      forM_ styles $ \(nm,val) -> O.unsafeSetProp nm (M.pToJSVal val) obj
      clearStyle_js element obj
#endif

{-# NOINLINE removeAttributeSVG_ #-}
removeAttributeSVG_ :: ENode -> Feature (Code ms IO ()) -> IO ()
removeAttributeSVG_ element attr =
#ifndef __GHCJS__
  return ()
#else
  case attr of
    NullFeature ->
      return ()

    CurrentValue _ ->
      set_value_js element ""

    Attribute nm _ ->
      E.removeAttributeNS element (Just ("http://www.w3.org/2000/svg" :: Txt)) nm

    Link _ unreg -> do
      forM_ unreg id
      E.removeAttributeNS element (Just ("http://www.w3.org/2000/svg" :: Txt)) ("href" :: Txt)

    On _ _ unreg ->
      forM_ unreg id

    On' ev _ _ unreg ->
      forM_ unreg id

    Style styles -> do
      obj <- O.create
      forM_ styles $ \(nm,val) -> O.unsafeSetProp nm (M.pToJSVal val) obj
      clearStyle_js element obj
#endif

{-# NOINLINE setAttribute_ #-}
setAttribute_ :: (Code ms IO () -> IO ()) -> ENode -> Feature (Code ms IO ()) -> IO (Feature (Code ms IO ()))
setAttribute_ c element attr =
#ifndef __GHCJS__
  return attr
#else
  case attr of
    NullFeature ->
      return NullFeature

    CurrentValue v -> do
      set_value_js element v
      return attr

    -- optimize this; we're doing a little more work than necessary!
    Attribute nm eval -> do
      either (\b -> void $
                      if b then
                        E.setAttribute element nm (mempty :: Txt)
                      else
                        E.removeAttribute element nm
             )
             (void . E.setAttribute element nm)
             eval
      return attr

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
      return (Link href (Just stopListener))

    On ev e _ -> do
      stopListener <-
        Ev.on
          element
          (Ev.unsafeEventName ev :: Ev.EventName E.Element T.MouseEvent) -- faked
            $ do Ev.preventDefault
                 Ev.stopPropagation
                 liftIO $ void $ c e
      return (On ev e (Just stopListener))

    On' ev os f _ -> do
      stopListener <-
        Ev.on
          element
          (Ev.unsafeEventName ev :: Ev.EventName E.Element T.CustomEvent) -- for the type checking; actually just an object
            $ do ce <- Ev.event
                 when (_preventDefault os) Ev.preventDefault
                 when (_stopPropagation os) Ev.stopPropagation
                 liftIO $ f (unsafeCoerce ce) >>= mapM_ c
                 return ()
      return (On' ev os f (Just stopListener))

    Style styles -> do
      obj <- O.create
      forM_ styles $ \(nm,val) -> O.unsafeSetProp nm (M.pToJSVal val) obj
      setStyle_js element obj
      return attr
#endif

{-# NOINLINE setAttributeSVG_ #-}
setAttributeSVG_ :: (Code ms IO () -> IO ()) -> ENode -> Feature (Code ms IO ()) -> IO (Feature (Code ms IO ()))
setAttributeSVG_ c element attr =
#ifndef __GHCJS__
  return attr
#else
  case attr of
    NullFeature ->
      return NullFeature

    CurrentValue v -> do
      set_value_js element v
      return attr

    -- optimize this; we're doing a little more work than necessary!
    Attribute nm eval -> do
      either (\b -> void $
                      if b then
                        E.setAttributeNS element (Just ("http://www.w3.org/2000/svg" :: Txt)) nm (mempty :: Txt)
                      else
                        E.removeAttributeNS element (Just ("http://www.w3.org/2000/svg" :: Txt)) nm
             )
             (void . E.setAttributeNS element (Just ("http://www.w3.org/2000/svg" :: Txt)) nm)
             eval
      return attr

    Link href _ -> do
      E.setAttributeNS element (Just ("http://www.w3.org/2000/svg" :: Txt)) ("href" :: Txt) href
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
      return (Link href (Just stopListener))

    On ev e _ -> do
      stopListener <-
        Ev.on
          element
          (Ev.unsafeEventName ev :: Ev.EventName E.Element T.MouseEvent) -- faked
            $ do Ev.preventDefault
                 Ev.stopPropagation
                 liftIO $ void $ c e
      return (On ev e (Just stopListener))

    On' ev os f _ -> do
      stopListener <-
        Ev.on
          element
          (Ev.unsafeEventName ev :: Ev.EventName E.Element T.CustomEvent) -- for the type checking; actually just an object
            $ do ce <- Ev.event
                 when (_preventDefault os) Ev.preventDefault
                 when (_stopPropagation os) Ev.stopPropagation
                 v <- liftIO $ M.toJSVal ce
                 liftIO $ f (unsafeCoerce ce) >>= mapM_ c 
                 return ()
      return (On' ev os f (Just stopListener))

    Style styles -> do
      obj <- O.create
      forM_ styles $ \(nm,val) -> O.unsafeSetProp nm (M.pToJSVal val) obj
      setStyle_js element obj
      return attr
#endif

{-# NOINLINE cleanupAttr #-}
cleanupAttr :: Feature (Code ms IO ()) -> IO ()
cleanupAttr attr =
#ifndef __GHCJS__
  return ()
#else
  case attr of
    Link _ unreg -> forM_ unreg id
    On _ _ unreg -> forM_ unreg id
    On' _ _ _ unreg -> forM_ unreg id
    _ -> return ()
#endif

type Win =
#ifdef __GHCJS__
  W.Window
#else
  ()
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

type Doc =
#ifdef __GHCJS__
  D.Document
#else
  ()
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

type Loc =
#ifdef __GHCJS__
  L.Location
#else
  ()
#endif

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


instance Cond (Atom (Code ms IO ())) where
  nil = NullAtom Nothing

instance IsString (Atom (Code ms IO ())) where
  fromString = jss . fromString

instance FromTxt (Atom (Code ms IO ())) where
  fromTxt = jss

instance {-# OVERLAPS #-} IsString [Atom (Code ms IO ())] where
  fromString s = [fromString s]

instance FromTxt [Atom (Code ms IO ())] where
  fromTxt t = [fromTxt t]

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

#ifdef LENS
makePrisms ''Atom
makeLenses ''Atom
#endif

html :: Txt -> [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
html _tag _attributes _children =
  let _node = Nothing
  in Atom {..}

raw :: Txt -> [Feature (Code ms IO ())] -> Txt -> Atom (Code ms IO ())
raw _tag _attributes _content =
  let _node = Nothing
  in Raw {..}

notNil (NullAtom _) = False
notNil _ = True

cnode :: Bool -> Atom (Code ms IO ()) -> Atom (Code ms IO ())
cnode = cond

keyed :: Txt -> [Feature (Code ms IO ())] -> [(Int,Atom (Code ms IO ()))] -> Atom (Code ms IO ())
keyed _tag _attributes _keyed0 =
  let _node = Nothing
      _keyed = filter (notNil . snd) _keyed0
  in KAtom {..}

svgHTML :: Txt -> [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
svgHTML _tag _attributes _children =
  let _node = Nothing
  in SVGAtom {..}

jss :: Txt -> Atom (Code ms IO ())
jss _content =
  let _tnode = Nothing
  in Text {..}

ujss :: Txt -> Atom (Code ms IO ())
ujss = jss . unindent

construct :: forall ms ms' ts' m c e atom.
        IsConstruct' ts' ms' m
     => ([Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ()))
     -> ([Feature (Code ms IO ())] -> Construct' ts' ms' m -> Atom (Code ms IO ()))
construct f = \as c ->
  case f [] [] of
    Atom _ t _ _ -> Managed Nothing t as (Construct' c)
    _ -> error "Incorrect usage of construct; Constructs may only be embedded in Atoms."

-- tagged div [] [hashed 'a' (div [] []), hashed 4 (div [] [])]
tagged :: ([Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ()))
      -> ([Feature (Code ms IO ())] -> [(Int,Atom (Code ms IO ()))] -> Atom (Code ms IO ()))
tagged f = \as ks ->
  let Atom _ t _ _ = f [] []
  in keyed t as ks

hashed :: Hashable a => a -> Atom (Code ms IO ()) -> (Int,Atom (Code ms IO ()))
hashed k h = (hash k,h)

css :: CSS -> Atom (Code ms IO ())
css = css' False

css' :: Bool -> CSS -> Atom (Code ms IO ())
css' b = html "style" [ type_ "text/css", scoped b ] . ((jss "\n"):) . go False
  where
    go :: Bool -> CSS -> [Atom (Code ms IO ())]
    go b (Return _) = []
    go b (Lift s) = go b (runIdentity s)
    go b c@(Do msg) =
      case prj msg of
        Just (CSS3_ atRule sel mCSS k) ->
          case mCSS of
            Nothing ->
              jss (atRule <> sel <> ";\n")
              : go False k
            Just c' ->
              ( jss (atRule <> sel <> " {\n")
              : go True c'
              ) ++ ( jss "\n}\n\n"
                   : go False k
                   )
        Just (CSS_ sel ss r) ->
          ( jss ( (if b then "\t" else mempty)
                     <> sel
                     <> " {\n"
                     <> (Txt.intercalate (if b then ";\n\t" else ";\n") $ renderStyles b ss)
                     <> (if b then "\n\t}\n\n" else "\n}\n\n")
                )
          : go b r
          )
        _ -> []

scss :: StaticCSS -> Atom (Code ms IO ())
scss = scss' False

scss' :: Bool -> StaticCSS -> Atom (Code ms IO ())
scss' b = raw "style" [type_ "text/css", scoped b] . cssText

styles :: CSS -> Atom (Code ms IO ())
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

--------------------------------------------------------------------------------
-- Nodes

abbr :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
abbr = html "abbr"

address :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
address = html "address"

area :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
area = html "area"

a :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
a = html "a"

article :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
article = html "article"

aside :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
aside = html "aside"

audio :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
audio = html "audio"

base :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
base = html "base"

bdi :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
bdi = html "bdi"

bdo :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
bdo = html "bdo"

big :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
big = html "big"

blockquote :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
blockquote = html "blockquote"

body :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
body = html "body"

b :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
b = html "b"

br :: Atom (Code ms IO ())
br = html "br" [] []

button :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
button = html "button"

canvas :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
canvas = html "canvas"

caption :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
caption = html "caption"

cite :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
cite = html "cite"

code :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
code = html "code"

col :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
col = html "col"

colgroup :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
colgroup = html "colgroup"

dataN :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
dataN = html "data"

datalist :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
datalist = html "datalist"

dd :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
dd = html "dd"

description :: Txt -> Atom (Code ms IO ())
description d = meta [ name "description", content d ] []

dl :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
dl = html "dl"

dt :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
dt = html "dt"

del :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
del = html "del"

details :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
details = html "details"

dfn :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
dfn = html "dfn"

dialog :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
dialog = html "dialog"

div :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
div = html "div"

em :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
em = html "em"

embed :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
embed = html "embed"

fieldset :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
fieldset = html "fieldset"

figcaption :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
figcaption = html "figcaption"

figure :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
figure = html "figure"

footer :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
footer = html "footer"

form :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
form = html "form"

head :: [Atom (Code ms IO ())] -> Atom (Code ms IO ())
head = html "head" []

header :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
header = html "header"

h1 :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
h1 = html "h1"

h2 :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
h2 = html "h2"

h3 :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
h3 = html "h3"

h4 :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
h4 = html "h4"

h5 :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
h5 = html "h5"

h6 :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
h6 = html "h6"

hgroup :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
hgroup = html "hgroup"

hr :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
hr = html "hr"

html_ :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
html_ = html "html"

iframe :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
iframe = html "iframe"

img :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
img = html "img"

input :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
input = html "input"

textInput :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
textInput fs = html "input" (type_ "text":fs)

ins :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
ins = html "ins"

iN :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
iN = html "i"

kbd :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
kbd = html "kbd"

keygen :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
keygen = html "keygen"

label :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
label = html "label"

legend :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
legend = html "legend"

li :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
li = html "li"

linkN :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
linkN = html "link"

mainN :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
mainN = html "main"

mapN :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
mapN = html "map"

mark :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
mark = html "mark"

menu :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
menu = html "menu"

menuitem :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
menuitem = html "menuitem"

meta :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
meta = html "meta"

meter :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
meter = html "meter"

nav :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
nav = html "nav"

noscript :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
noscript = html "noscript"

object_ :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
object_ = html "object"

optgroup :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
optgroup = html "optgroup"

option :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
option = html "option"

ol :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
ol = html "ol"

output :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
output = html "output"

p :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
p = html "p"

param :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
param = html "param"

picture :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
picture = html "picture"

pre :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
pre = html "pre"

progress :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
progress = html "progress"

q :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
q = html "q"

rp :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
rp = html "rp"

rt :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
rt = html "rt"

ruby :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
ruby = html "ruby"

samp :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
samp = html "samp"

script :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
script = html "script"

s :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
s = html "s"

section :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
section = html "section"

selectN :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
selectN = html "select"

small :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
small = html "small"

source :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
source = html "source"

span :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
span = html "span"

strong :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
strong = html "strong"

style :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
style = html "style"

sub :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
sub = html "sub"

summary :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
summary = html "summary"

sup :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
sup = html "sup"

table :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
table = html "table"

tbody :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
tbody = html "tbody"

td :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
td = html "td"

textarea :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
textarea = html "textarea"

tfoot :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
tfoot = html "tfoot"

th :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
th = html "th"

thead :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
thead = html "thead"

time :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
time = html "time"

title :: Txt -> Atom (Code ms IO ())
title jst = html "title" [] [ jss jst ]

tr :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
tr = html "tr"

track :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
track = html "track"

u :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
u = html "u"

ul :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
ul = html "ul"

varN :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
varN = html "var"

video :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
video = html "video"

viewport :: Txt -> Atom (Code ms IO ())
viewport jst = html "meta" [ name "viewport", content jst ] []

wbr :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
wbr = html "wbr"

--------------------------------------------------------------------------------
-- SVG

circle :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
circle = svgHTML "circle"

clipPath :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
clipPath = svgHTML "clipPath"

defs :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
defs = svgHTML "defs"

ellipse :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
ellipse = svgHTML "ellipse"

g :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
g = svgHTML "g"

image :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
image = svgHTML "image"

line :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
line = svgHTML "line"

linearGradient :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
linearGradient = svgHTML "linearGradient"

mask :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
mask = svgHTML "mask"

path :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
path = svgHTML "path"

patternN :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
patternN = svgHTML "pattern"

polygon :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
polygon = svgHTML "polygon"

polyline :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
polyline = svgHTML "polyline"

radialGradient :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
radialGradient = svgHTML "radialGraedient"

rect :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
rect = svgHTML "rect"

stop_ :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
stop_ = svgHTML "stop"

svg :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
svg = svgHTML "svg"

text :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
text = svgHTML "text"

tspan :: [Feature (Code ms IO ())] -> [Atom (Code ms IO ())] -> Atom (Code ms IO ())
tspan = svgHTML "tspan"
