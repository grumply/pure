{-# language UndecidableInstances #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language DeriveFunctor #-}
{-# language MagicHash #-}
{-# language CPP #-}
module Nuclear.Atom where

import Ef.Base hiding (Client,After,Before,current,Lazy,Eager)
import qualified Ef.Get as Get
import qualified Ef.Set as Set

import Data.JSText
#ifdef __GHCJS__
import qualified Data.JSString as JSText
#else
import qualified Data.Text as JSText
#endif

import Nuclear.Attribute
import Nuclear.CSS
import Nuclear.Key
import Nuclear.Node
import Nuclear.Revent
import Nuclear.UnsafeEq
import Nuclear.Vault
import Nuclear.With
import Nuclear.ToText
import Nuclear.FromText

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
import Data.List as List hiding (delete)
import Data.Maybe
import Data.String
import Data.Traversable
import GHC.Prim

import qualified Data.HashMap.Strict as Map

import System.IO.Unsafe
import Unsafe.Coerce

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
  changeText_js :: T.Text -> JSText -> IO ()

foreign import javascript unsafe
  "for (var property in $2) { $1.style[property] = $2[property]; }"
  setStyle_js :: E.Element -> O.Object -> IO ()

foreign import javascript unsafe
  "$1[$2] = null;"
  setPropertyNull :: O.Object -> JSText -> IO ()

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
  set_value_js :: E.Element -> JSText -> IO ()

foreign import javascript unsafe
  "$1.appendChild($2);"
  append_child_js :: E.Element -> N.Node -> IO ()
#endif

type HTML ms = Node Atom' (Code ms IO ())

reflect :: forall ts ms m c.
           ( IsAtom ts ms m
           , MonadIO c
           )
        => Atom ts ms m
        -> c (Promise (HTML ms))
reflect c =
  with c $ do
    AtomState {..} :: AtomState m <- get
    (l,_,_) <- liftIO $ readIORef asLive
    return (unsafeCoerce l)

atom :: forall ms ts m c e atom.
        IsAtom ts ms m
     => ([Attribute ms] -> [HTML ms] -> HTML ms)
     -> ([Attribute ms] -> Atom ts ms m -> HTML ms)
atom f = \as c ->
  let Node _ t _ _ = f [] []
  in Managed Nothing t as (Atom' c)

-- -- flow div [] [Nothing,Just $ div [] []]
-- flow :: ([Attribute ms] -> [HTML ms] -> HTML ms)
--      -> ([Attribute ms] -> [Maybe (HTML ms)] -> HTML ms)
-- flow f = \as fs ->
--   let Node _ t _ _ = f [] []
--   in fnode t as fs

-- tagged div [] [(1,div [] [])]
tagged :: ([Attribute ms] -> [HTML ms] -> HTML ms)
      -> ([Attribute ms] -> [(Int,HTML ms)] -> HTML ms)
tagged f = \as ks ->
  let Node _ t _ _ = f [] []
  in keyed t as ks

hashed :: Hashable a => a -> HTML ms -> (Int,HTML ms)
hashed k h = (hash k,h)

ujss :: JSText -> HTML ms
ujss = jss . unindent


css :: CSS -> HTML ms
css = css' False

css' :: Bool -> CSS -> HTML ms
css' b = html "style" [ type_ "text/css", scoped b ] . ((jss "\n"):) . go False
  where
    go :: Bool -> CSS -> [HTML ms]
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
                     <> (JSText.intercalate (if b then ";\n\t" else ";\n") $ renderStyles b ss)
                     <> (if b then "\n\t}\n\n" else "\n}\n\n")
                )
          : go b r
          )
        _ -> []

scss :: CSSText -> HTML ms
scss = scss' False

scss' :: Bool -> CSSText -> HTML ms
scss' b = raw "style" [type_ "text/css", scoped b] . cssText

styles :: CSS -> HTML ms
styles = css' True . classify
  where
    classify (Return r) = Return r
    classify (Lift sup) = Lift (fmap classify sup)
    classify (Do e) =
      case prj e of
        Just (CSS_ sel ss k) ->
          Do (inj (CSS_ (JSText.cons '.' sel) ss (classify k)))
        Just (CSS3_ at sel mcss k) ->
          Do (inj (CSS3_ at sel (fmap classify mcss) (classify k))) 
        _ -> error "impossible"

data DiffStrategy = Lazy | Eager | Manual deriving (Eq)

type Differ ms m =
       (m -> HTML ms)
    -> ((m,HTML ms) -> IO ())
    -> (Code ms IO () -> IO ())
    -> AtomState m
    -> Code ms IO ()

data AState m = AState
  { as_live :: forall ms. IORef (HTML ms, HTML ms, m)
  , as_model :: m
  }

data AtomPatch m where
  APatch ::
      -- only modify ap_AState with atomicModifyIORef
    { ap_send         :: Code ms IO () -> IO ()
    , ap_AState       :: IORef (Maybe (AState m),Bool) -- an AState record for manipulation; nullable by component to stop a patch.
    , ap_patchView    :: (m -> HTML ms)
    , ap_viewTrigger  :: (m,HTML ms) -> IO ()
    } -> AtomPatch m

type IsAtom ts ms m =
  ( A m <: ms
  , A m <. ts
  , Delta (Modules ts) (Messages ms)
  , Eq m
  )

data AtomState m where
  AtomState ::
    { asPatch        :: Maybe (AtomPatch m)
    , asDiffer       :: AtomState m -> Code ms IO ()
    , asDiffStrategy :: DiffStrategy
    , asViews        :: Network (m,HTML ms)
    , asUpdates      :: Network m
    , asModel        :: m
    , asLive         :: IORef (HTML ms, HTML ms, m)
    } -> AtomState m

type A m
  = '[ State () (AtomState m)
     , State () Shutdown
     , Revent
     ]

data Atom' where
  Atom' :: IsAtom ts ms m => Atom ts ms m -> Atom'
instance Eq Atom' where
 (==) (Atom' c) (Atom' c') =
  let Key k1 :: Key GHC.Prim.Any = unsafeCoerce (key c)
      Key k2 :: Key GHC.Prim.Any = unsafeCoerce (key c')
  in prettyUnsafeEq k1 k2

data Atom ts ms m
  = Atom
      { key       :: !(Key (Code ms IO `As` IO, IORef (HTML ms,HTML ms,m)))
      , build     :: !(   Modules (A m) (Action ts IO)
                       -> IO (Modules ts (Action ts IO))
                      ) -- a builder from Atom_ (..) to the desired cModules
      , prime     :: !(Code ms IO ())
      , model     :: !(m) -- an initial m; used to seed the view
      , view      :: !(m -> HTML ms)
      }

instance Eq (Atom ts ms m) where
  (==) (Atom k _ _ _ _) (Atom k' _ _ _ _) =
    let Key k1 = k
        Key k2 = k'
    in prettyUnsafeEq k1 k2 

instance Ord (Atom ts ms m) where
  compare (Atom (Key k) _ _ _ _) (Atom (Key k') _ _ _ _) = compare k k'

instance IsAtom ts ms m
  => With (Atom ts ms m)
          (Code ms IO)
          IO
  where
    using_ c = do
      -- keep an eye on this; there was a bug in fission that caused multiple-instantiation
      -- (since the lookup+mk is non-atomic) but I'm not sure it will arise here.
      mi_ <- lookupAtom (key c)
      case mi_ of
        Just (as,_) -> return (runAs as)
        Nothing -> do
          mkAtom (Just differ) Nothing c
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
      miohhm <- lookupAtom (key c)
      case miohhm of
        Just (_,iohhm) -> do
          (h,_,_) <- liftIO $ readIORef iohhm
          cleanup [h]
          delete h
        _ -> return ()
      deleteAtom (key c)

atomVault__ :: Vault
atomVault__ = Vault (unsafePerformIO (newMVar Map.empty))

lookupAtom :: (MonadIO c) => Key phantom -> c (Maybe phantom)
lookupAtom = vaultLookup atomVault__

getAtomName :: IsAtom t ms m => Atom ts ms m -> JSText
getAtomName = toText . key

addAtom :: (MonadIO c) => Key phantom -> phantom -> c ()
addAtom = vaultAdd atomVault__

deleteAtom :: (MonadIO c) => Key phantom -> c ()
deleteAtom = vaultDelete atomVault__

mkAtom :: forall ms ts m.
          ( IsAtom ts ms m
          , A m <: ms
          )
       => Maybe (Differ ms m)
       -> Maybe ENode
       -> Atom ts ms m
       -> IO (IORef (HTML ms,HTML ms,m))
mkAtom mDiffer mparent c@Atom {..} = do
  let !m = model
  sig :: Signal ms IO (Code ms IO ()) <- runner
  sigBuf <- newSignalBuffer
  updates :: Network m <- network
  views :: Network (m,HTML ms) <- network
  let asComp = constructAs sigBuf sig
      sendEv :: Code ms IO () -> IO ()
      sendEv = void . runAs asComp
  let !raw = view m
  doc <- getDocument
  i <- buildAndEmbedMaybe sendEv doc mparent raw
  cs_live_ :: IORef (HTML ms,HTML ms,m) <- newIORef (i,raw,m)
  let cs = AState (unsafeCoerce cs_live_) m
  sdn :: Network () <- network
  addAtom key (asComp,cs_live_)
  let trig = syndicate views
  built <- build $ state (AtomState
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
  forkIO $ driverPrintExceptions ("Atom (" ++ show key ++ ") exception. If this is a DriverStopped exception, this Atom may be blocked in its event loop, likely caused by cyclic 'with' calls. Exception") sigBuf obj'
  return cs_live_

diff :: forall m ms. ('[State () (AtomState m)] <: ms)
     => Proxy m -> Code ms IO ()
diff _ = do
  as@AtomState {..} :: AtomState m <- get
  unsafeCoerce (asDiffer as)

setLazyDiff :: forall m ms. ('[State () (AtomState m)] <: ms)
            => Proxy m -> Code ms IO ()
setLazyDiff _ = do
  AtomState {..} :: AtomState m <- get
  put AtomState { asDiffStrategy = Lazy, .. }

setEagerDiff :: forall m ms. ('[State () (AtomState m)] <: ms)
             => Proxy m -> Code ms IO ()
setEagerDiff _ = do
  AtomState {..} :: AtomState m <- get
  put AtomState { asDiffStrategy = Eager, .. }

setManualDiff :: forall m ms. ('[State () (AtomState m)] <: ms)
              => Proxy m -> Code ms IO ()
setManualDiff _ = do
  AtomState {..} :: AtomState m <- get
  put AtomState { asDiffStrategy = Manual, .. }

currentView :: forall ts ms c m.
               ( IsAtom ts ms m
               , MonadIO c
               )
            => Atom ts ms m
            -> c (Promise (HTML ms))
currentView c = with c $ ownView c

ownView :: forall ts ms c m.
           ( IsAtom ts ms m
           , MonadIO c
           )
        => Atom ts ms m
        -> Code ms c (HTML ms)
ownView _ = do
  AtomState {..} :: AtomState m <- get
  (h,_,_) <- liftIO $ readIORef asLive
  return (unsafeCoerce h)

onViewChange :: forall ts ms ms' m c.
                ( IsAtom ts ms m
                , MonadIO c
                , '[Revent] <: ms'
                )
            => Atom ts ms m
            -> ((m,HTML ms) -> Code '[Event (m,HTML ms)] (Code ms' c) ())
            -> Code ms' c (Subscription ms' c (m,HTML ms),Periodical ms' c (m,HTML ms))
onViewChange c f = do
  p <- periodical
  Just s <- subscribe p f
  buf <- getReventBuffer
  with c $ do
    AtomState {..} :: AtomState m <- get
    joinNetwork (unsafeCoerce asViews :: Network (m,HTML ms)) p buf
  return (s,p)

onModelChange :: forall ts ms ms' m c.
                ( IsAtom ts ms m
                , MonadIO c
                , '[Revent] <: ms'
                )
              => Atom ts ms m
              -> (m -> Code '[Event m] (Code ms' c) ())
              -> Code ms' c (Subscription ms' c m,Periodical ms' c m)
onModelChange c f = do
  p <- periodical
  Just s <- subscribe p f
  buf <- getReventBuffer
  with c $ do
    AtomState {..} :: AtomState m <- get
    joinNetwork asUpdates p buf
  return (s,p)

{-# INLINE observe #-}
observe :: ('[State () (AtomState m)] <: ms) => Code ms IO m
observe = do
  AtomState {..} <- get
  return asModel

{-# INLINE set #-}
set :: forall ms m.
        ( '[State () (AtomState m)] <: ms
        , Eq m
        )
     => m -> Code ms IO ()
set !new = do
  (AtomState {..},(old,cmp')) <- modify $ \(AtomState {..} :: AtomState m) ->
    let !old = asModel
        cmp' = AtomState { asModel = new, .. }
    in (cmp',(old,cmp'))
  syndicate asUpdates new
  let d :: AtomState m -> Code ms IO ()
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
           ( '[State () (AtomState m)] <: ms
           , Eq m
           )
        => (m -> m)
        -> Code ms IO ()
update f = do
  (AtomState {..},(old,new,cmp')) <- modify $ \AtomState {..} ->
    let !old = asModel
        !new = f old
        cmp' = AtomState { asModel = new, ..  }
    in (cmp',(old,new,cmp'))
  syndicate asUpdates new
  let d :: AtomState m -> Code ms IO ()
      d = unsafeCoerce asDiffer
  case reallyUnsafePtrEquality# old new of
    1# -> return ()
    _  ->
      case asDiffStrategy of
        Lazy   -> unless (old == new) (d cmp')
        Eager  -> d cmp'
        Manual -> return ()

differ :: (A m <: ms) => Differ ms m
differ view trig sendEv AtomState {..} = do
  let setupDiff = do
        let !new_as = AState (unsafeCoerce asLive) asModel
        new_ap_AState <- liftIO $ newIORef (Just new_as,False)
        let !aPatch = APatch sendEv new_ap_AState view trig
        put AtomState { asPatch = Just aPatch, .. }
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

createElement :: Doc -> JSText -> IO (Maybe ENode)
createElement doc tag =
#ifdef __GHCJS__
  D.createElement doc (Just tag)
#else
  return (Just ())
#endif

createTextNode :: Doc -> JSText -> IO (Maybe TNode)
createTextNode doc c =
#ifdef __GHCJS__
  D.createTextNode doc c
#else
  return (Just ())
#endif

createElementNS :: Doc -> JSText -> JSText -> IO (Maybe ENode)
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

setInnerHTML :: ENode -> JSText -> IO ()
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

changeText :: TNode -> JSText -> IO ()
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
embed_ :: ENode -> Node atom' e -> IO ()
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
rebuild :: IsAtom ts ms m => Atom ts ms m -> Maybe ENode -> HTML ms -> IO ()
rebuild c me h =
#ifndef __GHCJS__
    return ()
#else
    go me h
  where
    go mparent NullNode {..} =
      forM_ mparent $ \parent ->
        embed_ parent NullNode {..}

    go mparent Raw {..} =
      forM_ mparent $ \parent ->
        embed_ parent Raw {..}

    go mparent Node {..} = do
      forM_ mparent $ \parent ->
        embed_ parent Node {..}
      forM_ _children (go _node)

    go mparent SVGNode {..} = do
      forM_ mparent $ \parent ->
        embed_ parent SVGNode {..}
      forM_ _children (go _node)

    go mparent KNode {..} = do
      forM_ mparent $ \parent ->
        embed_ parent KNode {..}
      forM_ _keyed $ go _node . snd

    go mparent Text {..} =
      forM_ mparent $ \parent ->
        embed_ parent Text {..}

    go mparent m@Managed {..} =
      case _atom' of
        Atom' c -> do
          mi_ <- lookupAtom (key c)
          forM_ mi_ $ \(_,x_) -> do
            (h,_,_) <- readIORef x_
            rebuild c _node h
            forM_ mparent $ \parent ->
              embed_ parent h
#endif

setAttributes :: [Feature e] -> (e -> IO ()) -> ENode -> IO [Feature e]
setAttributes as f el =
#ifdef __GHCJS__
  forM as (setAttribute_ f el)
#else
  return as
#endif


{-# NOINLINE buildAndEmbedMaybe #-}
buildAndEmbedMaybe :: (e -> IO ()) -> Doc -> Maybe ENode -> Node Atom' e -> IO (Node Atom' e)
buildAndEmbedMaybe f doc = go
  where
    go mparent nn@NullNode {..} = do
      _cond@(Just el) <- createElement doc "span" -- what's a better choice here? I need something that is unrenderable....
      forM_ mparent (flip appendChild el)
      return $ NullNode _cond

    go mparent Raw {..} = do
      _node@(Just el) <- createElement doc _tag
      _attributes <- setAttributes _attributes f el
      setInnerHTML el _content
      forM_ mparent (flip appendChild el)
      return $ Raw _node _tag _attributes _content

    go mparent Node {..} = do
      _node@(Just el) <- createElement doc _tag
      _attributes <- setAttributes _attributes f el
      forM_ mparent (flip appendChild el)
      _children <- mapM (go (Just el)) _children
      return $ Node _node _tag _attributes _children

    go mparent SVGNode {..} = do
      _node@(Just el) <- createElementNS doc ("http://www.w3.org/2000/svg") _tag
      _attributes <- setAttributes _attributes f el
      forM_ mparent (flip appendChild el)
      _children <- mapM (go (Just el)) _children
      return $ SVGNode _node _tag _attributes _children

    go mparent KNode {..} = do
      _node@(Just el) <- createElement doc _tag
      _attributes <- setAttributes _attributes f el
      forM_ mparent (flip appendChild el)
      _keyed <- mapM (\(k,x) -> go (Just el) x >>= \y -> return (k,y)) _keyed
      return $ KNode _node _tag _attributes _keyed

    go mparent Text {..} = do
      _tnode@(Just el) <- createTextNode doc _content
      forM_ mparent (flip appendChild el)
      return $ Text _tnode _content

    go mparent Managed {..} =
      case _atom' of
        Atom' a ->
          case _node of
            Nothing -> do
              _node@(Just el) <- createElement doc _tag
              _attributes <- setAttributes _attributes f el
              mi_ <- lookupAtom (key a)
              case mi_ of
                Nothing -> do
                  -- never built before; make and embed
                  mkAtom (Just differ) _node a
                  forM_ mparent (`embed_` Managed {..})
                  return Managed {..}
                Just (_,x_) -> do
                  -- built before but not in this context; rebuild and embed
                  (h,_,_) <- readIORef x_
                  rebuild a mparent h
--                  forM_ mparent (`embed` Managed {..})
                  return Managed {..}

            Just e -> do
              mi_ <- lookupAtom (key a)
              forM_ mi_ $ \(_,x_) -> do
                -- build before in this context; rebuild and embed
                (h,_,_) <- readIORef x_
                rebuild a mparent (unsafeCoerce h) -- should be safe
                -- forM_ mparent (`embed` Managed {..})
              return Managed {..}

{-# NOINLINE buildHTML #-}
buildHTML :: Doc -> (e -> IO ()) -> Node Atom' e -> IO (Node Atom' e)
buildHTML doc f = buildAndEmbedMaybe f doc Nothing

-- Useful for standalone components without a Fusion root.
renderAtom :: IsAtom ts ms m => Atom ts ms m -> ENode -> HTML ms -> IO (HTML ms)
renderAtom a parent html = do
  let f e = void $ with a e
  doc <- getDocument
  html' <- buildHTML doc f html
  embed_ parent html'
  return html'

getElement Text {} = Nothing
getElement n = _node n

diff_ :: AtomPatch m -> IO Int
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
replace :: Node atom0 ms0 -> Node atom1 ms1 -> IO ()
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
delete :: Node atom' e -> IO ()
#ifndef __GHCJS__
delete _ = return ()
#else
delete Text {..} = forM_ _tnode (delete_js . toNode)-- this won't work, will it?
delete n = forM_ (_node n) (delete_js . toNode)
#endif

{-# NOINLINE remove #-}
remove :: Node atom' e -> IO ()
#ifndef __GHCJS__
remove _ = return ()
#else
remove (Text {..}) = forM_ _tnode (remove_js . toNode)
remove n = forM_ (_node n) (remove_js . toNode)
#endif

{-# NOINLINE cleanup #-}
cleanup :: [Node atom' e] -> IO ()
#ifndef __GHCJS__
cleanup _ = return ()
#else
cleanup (NullNode{}:rest) = cleanup rest
cleanup (Raw {..}:rest) = do
  forM_ _attributes cleanupAttr
  cleanup rest
cleanup (Node {..}:rest) = do
  forM_ _attributes cleanupAttr
  cleanup _children
  cleanup rest
cleanup (SVGNode {..}:rest) = do
  forM_ _attributes cleanupAttr
  cleanup _children
  cleanup rest
cleanup (KNode {..}:rest) = do
  forM_ _attributes cleanupAttr
  cleanup (map snd _keyed)
  cleanup rest
cleanup (Managed {..}:rest) = do
  forM_ _attributes cleanupAttr
  cleanup rest
cleanup _ = return ()
#endif

{-# NOINLINE insertAt #-}
insertAt :: ENode -> Int -> Node atom' e -> IO ()
#ifndef __GHCJS__
insertAt _ _ _ = return ()
#else
insertAt parent ind Text {..} = forM_ _tnode $ insert_at_js parent ind . toNode
insertAt parent ind n = forM_ (_node n) $ insert_at_js parent ind . toNode
#endif

{-# NOINLINE prepend #-}
prepend :: ENode -> Node atom' e -> IO ()
#ifndef __GHCJS__
prepend _ _ = return ()
#else
prepend parent Text {..} = forM_ _tnode $ prepend_child_js parent . toNode
prepend parent n = forM_ (_node n) $ prepend_child_js parent . toNode
#endif

{-# NOINLINE insertBefore_ #-}
insertBefore_ :: ENode -> Node atom' e -> Node atom' e -> IO ()
#ifndef __GHCJS__
insertBefore_ _ _ _ = return ()
#else
insertBefore_ parent child@(Text {}) new@(Text{}) = void $ N.insertBefore parent (_tnode new) (_tnode child)
insertBefore_ parent child new@(Text{}) = void $ N.insertBefore parent (_tnode new) (_node child)
insertBefore_ parent child@(Text {}) new = void $ N.insertBefore parent (_node new) (_tnode child)
insertBefore_ parent child new = void $ N.insertBefore parent (_node new) (_node child)
#endif

diffHelper :: (e -> IO ()) -> Doc -> Node Atom' e -> Node Atom' e -> Node Atom' e -> IO (Either (Node Atom' e) (Node Atom' e))
diffHelper f doc = go
  where
    go old mid new =
      if reallyUnsafeEq mid new then
        return $ Right old
      else
        go' old mid new

      where
        go' old@NullNode{} _ new = do
          case new of
            NullNode _ -> return $ Right old
            _          -> do
              new' <- buildHTML doc f new
              replace old new'
              cleanup [old]
              delete old
              return $ Right new'

        go' old _ new@NullNode{} = do
          new' <- buildHTML doc f new
          replace old new'
          cleanup [old]
          delete old
          return $ Right new'

        go' old@Node {} mid@Node {} new@Node {} =
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
                  return $ Right $ Node (_node old) (_tag old) a' c'
          else do new' <- buildHTML doc f new
                  -- shouldn't ever hit a nothing, but I stil don't like it
                  replace old new'
                  cleanup [old]
                  delete old
                  return $ Left new'

        go' old@SVGNode {} mid@SVGNode {} new@SVGNode {} =
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
                  return $ Right $ Node (_node old) (_tag old) a' c'
          else do new' <- buildHTML doc f new
                  -- shouldn't ever hit a nothing, but I stil don't like it
                  replace old new'
                  cleanup [old]
                  delete old
                  return $ Left new'

        go' old@(KNode old_node old_tag old_attributes old_keyed)
          mid@(KNode mid_node _ mid_attributes mid_keyed)
          new@(KNode _ new_tag new_attributes new_keyed) =
          if prettyUnsafeEq old_tag new_tag
          then do let Just n = old_node
                  a' <- if reallyUnsafeEq mid_attributes new_attributes then return old_attributes else
                          runElementDiff f n old_attributes mid_attributes new_attributes
                  c' <- if reallyUnsafeEq mid_keyed new_keyed then return old_keyed else
                          diffKeyedChildren n old_keyed mid_keyed new_keyed
                  return $ Right $ KNode old_node old_tag a' c'
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
          if    (_atom' old) == (_atom' new)
            && prettyUnsafeEq (_tag old) (_tag new)
          then do
            let Just n = _node old
            a' <- if reallyUnsafeEq (_attributes mid) (_attributes new) then return (_attributes old) else
                    runElementDiff f n (_attributes old) (_attributes mid) (_attributes new)
            return $ Right $ Managed (_node old) (_tag old) a' (_atom' old)
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
                    (NullNode {},NullNode {}) ->
                      continue old

                    (_,NullNode {}) -> do
                      new' <- buildHTML doc f new
                      replace old new'
                      cleanup [old]
                      delete old
                      continue new'

                    (NullNode{},_) -> do
                      new' <- buildHTML doc f new
                      replace old new'
                      cleanup [old]
                      delete old
                      continue new'

                    (m,n) -> do
                      enew <- go old mid new
                      continue (either id id enew)

    -- note that keyed nodes are filtered for NullNodes during construction
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
applyStyleDiffs :: ENode -> [(JSText,JSText)] -> [(JSText,JSText)] -> IO [(JSText,JSText)]
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
runElementDiff :: (e -> IO ()) -> ENode -> [Feature e] -> [Feature e] -> [Feature e] -> IO [Feature e]
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
runElementDiffSVG :: (e -> IO ())
                  -> ENode
                  -> [Feature e]
                  -> [Feature e]
                  -> [Feature e]
                  -> IO [Feature e]
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
removeAttribute_ :: ENode -> Feature e -> IO ()
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
      E.removeAttribute element ("href" :: JSText)

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
removeAttributeSVG_ :: ENode -> Feature e -> IO ()
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
      E.removeAttributeNS element (Just ("http://www.w3.org/2000/svg" :: JSText)) nm

    Link _ unreg -> do
      forM_ unreg id
      E.removeAttributeNS element (Just ("http://www.w3.org/2000/svg" :: JSText)) ("href" :: JSText)

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
setAttribute_ :: (e -> IO ()) -> ENode -> Feature e -> IO (Feature e)
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
                        E.setAttribute element nm (mempty :: JSText)
                      else
                        E.removeAttribute element nm
             )
             (void . E.setAttribute element nm)
             eval
      return attr

    Link href _ -> do
      E.setAttribute element ("href" :: JSText) href
      stopListener <-
        Ev.on
          element
          (Ev.unsafeEventName "click" :: Ev.EventName E.Element T.MouseEvent)
            $ do Ev.preventDefault
                 liftIO $ do
                   win <- getWindow
                   Just hist <- W.getHistory win
                   H.pushState hist (M.pToJSVal (0 :: Int)) ("" :: JSText) href
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
setAttributeSVG_ :: (e -> IO ()) -> ENode -> Feature e -> IO (Feature e)
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
                        E.setAttributeNS element (Just ("http://www.w3.org/2000/svg" :: JSText)) nm (mempty :: JSText)
                      else
                        E.removeAttributeNS element (Just ("http://www.w3.org/2000/svg" :: JSText)) nm
             )
             (void . E.setAttributeNS element (Just ("http://www.w3.org/2000/svg" :: JSText)) nm)
             eval
      return attr

    Link href _ -> do
      E.setAttributeNS element (Just ("http://www.w3.org/2000/svg" :: JSText)) ("href" :: JSText) href
      stopListener <-
        Ev.on
          element
          (Ev.unsafeEventName "click" :: Ev.EventName E.Element T.MouseEvent)
            $ do Ev.preventDefault
                 liftIO $ do
                   win <- getWindow
                   Just hist <- W.getHistory win
                   H.pushState hist (M.pToJSVal (0 :: Int)) ("" :: JSText) href
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
cleanupAttr :: Feature e -> IO ()
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

getFirstElementByTagName :: MonadIO c => JSText -> c ENode
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

redirect :: MonadIO c => JSText -> c ()
redirect redir = do
  loc <- getLocation
#ifdef __GHCJS__
  L.assign loc redir
#else
  return loc
#endif

