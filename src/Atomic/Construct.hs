{-# language UndecidableInstances #-}
{-# language DeriveDataTypeable #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language DeriveFunctor #-}
{-# language MagicHash #-}
{-# language CPP #-}
module Atomic.Construct (module Atomic.Construct) where

import Ef.Base hiding (Object,Client,After,Before,current,Lazy,Eager,construct,Index,observe,uncons,distribute,embed)
import qualified Ef.Base

import Data.Txt as Txt hiding (replace,map,head,filter)
import Data.JSON

import Atomic.Attribute
import Atomic.Cond
import Atomic.CSS
import Atomic.Key
import Atomic.Revent
import Atomic.Vault
import Atomic.With
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
import GHC.Prim

import qualified Data.HashMap.Strict as Map

import Prelude hiding (div,head,span)
import Language.Haskell.TH hiding (Loc)
import Language.Haskell.TH.Syntax hiding (Loc)

import System.IO.Unsafe
import Unsafe.Coerce

import Control.Lens (makePrisms,makeLenses)
import Control.Lens.Plated (Plated(..))
import Control.Lens.At
import Control.Lens.Prism

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
  "$1.insertBefore($3,$1.children[$2]);"
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

foreign import javascript unsafe
  "$1.innerHTML = '';" clear_node_js :: N.Node -> IO ()
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
    ::  { _tnode      :: !(Maybe TNode)
        , _content    :: !Txt
        } -> Atom e

  Raw
    :: { _node        :: !(Maybe ENode)
       , _tag         :: !Txt
       , _attributes  :: ![Feature e]
       , _content     :: !Txt
       } -> Atom e

  KAtom
    ::  { _node       :: !(Maybe ENode)
        , _tag        :: !Txt
        , _attributes :: ![Feature e]
        , _keyed      :: ![(Int,Atom e)]
        } -> Atom e

  Atom
    ::  { _node       :: !(Maybe ENode)
        , _tag        :: !Txt
        , _attributes :: ![Feature e]
        , _atoms   :: ![Atom e]
        } -> Atom e

  -- TODO: SVG keyed node
  SVGAtom
    ::  { _node       :: !(Maybe ENode)
        , _tag        :: !Txt
        , _attributes :: ![Feature e]
        , _atoms   :: ![Atom e]
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
        , _constr      :: !Constr
        } -> Atom e
  deriving (Functor)

instance Plated (Atom e) where
  plate f (KAtom n t as ks) = KAtom n t as <$> traverse (\(i,k) -> fmap (\k' -> (i,k')) (f k)) ks
  plate f (Atom n t as cs) = Atom n t as <$> traverse f cs
  plate f (SVGAtom n t as cs) = SVGAtom n t as <$> traverse f cs
  plate f (KSVGAtom n t as ks) = KSVGAtom n t as <$> traverse (\(i,k) -> fmap (\k' -> (i,k')) (f k)) ks
  plate _ a = pure a

type instance Index (Atom e) = Int
type instance IxValue (Atom e) = Atom e

instance Ixed (Atom e) where
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

instance ToJSON (Atom e) where
  toJSON a =
#ifdef __GHCJS__
    objectValue $
#endif
      go a
    where
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

-- _Right :: Prism (Either c a) (Either c b) a b
-- _Right = prism Right $ either (Left . Left) Right

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

_construct :: ([Feature e] -> [Atom e] -> Atom e) -> Prism (Atom e) (Atom e) ([Feature e],Constr) ([Feature e],Constr)
_construct x = prism build $ \a ->
  case construct x [] (undefined :: Construct '[] ()) of
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
      case construct x [] (undefined :: Construct '[] ()) of
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
list x _attributes _keyed0 =
  case x [] [] of
    Atom _ _tag _ _ ->
      let
        _node = Nothing
        _keyed = filter (notNullAtom . snd) _keyed0
        notNullAtom (NullAtom _) = False
        notNullAtom _ = True
      in
        KAtom {..}
    SVGAtom _ _tag _ _ ->
      let
        _node = Nothing
        _keyed = filter (notNullAtom . snd) _keyed0
        notNullAtom (NullAtom _) = False
        notNullAtom _ = True
      in
        KSVGAtom {..}

construct :: ([Feature e] -> [Atom e] -> Atom e)
          -> (forall ts' ms' m. IsConstruct' ts' ms' m => [Feature e] -> Construct' ts' ms' m -> Atom e)
construct f = \as c ->
  case f [] [] of
    Atom _ t _ _ -> Managed Nothing t as (Construct' c)
    _ -> error "Incorrect usage of construct; Constructs may only be embedded in plain html Atoms."

hashed :: Hashable a => ([Feature e] -> [Atom e] -> Atom e) -> [Feature e] -> [(a,Atom e)] -> Atom e
hashed x _attributes _keyed0 = list x _attributes (map (first hash) _keyed0)

css :: CSS -> Atom e
css = css' False

css' :: Bool -> CSS -> Atom e
css' b = mkAtom "style" [ typeA "text/css", cond b scopedA ] . ((text "\n"):) . go False
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
scss' b = raw (mkAtom "style") [typeA "text/css", cond b scopedA] . cssText

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


-- Useful for standalone components without a Atomic root.
renderConstruct' :: IsConstruct' ts ms m => Construct' ts ms m -> ENode -> Atom (Code ms IO ()) -> IO (Atom (Code ms IO ()))
renderConstruct' a parent html = do
  let f e = void $ with a e
  doc <- getDocument
  html' <- buildHTML doc f html
  embed_ parent html'
  return html'

-- rebuild finds managed nodes and re-embeds them in case they were
-- removed for other uses
rebuild :: Atom e -> IO ()
rebuild h =
#ifndef __GHCJS__
    return ()
#else
    go h
  where
    go Atom {..}    = mapM_ go _atoms
    go SVGAtom {..} = mapM_ go _atoms
    go KAtom {..}   = mapM_ (go . snd) _keyed
    go m@Managed {..} =
      case _constr of
        Construct' c -> do
          mi_ <- lookupConstruct (key c)
          forM_ mi_ $ \(_,x_) -> do
            (h,_,_) <- readIORef x_
            rebuild h
            forM_ _node $ \node ->
              embed_ node h
    go _ =
      return ()
#endif

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

data DiffStrategy = Unequal | Eager | Manual deriving (Eq)

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
type ConstructKey ms m = ConstructKey' (Appended ms (ConstructBase m)) m
type ConstructBuilder' ts m = Modules (ConstructBase m) (Action ts IO) -> IO (Modules ts (Action ts IO))
type ConstructBuilder ts m = ConstructBuilder' (Appended ts (ConstructBase m)) m
type ConstructPrimer' ms = Code ms IO ()
type ConstructPrimer ms m = ConstructPrimer' (Appended ms (ConstructBase m))
type Construct ms m = Construct' (Appended ms (ConstructBase m)) (Appended ms (ConstructBase m)) m

instance ToTxt (Feature e) where
  toTxt NullFeature          = mempty

  toTxt (Attribute attr val) =
    if Txt.null val then
      attr
    else
      attr <> "=\"" <> val <> "\""

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

  toTxt (SVGLink href _) = "xlink:href=\"" <> href <> "\""

  toTxt (XLink xl v)     = xl <> "=\"" <> v <> "\""

instance ToTxt [Feature e] where
  toTxt fs =
    Txt.intercalate
     (Txt.singleton ' ')
     (Prelude.filter (not . Txt.null) $ Prelude.map toTxt fs)

data Construct' ts ms m
  = Construct
      { key       :: !(ConstructKey' ms m)
      , build     :: !(ConstructBuilder' ts m)
      , prime     :: !(ConstructPrimer' ms)
      , model     :: !(m)
      , render    :: !(m -> Atom (Code ms IO ()))
      }

instance ToTxt (Construct' ts ms m) where
  toTxt = toTxt . key

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
      -- FIXME: likely a bug here with double initialization in multithreaded contexts!
      mi_ <- lookupConstruct (key c)
      case mi_ of
        Just (as,_) -> return (runAs as)
        Nothing -> do
          mkConstruct BuildOnly c
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

{-# NOINLINE constructShutdownNetwork #-}
constructShutdownNetwork :: Network ()
constructShutdownNetwork = unsafePerformIO network

{-# NOINLINE constructVault__ #-}
constructVault__ :: Vault
constructVault__ = Vault (unsafePerformIO (newMVar Map.empty))

lookupConstruct :: (MonadIO c) => Key phantom -> c (Maybe phantom)
lookupConstruct = vaultLookup constructVault__

getConstructName :: IsConstruct' ts ms m => Construct' ts ms m -> Txt
getConstructName = toTxt . key

addConstruct :: (MonadIO c) => Key phantom -> phantom -> c ()
addConstruct = vaultAdd constructVault__

deleteConstruct :: (MonadIO c) => Key phantom -> c ()
deleteConstruct = vaultDelete constructVault__

data MkConstructAction
  = ClearAndAppend ENode
  | forall e. Replace (Atom e)
  | Append ENode
  | BuildOnly

mkConstruct :: forall ms ts m.
          ( IsConstruct' ts ms m
          , ConstructBase m <: ms
          )
       => MkConstructAction
       -> Construct' ts ms m
       -> IO (IORef (Atom (Code ms IO ()),Atom (Code ms IO ()),m))
mkConstruct mkConstructAction c@Construct {..} = do
  let !m = model
      !raw = render m
  doc <- getDocument
  sig :: Signal ms IO (Code ms IO ()) <- runner
  sigBuf <- newSignalBuffer
  us :: Network m <- network
  views :: Network (m,Atom (Code ms IO ())) <- network
  let asComp = constructAs sigBuf sig
      sendEv :: Code ms IO () -> IO ()
      sendEv = void . runAs asComp
      initialize =
        case mkConstructAction of
          ClearAndAppend n -> do
            i <- buildAndEmbedMaybe sendEv doc Nothing raw
            clearNode (Just $ toNode n)
            forM_ (getNode i) (appendChild n)
            return i
          Replace a -> do
            i <- buildAndEmbedMaybe sendEv doc Nothing raw
            replace a i
            return i
          Append en -> do
            i <- buildAndEmbedMaybe sendEv doc (Just en) raw
            return i
          BuildOnly -> do
            i <- buildAndEmbedMaybe sendEv doc Nothing raw
            return i
  doc <- getDocument

  -- Initially, I thought I could put this in an animation frame, but that has odd effects
  i <- initialize

  cs_live_ :: IORef (Atom (Code ms IO ()),Atom (Code ms IO ()),m) <- newIORef (i,raw,m)

  -- keep out of forkIO to prevent double-initialization
  addConstruct key (asComp,cs_live_)

  forkIO $ do
    let cs = AState (unsafeCoerce cs_live_) m
    sdn :: Network () <- network
    let trig = syndicate views
    built <- build $ state (ConstructState
                                Nothing
                                (differ render trig sendEv)
                                Unequal
                                views
                                us
                                model
                                cs_live_
                            )
                    *:* state (Shutdown sdn)
                    *:* revent sigBuf
                    *:* Empty
    (obj',_) <- Ef.Base.Object built Ef.Base.! do
      connect constructShutdownNetwork $ const (Ef.Base.lift shutdownSelf)
      prime
#ifdef __GHCJS__
    driverPrintExceptions
      ("Construct' (" ++ show key ++ ") exception. If this is a DriverStopped exception, this Construct' may be blocked in its event loop, likely caused by cyclic 'with' calls. Exception: ")
#else
    driver
#endif
        sigBuf obj'
  return cs_live_

diff :: forall m ms. ('[State () (ConstructState m)] <: ms)
     => Proxy m -> Code ms IO ()
diff _ = do
  as@ConstructState {..} :: ConstructState m <- get
  unsafeCoerce (asDiffer as)

setUnequalDiff :: forall m ms. ('[State () (ConstructState m)] <: ms)
            => Proxy m -> Code ms IO ()
setUnequalDiff _ = do
  ConstructState {..} :: ConstructState m <- get
  put ConstructState { asDiffStrategy = Unequal, .. }

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
            -> Code ms' c (IO ())
onViewChange c f = do
  p <- periodical
  Just s <- subscribe p f
  buf <- getReventBuffer
  Just leaveNW <- demandMaybe =<< with c (do
    ConstructState {..} :: ConstructState m <- get
    let av = unsafeCoerce asViews :: Network (m,Atom (Code ms IO ()))
    joinNetwork av p buf
    return (leaveNetwork av p))
  return (stop s >> leaveNW)

onOwnViewChange :: forall ts ms m c.
                ( IsConstruct' ts ms m
                , MonadIO c
                )
            => Construct' ts ms m
            -> ((m,Atom (Code ms IO ())) -> Code '[Event (m,Atom (Code ms IO ()))] (Code ms c) ())
            -> Code ms c (IO ())
onOwnViewChange _ f = do
  p <- periodical
  Just s <- subscribe p f
  buf <- getReventBuffer
  ConstructState {..} :: ConstructState m <- get
  let av = unsafeCoerce asViews :: Network (m,Atom (Code ms IO ()))
  joinNetwork av p buf
  return (stop s >> leaveNetwork av p)

onModelChange :: forall ts ms ms' m c.
                ( IsConstruct' ts ms m
                , MonadIO c
                , '[Revent] <: ms'
                )
              => Construct' ts ms m
              -> (m -> Code '[Event m] (Code ms' c) ())
              -> Code ms' c (IO ())
onModelChange c f = do
  p <- periodical
  Just s <- subscribe p f
  buf <- getReventBuffer
  Just leaveNW <- demandMaybe =<< with c (do
    ConstructState {..} :: ConstructState m <- get
    joinNetwork asUpdates p buf
    return (leaveNetwork asUpdates p))
  return (stop s >> leaveNW)

{-# INLINE gets #-}
gets :: ('[State () (ConstructState m)] <: ms) => Code ms IO m
gets = do
  ConstructState {..} <- get
  return asModel

{-# INLINE puts #-}
puts :: forall ms m.
        ( '[State () (ConstructState m)] <: ms
        , Eq m
        )
     => m -> Code ms IO ()
puts !new = do
  (ConstructState {..},(old,cmp')) <- modify $ \(ConstructState {..} :: ConstructState m) ->
    let !old = asModel
        cmp' = ConstructState { asModel = new, .. }
    in (cmp',(old,cmp'))
  syndicate asUpdates new
  let d :: ConstructState m -> Code ms IO ()
      d = unsafeCoerce asDiffer
#ifdef __GHCJS__
  case reallyUnsafePtrEquality# old new of
    1# -> return ()
    _  ->
      case asDiffStrategy of
        Unequal   -> unless (old == new) (d cmp')
        Eager  -> d cmp'
        Manual -> return ()
#else
  d cmp'
#endif

{-# INLINE updates #-}
updates :: forall ms m.
           ( '[State () (ConstructState m)] <: ms
           , Eq m
           )
        => (m -> m)
        -> Code ms IO ()
updates f = do
  (ConstructState {..},(old,!new,cmp')) <- modify $ \ConstructState {..} ->
    let !old = asModel
        !new = f old
        cmp' = ConstructState { asModel = new, ..  }
    in (cmp',(old,new,cmp'))
  syndicate asUpdates new
  let d :: ConstructState m -> Code ms IO ()
      d = unsafeCoerce asDiffer
#ifdef __GHCJS__
  case reallyUnsafePtrEquality# old new of
    1# -> return ()
    _  ->
      case asDiffStrategy of
        Unequal   -> unless (old == new) (d cmp')
        Eager  -> d cmp'
        Manual -> return ()
#else
  d cmp'
#endif

differ :: (ConstructBase m <: ms) => Differ ms m
differ render trig sendEv ConstructState {..} = do
#ifdef __GHCJS__
  let setupDiff = do
        let !new_as = AState (unsafeCoerce asLive) asModel
        new_ap_AState <- liftIO $ newIORef (Just new_as,False)
        let !aPatch = APatch sendEv new_ap_AState render trig
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
#else
  let v = render asModel
  liftIO $ do
    writeIORef asLive $ unsafeCoerce (v,v,asModel)
    syndicate asViews $ unsafeCoerce (asModel,v)
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
embed_ parent Text {..} =
  forM_ _tnode $ \node -> do
    ae <- isAlreadyEmbeddedText node parent
    unless ae (void $ appendChild parent node)
embed_ parent n =
  forM_ (_node n) $ \node -> do
    ae <- isAlreadyEmbedded node parent
    unless ae (void $ appendChild parent node)

setAttributes :: [Feature e] -> (e -> IO ()) -> ENode -> IO [Feature e]
setAttributes as f el =
#ifdef __GHCJS__
  forM as (setAttribute_ f el)
#else
  return as
#endif

{-# NOINLINE buildAndEmbedMaybe #-}
buildAndEmbedMaybe :: (e -> IO ()) -> Doc -> Maybe ENode -> Atom e -> IO (Atom e)
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
      _atoms <- mapM (go (Just el)) _atoms
      return $ Atom _node _tag _attributes _atoms

    go mparent SVGAtom {..} = do
      _node@(Just el) <- createElementNS doc ("http://www.w3.org/2000/svg") _tag
      _attributes <- setAttributes _attributes f el
      forM_ mparent (flip appendChild el)
      _atoms <- mapM (go (Just el)) _atoms
      return $ SVGAtom _node _tag _attributes _atoms

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

    go mparent m@Managed {..} =
      case _constr of
        Construct' a -> do
          case _node of
            Nothing -> do
              _node@(Just el) <- createElement doc _tag
              _attributes <- setAttributes _attributes f el
              mi_ <- lookupConstruct (key a)
              case mi_ of
                Nothing -> do
                  -- never built before; make and embed
                  x_ <- mkConstruct BuildOnly a
                  (h,_,_) <- liftIO $ readIORef x_
                  forM_ mparent (`embed_` Managed {..})
                  embed_ el h
                  return Managed {..}
                Just (_,x_) -> do
                  (h,_,_) <- liftIO $ readIORef x_
                  rebuild Managed {..}
                  embed_ el h
                  forM_ mparent (`embed_` Managed {..})
                  return Managed {..}

            Just e -> do
              mi_ <- lookupConstruct (key a)
              case mi_ of
                Nothing -> do
                  -- shut down?
                  x_ <- mkConstruct BuildOnly a
                  (h,_,_) <- liftIO $ readIORef x_
                  forM_ mparent (`embed_` Managed {..})
                  embed_ e h
                  return Managed {..}
                Just (_,x_) -> do
                  (h,_,_) <- liftIO $ readIORef x_
                  rebuild m
                  embed_ e h
                  return m

{-# NOINLINE buildHTML #-}
buildHTML :: Doc -> (e -> IO ()) -> Atom e -> IO (Atom e)
buildHTML doc f = buildAndEmbedMaybe f doc Nothing

getElement Text {} = Nothing
getElement n = _node n

getNode Text {..} = fmap toNode _tnode
getNode n = fmap toNode $ _node n


diff_ :: ConstructPatch m -> IO ()
diff_ APatch {..} = do
#ifdef __GHCJS__
  -- made a choice here to do all the diffing in the animation frame; this way we
  -- can avoid recalculating changes multiple times during a frame. No matter how
  -- many changes occur in any component, the diff is only calculated once per frame.
  mv <- newEmptyMVar
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
        putMVar mv ()
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
      (live_html,!raw_html,!live_m) <- readIORef as_live
      let !new_html = ap_patchView as_model
      new_live_html <- either id id <$> diffHelper ap_send doc live_html raw_html new_html
      writeIORef as_live (new_live_html,new_html,as_model)
      ap_viewTrigger (as_model,new_html)
#endif

{-# NOINLINE replace #-}
replace :: Atom e -> Atom e' -> IO ()
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
delete Text {..} = forM_ _tnode (delete_js . toNode)-- this won't work, will it?
delete n = forM_ (_node n) (delete_js . toNode)
#endif

{-# NOINLINE cleanup #-}
cleanup :: [Atom e] -> IO ()
#ifndef __GHCJS__
cleanup _ = return ()
#else
cleanup (NullAtom{}:rest) = cleanup rest
cleanup (Raw {..}:rest) = do
  forM_ _attributes cleanupAttr
  cleanup rest
cleanup (Atom {..}:rest) = do
  forM_ _attributes cleanupAttr
  cleanup _atoms
  cleanup rest
cleanup (SVGAtom {..}:rest) = do
  forM_ _attributes cleanupAttr
  cleanup _atoms
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
insertAt :: ENode -> Int -> Atom e -> IO ()
#ifndef __GHCJS__
insertAt _ _ _ = return ()
#else
insertAt parent ind Text {..} = forM_ _tnode $ insert_at_js parent ind . toNode
insertAt parent ind n = forM_ (_node n) $ insert_at_js parent ind . toNode
#endif

{-# NOINLINE insertBefore_ #-}
insertBefore_ :: ENode -> Atom e -> Atom e -> IO ()
#ifndef __GHCJS__
insertBefore_ _ _ _ = return ()
#else
insertBefore_ parent child@(Text {}) new@(Text{}) = void $ N.insertBefore parent (_tnode new) (_tnode child)
insertBefore_ parent child new@(Text{}) = void $ N.insertBefore parent (_tnode new) (_node child)
insertBefore_ parent child@(Text {}) new = void $ N.insertBefore parent (_node new) (_tnode child)
insertBefore_ parent child new = void $ N.insertBefore parent (_node new) (_node child)
#endif

diffHelper :: (e -> IO ()) -> Doc -> Atom e -> Atom e -> Atom e -> IO (Either (Atom e) (Atom e))
diffHelper f doc =
#ifdef __GHCJS__
    go
#else
    \_ _ n -> return $ Left n
#endif
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
                  c' <- if reallyUnsafeEq (_atoms mid) (_atoms new) then do
                          return (_atoms old)
                        else
                          diffChildren n (_atoms old) (_atoms mid) (_atoms new)
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
                  -- liftIO $ putStrLn "SVG Atoms with same tag"
                  a' <- if reallyUnsafeEq (_attributes mid) (_attributes new) then do
                          -- liftIO $ putStrLn "SVG attribtues reallyUnsafeEq"
                          return (_attributes old)
                        else do
                          -- liftIO $ putStrLn "SVG attributes not reallyUnsafeEq"
                          -- liftIO $ print (map toTxt (_attributes mid))
                          -- liftIO $ print (map toTxt (_attributes new))
                          runElementDiff f n (_attributes old) (_attributes mid) (_attributes new)
                  c' <- if reallyUnsafeEq (_atoms mid) (_atoms new) then do
                          -- liftIO $ putStrLn "SVG children reallyUnsafeEq"
                          return (_atoms old)
                        else do
                          -- liftIO $ putStrLn "SVG children not reallyUnsafeEq"
                          diffChildren n (_atoms old) (_atoms mid) (_atoms new)
                  return $ Right $ SVGAtom (_node old) (_tag old) a' c'
          else do new' <- buildHTML doc f new
                  -- liftIO $ putStrLn "SVG atoms have different tags"
                  -- shouldn't ever hit a nothing, but I stil don't like it
                  replace old new'
                  cleanup [old]
                  delete old
                  return $ Left new'

        go' old@(KAtom old_node old_tag old_attributes old_keyed)
          mid@(KAtom midAnode _ midAattributes midAkeyed)
          new@(KAtom _ new_tag new_attributes new_keyed) =
          if prettyUnsafeEq old_tag new_tag
          then do let Just n = old_node
                  a' <- if reallyUnsafeEq midAattributes new_attributes then return old_attributes else
                          runElementDiff f n old_attributes midAattributes new_attributes
                  c' <- if reallyUnsafeEq midAkeyed new_keyed then return old_keyed else
                          diffKeyedChildren n old_keyed midAkeyed new_keyed
                  return $ Right $ KAtom old_node old_tag a' c'
          else do new' <- buildHTML doc f new
                  replace old new'
                  cleanup [old]
                  delete old
                  return $ Left new'

        go' txt@(Text (Just t) cnt) mid@(Text _ mcnt) new@(Text _ cnt') =
          if reallyUnsafeEq mcnt cnt' then do
            return $ Right txt
          else
            if prettyUnsafeEq cnt cnt' then do
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
              forM_ (_node n') (swapContent o)
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
          upds <- if reallyUnsafeEq mids news then do
                    -- liftIO $ putStrLn "mids === news"
                    return olds
                  else do
                    -- liftIO $ putStrLn "mids /== news"
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
          -- liftIO $ putStrLn "Really unsafe equality of element attributes"
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
                if prettyUnsafeEq val val' then do
                  -- liftIO $ putStrLn $ "nm == nm' and val == val':" ++ show (nm,val)
                  continue old
                else do
                  -- liftIO $ putStrLn $ show (nm,val,nm',val')
                  update
              else
                replace

            (On e m _,On e' m' _) ->
              if prettyUnsafeEq e e' && reallyUnsafeEq m m' then do
                -- liftIO $ putStrLn $ "On: Event types same, IO actons really unsafe eq: " ++ show (e,e')
                continue old
              else do
                -- liftIO $ putStrLn $ "On: Event type not eq or IO actions not eq: " ++ show (e,e')
                replace

            (On' e os g _,On' e' os' g' _) ->
              if prettyUnsafeEq e e' && prettyUnsafeEq os os' && reallyUnsafeEq g g' then do
                -- liftIO $ putStrLn $ "On': Event types same, IO actons really unsafe eq: " ++ show (e,e')
                continue old
              else do
                -- liftIO $ putStrLn $ "On': Event type not eq or IO actions not eq: " ++ show (e,e')
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

    SVGLink _ unreg -> do
      forM_ unreg id
      E.removeAttributeNS element (Just ("http://www.w3.org/1999/xlink" :: Txt)) ("xlink:href" :: Txt)

    XLink nm _ ->
      E.removeAttributeNS element (Just ("http://www.w3.org/1999/xlink" :: Txt)) nm
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
    Attribute nm val -> do
      -- liftIO $ putStrLn $ "Setting attribute: " ++ show (nm,eval)
      E.setAttribute element nm val
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
                 when (_preventDef os) Ev.preventDefault
                 when (_stopProp os) Ev.stopPropagation
                 liftIO $ f (unsafeCoerce ce) >>= mapM_ c
                 return ()
      return (On' ev os f (Just stopListener))

    Style styles -> do
      obj <- O.create
      forM_ styles $ \(nm,val) -> O.unsafeSetProp nm (M.pToJSVal val) obj
      setStyle_js element obj
      return attr

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
      return (SVGLink href (Just stopListener))

    XLink nm val -> do
      E.setAttributeNS element (Just ("http://www.w3.org/1999/xlink" :: Txt)) nm val
      return attr
#endif

{-# NOINLINE cleanupAttr #-}
cleanupAttr :: Feature e -> IO ()
cleanupAttr attr =
#ifndef __GHCJS__
  return ()
#else
  case attr of
    SVGLink _ unreg -> forM_ unreg id
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

makePrisms ''Atom
makeLenses ''Atom
