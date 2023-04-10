module Pure.Conjurer (module Pure.Conjurer, module Export) where

import Pure.Conjurer.API as Export
import Pure.Conjurer.Callbacks as Export
import Pure.Conjurer.Context as Export
import Pure.Conjurer.Creatable as Export
import Pure.Conjurer.Fieldable as Export
import Pure.Conjurer.Formable as Export
import Pure.Conjurer.Index as Export
import Pure.Conjurer.Interactions as Export
import Pure.Conjurer.Key as Export
import Pure.Conjurer.Listable as Export
import Pure.Conjurer.Name as Export
import Pure.Conjurer.Pathable as Export
import Pure.Conjurer.Permissions as Export
import Pure.Conjurer.Previewable as Export
import Pure.Conjurer.Previews as Export
import Pure.Conjurer.Producible as Export
import Pure.Conjurer.Readable as Export
import Pure.Conjurer.Rep as Export
import Pure.Conjurer.Resource as Export
import Pure.Conjurer.Rootable as Export
import Pure.Conjurer.Routable as Export
import Pure.Conjurer.Slug as Export
import Pure.Conjurer.Updatable as Export

import Pure.Auth (Authentication)
import Data.JSON (ToJSON(..),FromJSON(..),encodeBS,decodeBS)
import Data.Txt as Txt
import Data.Theme
import Data.Events
import Data.Default
import Data.HTML
import Data.Router as Router (Routing,goto,catchError,getRoutingState,putRoutingState,runRouting)
import Data.Scroll
import Data.Sorcerer as Sorcerer hiding (Read,pattern Update)
import qualified Data.Sorcerer as Sorcerer
import Data.View
import Data.View.Render ()
import qualified Data.Websocket as WS
import Data.Websocket ((<:>))
import Effect.Websocket

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict as Map
import Data.Set as Set
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad
import Data.Foldable
import Data.Hashable as Export
import Data.IORef
import Data.Maybe
import Data.Typeable
import GHC.Exts
import GHC.Generics
import System.IO.Unsafe
import Unsafe.Coerce

import Prelude hiding (Read)

type Conjurable a =
  ( Typeable a
  , Amendable a
  , Nameable a
  , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
  , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Hashable (Name a), Ord (Name a)
  , ToJSON (Resource a), FromJSON (Resource a)
  , ToJSON (Product a), FromJSON (Product a)
  , ToJSON (Preview a), FromJSON (Preview a)
  , ToJSON (Action a), FromJSON (Action a)
  , ToJSON (Reaction a), FromJSON (Reaction a)
  , ToJSON (Amend a), FromJSON (Amend a)
  )

--------------------------------------------------------------------------------  

conjure
  :: forall a.
    ( Typeable a
    , Amendable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => IO ()
conjure = do
  sorcerer @(ResourceMsg a) @'[Resource a]
  sorcerer @(IndexMsg a) @'[Index a]
  sorcerer @(ProductMsg a) @'[Product a]
  sorcerer @(PreviewMsg a) @'[Preview a]
  sorcerer @(PreviewsMsg a) @'[Previews a]

type Queue = TMVar (TQueue (IO ()))

{-# NOINLINE lockTable #-}
lockTable :: IORef (Map TypeRep (TMVar (Map Any Queue)))
lockTable = unsafePerformIO (newIORef mempty)

getResourceLocks :: forall a. (Ord (Context a), Ord (Name a), Typeable a) => IO (TMVar (Map Any Queue))
getResourceLocks =
  atomicModifyIORef' lockTable $ \lt ->
    let tr = typeRep (Proxy :: Proxy a) in
    case Map.lookup tr lt of
      Just rqs -> (lt,rqs)
      Nothing -> unsafePerformIO do
        rqs <- newTMVarIO (unsafeCoerce (mempty :: Map (Context a,Name a) Queue))
        pure (Map.insert tr rqs lt,rqs)

-- WARNINGS: 
--   Always acquire locks in the same order!
--
--   Make sure you conform to some unidirectional resource hierarchy for
--   calling any of the update methods, like `tryUpdate` - especially if
--   you call them from within an `onUpdate` callback, for example!
--
-- If I make `withLock` an optional combinator, and combine it with a retry 
-- semantics, I could push burden of managing the possibility of deadlocks onto
-- the user of the library, but that requires that the user of the library have
-- a deeper knowledge of its inner workings. Instaed, I find this approach,
-- where dependent updates should always be unidirectional, and enforced by the 
-- library user, is a reasonable trade-off for simplicity of understanding and
-- ease-of-use.
--
-- I have a feeling I'll be back here soon, cursing myself. Apologies, in advance.
--
withLock :: forall a b. (Ord (Context a), Ord (Name a), Typeable a) => Context a -> Name a -> IO b -> IO b
withLock ctx nm f = do
  getResourceLocks @a >>= start
  where
    cleanup rqs_ = do
      join $ atomically do
        rqs <- takeTMVar rqs_
        case Map.lookup (ctx,nm) (unsafeCoerce rqs) of
          Nothing -> error "Unexpected"
          Just q -> do
            tq <- takeTMVar q
            x <- tryReadTQueue tq
            case x of
              Nothing -> do
                putTMVar rqs_ (unsafeCoerce (Map.delete (ctx,nm) (unsafeCoerce rqs)))
                pure (pure ())
              Just next -> do
                putTMVar q tq
                putTMVar rqs_ rqs
                pure next

    start rqs_ = do
      join $ atomically do
        rqs <- takeTMVar rqs_
        case Map.lookup (ctx,nm) (unsafeCoerce rqs) of
          Just q -> do
            tq <- takeTMVar q
            mv <- newEmptyTMVar
            writeTQueue tq do
              atomically do
                putTMVar mv ()
            putTMVar q tq
            putTMVar rqs_ rqs
            pure do
              atomically (takeTMVar mv)
              f `finally` cleanup rqs_

          Nothing -> do
            q <- newTMVar =<< newTQueue
            let rqs' = unsafeCoerce (Map.insert (ctx,nm) q (unsafeCoerce rqs))
            putTMVar rqs_ rqs'
            pure do
              f `finally` cleanup rqs_

tryCreate
  :: forall a.
    ( Typeable a
    , Processable a, Amendable a, Nameable a, Previewable a, Producible a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> Context a -> Resource a -> IO (Maybe (Name a,Product a,Preview a,[(Name a,Preview a)]))
tryCreate Permissions {..} Callbacks {..} ctx a0 = do
  ma <- process a0
  case ma of
    Nothing -> pure Nothing
    Just a -> do
      let name = toName a
      can <- canCreate ctx name a
      if can then do
        withLock ctx name do
          Sorcerer.observe (ResourceStream ctx name) (CreateResource a) >>= \case
            Added (new :: Resource a) -> do
              pro <- produce ctx name new Nothing
              pre <- preview ctx name new pro
              (Sorcerer.Update (_ :: Product a)) <- Sorcerer.transact (ProductStream ctx name) (SetProduct pro)
              (Sorcerer.Update (_ :: Preview a)) <- Sorcerer.transact (PreviewStream ctx name) (SetPreview pre)
              Sorcerer.write (IndexStream @a) (ResourceAdded ctx name)
              (Sorcerer.Update (Previews (lst :: [(Name a,Preview a)]))) <-
                Sorcerer.transact (PreviewsStream ctx) (SetPreviewItem name pre)
              caching <- isCaching @a
              when caching do
                !_ <- cacheProduct ctx name pro
                !_ <- cachePreview ctx name pre
                !_ <- cacheListing ctx lst
                pure ()
              onCreate ctx name new pro pre lst
              pure (Just (name,pro,pre,lst))
            _ ->
              pure Nothing
      else
        pure Nothing

tryUpdate
  :: forall a.
    ( Typeable a
    , Processable a, Amendable a, Nameable a, Previewable a, Producible a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> Context a -> Name a -> Resource a -> IO (Maybe (Product a,Preview a,[(Name a,Preview a)]))
tryUpdate Permissions {..} Callbacks {..} ctx name a0 = do
  ma <- process a0
  case ma of
    Nothing -> pure Nothing
    Just a -> do
      can <- canUpdate ctx name
      if can then do
        withLock ctx name do
          Sorcerer.transact (ResourceStream ctx name) (SetResource a) >>= \case
            Sorcerer.Update (new :: Resource a) -> do
              mpro <- Sorcerer.read (ProductStream ctx name)
              pro <- produce ctx name new mpro
              pre <- preview ctx name new pro
              (Sorcerer.Update (_ :: Product a)) <- Sorcerer.transact (ProductStream ctx name) (SetProduct pro)
              (Sorcerer.Update (_ :: Preview a)) <- Sorcerer.transact (PreviewStream ctx name) (SetPreview pre)
              (Sorcerer.Update (Previews (lst :: [(Name a,Preview a)]))) <-
                Sorcerer.transact (PreviewsStream ctx) (SetPreviewItem name pre)
              caching <- isCaching @a
              when caching do
                !_ <- cacheProduct ctx name pro
                !_ <- cachePreview ctx name pre
                !_ <- cacheListing ctx lst
                pure ()
              onUpdate ctx name new pro pre lst
              pure (Just (pro,pre,lst))
            _ ->
              pure Nothing
      else
        pure Nothing

tryAmend
  :: forall a.
    ( Typeable a
    , Amendable a, Previewable a, Producible a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> Context a -> Name a -> Amend a -> IO (Maybe (Product a,Preview a,[(Name a,Preview a)]))
tryAmend Permissions {..} Callbacks {..} ctx name a = do
  can <- canAmend ctx name a
  if can then
    withLock ctx name do
      Sorcerer.transact (ResourceStream ctx name) (AmendResource a) >>= \case
        Sorcerer.Update (new :: Resource a) -> do
          mpro <- Sorcerer.read (ProductStream ctx name)
          pro <- produce ctx name new mpro
          pre <- preview ctx name new pro
          (Sorcerer.Update (_ :: Product a)) <- Sorcerer.transact (ProductStream ctx name) (SetProduct pro)
          (Sorcerer.Update (_ :: Preview a)) <- Sorcerer.transact (PreviewStream ctx name) (SetPreview pre)
          (Sorcerer.Update (Previews (lst :: [(Name a,Preview a)]))) <-
            Sorcerer.transact (PreviewsStream ctx) (SetPreviewItem name pre)
          caching <- isCaching @a
          when caching do
            !_ <- cacheProduct ctx name pro
            !_ <- cachePreview ctx name pre
            !_ <- cacheListing ctx lst
            pure ()
          onAmend ctx name new pro pre lst a
          pure (Just (pro,pre,lst))
        _ ->
          pure Nothing
  else
    pure Nothing

tryDelete
  :: forall a.
    ( Typeable a
    , Amendable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> Context a -> Name a -> IO (Maybe (Product a,Preview a,[(Name a,Preview a)]))
tryDelete Permissions {..} Callbacks {..} ctx name = do
  can <- canDelete ctx name
  if can then do
    withLock ctx name do
      Sorcerer.observe (ResourceStream ctx name) DeleteResource >>= \case
        Deleted r -> do
          Deleted pre <- Sorcerer.observe (PreviewStream ctx name) DeletePreview
          Deleted pro <- Sorcerer.observe (ProductStream ctx name) DeleteProduct
          (Sorcerer.Update (Previews (lst :: [(Name a,Preview a)]))) <-
            Sorcerer.transact (PreviewsStream ctx) (DeletePreviewItem name)
          onDelete ctx name r pro pre lst
          caching <- isCaching @a
          when caching do
            !_ <- deleteProduct ctx name
            !_ <- deletePreview ctx name
            !_ <- cacheListing ctx lst
            pure ()
          pure (Just (pro,pre,lst))
        _ -> do
          pure Nothing
  else
    pure Nothing

tryReadResource
  :: forall a.
    ( Typeable a
    , Amendable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> Context a -> Name a -> IO (Maybe (Resource a))
tryReadResource Permissions {..} Callbacks {..} ctx name = do
  -- Note: Resource often hides details that might shouldn't be user-facing, 
  -- so we protect it with canUpdate rather than canRead. I can't think of a
  -- case where you would want to view a resource, but not update it. At least,
  -- it is not how I originally intended for the library to be used, but I can 
  -- see an argument for having a dedicated permissions check.
  can <- canUpdate ctx name
  if can then do
    mres <- Sorcerer.read (ResourceStream ctx name)
    case mres of
      Nothing -> pure Nothing
      Just res -> do
        onResource ctx name res
        pure (Just res)
  else
    pure Nothing

tryReadPreview
  :: forall a.
    ( Typeable a
    , ToJSON (Preview a), FromJSON (Preview a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    ) => Permissions a -> Callbacks a -> Context a -> Name a -> IO (Maybe (Preview a))
tryReadPreview Permissions {..} Callbacks {..} ctx name = do
  can <- canRead ctx name
  if can then do
    mpre <- Sorcerer.read (PreviewStream ctx name)
    case mpre of
      Nothing -> pure Nothing
      Just pre -> do
        onPreview ctx name pre
        pure (Just pre)
  else
    pure Nothing

tryReadProduct
  :: forall a.
    ( Typeable a
    , ToJSON (Product a), FromJSON (Product a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    ) => Permissions a -> Callbacks a -> Context a -> Name a -> IO (Maybe (Product a))
tryReadProduct Permissions {..} Callbacks {..} ctx name = do
  can <- canRead ctx name
  if can then do
    mpro <- Sorcerer.read (ProductStream ctx name)
    case mpro of
      Nothing -> do
        pure Nothing
      Just pro -> do
        onRead ctx name pro
        pure (Just pro)
  else do
    pure Nothing

tryReadListing
  :: forall a.
    ( Typeable a
    , ToJSON (Preview a), FromJSON (Preview a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a), Ord (Name a)
    ) => Permissions a -> Callbacks a -> Context a -> IO (Maybe [(Name a,Preview a)])
tryReadListing Permissions {..} Callbacks {..} ctx = do
  can <- canList ctx
  if can then do
    mps <- Sorcerer.read (PreviewsStream ctx) >>= \case
      Just (Previews ps) -> pure (Just ps)
      Nothing -> pure Nothing
    case mps of
      Nothing -> pure Nothing
      Just ps -> do
        onList ctx ps
        pure (Just ps)
  else
    pure Nothing

tryInteract
  :: forall a.
    ( Typeable a
    , Amendable a
    , ToJSON (Amend a)
    , FromJSON (Product a), ToJSON (Product a)
    , FromJSON (Amend a), ToJSON (Amend a)
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => Permissions a -> Callbacks a -> Interactions a -> Context a -> Name a -> Action a -> IO (Maybe (Reaction a))
tryInteract Permissions {..} Callbacks {..} Interactions {..} ctx name action = do
  can <- canInteract ctx name action
  if can then do
    mpro <- Sorcerer.read (ProductStream ctx name)
    case mpro of
      Nothing -> pure Nothing
      Just pro -> do
        reaction <- interact ctx name pro action
        onInteract ctx name pro action reaction
        pure (Just reaction)
  else
    pure Nothing

--------------------------------------------------------------------------------

publishing ::
  ( Typeable a
  , Processable a, Amendable a, Nameable a, Previewable a, Producible a
  , ToJSON (Action a), FromJSON (Action a)
  , ToJSON (Reaction a), FromJSON (Reaction a)
  , ToJSON (Resource a), FromJSON (Resource a)
  , ToJSON (Product a), FromJSON (Product a)
  , ToJSON (Preview a), FromJSON (Preview a)
  , ToJSON (Context a), FromJSON (Context a)
  , ToJSON (Name a), FromJSON (Name a)
  , Pathable (Context a), Hashable (Context a), Ord (Context a)
  , Pathable (Name a), Hashable (Name a), Ord (Name a)
  , FromJSON (Amend a), ToJSON (Amend a)
  ) => Permissions a -> Callbacks a -> Interactions a
    -> WS.Endpoints '[] (PublishingAPI a) '[] (PublishingAPI a)
publishing ps cs i = WS.Endpoints publishingAPI msgs reqs
  where
    msgs = WS.non
    reqs = handleCreateResource ps cs
       <:> handleReadResource ps cs
       <:> handleUpdateResource ps cs
       <:> handleDeleteResource ps cs
       <:> handlePreviewResource ps cs
       <:> handleAmendResource ps cs
       <:> handlePreviewAmendResource ps cs
       <:> handleInteractResource ps cs i
       <:> WS.non

reading ::
  ( Typeable a
  , ToJSON (Product a), FromJSON (Product a)
  , ToJSON (Preview a), FromJSON (Preview a)
  , ToJSON (Context a), FromJSON (Context a)
  , ToJSON (Name a), FromJSON (Name a)
  , Pathable (Context a), Hashable (Context a), Ord (Context a)
  , Pathable (Name a), Hashable (Name a), Ord (Name a)
  ) => Permissions a -> Callbacks a
    -> WS.Endpoints '[] (ReadingAPI a) '[] (ReadingAPI a)
reading ps cs = WS.Endpoints readingAPI msgs reqs
  where
    msgs = WS.non
    reqs = handleReadProduct ps cs
       <:> handleReadPreview ps cs
       <:> handleReadListing ps cs
       <:> WS.non

handleCreateResource
  :: forall a.
    ( Typeable a
    , Processable a, Amendable a, Nameable a, Producible a, Previewable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> WS.RequestHandler (CreateResource a)
handleCreateResource permissions callbacks = WS.responding do
  (ctx,resource) <- WS.acquire
  response <- WS.liftIO do
    tryCreate permissions callbacks ctx resource >>= \case
      Just (name,_,_,_) -> pure (Just name)
      _ -> pure Nothing
  WS.reply response

handleReadResource
  :: forall a.
    ( Typeable a
    , Amendable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Hashable (Context a), Pathable (Context a), Ord (Context a)
    , Hashable (Name a), Pathable (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> WS.RequestHandler (ReadResource a)
handleReadResource permissions callbacks = WS.responding do
  (ctx,name) <- WS.acquire
  response <- WS.liftIO (tryReadResource permissions callbacks ctx name)
  WS.reply response

handleUpdateResource
  :: forall a.
    ( Typeable a
    , Processable a, Amendable a, Nameable a, Producible a, Previewable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> WS.RequestHandler (UpdateResource a)
handleUpdateResource permissions callbacks = WS.responding do
  (ctx,name,resource) <- WS.acquire
  response <- WS.liftIO do
    result <- tryUpdate permissions callbacks ctx name resource
    pure (isJust result)
  WS.reply response

handleDeleteResource
  :: forall a.
    ( Typeable a
    , Amendable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> WS.RequestHandler (DeleteResource a)
handleDeleteResource permissions callbacks = WS.responding do
  (ctx,name) <- WS.acquire
  response <- WS.liftIO do
     result <- tryDelete permissions callbacks ctx name
     pure (isJust result)
  WS.reply response

handlePreviewResource
  :: forall a.
    ( Typeable a
    , Processable a, Nameable a, Producible a, Previewable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    ) => Permissions a -> Callbacks a -> WS.RequestHandler (PreviewResource a)
handlePreviewResource permissions callbacks = WS.responding do
  (ctx,res0) <- WS.acquire
  response <- WS.liftIO do
    -- I don't like the ordering here, but process 
    -- needs to run before canCreate because it 
    -- may seed/alter the Context and/or Name.
    mres <- processPreview res0
    case mres of
      Nothing -> pure Nothing
      Just res -> do
        let name = toName res
        can <- canCreate permissions ctx name res
        if can then do
          mpro <- Sorcerer.read (ProductStream ctx name)
          pro <- producePreview ctx name res mpro
          pre <- previewPreview ctx name res pro
          pure (Just (ctx,name,pre,pro,res))
        else
          pure Nothing
  WS.reply response

handlePreviewAmendResource
  :: forall a.
    ( Typeable a
    , Amendable a, Nameable a, Producible a, Previewable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    , ToJSON (Amend a), FromJSON (Amend a)
    ) => Permissions a -> Callbacks a -> WS.RequestHandler (PreviewAmendResource a)
handlePreviewAmendResource permissions callbacks = WS.responding do
  (ctx,name,a) <- WS.acquire
  response <- WS.liftIO do
    tryReadResource permissions callbacks ctx name >>= \case
      Nothing -> pure Nothing
      Just resource -> do
        can <- canAmend permissions ctx name a
        if can then do
          case amend a resource of
            Nothing -> pure Nothing
            Just res -> do
              mpro <- Sorcerer.read (ProductStream ctx name)
              pro <- producePreview ctx name res mpro
              pre <- previewPreview ctx name res pro
              pure (Just (ctx,name,pre,pro,res))
        else
          pure Nothing
  WS.reply response

handleInteractResource
  :: forall a.
    ( Typeable a
    , Amendable a
    , FromJSON (Amend a), ToJSON (Amend a)
    , FromJSON (Product a), ToJSON (Product a)
    , FromJSON (Action a)
    , ToJSON (Reaction a)
    , FromJSON (Context a), Hashable (Context a), Pathable (Context a), Ord (Context a)
    , FromJSON (Name a), Hashable (Name a), Pathable (Name a), Ord (Name a)
    ) => Permissions a -> Callbacks a -> Interactions a -> WS.RequestHandler (InteractResource a)
handleInteractResource permissions callbacks interaction = WS.responding do
  (ctx,name,action) <- WS.acquire
  response <- WS.liftIO (tryInteract permissions callbacks interaction ctx name action)
  WS.reply response

handleAmendResource
  :: forall a.
    ( Typeable a
    , Amendable a, Producible a, Previewable a
    , ToJSON (Resource a), FromJSON (Resource a)
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    , FromJSON (Amend a), ToJSON (Amend a)
    ) => Permissions a -> Callbacks a -> WS.RequestHandler (AmendResource a)
handleAmendResource permissions callbacks = WS.responding do
  (ctx,name,amend) <- WS.acquire
  response <- WS.liftIO do
    result <- tryAmend permissions callbacks ctx name amend
    pure (isJust result)
  WS.reply response

handleReadProduct
  :: forall a.
    ( Typeable a
    , ToJSON (Product a), FromJSON (Product a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    ) => Permissions a -> Callbacks a -> WS.RequestHandler (ReadProduct a)
handleReadProduct permissions callbacks = WS.responding do
  (ctx,name) <- WS.acquire
  caching <- WS.liftIO (isCaching @a)
  if caching then do
    response <- WS.liftIO (tryReadProductFromCache permissions callbacks ctx name)
    case response of
      Just rsp -> customReplyRaw rsp
      Nothing  -> WS.reply Nothing
  else do
    response <- WS.liftIO (tryReadProduct permissions callbacks ctx name)
    WS.reply response

handleReadPreview
  :: forall a.
    ( Typeable a
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    ) => Permissions a -> Callbacks a -> WS.RequestHandler (ReadPreview a)
handleReadPreview permissions callbacks = WS.responding do
  (ctx,name) <- WS.acquire
  caching <- WS.liftIO (isCaching @a)
  if caching then do
    response <- WS.liftIO (tryReadPreviewFromCache permissions callbacks ctx name)
    case response of
      Just rsp -> customReplyRaw rsp
      Nothing  -> WS.reply Nothing
  else do
    response <- WS.liftIO (tryReadPreview permissions callbacks ctx name)
    WS.reply response

handleReadListing
  :: forall a.
    ( Typeable a
    , ToJSON (Preview a), FromJSON (Preview a)
    , ToJSON (Context a), FromJSON (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Pathable (Context a), Hashable (Context a), Ord (Name a)
    ) => Permissions a -> Callbacks a -> WS.RequestHandler (ReadListing a)
handleReadListing permissions callbacks = WS.responding do
  ctx <- WS.acquire
  caching <- WS.liftIO (isCaching @a)
  if caching then do
    response <- WS.liftIO (tryReadListingFromCache permissions callbacks ctx)
    case response of
      Just rsp -> customReplyRaw rsp
      Nothing  -> WS.reply Nothing
  else do
    response <- WS.liftIO (tryReadListing permissions callbacks ctx)
    WS.reply response

--------------------------------------------------------------------------------

newtype ResponseMap = ResponseMap (Map Any (IORef ByteString))

data Cache = Cache
  { table    :: IORef (Set TypeRep)
  , previews :: IORef (Map TypeRep (IORef ResponseMap))
  , products :: IORef (Map TypeRep (IORef ResponseMap))
  , listings :: IORef (Map TypeRep (IORef ResponseMap))
  }

{-# NOINLINE conjurerCache #-}
conjurerCache :: Pure.Conjurer.Cache
conjurerCache = unsafePerformIO do
  Pure.Conjurer.Cache
    <$> newIORef Set.empty
    <*> newIORef Map.empty
    <*> newIORef Map.empty
    <*> newIORef Map.empty

isCaching :: forall a. Typeable a => IO Bool
isCaching =
  let ty = typeRep (Proxy :: Proxy a)
  in fmap (Set.member ty) (readIORef (table conjurerCache))

setCaching :: forall a. Typeable a => IO ()
setCaching =
  let ty = typeRep (Proxy :: Proxy a)
  in atomicModifyIORef' (table conjurerCache) $ \s ->
    (Set.insert ty s,())

cache
  :: forall resource.
    ( Typeable resource
    , ToJSON (Product resource), FromJSON (Product resource)
    , ToJSON (Preview resource), FromJSON (Preview resource)
    , ToJSON (Context resource), FromJSON (Context resource)
    , ToJSON (Name resource), FromJSON (Name resource)
    , Ord (Name resource), Hashable (Name resource), Pathable (Name resource), Ord (Name resource)
    , Ord (Context resource), Hashable (Context resource), Pathable (Context resource), Ord (Context resource)
    ) => IO ()
cache = do
  setCaching @resource

  contexts <- newIORef Set.empty

  Export.iterate @resource $ \ctx nm -> do

    Sorcerer.read (PreviewStream ctx nm) >>=
      traverse_ (cachePreview ctx nm :: Preview resource -> IO ())

    Sorcerer.read (ProductStream ctx nm) >>=
      traverse_ (cacheProduct ctx nm :: Product resource -> IO ())

    modifyIORef contexts (Set.insert ctx)

  cs <- readIORef contexts

  for_ cs $ \ctx -> do

    Sorcerer.read (PreviewsStream ctx) >>= \case
      Just (Previews nps :: Previews resource) -> cacheListing ctx nps
      _ -> pure ()

cacheProduct
  :: forall resource.
    ( Typeable resource
    , ToJSON (Product resource)
    , Ord (Context resource), Ord (Name resource)
    ) => Context resource -> Name resource -> Product resource -> IO ()
cacheProduct ctx nm (encodeBS . Just -> !pro) = do
  -- Don't look; I'm hideous!
  let ty = typeRep (Proxy :: Proxy (ReadProduct resource))
  atomicModifyIORef' (products conjurerCache) $ \m ->
    case Map.lookup ty m of
      Nothing -> unsafePerformIO do
        pro_ <- newIORef pro
        rm_ <- newIORef (ResponseMap $ unsafeCoerce (Map.singleton (ctx,nm) pro_))
        pure (Map.insert ty rm_ m,())
      Just rm_ -> unsafePerformIO do
        modifyIORef rm_ $ \x@(ResponseMap rm) -> unsafePerformIO do
          case Map.lookup (ctx,nm) (unsafeCoerce rm) of
            Nothing -> do
              pro_ <- newIORef pro
              pure $ ResponseMap $ unsafeCoerce (Map.insert (ctx,nm) pro_ (unsafeCoerce rm))
            Just pro_ -> do
              writeIORef pro_ pro
              pure x
        pure (m,())

deleteProduct
  :: forall resource.
    ( Typeable resource
    , Ord (Context resource), Ord (Name resource)
    ) => Context resource -> Name resource -> IO ()
deleteProduct ctx nm = do
   -- Don't look; I'm hideous!
  let ty = typeRep (Proxy :: Proxy (ReadProduct resource))
  atomicModifyIORef' (products conjurerCache) $ \m ->
    case Map.lookup ty m of
      Nothing -> (m,())
      Just rm_ -> unsafePerformIO do
        modifyIORef rm_ $ \(ResponseMap rm) ->
          ResponseMap $ unsafeCoerce (Map.delete (ctx,nm) (unsafeCoerce rm))
        pure (m,())

tryReadProductFromCache
  :: forall a.
    ( Typeable a
    , Ord (Context a), Ord (Name a)
    , FromJSON (Product a)
    ) => Permissions a -> Callbacks a -> Context a -> Name a -> IO (Maybe ByteString)
tryReadProductFromCache Permissions {..} Callbacks {..} ctx nm = do
  can <- canRead ctx nm
  if can then do
    let ty = typeRep (Proxy :: Proxy (ReadProduct a))
    ps <- readIORef (products conjurerCache)
    case Map.lookup ty ps of
      Nothing -> do
        pure Nothing
      Just pros_ -> do
        ResponseMap pros <- readIORef pros_
        case Map.lookup (ctx,nm) (unsafeCoerce pros) of
          Just bs_ -> do
            bs <- readIORef bs_
            onRead ctx nm (fromJust (decodeBS bs)) -- this will error if `decode . encode /= Just`
            pure (Just bs)
          _ -> do
            pure Nothing
  else do
    pure Nothing

cachePreview
  :: forall resource.
    ( Typeable resource
    , ToJSON (Preview resource)
    , Ord (Context resource), Ord (Name resource)
    ) => Context resource -> Name resource -> Preview resource -> IO ()
cachePreview ctx nm (encodeBS . Just -> !pre) = do
  -- Don't look; I'm hideous!
  let ty = typeRep (Proxy :: Proxy (ReadPreview resource))
  atomicModifyIORef' (Pure.Conjurer.previews conjurerCache) $ \m ->
    case Map.lookup ty m of
      Nothing -> unsafePerformIO do
        pre_ <- newIORef pre
        rm_ <- newIORef (ResponseMap $ unsafeCoerce (Map.singleton (ctx,nm) pre_))
        pure (Map.insert ty rm_ m,())
      Just rm_ -> unsafePerformIO do
        modifyIORef rm_ $ \x@(ResponseMap rm) -> unsafePerformIO do
          case Map.lookup (ctx,nm) (unsafeCoerce rm) of
            Nothing -> do
              pre_ <- newIORef pre
              pure $ ResponseMap $ unsafeCoerce (Map.insert (ctx,nm) pre_ (unsafeCoerce rm))
            Just pre_ -> do
              writeIORef pre_ pre
              pure x
        pure (m,())

deletePreview
  :: forall resource.
    ( Typeable resource
    , Ord (Context resource), Ord (Name resource)
    ) => Context resource -> Name resource -> IO ()
deletePreview ctx nm = do
   -- Don't look; I'm hideous!
  let ty = typeRep (Proxy :: Proxy (ReadPreview resource))
  atomicModifyIORef' (Pure.Conjurer.previews conjurerCache) $ \m ->
    case Map.lookup ty m of
      Nothing -> (m,())
      Just rm_ -> unsafePerformIO do
        modifyIORef rm_ $ \(ResponseMap rm) ->
          ResponseMap $ unsafeCoerce (Map.delete (ctx,nm) (unsafeCoerce rm))
        pure (m,())

tryReadPreviewFromCache
  :: forall a.
    ( Typeable a
    , Ord (Context a), Ord (Name a)
    , FromJSON (Preview a)
    ) => Permissions a -> Callbacks a -> Context a -> Name a -> IO (Maybe ByteString)
tryReadPreviewFromCache Permissions {..} Callbacks {..} ctx nm = do
  can <- canRead ctx nm
  if can then do
    let ty = typeRep (Proxy :: Proxy (ReadPreview a))
    ps <- readIORef (Pure.Conjurer.previews conjurerCache)
    case Map.lookup ty ps of
      Nothing -> pure Nothing
      Just pros_ -> do
        ResponseMap pres <- readIORef pros_
        case Map.lookup (ctx,nm) (unsafeCoerce pres) of
          Just bs_ -> do
            bs <- readIORef bs_
            onPreview ctx nm (fromJust (decodeBS bs)) -- this will error if `decode . encode /= Just`
            pure (Just bs)
          _ ->
            pure Nothing
  else
    pure Nothing

cacheListing
  :: forall resource.
    ( Typeable resource
    , ToJSON (Preview resource)
    , ToJSON (Name resource)
    , Ord (Context resource)
    ) => Context resource -> [(Name resource,Preview resource)] -> IO ()
cacheListing ctx (encodeBS . Just -> !nps) = do
  -- Don't look; I'm hideous!
  let ty = typeRep (Proxy :: Proxy (ReadListing resource))
  atomicModifyIORef' (listings conjurerCache) $ \m ->
    case Map.lookup ty m of
      Nothing -> unsafePerformIO do
        nps_ <- newIORef nps
        rm_ <- newIORef (ResponseMap $ unsafeCoerce (Map.singleton ctx nps_))
        pure (Map.insert ty rm_ m,())
      Just rm_ -> unsafePerformIO do
        modifyIORef rm_ $ \x@(ResponseMap rm) -> unsafePerformIO do
          case Map.lookup ctx (unsafeCoerce rm) of
            Nothing -> do
              nps_ <- newIORef nps
              pure $ ResponseMap $ unsafeCoerce (Map.insert ctx nps_ (unsafeCoerce rm))
            Just nps_ -> do
              writeIORef nps_ nps
              pure x
        pure (m,())

tryReadListingFromCache
  :: forall a.
    ( Typeable a
    , Ord (Context a)
    , FromJSON (Preview a)
    , FromJSON (Name a)
    ) => Permissions a -> Callbacks a -> Context a -> IO (Maybe ByteString)
tryReadListingFromCache Permissions {..} Callbacks {..} ctx = do
  can <- canList ctx
  if can then do
    let ty = typeRep (Proxy :: Proxy (ReadListing a))
    ls <- readIORef (listings conjurerCache)
    case Map.lookup ty ls of
      Nothing -> pure Nothing
      Just lsts_ -> do
        ResponseMap lsts <- readIORef lsts_
        case Map.lookup ctx (unsafeCoerce lsts) of
          Just bs_ -> do
            bs <- readIORef bs_
            onList ctx (fromJust (decodeBS bs)) -- this will error if `decode . encode /= Just`
            pure (Just bs)
          _ ->
            pure Nothing
  else
    pure Nothing

customReplyRaw :: ByteString -> WS.Responding rq rsp ()
#ifdef __GHCJS__
customReplyRaw = WS.replyRaw . toTxt
#else
customReplyRaw = WS.replyRaw
#endif

--------------------------------------------------------------------------------

data Route a
  = ReadR (Context a) (Name a)
  | ListR (Context a)
  | CreateR (Context a)
  | UpdateR (Context a) (Name a)

routeContext :: Route resource -> Context resource
routeContext = \case
  ReadR   ctx _ -> ctx
  ListR   ctx   -> ctx
  CreateR ctx   -> ctx
  UpdateR ctx _ -> ctx

routeName :: Route resource -> Maybe (Name resource)
routeName = \case
  ReadR   _ nm -> Just nm
  UpdateR _ nm -> Just nm
  _            -> Nothing

-- Erases a resource type; useful for cases that deal across resource domains, 
-- like navigational headers that need to match the route to, e.g. highlight
-- an active menu based on the route/resource.
data SomeRoute _role
  = forall resource.
    ( Typeable resource
    , Routable resource
    , FromJSON (Context resource), ToJSON (Context resource), Pathable (Context resource), Ord (Context resource)
    , FromJSON (Name resource), ToJSON (Name resource), Pathable (Name resource), Ord (Name resource)
    , Ownable resource
    ) => SomeRoute (Route resource)

fromSomeRoute :: forall _role resource. Typeable resource => SomeRoute _role -> Maybe (Route resource)
fromSomeRoute (SomeRoute rt) = cast rt

{- 
-- I have a feeling these are expensive.
deriving instance (Generic (Context a), Generic (Name a)) => Generic (Route a)
deriving instance (Ord (Context a), Ord (Name a)) => Ord (Route a)
deriving instance (Eq (Context a), Eq (Name a)) => Eq (Route a)
deriving instance (Show (Context a), Show (Name a)) => Show (Route a)
deriving instance (ToJSON (Context a),ToJSON (Name a)) => ToJSON (Route a)
deriving instance (FromJSON (Context a),FromJSON (Name a)) => FromJSON (Route a)
-}

data Conjured = Create | Read | Update | List
instance Theme Conjured
instance Theme Create
instance Theme Update
instance Theme Read
instance Theme Pure.Conjurer.List

pages :: forall _role a. (Creatable _role a, Listable _role a, Readable _role a, Updatable _role a, Websocket _role, Authentication _role) => Route a -> View
pages = \case
  ReadR ctx nm   -> toRead @_role ctx nm
  ListR ctx      -> Export.toList @_role ctx
  CreateR ctx    -> toCreate @_role ctx
  UpdateR ctx nm -> toUpdate @_role ctx nm

readPages :: forall _role a. (Readable _role a, Listable _role a, Websocket _role) => Route a -> View
readPages = \case
  ReadR ctx nm   -> toRead @_role ctx nm
  UpdateR ctx nm -> toRead @_role ctx nm
  ListR ctx      -> Export.toList @_role ctx
  CreateR ctx    -> Export.toList @_role ctx

routes :: forall a route. (Typeable a, Routable a)
       => (Route a -> route) -> Routing route ()
routes lift = do
  updateRoute (\ctx nm -> lift (UpdateR ctx nm))
  createRoute (lift . CreateR)
  readRoute (\ctx nm -> lift (ReadR ctx nm))
  listRoute (lift . ListR)

location :: Routable a => Route a -> Txt
location = \case
  ReadR ctx nm   -> toReadRoute ctx nm
  ListR ctx      -> toListRoute ctx
  CreateR ctx    -> toCreateRoute ctx
  UpdateR ctx nm -> toUpdateRoute ctx nm

instance (Typeable resource, Routable resource) => Pathable (Route resource) where
  toPath = location
  fromPath = do
    st <- getRoutingState
    (r,st') <- runRouting (routes id) st
    case r of
      Left rt -> do
        putRoutingState st'
        pure rt
      _ -> do
        pure Nothing

ref :: (Routable a, HasFeatures v) => Route a -> v -> v
ref = go . location
  where
    go t a = OnClickWith intercept (\_ -> storeScrollPosition >> Router.goto t) (Href t a)

resource :: Routable a => Route a -> IO ()
resource = go . location
  where
    go r = do
      storeScrollPosition
      Router.goto r

preload
  :: forall _role a v.
    ( Typeable _role
    , Typeable a
    , ToJSON (Name a), FromJSON (Name a), Ord (Name a)
    , ToJSON (Context a), FromJSON (Context a), Ord (Context a)
    , FromJSON (Product a)
    , FromJSON (Preview a)
    , HasFeatures v
    , Websocket _role
    ) => Route a -> v -> v
preload rt = OnMouseDown load . OnTouchStart load
  where
    load _ = case rt of
      ReadR ctx nm ->
        void $ forkIO $ void $ do
          req @_role Cached (readingAPI @a)
            (readProduct @a)
            (ctx,nm)

      ListR ctx ->
        void $ forkIO $ void $ do
          req @_role Cached (readingAPI @a)
            (readListing @a)
            ctx

      _ ->
        pure ()

--------------------------------------------------------------------------------
-- A very simple static renderer. The goal is to generate content for indexing
-- by crawlers, not to generate a static version of the site viewable by the
-- public.

generateStatic
  :: forall a.
    ( Typeable a
    , Routable a
    , ToJSON (Name a), FromJSON (Name a), Hashable (Name a), Pathable (Name a), Ord (Name a)
    , ToJSON (Context a), FromJSON (Context a), Hashable (Context a), Pathable (Context a), Ord (Context a)
    , ToJSON (Product a), FromJSON (Product a)
    , Viewable (Product a)
    ) => IO ()
generateStatic = generateStaticWith @a "dist/static/" defaultTemplate

defaultTemplate :: (Typeable a, Viewable a) => a -> IO Txt
defaultTemplate a = pure $
  "<!DOCTYPE html><html><head></head><body>" <> toTxt (toView a) <> "</body></html>"

generateStaticWith
  :: forall a.
    ( Typeable a
    , Routable a
    , ToJSON (Name a), FromJSON (Name a), Hashable (Name a), Pathable (Name a), Ord (Name a)
    , ToJSON (Context a), FromJSON (Context a), Hashable (Context a), Pathable (Context a), Ord (Context a)
    , ToJSON (Product a), FromJSON (Product a)
    ) => FilePath -> (Product a -> IO Txt) -> IO ()
generateStaticWith path template = do
  Export.iterate @a $ \ctx nm -> do
    Sorcerer.read (ProductStream ctx nm) >>= \case
      Just pro -> do
        page <- template pro
        let p = path <> fromTxt (toReadRoute ctx nm) <> ".html"
        createDirectoryIfMissing True (takeDirectory p)
        Prelude.writeFile p (fromTxt page)
      Nothing ->
        pure ()













