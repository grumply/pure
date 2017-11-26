{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE CPP #-}
module Pure.App (module Pure.App, module Export)where

import Ef.Base as Export hiding (As,Index,child,transform,watch,construct,uncons,distribute,embed,observe,End,Nat(..),initialize,(!),maps,run)
import qualified Ef.Base

import qualified Pure.WebSocket as Export
import qualified Pure.Route     as Export hiding (route)
import qualified Pure.Router    as Export

import qualified Pure.Signals as Export
import qualified Pure.Types as Export hiding (Text,Null,build,prime,key)

import Pure.Data as Export hiding (hashed)
import Pure.Data.JSV

import Pure.Lifted
import Pure.DOM
import Pure.Types hiding (Base)
import Pure.Router hiding (hashed)
import Pure.Route as Route
import Pure.Service hiding (Base,hashed)
import Pure.Signals

#ifdef __GHCJS_
import qualified GHCJS.Marshal.Pure as M
import qualified GHCJS.Types as T
#endif

import Control.Concurrent
import Data.Function
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Proxy
import GHC.Prim

import System.IO.Unsafe
import Unsafe.Coerce

data Page
  = Page
    { getHead    :: Controller_
    , getContent :: Controller_
    }
  | Partial
    { getContent :: Controller_
    }
  deriving Eq

page :: ( IsMVC' ts ms m
        , IsMVC' ts' ms' m'
        )
     => Controller' ts ms m
     -> Controller' ts' ms' m'
     -> Page
page h b = Page (Controller_ h) (Controller_ b)

partial :: IsMVC' ts ms m
        => Controller' ts ms m
        -> Page
partial = Partial . Controller_

type IsApp' ts ms c r = (ms <: Base r, ts <. Base r, ts <=> ms, Eq r, MonadIO c)

type IsApp ms r = IsApp' ms ms IO r

type Base r = '[ State () (Router r), Evented, State () Shutdown ]

type AppKey ms r = forall ms' . ms' ~ Appended ms (Base r)  => Key (Ef.Base.As (Ef ms' IO))

type AppBuilder ts r = forall b ts' a.
                        (b ~ Base r, ts' ~ Appended ts b, a ~ Action ts' IO)
                     => Modules b a -> IO (Modules ts' a)

type AppPrimer ms r = forall ms'. ms' ~ Appended ms (Base r) => Ef ms' IO ()

type AppRouter ms r = forall ms'. ms' ~ Appended ms (Base r) => Narrative Route (Ef ms' IO) r

type AppPages ms r = forall ms'. ms' ~ Appended ms (Base r) => r -> Ef ms' IO Page

data App' ts ms c r
  = App
    { key     :: !(Key (Ef.Base.As (Ef ms c)))
    , build   :: !(Modules (Base r) (Action ts c) -> c (Modules ts (Action ts c)))
    , prime   :: !(Ef ms c ())
    , root    :: !(Maybe Txt)
    , routes  :: !(Narrative Route (Ef ms c) r)
    , pages   :: !(r -> Ef ms c Page)
    }
type App ms r = forall ms'. ms' ~ Appended ms (Base r) => App' ms' ms' IO r

instance Eq (App' ts ms c r) where
  (==) (App s _ _ _ _ _) (App s' _ _ _ _ _) =
    let Key k1 = s
        Key k2 = s'
    in case reallyUnsafePtrEquality# k1 k2 of
         1# -> True
         _  -> k1 == k2

simpleApp :: e ~ Ef (Base r) IO => Narrative Route e r -> (r -> e Page) -> App '[] r
simpleApp = App "main" return (return ()) Nothing

onRoute :: ( IsApp' ts ms IO r
           , Monad c'
           , MonadIO c'
           , With (App' ts ms IO r) (Ef ms IO) IO
           , ms' <: '[Evented]
           , e ~ Ef ms' c'
           )
         => App' ts ms IO r
         -> (r -> Ef '[Event r] e ())
         -> e (IO ())
onRoute fus rf = do
  buf <- get
  Just stopper <- demandMaybe =<< with fus (do
    crn <- getRouteSyndicate
    connect_ crn (return buf) rf
    )
  return stopper

data Carrier where Carrier :: IORef (MVCView ms m) -> Carrier

run :: forall ts ms c r. IsApp' ts ms c r => App' ts ms c r -> c ()
run app@App {..} = animator `seq` do
  q   <- newEvQueue
  ort <- getAppRoot root
  ph  <- liftIO $ create "template"
  liftIO $ append ort ph
  rt'     <- liftIO $ newIORef (MVCView (NullView (Just ph) :: View '[])
                                        (Just $ NullView $ Just ph)
                                        (Const ())
                               )
  nw :: Syndicate r   <- syndicate
  sdn :: Syndicate () <- syndicate
  built      <- build $ mkRouter nw routes
                     *:* state q
                     *:* state (Shutdown sdn)
                     *:* Empty
  (sig :: Signal ms c (Ef ms c ()),_) <- runner
  addApp key =<< unsafeConstructAs q
  (obj,_) <- Ef.Base.Object built Ef.Base.! do
    crn <- getRouteSyndicate
    connect crn $ \r -> do
      pg <- lift $ pages r
      go True ort (Carrier rt') pg
    Shutdown sdn <- get
    connect sdn $ \_ -> do
      publish serviceShutdownSyndicate ()
      publish controllerShutdownSyndicate ()
    prime
    setupRouter (Proxy :: Proxy r)
#ifdef __GHCJS__
  driverPrintExceptions ("App exception (" ++ show key ++ "): ")
#else
  driver
#endif
    q obj
  where
    go first ort (Carrier rt) p = do
      go' p
      where
        go' (Partial b'@(Controller_ b)) = do
          b <- liftIO $ do
            mb_ <- lookupController (Pure.Types.key b)
            case mb_ of
              Nothing -> do
                MVCView _ mold _ <- readIORef rt
                mounted <- newIORef (return ())
                iob <- if first then
                         mkController mounted (ClearAndAppend ort) b
                       else
                         case mold of
                           Nothing  -> mkController mounted (ClearAndAppend ort) b
                           Just old -> mkController mounted (Replace old) b
                join $ readIORef mounted
                return (Carrier $ mvcrView iob)
              Just MVCRecord {..} -> do
                MVCView _ mold _ <- readIORef rt
                rebuild (ManagedView Nothing "" [] b')
                MVCView _ mnew _ <- readIORef mvcrView
                -- for_ mnew rebuild
                if first then do
                  clear (toNode ort)
                  for_ mnew $ \new ->
                    for_ (getHost new) (addAnimation . append ort)
                else
                  case (mold,unsafeCoerce mnew) of
                    (Just old,Just new) -> void $ addAnimation $ replace old (unsafeCoerce new)
                    _                   -> return ()
                return (Carrier mvcrView)
          become $ \r -> do
            pg <- lift $ pages r
            go False ort b pg

        go' (Page hc'@(Controller_ hc) b'@(Controller_ b)) = do
          b <- liftIO $ do
            mh_ <- lookupController (Pure.Types.key hc)
            case mh_ of
              Nothing -> void $ do
                Just h <- findByTag "head"
                mounted <- newIORef (return ())
                mkController mounted (Replace (NullView (Just h))) hc
                join $ readIORef mounted
              Just MVCRecord {..} -> do
                Just h <- findByTag "head"
                MVCView _ mnew _ <- readIORef mvcrView
                for_ mnew $ \new -> do
                  rebuild new
                  void $ addAnimation $ replace (NullView $ Just h) new
            mb_ <- lookupController (Pure.Types.key b)
            case mb_ of
              Nothing -> do
                MVCView _ mold _ <- readIORef rt
                mounted <- newIORef (return ())
                cr <- flip (mkController mounted) b $
                        if first
                          then ClearAndAppend ort
                          else maybe (ClearAndAppend ort) Replace mold
                join $ readIORef mounted
                return (Carrier $ mvcrView cr)
              Just MVCRecord {..} -> do
                MVCView _ mold _ <- readIORef rt
                rebuild (ManagedView Nothing "" [] b')
                MVCView _ mnew _ <- readIORef mvcrView
                -- for_ mnew rebuild
                if first then do
                  clear (toNode ort)
                  for_ mnew $ \new ->
                    for_ (getHost new) (addAnimation . append ort)
                else
                  case (mold,unsafeCoerce mnew) of
                    (Just o,Just n) -> void $ addAnimation $ replace o n
                    _               -> return ()
                return (Carrier mvcrView)
          become $ \r -> do
            pg <- lift $ pages r
            go False ort b pg

getAppRoot :: MonadIO c => Maybe Txt -> c Node
getAppRoot mt = do
  me <- forM mt (liftIO . findById)
  case join me of
    Nothing -> do
      me <- liftIO $ findById "pure"
      case me of
        Nothing -> toNode . fromJust <$> liftIO (findByTag "body")
        Just e  -> return (toNode e)
    Just e  -> return (toNode e)

data AppNotStarted = AppNotStarted deriving Show
instance Exception AppNotStarted

instance (IsApp' ts ms c r, MonadIO c) => With (App' ts ms c r) (Ef ms c) c
  where
    using_ f = do
      mi_ <- lookupApp (Pure.App.key f)
      case mi_ of
        Just as -> return (runAs as)
        -- Nothing -> what to do here?
    with_ f m = do
      run <- using_ f
      run m
    shutdown_ f = do
      void $ with_ f $ do
        buf <- get
        Shutdown sdn <- get
        publish sdn ()
        delay 0 $ do
          deleteApp (Pure.App.key f)
          liftIO $ do
            killBuffer buf
            myThreadId >>= killThread

{-# NOINLINE appVault__ #-}
appVault__ = unsafePerformIO createVault

lookupApp :: MonadIO c => Key phantom -> c (Maybe phantom)
lookupApp = liftIO . vaultLookup appVault__

addApp :: MonadIO c => Key phantom -> phantom -> c ()
addApp k = liftIO . vaultAdd appVault__ k

deleteApp :: MonadIO c => Key phantom -> c ()
deleteApp = liftIO . vaultDelete appVault__

setupRouter :: forall ms c rTy. (Eq rTy, MonadIO c, ms <: '[Evented,State () (Router rTy)])
            => Proxy rTy -> Ef ms c (IO ())
setupRouter _ = do
  crn :: Syndicate rTy <- getRouteSyndicate
  psn <- getWindowSyndicatePreventDefault "popstate"
  ln <- getWindowSyndicate "load"
  connect ln $ \_ -> liftIO setPopped >> end
  pn  <- liftIO getPathname
  qps <- liftIO getSearch
  let p = pn <> qps
  rtr  <- getRouter
  mncr <- Route.route rtr p
  setRoute mncr
  forM_ mncr $ publish crn
  connect psn (go crn p mncr)
  where
    go crn = go'
      where
        go' p cr _ = do
          loaded <- liftIO getPopped
          when loaded $ do
            rtr <- lift getRouter
            pn  <- liftIO getPathname
            qps <- liftIO getSearch
            let p' = pn <> qps
            -- prevent recalculation with popstate on hash change
            unless (p' == p) $ do
              mncr <- lift $ Route.route rtr p'
              forM_ mncr $ \ncr ->
                when (mncr /= cr) $
                  lift $ do
                    setRoute mncr
                    publish crn ncr
              become (go' p' mncr)

goto :: (MonadIO c, With (App' ts ms IO r) (Ef ms IO) IO, ms <: '[State () (Router r)], ms' <: '[Evented])
     => App' ts ms IO r -> Txt -> r -> Ef ms' c (Promise ())
goto f rts rt = do
  pushPath rts
  with f $ do
    crn <- getRouteSyndicate
    publish crn rt
