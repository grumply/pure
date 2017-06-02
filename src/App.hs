{-# language UndecidableInstances #-}
{-# language OverloadedStrings #-}
{-# language MagicHash #-}
{-# language CPP #-}
module App (module App,module Export) where

import Ef.Base as Export hiding (As,Index,transform,watch,construct,uncons,distribute,embed,observe,End,Nat(..))
import qualified Ef.Base
import Ef.Reflect as Export
import Prelude as Export hiding (all,exponent,div,head,span,tan,lookup,reverse)

import Data.Millis
import Data.Txt (Txt)
import qualified Data.Txt as Txt
import qualified Data.JSON as JSON

#ifdef __GHCJS__
import qualified GHCJS.DOM.Window as W
import qualified GHCJS.DOM.History as H
import qualified GHCJS.Marshal.Pure as M
import qualified GHCJS.DOM.Location as L
import qualified GHCJS.DOM.PopStateEvent as PSE
import qualified GHCJS.DOM.Document as D
import qualified GHCJS.DOM.EventM as Ev
import qualified GHCJS.DOM.EventTargetClosures as Ev
import qualified GHCJS.DOM.Element as E
import qualified GHCJS.DOM.Node as N
import qualified GHCJS.DOM.Types as T
import qualified GHCJS.Types as T
import GHCJS.DOM.RequestAnimationFrameCallback (newRequestAnimationFrameCallbackAsync)
import GHCJS.DOM.Window (requestAnimationFrame)
#endif

import Control.Concurrent
import Data.Function
import Data.IORef
import Data.Maybe
import Data.Monoid
import Data.Proxy
import GHC.Prim

import Data.Ratio
import qualified Data.HashMap.Strict as Map

import Atomic.Component hiding (Base,key)
import qualified Atomic.Component as Component
import Atomic.Service hiding (Base,key)
import Atomic          as Export hiding (stop,accept)
import qualified Atomic
import Atomic.WebSocket as Export hiding (LazyByteString)
import qualified Atomic.Route as Route

import System.IO.Unsafe
import Unsafe.Coerce

__timeInMicros :: (MonadIO c)
               => c Integer
__timeInMicros = getMicros <$> micros

{-# NOINLINE __startTime__ #-}
__startTime__ :: Integer
__startTime__ = unsafePerformIO __timeInMicros

hashWithStartTime :: Hashable a => a -> Int
hashWithStartTime x = hash (__startTime__,x)

type IsApp' ts ms c r = (Base r <: ms, Base r <. ts, Delta (Modules ts) (Messages ms), Eq r, MonadIO c)
type IsApp ms r = IsApp' ms ms IO r

type Base r = '[ State () (Router r), Evented, State () Shutdown ]

type AppKey ms r = Key (Ef.Base.As (Ef (Appended ms (Base r)) IO))
type AppBuilder ts r = Modules (Base r) (Action (Appended ts (Base r)) IO) -> IO (Modules (Appended ts (Base r)) (Action (Appended ts (Base r)) IO))
type AppPrimer ms r = Ef (Appended ms (Base r)) IO ()
type AppRouter ms r = Ef '[Route] (Ef (Appended ms (Base r)) IO) r
type AppPages ms r = r -> Ef (Appended ms (Base r)) IO System

data App' ts ms c r
  = App
    { key     :: !(Key (Ef.Base.As (Ef ms c)))
    , build   :: !(Modules (Base r) (Action ts c) -> c (Modules ts (Action ts c)))
    , prime   :: !(Ef ms c ())
    , root    :: !(Maybe Txt)
    , routes  :: !(Ef '[Route] (Ef ms c) r)
    , pages   :: !(r -> Ef ms c System)
    }
type App ms r = App' (Appended ms (Base r)) (Appended ms (Base r)) IO r

instance Eq (App' ts ms c r) where
  (==) (App s _ _ _ _ _) (App s' _ _ _ _ _) =
    let Key k1 = s
        Key k2 = s'
    in case reallyUnsafePtrEquality# k1 k2 of
         1# -> True
         _  -> k1 == k2

simpleApp :: Ef '[Route] (Ef (Base r) IO) r -> (r -> Ef (Base r) IO System) -> App '[] r
simpleApp = App "main" return (return ()) Nothing

onRoute :: ( IsApp' ts ms IO r
           , Monad c',  MonadIO c'
           , With (App' ts ms IO r) (Ef ms IO) IO
           , '[Evented] <: ms'
           )
         => App' ts ms IO r
         -> (r -> Ef '[Event r] (Ef ms' c') ())
         -> Ef ms' c' (IO ())
onRoute fus rf = do
  buf <- get
  Just stopper <- demandMaybe =<< with fus (do
    crn <- getRouteSyndicate
    connect_ crn (return buf) rf
    )
  return stopper

data Carrier where Carrier :: IORef (ControllerView ms m) -> Carrier

run :: forall ts ms c r.
       IsApp' ts ms c r
    => App' ts ms c r
    -> c ()
run app@App {..} = do
  doc     <- getDocument
  q       <- newEvQueue
  ort     <- getAppRoot root
  Just ph <- liftIO $ createElement doc "template"
  liftIO $ appendChild ort ph
  rt'     <- liftIO $ newIORef (ControllerView (NullHTML (Just ph) :: View '[])
                                            (NullHTML $ Just ph)
                                            (Const ())
                                            True
                               )
  nw :: Syndicate r   <- syndicate
  sdn :: Syndicate () <- syndicate
  built      <- build $ mkRouter nw routes
                     *:* state q
                     *:* state (Shutdown sdn)
                     *:* Empty
  (sig :: Signal ms c (Ef ms c ()),_) <- runner
  addApp key =<< unsafeConstructAs q
  (obj,_) <- Ef.Base.Object built ! do
    crn <- getRouteSyndicate
    connect crn $ \r -> do
      pg <- lift $ pages r
      go True ort doc (Carrier rt') pg
    Shutdown sdn <- get
    connect sdn $ \_ -> do
      publish mediatorShutdownSyndicate ()
      publish constructShutdownSyndicate ()
    prime
    setupRouter (Proxy :: Proxy r)
#ifdef __GHCJS__
  driverPrintExceptions
    ("App "
     ++ show key
     ++ " blocked in eventloop; likely caused by cyclic with calls. The standard solution is a 'delay'ed call to 'demand'. "
    )
#else
  driver
#endif
    q obj
  where
    go first ort doc (Carrier rt) p = do
      go' p
      where
        go' (Subsystem b'@(Controller' b)) = do
          b <- liftIO $ do
            mb_ <- lookupController (Component.key b)
            case mb_ of
              Nothing -> do
                ControllerView _ old _ _ <- readIORef rt
                iob <- if first then
                         mkController (ClearAndAppend ort) b
                       else
                         mkController (Replace old) b
                return (Carrier $ crView iob)
              Just ControllerRecord {..} -> do
                ControllerView _ old _ _ <- readIORef rt
                ControllerView _ new _ _ <- readIORef crView
                rebuild new
                if first then do
                  clearNode (Just (toNode ort))
                  mn <- getNode new
                  forM_ mn (appendChild ort)
                else
                  replace old new
                return (Carrier crView)
          become $ \r -> do
            pg <- lift $ pages r
            go False ort doc b pg

        go' (System hc'@(Controller' hc) b'@(Controller' b)) = do
          b <- liftIO $ do
            mh_ <- lookupController (Component.key hc)
            case mh_ of
              Nothing -> void $ do
#ifdef __GHCJS__
                Just h_ <- D.getHead doc
                let h = T.castToElement h_
#else
                let h = ()
#endif
                mkController (Replace (NullHTML (Just h))) hc
              Just ControllerRecord {..} -> do
#ifdef __GHCJS__
                Just h_ <- D.getHead doc
                let h = T.castToElement h_
#else
                let h = ()
#endif
                ControllerView _ new _ _ <- readIORef crView
                rebuild new
                replace (NullHTML $ Just h) new
            mb_ <- lookupController (Component.key b)
            case mb_ of
              Nothing -> do
                ControllerView _ old _ _ <- readIORef rt
                cr <- if first then
                        mkController (ClearAndAppend ort) b
                      else
                        mkController (Replace old) b
                return (Carrier $ crView cr)
              Just ControllerRecord {..} -> do
                ControllerView _ old _ _ <- readIORef rt
                ControllerView _ new _ _ <- readIORef crView
                rebuild new
                if first then do
                  clearNode (Just (toNode ort))
                  mn <- getNode new
                  forM_ mn (appendChild ort)
                else
                  replace old new
                return (Carrier crView)
          become $ \r -> do
            pg <- lift $ pages r
            go False ort doc b pg

getAppRoot :: (MonadIO c) => Maybe Txt -> c ENode
getAppRoot mt = do
  doc <- getDocument
#ifdef __GHCJS__
  me <-
    case mt of
      Nothing -> D.getElementById doc ("atomic" :: Txt)
      Just n  -> D.getElementById doc n
  case me of
    Nothing -> liftIO $ getFirstElementByTagName ("body" :: Txt)
    Just e  -> return e
#else
  return ()
#endif

data AppNotStarted = AppNotStarted deriving Show
instance Exception AppNotStarted

instance ( IsApp' ts ms c r
         , MonadIO c
         )
  => With (App' ts ms c r)
          (Ef ms c)
          c
  where
    using_ f = do
      mi_ <- lookupApp (key f)
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
          deleteApp (key f)
          liftIO $ do
            killBuffer buf
            myThreadId >>= killThread

{-# NOINLINE organismVault__ #-}
organismVault__ = Vault (unsafePerformIO (newMVar Map.empty))

lookupApp :: (MonadIO c)
         => Key phantom -> c (Maybe phantom)
lookupApp = vaultLookup organismVault__

addApp :: (MonadIO c)
      => Key phantom -> phantom -> c ()
addApp = vaultAdd organismVault__

deleteApp :: (MonadIO c)
             => Key phantom -> c ()
deleteApp = vaultDelete organismVault__

setupRouter :: forall ms c routeType.
               (Eq routeType, MonadIO c, '[Evented,State () (Router routeType)] <: ms)
            => Proxy routeType
            -> Ef ms c (IO (),Promise (IO ()))
setupRouter _ = do
  crn :: Syndicate routeType <- getRouteSyndicate
  psn <- getWindowSyndicatePreventDefault popstate
  loc <- getLocation
  pn  <- getPathname
  qps <- getSearch
  let p = pn <> qps
  rtr  <- getRouter
  mncr <- Route.route rtr p
  setRoute mncr
  forM_ mncr $ publish crn
  delay 500000 (connect psn (go crn loc p mncr))
  where
    go crn loc = go'
      where
        go' p cr _ = do
          rtr <- lift getRouter
          pn  <- getPathname
          qps <- getSearch
          let p' = pn <> qps
          -- prevent recalculation with popstate on hash change
          unless (p' == p) $ do
            mncr <- lift $ Route.route rtr p'
            forM_ mncr $ \ncr ->
              when (mncr /= cr) $ do
                lift $ do
                  setRoute mncr
                  publish crn ncr

goto :: ( MonadIO c
        , With (App' ts ms IO r) (Ef ms IO) IO
        , '[State () (Router r)] <: ms
        , '[Evented] <: ms'
        )
     => App' ts ms IO r -> Txt -> r -> Ef ms' c (Promise ())
goto f rts rt = do
  pushPath rts
  with f $ do
    crn <- getRouteSyndicate
    publish crn rt

