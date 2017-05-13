{-# language UndecidableInstances #-}
{-# language OverloadedStrings #-}
{-# language MagicHash #-}
{-# language CPP #-}
module App (module App,module Export) where

import Ef.Base as Export hiding (As,Index,transform,watch,construct,uncons,distribute,embed,observe)
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

type Base r = '[ State () (Router r), Revent, State () Shutdown ]

type AppKey ms r = Key (Ef.Base.As (Code (Appended ms (Base r)) IO) IO)
type AppBuilder ts r = Modules (Base r) (Action (Appended ts (Base r)) IO) -> IO (Modules (Appended ts (Base r)) (Action (Appended ts (Base r)) IO))
type AppPrimer ms r = Code (Appended ms (Base r)) IO ()
type AppRouter ms r = Code '[Route] (Code (Appended ms (Base r)) IO) r
type AppPages ms r = r -> Code (Appended ms (Base r)) IO System

data App' ts ms c r
  = App
    { key     :: !(Key (Ef.Base.As (Code ms c) c))
    , build   :: !(Modules (Base r) (Action ts c) -> c (Modules ts (Action ts c)))
    , prime   :: !(Code ms c ())
    , root    :: !(Maybe Txt)
    , routes  :: !(Code '[Route] (Code ms c) r)
    , pages   :: !(r -> Code ms c System)
    }
type App ms r = App' (Appended ms (Base r)) (Appended ms (Base r)) IO r

instance Eq (App' ts ms c r) where
  (==) (App s _ _ _ _ _) (App s' _ _ _ _ _) =
    let Key k1 = s
        Key k2 = s'
    in case reallyUnsafePtrEquality# k1 k2 of
         1# -> True
         _  -> k1 == k2

simpleApp :: Code '[Route] (Code (Base r) IO) r -> (r -> Code (Base r) IO System) -> App '[] r
simpleApp = App "main" return (return ()) Nothing

onRoute :: ( IsApp' ts ms IO r
           , Monad c',  MonadIO c'
           , With (App' ts ms IO r) (Code ms IO) IO
           , '[Revent] <: ms'
           )
         => App' ts ms IO r
         -> (r -> Code '[Event r] (Code ms' c') ())
         -> Code ms' c' (IO ())
onRoute fus rf = do
  p <- periodical
  Just s <- subscribe p rf
  buf <- getReventBuffer
  Just leaveNW <- demandMaybe =<< with fus (do
    crn <- getRouteNetwork
    joinNetwork crn p buf
    return (leaveNetwork crn p))
  return (stop s >> leaveNW)

data Carrier where
  Carrier :: IORef (Atom (Code ms IO ()),Atom (Code ms IO ()),m)
          -> Carrier

run :: forall ts ms c r.
       IsApp' ts ms c r
    => App' ts ms c r
    -> c ()
run App {..} = do
  doc     <- getDocument
  q       <- newSignalBuffer
  ort     <- getAppRoot root
  Just ph <- liftIO $ createElement doc "template"
  liftIO $ appendChild ort ph
  rt'     <- liftIO $ newIORef (NullAtom $ Just ph,NullAtom $ Just ph,())
  nw :: Network r   <- network
  sdn :: Network () <- network
  built      <- build $ mkRouter nw routes
                     *:* revent q
                     *:* state (Shutdown sdn)
                     *:* Empty
  sig :: Signal ms c (Code ms c ()) <- runner
  addApp key (constructAs q sig)
  (obj,_) <- Ef.Base.Object built ! do
    p' <- periodical
    subscribe p' $ \r -> do
      pg <- lift $ pages r
      go True ort doc (Carrier rt') pg
    crn <- getRouteNetwork
    joinNetwork crn p' q
    onSelfShutdown $ do
      syndicate mediatorShutdownNetwork ()
      syndicate constructShutdownNetwork ()
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
        go' (Subsystem b'@(Component' b)) = do
          b <- liftIO $ do
            mb_ <- lookupComponent (Component.key b)
            case mb_ of
              Nothing -> do
                (old,_,_) <- readIORef rt
                iob <- if first then
                         mkComponent (ClearAndAppend ort) b
                       else
                         mkComponent (Replace old) b
                return (Carrier iob)
              Just (_,x_) -> do
                (old,_,_) <- readIORef rt
                (new,_,_) <- readIORef x_
                rebuild new
                if first then do
                  clearNode (Just (toNode ort))
                  mn <- getNode new
                  forM_ mn (appendChild ort)
                else
                  replace old new
                return (Carrier x_)
          become $ \r -> do
            pg <- lift $ pages r
            go False ort doc b pg

        go' (System hc'@(Component' hc) b'@(Component' b)) = do
          b <- liftIO $ do
            mh_ <- lookupComponent (Component.key hc)
            case mh_ of
              Nothing -> void $ do
#ifdef __GHCJS__
                Just h_ <- D.getHead doc
                let h = T.castToElement h_
#else
                let h = ()
#endif
                mkComponent (Replace (NullAtom (Just h))) hc
              Just (_,x_) -> do
#ifdef __GHCJS__
                Just h_ <- D.getHead doc
                let h = T.castToElement h_
#else
                let h = ()
#endif
                (new,_,_) <- readIORef x_
                rebuild new
                replace (NullAtom $ Just h) new
            mb_ <- lookupComponent (Component.key b)
            case mb_ of
              Nothing -> do
                (old,_,_) <- readIORef rt
                iob <- if first then
                         mkComponent (ClearAndAppend ort) b
                       else do
                         mkComponent (Replace old) b
                return (Carrier iob)
              Just (_,x_) -> do
                (old,_,_) <- readIORef rt
                (new,_,_) <- readIORef x_
                rebuild new
                if first then do
                  clearNode (Just (toNode ort))
                  mn <- getNode new
                  forM_ mn (appendChild ort)
                else
                  replace old new
                return (Carrier x_)
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
          (Code ms c)
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
      with_ f $ do
        buf <- getReventBuffer
        Shutdown sdn <- get
        syndicate sdn ()
        liftIO $ do
          killBuffer buf
          myThreadId >>= killThread
      deleteApp (key f)

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
               (Eq routeType, MonadIO c, '[Revent,State () (Router routeType)] <: ms)
            => Proxy routeType
            -> Code ms c (IO (),Promise (IO ()))
setupRouter _ = do
  crn :: Network routeType <- getRouteNetwork
  psn <- getWindowNetworkPreventDefault popstate
  loc <- getLocation
  pn  <- getPathname
  qps <- getSearch
  let p = pn <> qps
  rtr  <- getRouter
  mncr <- Route.route rtr p
  setRoute mncr
  forM_ mncr $ syndicate crn
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
                  syndicate crn ncr
           

goto :: ( MonadIO c
        , With (App' ts ms IO r) (Code ms IO) IO
        , '[State () (Router r)] <: ms
        , '[Revent] <: ms'
        )
     => App' ts ms IO r -> Txt -> r -> Code ms' c (Promise ())
goto f rts rt = do
  pushPath rts
  with f $ do
    crn <- getRouteNetwork
    syndicate crn rt

