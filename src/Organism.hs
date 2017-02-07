{-# language UndecidableInstances #-}
{-# language OverloadedStrings #-}
{-# language MagicHash #-}
{-# language CPP #-}
module Organism (module Organism,module Export) where

import Ef.Base as Export hiding (transform,watch,construct)
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

import Atomic.Construct
import Atomic.Mediator hiding (key)
import Atomic          as Export hiding (stop,accept)
import qualified Atomic
import Atomic.WebSocket as Export hiding (LazyByteString)
import qualified Atomic.Route as Route

import System.IO.Unsafe
import Unsafe.Coerce

#ifdef __GHCJS__
foreign import javascript unsafe
  "console.log($1);console.log($2);"
  printAny_js :: Txt -> T.JSVal -> IO ()

debug :: (MonadIO c) => Txt -> a -> c ()
debug label a = liftIO $ printAny_js label $ unsafeCoerce (unsafeCoerce a :: GHC.Prim.Any)
#endif

__timeInMicros :: (MonadIO c)
               => c Integer
__timeInMicros = getMicros <$> micros

{-# NOINLINE __startTime__ #-}
__startTime__ :: Integer
__startTime__ = unsafePerformIO __timeInMicros

hashWithStartTime :: Hashable a => a -> Int
hashWithStartTime x = hash (__startTime__,x)

type IsOrganism' ts ms c r =
  ( OrganismBase r <: ms
  , OrganismBase r <. ts
  , Delta (Modules ts) (Messages ms)
  , Eq r
  , MonadIO c
  )
type IsOrganism ms r = IsOrganism' ms ms IO r

type OrganismBase r =
  '[ State () (Router r)
   , Revent
   , State () Shutdown
   ]

type OrganismKey' ms c = Key (As (Code ms c) c)
type OrganismKey ms r = OrganismKey' (Appended ms (OrganismBase r)) IO
type OrganismBuilder' ts c r = Modules (OrganismBase r) (Action ts c) -> c (Modules ts (Action ts c))
type OrganismBuilder ts r = OrganismBuilder' (Appended ts (OrganismBase r)) IO r
type OrganismPrimer' ms c = Code ms c ()
type OrganismPrimer ms r = OrganismPrimer' (Appended ms (OrganismBase r)) IO
type OrganismRouter' ms c r = Code '[Route] (Code ms c) r
type OrganismRouter ms r = OrganismRouter' (Appended ms (OrganismBase r)) IO r
type OrganismPages' ms c r = r -> Code ms c System
type OrganismPages ms r = OrganismPages' (Appended ms (OrganismBase r)) IO r
type OrganismRoot = Maybe Txt
type Organism ms r = Organism' (Appended ms (OrganismBase r)) (Appended ms (OrganismBase r)) IO r

data Organism' ts ms c r
  = Organism
    { site    :: !(OrganismKey' ms c)
    , root    :: !(OrganismRoot)
    , build   :: !(OrganismBuilder' ts c r)
    , prime   :: !(OrganismPrimer' ms c)
    , routes  :: !(OrganismRouter' ms c r)
    , pages   :: !(OrganismPages' ms c r)
    }

instance Eq (Organism' ts ms c r) where
  (==) (Organism s _ _ _ _ _) (Organism s' _ _ _ _ _) =
    let Key k1 = s
        Key k2 = s'
    in case reallyUnsafePtrEquality# k1 k2 of
         1# -> True
         _  -> k1 == k2

simpleOrganism :: Code '[Route] (Code (OrganismBase r) IO) r -> (r -> Code (OrganismBase r) IO System) -> Organism '[] r
simpleOrganism = Organism "main" Nothing return (return ())

onRoute :: ( IsOrganism' ts ms IO r
           , Monad c',  MonadIO c'
           , With (Organism' ts ms IO r) (Code ms IO) IO
           , '[Revent] <: ms'
           )
         => Organism' ts ms IO r
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
       IsOrganism' ts ms c r
    => Organism' ts ms c r
    -> c ()
run Organism {..} = do
  doc     <- getDocument
  q       <- newSignalBuffer
  ort     <- getOrganismRoot root
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
  addOrganism site (constructAs q sig)
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
    ("Organism "
     ++ show site
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
        go' (Subsystem b'@(Construct' b)) = do
          b <- liftIO $ do
            mb_ <- lookupConstruct (key b)
            case mb_ of
              Nothing -> do
                (old,_,_) <- readIORef rt
                iob_ <- if first then
                          mkConstruct (ClearAndAppend ort) b
                        else
                          mkConstruct (Replace old) b
                iob <- takeMVar iob_
                return (Carrier iob)
              Just (_,x_) -> do
                (old,_,_) <- readIORef rt
                (new,_,_) <- readIORef x_
                rebuild b Nothing new
                when first $ do
                  clearNode (Just (toNode ort))
                  forM_ (getNode old) (appendChild ort)
                replace old new
                return (Carrier x_)
          become $ \r -> do
            pg <- lift $ pages r
            go False ort doc b pg

        go' (System hc'@(Construct' hc) b'@(Construct' b)) = do
          b <- liftIO $ do
            mh_ <- lookupConstruct (key hc)
            case mh_ of
              Nothing -> void $ do
#ifdef __GHCJS__
                Just h_ <- D.getHead doc
                let h = T.castToElement h_
#else
                let h = ()
#endif
                mkConstruct (Replace (NullAtom (Just h))) hc
              Just (_,x_) -> do
#ifdef __GHCJS__
                Just h_ <- D.getHead doc
                let h = T.castToElement h_
#else
                let h = ()
#endif
                (new,_,_) <- readIORef x_
                rebuild hc Nothing new
                replace (NullAtom $ Just h) new
            mb_ <- lookupConstruct (key b)
            case mb_ of
              Nothing -> do
                (old,_,_) <- readIORef rt
                iob_ <- if first then
                          mkConstruct (ClearAndAppend ort) b
                        else do
                          mkConstruct (Replace old) b
                iob <- takeMVar iob_
                return (Carrier iob)
              Just (_,x_) -> do
                (old,_,_) <- readIORef rt
                (new,_,_) <- readIORef x_
                rebuild b Nothing new
                when first $ do
                  clearNode (Just (toNode ort))
                  forM_ (getNode old) (appendChild ort)
                replace old new
                return (Carrier x_)
          become $ \r -> do
            pg <- lift $ pages r
            go False ort doc b pg

getOrganismRoot :: (MonadIO c) => Maybe Txt -> c ENode
getOrganismRoot mt = do
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

data OrganismNotStarted = OrganismNotStarted deriving Show
instance Exception OrganismNotStarted

instance ( IsOrganism' ts ms c r
         , MonadIO c
         )
  => With (Organism' ts ms c r)
          (Code ms c)
          c
  where
    using_ f = do
      mi_ <- lookupOrganism (site f)
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
      deleteOrganism (site f)

{-# NOINLINE organismVault__ #-}
organismVault__ = Vault (unsafePerformIO (newMVar Map.empty))

lookupOrganism :: (MonadIO c)
         => Key phantom -> c (Maybe phantom)
lookupOrganism = vaultLookup organismVault__

addOrganism :: (MonadIO c)
      => Key phantom -> phantom -> c ()
addOrganism = vaultAdd organismVault__

deleteOrganism :: (MonadIO c)
             => Key phantom -> c ()
deleteOrganism = vaultDelete organismVault__

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
            become (go' p' mncr)

goto :: ( MonadIO c
        , With (Organism' ts ms IO r) (Code ms IO) IO
        , '[State () (Router r)] <: ms
        , '[Revent] <: ms'
        )
     => Organism' ts ms IO r -> Txt -> r -> Code ms' c (Promise ())
goto f rts rt = do
  pushPath rts
  with f $ do
    crn <- getRouteNetwork
    syndicate crn rt

