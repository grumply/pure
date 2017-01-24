{-# language UndecidableInstances #-}
{-# language OverloadedStrings #-}
{-# language MagicHash #-}
{-# language CPP #-}
module Fusion (module Fusion,module Export) where

import Ef.Base as Export hiding (Object,transform,watch)
import qualified Ef.Base (Object(..))
import Ef.Reflect as Export

import Data.JSTime

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

import GHCJS.DOM.RequestAnimationFrameCallback (newRequestAnimationFrameCallbackAsync)
import GHCJS.DOM.Window (requestAnimationFrame)

import Control.Concurrent
import Data.Function
import Data.IORef
import Data.Maybe
import Data.Proxy
import GHC.Prim

import Data.Ratio
import qualified Data.HashMap.Strict as Map

import Nuclear.Atom
import Nuclear          as Export hiding (route,stop)
import qualified Nuclear
import Fusion.JS        as Export
import Nuclear.WebSocket as Export

import System.IO.Unsafe

import qualified GHCJS.DOM.Window as W
import qualified GHCJS.DOM.History as H
import qualified GHCJS.Marshal.Pure as M

__timeInMicros :: (MonadIO c)
               => c Integer
__timeInMicros = micros <$> microtime

{-# NOINLINE __startTime__ #-}
__startTime__ :: Integer
__startTime__ = unsafePerformIO __timeInMicros

hashWithStartTime :: Hashable a => a -> Int
hashWithStartTime x = hash (__startTime__,x)

type IsFusion ts ms c r =
  ( Fusion_ r <: ms
  , Fusion_ r <. ts
  , Delta (Modules ts) (Messages ms)
  , Eq r
  , MonadIO c
  )

type Fusion_ r =
  '[ State () (Router r)
   , Revent
   , State () Shutdown
   ]

data Fusion ts ms c r
  = Fusion
    { site    :: !(Key (As (Code ms c) IO))
    , root    :: !(Maybe JSText)
    , build   :: !(    Modules (Fusion_ r) (Action ts c)
                    -> c (Modules ts (Action ts c))
                  )
    , prime   :: !(Code ms c ())
    , routes  :: !(Code '[Route] (Code ms c) r)
    , pages   :: !(r -> Code ms c Page)
    }

instance Eq (Fusion ts ms c r) where
  (==) (Fusion s _ _ _ _ _) (Fusion s' _ _ _ _ _) =
    let Key k1 = s
        Key k2 = s'
    in case reallyUnsafePtrEquality# k1 k2 of
         1# -> True
         _  -> k1 == k2

type F r = Fusion (Fusion_ r) (Fusion_ r) IO r

simpleF :: Code '[Route] (Code (Fusion_ r) IO) r -> (r -> Code (Fusion_ r) IO Page) -> Fusion (Fusion_ r) (Fusion_ r) IO r
simpleF = Fusion "main" Nothing return (return ())

data Page
  = Page
    { getHead :: Atom'
    , getContent :: Atom'
    }
  | Partial
    { getContent :: Atom'
    }

page :: ( IsAtom ts ms m
        , IsAtom ts' ms' m'
        )
     => Atom ts ms m
     -> Atom ts' ms' m'
     -> Page
page h b = Page (Atom' h) (Atom' b)

partial :: IsAtom ts ms m
        => Atom ts ms m
        -> Page
partial = Partial . Atom'

onRoute :: ( IsFusion ts ms IO r
           , Monad c',  MonadIO c'
           , With (Fusion ts ms IO r) (Code ms IO) IO
           , '[Revent] <: ms'
           )
         => Fusion ts ms IO r
         -> (r -> Code '[Event r] (Code ms' c') ())
         -> Code ms' c' (Subscription ms' c' r,Periodical ms' c' r)
onRoute fus rf = do
  p <- periodical
  Just s <- subscribe p rf
  buf <- getReventBuffer
  with fus $ do
    crn <- getRouteNetwork
    joinNetwork crn p buf
  return (s,p)

goto :: ( MonadIO c
        , With (Fusion ts ms IO r) (Code ms IO) IO
        , '[State () (Router r)] <: ms
        , '[Revent] <: ms'
        )
     => Fusion ts ms IO r -> JSText -> r -> Code ms' c (Promise ())
goto f rts rt = do
  pushPath rts
  with f $ do
    crn <- getRouteNetwork
    syndicate crn rt


data Carrier where
  Carrier :: IORef (HTML ms,HTML ms,m)
          -> Carrier

run :: forall ts ms c r.
       IsFusion ts ms c r
    => Fusion ts ms c r
    -> c ()
run Fusion {..} = do
  doc        <- getDocument
  q          <- newSignalBuffer
  rt         <- getFusionRoot root
  rt'        <- liftIO $ newIORef (NullNode $ Just rt,NullNode $ Just rt,())
  nw :: Network r <- network
  sdn :: Network () <- network
  built      <- build $ mkRouter nw routes
                     *:* revent q
                     *:* state (Shutdown sdn)
                     *:* Empty
  sig :: Signal ms c (Code ms c ()) <- runner
  addFusion site (constructAs q sig)
  (obj,_) <- Ef.Base.Object built ! do
    p' <- periodical
    subscribe p' $ \r -> do
      pg <- lift $ pages r
      go doc (Carrier rt') pg
    crn <- getRouteNetwork
    joinNetwork crn p' q
    setupRouter (Proxy :: Proxy r)
    prime
  driverPrintExceptions
    ("Fusion "
     ++ show site
     ++ " blocked in eventloop; likely caused by cyclic with calls. The standard solution is a 'delay'ed call to 'demand'."
    ) q obj
  where
    go doc (Carrier rt) p = do
      go' p
      where
        go' (Partial b'@(Atom' b)) = do
          b <- liftIO $ do
            mb_ <- lookupAtom (key b)
            case mb_ of
              Nothing -> do
                (old,_,_) <- readIORef rt
                iob <- mkAtom (Just differ) Nothing b
                (new,_,_) <- readIORef iob
                replace old new
                return (Carrier iob)
              Just (_,x_) -> do
                (old,_,_) <- readIORef rt
                (new,_,_) <- readIORef x_
                rebuild b Nothing new
                replace old new
                return (Carrier x_)
          become $ \r -> do
            pg <- lift $ pages r
            go doc b pg

        go' (Page hc'@(Atom' hc) b'@(Atom' b)) = do
          b <- liftIO $ do
            mh_ <- lookupAtom (key hc)
            case mh_ of
              Nothing -> do
                Just h_ <- D.getHead doc
                let h = T.castToElement h_
                iohhm <- mkAtom (Just differ) Nothing hc
                (new,_,_) <- readIORef iohhm
                replace (NullNode $ Just h) new
              Just (_,x_) -> do
                Just h_ <- D.getHead doc
                let h = T.castToElement h_
                (new,_,_) <- readIORef x_
                rebuild hc Nothing new
                replace (NullNode $ Just h) new
            mb_ <- lookupAtom (key b)
            case mb_ of
              Nothing -> do
                (old,_,_) <- readIORef rt
                iob <- mkAtom (Just differ) Nothing b
                (new,_,_) <- readIORef iob
                replace old new
                return (Carrier iob)
              Just (_,x_) -> do
                (old,_,_) <- readIORef rt
                (new,_,_) <- readIORef x_
                rebuild b Nothing new
                replace old new
                return (Carrier x_)
          become $ \r -> do
            pg <- lift $ pages r
            go doc b pg

getFusionRoot :: (MonadIO c) => Maybe JSText -> c E.Element
getFusionRoot mt = do
  doc <- getDocument
  me <-
    case mt of
      Nothing -> D.getElementById doc ("fusion" :: JSText)
      Just n  -> D.getElementById doc n
  case me of
    Nothing -> liftIO $ getFirstElementByTagName ("body" :: JSText)
    Just e  -> return e

data FusionNotStarted = FusionNotStarted deriving Show
instance Exception FusionNotStarted

instance ( IsFusion ts ms c r
         , MonadIO c
         )
  => With (Fusion ts ms c r)
          (Code ms c)
          IO
  where
    using_ f = do
      mi_ <- lookupFusion (site f)
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
      deleteFusion (site f)

{-# NOINLINE fusionVault__ #-}
fusionVault__ = Vault (unsafePerformIO (newMVar Map.empty))

lookupFusion :: (MonadIO c)
         => Key phantom -> c (Maybe phantom)
lookupFusion = vaultLookup fusionVault__

addFusion :: (MonadIO c)
      => Key phantom -> phantom -> c ()
addFusion = vaultAdd fusionVault__

deleteFusion :: (MonadIO c)
             => Key phantom -> c ()
deleteFusion = vaultDelete fusionVault__

setupRouter :: forall ms c routeType.
               (Eq routeType, MonadIO c, '[Revent,State () (Router routeType)] <: ms)
            => Proxy routeType
            -> Code ms c (Promise (Behavior ms c PSE.PopStateEvent))
setupRouter _ = do
  buf <- getReventBuffer
  crn :: Network routeType <- getRouteNetwork
  pss <- getWindowSignalPreventDefault W.popState
  win <- getWindow
  Just loc <- liftIO $ W.getLocation win
  pn <- liftIO $ L.getPathname loc
  qps <- liftIO $ L.getSearch loc
  let p = pn <> qps
  rtr <- getRouter
  mncr <- Nuclear.route rtr p
  setRoute mncr
  forM_ mncr $ syndicate crn
  snd <$> delay 500000 (behavior pss $ go buf crn loc p mncr)
  where
    go buf crn loc = go'
      where
        go' p cr _ = do
          rtr <- lift getRouter
          pn <- liftIO $ L.getPathname loc
          qps <- liftIO $ L.getSearch loc
          let p' = pn <> qps
          -- prevent recalculation with popstate on hash change
          unless (p' == p) $ do
            mncr <- lift $ Nuclear.route rtr p'
            forM_ mncr $ \ncr ->
              when (mncr /= cr) $ do
                lift $ do
                  setRoute mncr
                  syndicate crn ncr
            become (go' p' mncr)

-- Note that this /should not/ be called within the first 500 milliseconds
-- of the application loading or it may be ignored; this is to work around
-- a browser disparity in the triggering of popstate events on load.
route :: (MonadIO c)
      => JSText -> c ()
route rt = do
  pushPath rt
  liftIO triggerPopstate_js

pushPath :: (MonadIO c)
        => JSText -> c ()
pushPath pth = do
  win <- getWindow
  liftIO $ do
    Just hist <- W.getHistory win
    H.pushState hist (M.pToJSVal (0 :: Int)) (mempty :: JSText) pth

