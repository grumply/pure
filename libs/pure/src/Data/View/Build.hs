{-# LANGUAGE CPP, OverloadedStrings, RankNTypes, ScopedTypeVariables, PatternSynonyms, ViewPatterns, MagicHash, RecordWildCards, BangPatterns, LambdaCase, FlexibleContexts, TypeFamilies #-}
{-# OPTIONS_GHC -O2 #-}
module Data.View.Build (inject,prebuild,cleanup,race',suspense,suspenses,anticipation,diffDeferred,buildPlan,build) where

import Control.Concurrent (MVar,newEmptyMVar,putMVar,takeMVar,readMVar,yield,forkIO,killThread)
import Control.Exception (catch,mask,evaluate,onException,BlockedIndefinitelyOnMVar)
import Control.Monad.ST (ST,runST)
import Control.Monad.ST.Unsafe (unsafeIOToST)
import Control.Monad (void,unless,join,when,(>=>),forM_)
import Data.Coerce (coerce)
import Data.Foldable (for_,traverse_)
import Data.Function (fix)
import Data.IORef (IORef,newIORef,modifyIORef',readIORef,writeIORef)
import Data.List as List (null,reverse,filter,length)
import Data.Maybe (fromJust,isJust)
import Data.STRef (STRef,newSTRef,readSTRef,modifySTRef',writeSTRef)
import Data.Traversable (for,traverse)
import Data.Typeable (Typeable,(:~:)(..),eqT,typeRep)
import GHC.Exts (reallyUnsafePtrEquality#,isTrue#,unsafeCoerce#,Any,inline)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Coerce (unsafeCoerce)

import Data.Map (Map)
import Data.Set (Set)
import qualified Data.Map.Strict as Map (fromList,toList,insert,difference,keys,differenceWith,null,elems,mergeWithKey,mapWithKey)
import qualified Data.Set as Set (fromList,toList,insert,delete,null)

import Data.View (pattern Null,Exists,it,lazy,stateWith',Modify,put,View(..),Features(..),Listener(..),Lifecycle(..),Comp(..),Target(..),getHost,setProps,queueComponentUpdate,Ref(..),ComponentPatch(..),asProxyOf,ListenerAction(..))
import Data.Time (Time,timeout)

import Data.Animation
import Data.Idle
import Data.DOM
  (
   Element(..)
  ,Node(..)
  ,Win(..)
  ,Doc(..)
  ,IsNode(..)
  ,toJSV
  ,same

  -- node creation
  ,create
  ,createNS
  ,createFrag
  ,createText

  -- node insertion
  ,append
  ,insertAt
  ,insertBefore
  ,setInnerHTML

  -- node replacement
  ,replaceNode
  ,replaceText

  -- node removal
  ,removeNode
  ,removeNodeMaybe
  ,clearNode

  -- attributes
  ,setAttribute
  ,setAttributeNS
  ,removeAttribute
  ,removeAttributeNS

  -- properties
  ,setProperty
  ,removeProperty

  -- styles
  ,setStyle
  ,removeStyle

  -- events
  ,Evt(..)
  ,Options(..)
  ,stopPropagation
  ,preventDefault
  ,addEventListener
  ,removeEventListener

  -- node getters
  ,getDocument
  ,getWindow

  -- JSV reference equality
  ,same

  -- callbacks
#ifdef __GHCJS__
  ,asyncCallback1
  ,syncCallback1
  ,OnBlocked(..)
  ,releaseCallback
#endif
  )

import Data.Queue (Queue,newQueue,arrive,collect)

import Data.Txt (Txt)
import qualified Data.Txt as Txt (intercalate)

{-
This module is likely to eat your lunch and it is already well fed.
-}

type Plan s = STRef s [IO ()]
type DiffST s a = a -> a -> a -> ST s a
type Removals s = STRef s [View]

newPlan :: ST s (Plan s)
newPlan = newSTRef []

{-# INLINE buildPlan #-}
buildPlan :: (forall s. Plan s -> Plan s -> ST s a) -> ([IO ()],[IO ()],a)
buildPlan f = runST $ do
  p  <- newPlan
  p' <- newPlan
  a  <- f p p'
  p  <- readSTRef p
  p' <- readSTRef p'
  return (p,p',a)

amendPlan :: Plan s -> IO () -> ST s ()
amendPlan plan f = f `seq` modifySTRef' plan (f:)

runPlan :: [IO ()] -> IO ()
runPlan plan = let !p = sequence_ (List.reverse plan) in p

sync :: (MVar () -> IO a) -> IO ()
sync f = do
  barrier <- newEmptyMVar
  f barrier
  takeMVar barrier

(===) :: a -> b -> Bool
x === y = isTrue# (unsafeCoerce# reallyUnsafePtrEquality# x y)

(/==) :: a -> b -> Bool
x /== y = Prelude.not (x === y)

-- | Given a host node and a View, build and embed the View.
{-# INLINE inject #-}
inject :: IsNode e => e -> View -> IO View
inject host v = do
  mtd <- newIORef []
  v' <- build mtd (Just $ toNode host) v
  runPlan =<< readIORef mtd
  pure v'

-- | Pre-build a view for later use.
{-# INLINE prebuild #-}
prebuild :: View -> IO View
prebuild v = do
  mtd <- newIORef []
  !v' <- build mtd Nothing v
  runPlan =<< readIORef mtd
  pure (Prebuilt v')

race' :: [View] -> (Exists View => View) -> View
race' vs v = lazy run v 
  where
    run = do
      mv <- newEmptyMVar
      tids <- for vs (\v -> forkIO (prebuild v >>= \x -> mask $ \_ -> putMVar mv x `onException` cleanup x))
      v <- takeMVar mv
      for_ tids killThread
      pure v

-- | Deep suspense. Crosses component boundaries via use of `prebuild`.
-- Works across, for example, nested `lazy`s.
--
-- Suppose you have a primary request whose response dictates further nested
-- requests. With `suspense`, it is possible to apply suspense to the full 
-- view, including the secondary requests, as if it were a unified request.
--
-- In the following example, a suspense view will render after 100ms unless
-- all lazy views have resolved:
--
-- > suspense (Milliseconds 100 0) "Loading" do
-- >   lazy <some IO action producing a list> do
-- >     Div <||>
-- >       [ lazy <some IO action based on `x`>
-- >           { ... await :: <some result from the nested `lazy`> ... }
-- >       | x <- await :: <some list response from the wrapping `lazy`>
-- >       ]
--
-- To bypass deep suspense, see `fork`, which initially renders
-- a `Null` and then subsequently replaces it with the desired view. 
--
-- Only the actions of `build` are subject to suspense. The component threads
-- generated in build are forked and, therefore, not subject to suspense.
-- Therefore, suspense includes `construct`, `onMount`, and the initial 
-- `render`/nested `build`.
--
suspense :: Time -> View -> View -> View
suspense t sus = suspenses True [(t,sus)]

-- | Shallow suspense. Does not cross component boundaries. 
-- Useful for fine-grained suspense of lazily-produced existentials, like
-- those of `lazy` and `parv`.
-- 
-- Importantly, the following will not work and requires `suspense`:
--
-- > anticipation 0 "Loading" do
-- >   lazy someHttpGET do
-- >     { ... await ... }
--
-- But, this could work (assuming no nested component boundaries in the body):
--
-- > lazy someHttpGET do
-- >   anticipation 0 "Loading" do
-- >     { ... await ... }
--
anticipation :: Time -> View -> View -> View
anticipation t sus = suspenses False [(t,sus)]

-- | `suspenses` allows for a sequence of replacing transclusions at timed
-- intervals while a target `View` is being built. This approach is likely to
-- cause bugs, especially in the presence of rich/dynamic views. If `deep`, the
-- view is built with `prebuild`, if not `deep`, the view is simply forced with
-- `evaluate`.
suspenses :: Bool -> [(Time,View)] -> View -> View
suspenses deep tvs v = stateWith' (\_ -> pure) initialize it
  where
    initialize :: Modify View => IO (View,View -> IO ())
    initialize = do
      mv <- newEmptyMVar

      t1 <- 
        forkIO $ do
          flip fix tvs $ \k -> \case
            (t,v):tvs ->
              timeout t (readMVar mv) >>= \case
                Just x -> put x
                _      -> put v >> k tvs
            _ -> readMVar mv >>= put

      t2 <- 
        forkIO $ do
          void $ do
            -- Mask exceptions while building so we can correctly
            -- cleanup the prebuilt view in the case that suspense
            -- is unmounted and a killThread is sent here.
            Control.Exception.mask $ \restore -> do
              -- `prebuild` is how deep suspense is achieved. If we instead
              -- just used `evaluate`, the initialization methods of 
              -- components would not be run and suspense would only apply 
              -- to any `unsafePerformIO` or similar in the view preimage.
              -- That would still be useful inside a `lazy @a` when `await @a`
              -- is called, but it is much less interesting than the deep 
              -- suspense achieved with prebuild.
              -- 
              -- The unfortunate side-effect is that the view-building, and 
              -- side-effects within suspense, cannot be stopped.
              -- 
              -- More fundamentally, this shows that there is a need for a
              -- better mechanism for lifecycle management within pure. An
              -- approach to automatic cleanup would be quite welcome. This
              -- is an extraordinarily difficult problem with, I assume, a
              -- very simply solution - I just don't yet have the correct 
              -- perspective.
              x <- if deep then prebuild v else evaluate v
              let onE | deep = cleanup x | otherwise = pure ()
              restore (putMVar mv x) `onException` onE 

      pure (Null,const (killThread t1 >> killThread t2))

{-# NOINLINE build #-}
build :: IORef [IO ()] -> Maybe Node -> View -> IO View
build = build'

{-# INLINABLE build' #-}
build' :: IORef [IO ()] -> Maybe Node -> View -> IO View
build' mtd = start
  where
    start mparent = go
      where
        go o@(Prebuilt v) =
          case mparent of
            Nothing -> pure (Prebuilt v)
            Just p  -> do
              case getHost v of
                Just h -> do
                  append p h
                  pure o
                Nothing ->
                  error "Data.View.Build.build: Invalid Prebuilt View. No host node found."
        go o@HTMLView {..} = do
          e <- create tag
          let n = Just (toNode e)
          fs <- setFeatures mtd e features
          !cs <- traverse (start n) children
          for_ mparent (`append` e)
          return o
            { elementHost = Just e
            , features = fs
            , children = cs
            }
        go o@(ComponentView rep _ comp props) = do
          stq_   <- newIORef . Just =<< newQueue
          props_ <- newIORef undefined
          state_ <- newIORef undefined
          live_  <- newIORef undefined
          c_     <- newIORef undefined
          let cr = Ref live_ props_ state_ c_ stq_
              c  = comp cr
              ps = props cr
          writeIORef props_ ps
          writeIORef c_ c
          state1 <- construct c
          writeIORef state_ state1
          state2 <- onMount c state1
          writeIORef state_ state2
          let new = if deferred c then NullView Nothing else render c ps state2
          live <- go new
          writeIORef live_ live
          mv <- newEmptyMVar
          modifyIORef' mtd (\xs -> putMVar mv ():onMounted c:xs)
          forkIO $ newComponentThread mv cr c live new ps state2
          -- GHC doesn't allow for record update of a ComponentView here
          return (ComponentView rep (Just cr) comp props)
        go (ReactiveView _ f) = go f
        go (WeakView _ f) = go f
        go TaggedView{..} = go taggedView
        go o@TextView {..} = do
          tn <- createText content
          for_ mparent (`append` tn)
          pure o { textHost = Just tn }
        go o@NullView{} = do
          e <- create "template"
          for_ mparent (`append` e)
          pure o { elementHost = Just e }
        go o@RawView {..} = do
          e <- create tag
          setInnerHTML e content
          fs <- setFeatures mtd e features
          for_ mparent (`append` e)
          return o
            { elementHost = Just e
            , features = fs
            }
        go o@KHTMLView {..} = do
          e <- create tag
          let n = Just (toNode e)
          fs <- setFeatures mtd e features
          !cs <- traverse (traverse (start n)) keyedChildren
          for_ mparent (`append` e)
          return o
            { elementHost = Just e
            , features = fs
            , keyedChildren = cs
            }
        go o@SVGView {..} = do
          e <- createNS "http://www.w3.org/2000/svg" tag
          let n = Just (toNode e)
          setXLinks e xlinks
          fs <- setFeatures mtd e features
          !cs <- traverse (start n) children
          for_ mparent (`append` e)
          return o
            { elementHost = Just e
            , features = fs
            , children = cs
            }
        go o@KSVGView {..} = do
          e <- createNS "http://www.w3.org/2000/svg" tag
          let n = Just (toNode e)
          setXLinks e xlinks
          fs <- setFeatures mtd e features
          !cs <- traverse (traverse (start n)) keyedChildren
          for_ mparent (`append` e)
          return o
            { elementHost = Just e
            , features = fs
            , keyedChildren = cs
            }
        go o@PortalView{..} = do
          e <- create "template"
          v <- start (Just $ toNode portalDestination) portalView
          for_ mparent (`append` e)
          return o
            { portalProxy = Just e
            , portalView = v
            }

setXLinks :: Element -> Map Txt Txt -> IO ()
setXLinks e = traverse_ (uncurry (setAttributeNS e "http://www.w3.org/1999/xlink")) . Map.toList

setFeatures :: IORef [IO ()] -> Element -> Features -> IO Features
setFeatures mtd e = go
  where
    go fs@Features_ {..} = do
      setClasses e classes
      setStyles e styles
      setAttributes e attributes
      setProperties e properties
      lf <- for lifecycles (addLifecycle mtd e)
      ls <- for listeners (addListener e)
      pure fs { lifecycles = lf, listeners = ls }

setClasses :: Element -> Set Txt -> IO ()
setClasses e cs
  | Set.null cs = return ()
  | otherwise   = setClasses' e cs

setClasses' :: Element -> Set Txt -> IO ()
setClasses' e = setAttribute e "class" . Txt.intercalate " " . Set.toList . Set.delete ""

setStyles :: Element -> Map Txt Txt -> IO ()
setStyles e ss
  | Map.null ss = return ()
  | otherwise = setStyles' e ss

setStyles' :: Element -> Map Txt Txt -> IO ()
setStyles' e = traverse_ (uncurry (setStyle e)) . Map.toList

setAttributes :: Element -> Map Txt Txt -> IO ()
setAttributes e as
  | Map.null as = return ()
  | otherwise = setAttributes' e as

setAttributes' :: Element -> Map Txt Txt -> IO ()
setAttributes' e = traverse_ (uncurry (setAttribute e)) . Map.toList

setProperties :: Element -> Map Txt Txt -> IO ()
setProperties e ps
  | Map.null ps = return ()
  | otherwise = setProperties' e ps

setProperties' :: Element -> Map Txt Txt -> IO ()
setProperties' e = traverse_ (uncurry (setProperty e)) . Map.toList

addListener :: Element -> Listener -> IO Listener
addListener e f@(On n t o a _ _) = do
#ifdef __GHCJS__
    target <- case t of
                ElementTarget  -> return (toJSV e)
                WindowTarget   -> fmap toJSV getWindow
                DocumentTarget -> fmap toJSV getDocument
    (cb,update,stopper) <- do

      a_ <- newIORef a

      stopper <- newIORef undefined

      let 
        stpr = join $ readIORef stopper
        upd = writeIORef a_

      cb <- syncCallback1 ContinueAsync $ \jsv -> do
        when (preventDef o) (preventDefault jsv)
        when (stopProp o) (stopPropagation jsv)
        ListenerAction a <- readIORef a_
        a (Evt jsv target stpr)

      writeIORef stopper $ do
        removeEventListener target n cb
        releaseCallback cb

      return (cb,upd,stpr)

    addEventListener target n cb (passive o)
    return f { eventUpdate = update, eventStopper = stopper }
#else
    return f
#endif

addLifecycle :: IORef [IO ()] -> Element -> Lifecycle -> IO Lifecycle
addLifecycle mtd e (Created f _) = do
  stop <- f (toNode e)
  return (Created f stop)
addLifecycle mtd e (Mounted f _) = do
  cleanup <- newIORef (pure ())
  modifyIORef' mtd ((f (toNode e) >>= writeIORef cleanup):)
  let stop = join (readIORef cleanup)
  return (Mounted f stop)

awaitComponentPatches pq = do
  mpq <- readIORef pq
  for mpq collect `catch` \(_ :: BlockedIndefinitelyOnMVar) -> return Nothing

newComponentThread :: forall props state. MVar () -> Ref props state -> Comp props state -> View -> View -> props -> state -> IO ()
newComponentThread barrier ref@Ref {..} comp@Comp {..} = \live view props state ->
  onExecuting state >>= \st -> takeMVar barrier >> loop (deferred || state /== st) live view props st
  where
    loop :: Bool -> View -> View -> props -> state -> IO ()
    loop = loop'

    loop' :: Bool -> View -> View -> props -> state -> IO ()
    loop' !first !old !mid props state = do
      mps <- if first then pure (Just []) else awaitComponentPatches crPatchQueue
      forM_ mps (go props state [])
      where
        go :: props -> state -> [(IO (),IO (),IO ())] -> [ComponentPatch props state] -> IO ()
        go newProps newState = go1
          where
            go1 :: [(IO (),IO (),IO ())] -> [ComponentPatch props state] -> IO ()
            go1 acc [] = do
              let (wills,dids,after) = unzip3 (List.reverse acc)

              sequence_ wills

              mtd <- newIORef []

              let
                !new = render newProps newState

                sameView = mid === new

                (!plan,!plan',!new_old)
                  | first || not sameView = buildPlan $ \p p' -> diffDeferred' mtd p p' old mid new
                  | otherwise = ([],[],old)

                hasAnimations = not (List.null plan)
                hasIdleWork = not (List.null plan')
                
              mounts <- plan `seq` plan' `seq` readIORef mtd

              -- I believe there are still some fundamental problems here w.r.t. ordering.
              -- Keep an eye. Specifically, what happens when a component updates its
              -- live view via a replacement and a parent component that transcludes the
              -- component replaces it before the animation has fired? The solution may be
              -- explicit, rather than temporally-implicit/animation-implicit, ordering of
              -- views.

              writeIORef crView new_old

              when hasAnimations $ do

                -- yield to any other threads that might be able to get to
                -- this point, as well. This should be the only yield in the
                -- reconciler, with the intention that it be a hinted 
                -- synchronization point.
                yield

#ifdef __GHCJS__
                if animated then do
                  sync $ \barrier -> do
                    -- In the worst case, updating within an animation frame costs
                    -- about 20%. But there are two major benefits: 
                    --
                    -- 1. (rate-limiting) synchronizing on the animation frame 
                    --    prevents unintentional update loops from eating all CPU 
                    --    cycles
                    --
                    -- 2. (batching updates) batching updates together minimizes reflows
                    --
                    -- In most cases, the cost of an animation frame is near-
                    -- trivial.
                    --
                    -- One downside is that some updates that could happen
                    -- together end up happening over two animation frames.
                    --
                    addAnimation $ do
                      runPlan plan
                      putMVar barrier ()
                else
                  runPlan plan
#else
                runPlan plan
#endif

              runPlan plan'

              runPlan mounts

              sequence_ dids

              sequence_ after
              
              loop False new_old new newProps newState

            go1 acc (cp:cps) =
              case cp of
                Unmount -> do
                  writeIORef crPatchQueue Nothing
                  onUnmounted
                  cleanup old
                  writeIORef crView  (error "look: Component invalidated.")
                  writeIORef crProps (error "ask: Component invalidated.")
                  writeIORef crState (error "get: Component invalidated.")

                UpdateProperties newProps' -> do

                  newState'    <- onReceive newProps' newState
                  shouldUpdate <- onForce   newProps' newState'

                  let
                    writeRefs = do
                      writeIORef crProps newProps'
                      writeIORef crState newState'

                  if shouldUpdate || not (List.null acc) then

                    let
                      will = onUpdate  newProps' newState'
                      did  = onUpdated newProps  newState
                      acts = (will >> writeRefs,did,return ())
                    in
                      go newProps' newState' (acts : acc) cps

                  else do

                    writeRefs
                    go newProps' newState' acc cps

                UpdateState f -> do

                  (newState',after) <- f newProps newState
                  shouldUpdate      <- onForce newProps newState'

                  let writeRef = writeIORef crState newState'

                  if shouldUpdate || not (List.null acc) then

                    let
                      will = onUpdate  newProps newState'
                      did  = onUpdated newProps newState
                      acts = (will >> writeRef,did,after)
                    in
                      go newProps newState' (acts : acc) cps

                  else do

                    writeRef
                    go newProps newState' acc cps

cleanupLifecycle :: Lifecycle -> IO ()
cleanupLifecycle (Created _ clean) = clean
cleanupLifecycle (Mounted _ clean) = clean

cleanupListener :: Listener -> IO ()
cleanupListener (On _ _ _ _ _ stp) = stp

{-# NOINLINE cleanup #-}
cleanup :: View -> IO ()
cleanup = cleanup'

{-# INLINABLE cleanup' #-}
cleanup' :: View -> IO ()
cleanup' RawView {..} = do
  for_ (listeners features) cleanupListener
  for_ (lifecycles features) cleanupLifecycle
cleanup' HTMLView {..} = do
  for_ (listeners features) cleanupListener
  for_ (lifecycles features) cleanupLifecycle
  for_ children cleanup'
cleanup' SVGView {..} = do
  for_ (listeners features) cleanupListener
  for_ (lifecycles features) cleanupLifecycle
  for_ children cleanup'
cleanup' ComponentView {..} = do
  for_ record (`queueComponentUpdate` Unmount)
cleanup' PortalView {..} = do
  for_ (getHost portalView) (addAnimation . removeNodeMaybe)
  cleanup portalView
cleanup' KHTMLView {..} = do
  for_ (listeners features) cleanupListener
  for_ (lifecycles features) cleanupLifecycle
  for_ keyedChildren (cleanup' . snd)
cleanup' KSVGView {..} = do
  for_ (listeners features) cleanupListener
  for_ (lifecycles features) cleanupLifecycle
  for_ keyedChildren (cleanup' . snd)
cleanup' Prebuilt {..} = do
  cleanup' prebuilt
cleanup' _ = return ()

{-# NOINLINE diffDeferred #-}
diffDeferred :: forall s. IORef [IO ()] -> Plan s -> Plan s -> View -> View -> View -> ST s View
diffDeferred = diffDeferred'

diffDeferred' :: forall s. IORef [IO ()] -> Plan s -> Plan s -> View -> View -> View -> ST s View
diffDeferred' mounted plan plan' old !mid !new =
  -- sameData = isTrue# (dataToTag# mid ==# dataToTag# new)
  if mid === new then return old else
      let
          replace = do
            !new' <- unsafeIOToST (build' mounted Nothing new)
            replaceDeferred plan plan' old new'

          sameTag = tag mid === tag new
          cmpTag = sameTag || tag mid == tag new
      in
        case (mid,new) of
          (Prebuilt pb,Prebuilt pb') ->
            diffDeferred' mounted plan plan' old pb pb'

          (Prebuilt pb,_) ->
            diffDeferred' mounted plan plan' old pb new

          (ReactiveView a f,ReactiveView a' f')
            | a === a'  -> return old
            | otherwise -> diffDeferred' mounted plan plan' old f f'

          (WeakView a f,WeakView a' f')
            | a === a' -> diffDeferred' mounted plan plan' old f f'

          (TaggedView t v,TaggedView t' v')
            | t === t' || t == t' ->
              diffDeferred' mounted plan plan' old v v'

          (ComponentView rep r v p,ComponentView rep' r' v' p')
            | rep === rep' || rep == rep'
            , ComponentView _ (Just r0) _ _ <- old ->
              if p === p' then return old else do
                let r = unsafeCoerce# r0
                unsafeIOToST $ setProps r (p' r)
                return (ComponentView rep' (Just r) v' p')

          (ComponentView {},_)
            | ComponentView _ (Just r0) _ _ <- old -> do
              !new' <- unsafeIOToST (build' mounted Nothing new)
              amendPlan plan $ do
                old <- readIORef (crView r0)
                replaceNode (fromJust $ getHost old) (fromJust $ getHost new')
                void $ queueComponentUpdate r0 Unmount
              return new'

          (HTMLView{},HTMLView{})
            | cmpTag -> diffElementDeferred mounted plan plan' old mid new

          (TextView _ t,TextView _ t')
            | t === t' || t == t' -> return old
            | otherwise -> replaceTextContentDeferred plan old new

          (NullView{},NullView{}) -> return old

          (RawView{},RawView{})
            | cmpTag
            , content mid === content new || content mid == content new -> do
              !fs <- diffFeaturesDeferred (coerce $ fromJust $ getHost old) plan (features old) (features mid) (features new)
              return old { features = fs }

          (SVGView{},SVGView{})
            | cmpTag -> diffSVGElementDeferred mounted plan plan' old mid new

          (KHTMLView{},KHTMLView{})
            | cmpTag -> diffKeyedElementDeferred mounted plan plan' old mid new

          (KSVGView{},KSVGView{})
            | cmpTag -> diffSVGKeyedElementDeferred mounted plan plan' old mid new

          (PortalView{},PortalView{})
            | same (toJSV (portalDestination old)) (toJSV (portalDestination new)) -> do
              !v <- diffDeferred' mounted plan plan' (portalView old) (portalView mid) (portalView new)
              return old { portalView = v }
            | otherwise -> do
              !built@(getHost -> Just h) <- unsafeIOToST (build' mounted Nothing (portalView new))
              amendPlan plan' (cleanup' old)
              amendPlan plan $ do
                for_ (getHost (portalView old)) removeNode
                append (toNode $ portalDestination new) h
              -- Recycling the portalProxy. Might lose some reference equality here?
              return (PortalView (portalProxy old) (portalDestination new) built)

          (PortalView{},_) -> do
            amendPlan plan' (cleanup' old)
            amendPlan plan $ for_ (getHost (portalView old)) removeNode
            replace

          (_,PortalView{}) -> do
            !proxy <- unsafeIOToST (build' mounted Nothing (NullView Nothing))
            replaceDeferred plan plan' old proxy
            !built@(getHost -> Just h) <- unsafeIOToST (build' mounted Nothing (portalView new))
            amendPlan plan $ append (toNode $ portalDestination new) h
            return (PortalView (fmap coerce $ getHost proxy) (portalDestination new) built)
          
          (_,Prebuilt pb) -> 
            -- I don't have any code that touches this case. 
            -- What would it look like? What usecase?
            diffDeferred' mounted plan plan' old mid pb

          _ -> replace

diffElementDeferred :: IORef [IO ()] -> Plan s -> Plan s -> DiffST s View
diffElementDeferred mounted plan plan' old@(elementHost -> ~(Just e)) mid new = do
  !fs <- diffFeaturesDeferred e plan (features old) (features mid) (features new)
  !cs <- diffChildrenDeferred e mounted plan plan' (children old) (children mid) (children new)
  return old
    { features = fs
    , children = cs
    }

diffSVGElementDeferred :: IORef [IO ()] -> Plan s -> Plan s -> DiffST s View
diffSVGElementDeferred mounted plan plan' old@(elementHost -> ~(Just e)) mid new = do
  diffXLinksDeferred e plan (xlinks mid) (xlinks new)
  !fs <- diffFeaturesDeferred e plan (features old) (features mid) (features new)
  !cs <- diffChildrenDeferred e mounted plan plan' (children old) (children mid) (children new)
  return old
    { features = fs
    , children = cs
    }

diffFeaturesDeferred :: Element -> Plan s -> DiffST s Features
diffFeaturesDeferred e plan old !mid !new = do
  if mid === new then return old else
    diffFeaturesDeferred' e plan old mid new

diffFeaturesDeferred' :: Element -> Plan s -> DiffST s Features
diffFeaturesDeferred' e plan old mid new = do
  diffClassesDeferred    e plan (classes mid) (classes new)
  diffStylesDeferred     e plan (styles mid) (styles new)
  diffAttributesDeferred e plan (attributes mid) (attributes new)
  diffPropertiesDeferred e plan (properties mid) (properties new)
  !ls <- diffListenersDeferred e plan (listeners old) (listeners mid) (listeners new)
  return old { listeners = ls }

diffXLinksDeferred :: Element -> Plan s -> Map Txt Txt -> Map Txt Txt -> ST s ()
diffXLinksDeferred e p !mid !new =
  if mid === new then return () else
    diffXLinksDeferred' e p mid new

diffXLinksDeferred' :: Element -> Plan s -> Map Txt Txt -> Map Txt Txt -> ST s ()
diffXLinksDeferred' e p mid new = do
  let r = diffXLinkMaps mid new
  amendPlan p $! mapM_ ($ e) r

diffXLinkMaps :: Map Txt Txt -> Map Txt Txt -> Map Txt (Element -> IO ())
diffXLinkMaps = Map.mergeWithKey diff remove add
  where
      diff nm val1 val2
        | val1 === val2 || val1 == val2
        = Nothing
        | otherwise = Just $ \e -> setAttributeNS e "http://www.w3.org/1999/xlink" nm val2
      remove = Map.mapWithKey (\k _ e -> removeAttributeNS e "http://www.w3.org/1999/xlink" k)
      add = Map.mapWithKey (\k v e -> setAttributeNS e "http://www.w3.org/1999/xlink" k v)

diffClassesDeferred :: Element -> Plan s -> Set Txt -> Set Txt -> ST s ()
diffClassesDeferred e p !mid !new =
  if mid === new then return () else
    diffClassesDeferred' e p mid new

diffClassesDeferred' :: Element -> Plan s -> Set Txt -> Set Txt -> ST s ()
diffClassesDeferred' e p mid new
  | Set.null mid && Set.null new = return ()
  | Set.null new = amendPlan p (removeAttribute e "class")
  | otherwise =
    let cs = Txt.intercalate " " $ Set.toList $ Set.delete "" new
    in amendPlan p (setAttribute e "class" cs)

diffStylesDeferred :: Element -> Plan s -> Map Txt Txt -> Map Txt Txt -> ST s ()
diffStylesDeferred e p mid new =
  if mid === new then return () else
    diffStylesDeferred' e p mid new

diffStylesDeferred' :: Element -> Plan s -> Map Txt Txt -> Map Txt Txt -> ST s ()
diffStylesDeferred' e p mid new = do
  let r = diffStyleMaps mid new
  amendPlan p $! mapM_ ($! e) r

diffStyleMaps :: Map Txt Txt -> Map Txt Txt -> Map Txt (Element -> IO ())
diffStyleMaps = Map.mergeWithKey diff remove add
  where
      diff nm !val1 !val2
        | val1 === val2 || val1 == val2
        = Nothing
        | otherwise = Just (\e -> setStyle e nm val2)
      remove = Map.mapWithKey (\k _ e -> removeStyle e k)
      add = Map.mapWithKey (\k v e -> setStyle e k v)

diffAttributesDeferred :: Element -> Plan s -> Map Txt Txt -> Map Txt Txt -> ST s ()
diffAttributesDeferred e p mid new =
  if mid === new then return () else
    diffAttributesDeferred' e p mid new

diffAttributesDeferred' :: Element -> Plan s -> Map Txt Txt -> Map Txt Txt -> ST s ()
diffAttributesDeferred' e p mid new = do
  let r = diffAttributeMaps mid new
  amendPlan p $! mapM_ ($! e) r

diffAttributeMaps :: Map Txt Txt -> Map Txt Txt -> Map Txt (Element -> IO ())
diffAttributeMaps = Map.mergeWithKey diff remove add
  where
      diff nm !val1 !val2
        | val1 === val2 || val1 == val2
        = Nothing
        | otherwise = Just (\e -> setAttribute e nm val2)
      remove = Map.mapWithKey (\k _ e -> removeAttribute e k)
      add = Map.mapWithKey (\k v e -> setAttribute e k v)

diffPropertiesDeferred :: Element -> Plan s -> Map Txt Txt -> Map Txt Txt -> ST s ()
diffPropertiesDeferred e p mid new =
  if mid === new then return () else
    diffPropertiesDeferred' e p mid new

diffPropertiesDeferred' :: Element -> Plan s -> Map Txt Txt -> Map Txt Txt -> ST s ()
diffPropertiesDeferred' e p mid new = do
  let r = diffPropertyMaps mid new
  amendPlan p $! mapM_ ($! e) r

diffPropertyMaps :: Map Txt Txt -> Map Txt Txt -> Map Txt (Element -> IO ())
diffPropertyMaps = Map.mergeWithKey diff remove add
  where
      diff nm !val1 !val2
        | val1 === val2 || val1 == val2
        = Nothing
        | otherwise = Just (\e -> setProperty e nm val2)
      remove = Map.mapWithKey (\k _ e -> removeProperty e k)
      add = Map.mapWithKey (\k v e -> setProperty e k v)

addListenerDeferred :: Element -> Plan s -> Listener -> ST s Listener
addListenerDeferred e plan l@(On n t o a _ _) = do
#ifdef __GHCJS__
  (target,cb,update,stopper) <- unsafeIOToST $ do
    target <- case t of
                ElementTarget  -> return (toJSV e)
                WindowTarget   -> fmap toJSV getWindow
                DocumentTarget -> fmap toJSV getDocument
    (cb,update,stopper) <- do

            a_ <- newIORef a

            stopper <- newIORef undefined

            let 
              stpr = join $ readIORef stopper
              upd = writeIORef a_

            cb <- syncCallback1 ContinueAsync $ \jsv -> do
              when (preventDef o) (preventDefault jsv)
              when (stopProp o) (stopPropagation jsv)
              ListenerAction a <- readIORef a_
              a (Evt jsv target stpr)

            writeIORef stopper $ do
              removeEventListener target n cb
              releaseCallback cb

            return (cb,upd,stpr)

    return (target,cb,update,stopper)

  amendPlan plan (addEventListener target n cb (passive o))
  return (On n t o a update stopper)
#else
  return l
#endif

removeListenerDeferred :: Plan s -> Listener -> ST s ()
removeListenerDeferred p (On _ _ _ _ _ stp) = amendPlan p stp

diffListenersDeferred :: Element -> Plan s -> [Listener] -> [Listener] -> [Listener] -> ST s [Listener]
diffListenersDeferred e p old !mid !new =
  if mid === new then return old else
    diffListenersDeferred' e p old mid new

diffListenersDeferred' :: Element -> Plan s -> [Listener] -> [Listener] -> [Listener] -> ST s [Listener]
diffListenersDeferred' e p olds mids news =
  case (mids,news) of
    ([],[]) -> return olds

    -- 0 1+
    ([],_ ) -> do
      for news (addListenerDeferred e p)

    -- 1+ 0
    (_,[]) -> do
      for_ olds (removeListenerDeferred p)
      pure news

    -- 1+ 1+
    _ -> go olds mids news
  where
    go os !ms !ns =
      if ms === ns then return os else
        diff os ms ns
      where
        diff (old:olds) (mid:mids) (new:news) = do
          !new'  <- diffListenerDeferred e p old mid new
          !news' <- go olds mids news
          return (new' : news')

        diff olds _ [] = do
          for_ olds (removeListenerDeferred p)
          return []

        diff ~[] _ news = do
          for news (addListenerDeferred e p)

diffListenerDeferred :: Element -> Plan s -> Listener -> Listener -> Listener -> ST s Listener
diffListenerDeferred e p old !mid !new =
  if mid === new then return old else
    diffListenerDeferred' e p old mid new

diffListenerDeferred' :: Element -> Plan s -> Listener -> Listener -> Listener -> ST s Listener
diffListenerDeferred' e p old mid new
  | cmpEvent, cmpTarget, cmpOptions = do
    unless sameAction (amendPlan p ((eventUpdate old) (eventAction new)))
    pure old
  | otherwise = do
    removeListenerDeferred p old
    addListenerDeferred e p new
  where
    sameAction = eventAction mid === eventAction new

    sameEvent = eventName mid === eventName new
    cmpEvent = sameEvent || eventName mid == eventName new

    sameTarget = eventTarget mid === eventTarget new
    cmpTarget = sameTarget || eventTarget mid == eventTarget new

    sameOptions = eventOptions mid === eventOptions new
    cmpOptions = sameOptions || eventOptions mid == eventOptions new

-- styleDiff :: Element -> Map Txt Txt -> Map Txt Txt -> Map Txt (IO ())
-- styleDiff e = Map.mergeWithKey diff remove add
--   where
--     diff nm val1 val2
--       | val1 == val2           = Nothing
--       | otherwise              = Just $ setStyle e nm val2
--     remove = Map.mapWithKey (\nm  _  -> removeStyle e nm)
--     add    = Map.mapWithKey (\nm val -> setStyle e nm val)
diffChildrenDeferred :: forall s. Element -> IORef [IO ()] -> Plan s -> Plan s -> DiffST s [View]
diffChildrenDeferred e mounted plan plan' olds !mids !news =
  if mids === news then return olds else
    diffChildrenDeferred' e mounted plan plan' olds mids news

diffChildrenDeferred' :: forall s. Element -> IORef [IO ()] -> Plan s -> Plan s -> DiffST s [View]
diffChildrenDeferred' (toNode -> e) mounted plan plan' olds mids news =
  case (mids,news) of
    ([],[]) -> return olds

    -- 0 1+
    ([],_ ) -> do
      !frag <- unsafeIOToST createFrag
      let n = Just (toNode frag)
      !news' <- unsafeIOToST (traverse (build' mounted n) news)
      amendPlan plan (append e frag)
      return news'

    -- 1+ 0
    (_,[]) -> do
      amendPlan plan (clearNode e)
      amendPlan plan' $! for_ olds cleanup
      return []

    -- 1+ 1+
    _ -> go olds mids news
  where
    go os !ms !ns =
      if ms === ns then return os else
        diff os ms ns
      where
        diff (old:olds) (mid:mids) (new:news) = do
          !new'  <- diffDeferred' mounted plan plan' old mid new
          !news' <- go olds mids news
          return (new' : news')

        diff olds _ [] = do
          removeManyDeferred plan plan' olds
          return []

        diff ~[] _ news = do
          !frag <- unsafeIOToST createFrag
          let n = Just (toNode frag)
          !news' <- unsafeIOToST (for news (build' mounted n))
          amendPlan plan (append e frag)
          return news'

removeManyDeferred :: Plan s -> Plan s -> [View] -> ST s ()
removeManyDeferred plan plan' vs = do
  amendPlan plan $! for_ vs (traverse_ removeNodeMaybe . getHost)
  amendPlan plan' $! for_ vs cleanup

removeDeferred :: Plan s -> Plan s -> View -> ST s ()
removeDeferred plan plan' v = do
  for_ (getHost v) (amendPlan plan . removeNodeMaybe)
  amendPlan plan' (cleanup' v)

replaceDeferred :: Plan s -> Plan s -> View -> View -> ST s View
replaceDeferred plan plan' old new = do
  amendPlan plan $
    case getHost old of
      Nothing -> error "Expected old host in replaceDeferred; got nothing."
      Just oh ->
        case getHost new of
          Nothing -> error "Expected new host in replaceDeferred; got nothing."
          Just nh -> replaceNode oh nh
  amendPlan plan' (cleanup' old)
  return new

replaceTextContentDeferred :: Plan s -> View -> View -> ST s View
replaceTextContentDeferred plan old@(textHost -> ~(Just oh)) new = do
  amendPlan plan (oh `replaceText` content new)
  return new { textHost = Just oh }

diffSVGKeyedElementDeferred :: IORef [IO ()] -> Plan s -> Plan s -> DiffST s View
diffSVGKeyedElementDeferred mounted plan plan' old@(elementHost -> ~(Just e)) mid new = do
  !fs <- diffFeaturesDeferred e plan (features old) (features mid) (features new)
  !cs <- diffKeyedChildrenDeferred e mounted plan plan' (keyedChildren old) (keyedChildren mid) (keyedChildren new)
  diffXLinksDeferred e plan (xlinks mid) (xlinks new)
  return $ old
    { features = fs
    , keyedChildren = cs
    }

diffKeyedElementDeferred :: IORef [IO ()] -> Plan s -> Plan s -> DiffST s View
diffKeyedElementDeferred mounted plan plan' old@(elementHost -> ~(Just e)) mid new = do
  !fs <- diffFeaturesDeferred e plan (features old) (features mid) (features new)
  !cs <- diffKeyedChildrenDeferred e mounted plan plan' (keyedChildren old) (keyedChildren mid) (keyedChildren new)
  return $ old
    { features = fs
    , keyedChildren = cs
    }

-- Keyed diffing works very much like elm's virtual-dom with short-cut in the case that the children are unchanged.
-- We use a fragment in the case of a transition from 0 children to some (n /= 0) children so that the animation frame
-- only has to do one node append. We use `clearNode` for quick empty with deferred cleanup.
--
-- Note: We try to do as little work as possible in the animation frame, so lists of the same action are collapsed into a single action.
--       See, for example, 0 1+, 1+ 0, 0 0+, and cleanup
--       Remaining cases are:
--          swap where three actions are added to the plan rather than one
--          2+ 2+/insert nk0 where insert is immediately followed by diff
diffKeyedChildrenDeferred :: forall s. Element -> IORef [IO ()] -> Plan s -> Plan s -> [(Int,View)] -> [(Int,View)] -> [(Int,View)] -> ST s [(Int,View)]
diffKeyedChildrenDeferred e mounted plan plan' olds !mids !news = do
  if mids === news then return olds else
    diffKeyedChildrenDeferred' e mounted plan plan' olds mids news

diffKeyedChildrenDeferred' :: forall s. Element -> IORef [IO ()] -> Plan s -> Plan s -> [(Int,View)] -> [(Int,View)] -> [(Int,View)] -> ST s [(Int,View)]
diffKeyedChildrenDeferred' (toNode -> e) mounted plan plan' olds mids news =
  case (mids,news) of
    ([],[]) -> do
      return olds

    -- 0 1+
    ([],_ ) -> do
      !frag <- unsafeIOToST createFrag
      let n = Just (toNode frag)
      !news' <- unsafeIOToST (traverse (traverse (build' mounted n)) news)
      amendPlan plan (append e frag)
      return news'

    -- 1+ 0
    (_,[]) -> do
      amendPlan plan (clearNode e)
      amendPlan plan' $! for_ (fmap snd olds) cleanup
      return []

    -- 1+ 1+
    _ -> do
      dc       <- newSTRef mempty
      news'    <- go dc 0 olds mids news
      removals <- readSTRef dc

      -- cleanup
      -- remove them in reverse order they were added since they were
      -- diffed beginning to end and added with (_:) rather than (++[_])
      removeManyDeferred plan plan' (reverse removals)

      return news'

  where
        go :: Removals s -> Int -> DiffST s [(Int,View)]
        go _ !_ olds !mids !news
          | mids === news = pure olds

        -- Invariant: we can always infer the shape of `mids` from the shape of `olds`;
        --            mids should always be an irrefutable pattern

        -- 2+ 2+
        go dc i (o0@(ok0,old0):os1@(o1@(ok1,old1):os2)) ~(m0@(mk0,mid0):ms1@(m1@(mk1,mid1):ms2)) ns@(n0@(nk0,new0):ns1@(n1@(nk1,new1):ns2))
          | mk0 == nk0 = do
              -- diff mk0 and nk0
              n  <- dKCD_helper o0 m0 n0
              ns <- go dc (i + 1) os1 ms1 ns1
              return (n:ns)

          | mk0 == nk1 && mk1 == nk0 = do
              -- swap mk0 mk1 and diff them in turn
              let ins ~(Just _0) ~(Just _1) = amendPlan plan (insertBefore _1 _0)
              ins (getHost old0) (getHost old1)
              n0' <- dKCD_helper o1 m1 n0
              n1' <- dKCD_helper o0 m0 n1
              ns  <- go dc (i + 2) os2 ms2 ns2
              return (n0':n1':ns)

          | mk0 == nk1 = do
              -- insert nk0
              n0' <- dKCD_ins i nk0 new0
              n1' <- dKCD_helper o0 m0 n1
              ns  <- go dc (i + 2) os1 ms1 ns2
              return (n0':n1':ns)

          | otherwise = do
              -- delete m0
              modifySTRef' dc (old0:)
              go dc (i + 1) os1 ms1 ns

        -- 1 2+
        go dc i (os@[o@(ok,old)]) ~(ms@[m@(mk,mid)]) new@(n0@(nk0,new0):ns@((nk1,new1):_))
          | mk == nk0 = do
              -- diff mk and nk0
              n'  <- dKCD_helper o m n0
              ns' <- go dc (i + 1) [] [] ns
              return (n':ns')
          | mk == nk1 = do
              -- insert nk0
              n'  <- dKCD_ins i nk0 new0
              ns' <- go dc (i + 1) os ms ns
              return (n':ns')
          | otherwise = do
              -- delete mk
              modifySTRef' dc (old:)
              go dc (i + 1) [] [] new

        -- 1+ 1
        go dc i (o@(ok,old):os) ~(m@(mk,mid):ms) (ns@[n@(nk,new)])
          | mk == nk = do
              -- diff mk and nk
              n' <- dKCD_helper o m n
              ns' <- go dc (i + 1) os ms []
              return (n':ns')
          | otherwise = do
              -- delete mk
              modifySTRef' dc (old:)
              go dc (i + 1) os ms ns

        -- 0+ 0
        go dc _ olds mids [] = do
          for_ olds (modifySTRef' dc . (:) . snd)
          return []

        -- 0 0+
        go dc _ [] _ news = do
          !frag <- unsafeIOToST createFrag
          let n = Just (toNode frag)
          !news' <- unsafeIOToST (traverse (traverse (build' mounted n)) news)
          amendPlan plan (append e frag)
          return news'

        dKCD_helper :: DiffST s (Int,View)
        dKCD_helper (_,old) (_,mid) (nk,new) = do
          !new' <- diffDeferred' mounted plan plan' old mid new
          return (nk,new')

        dKCD_ins :: Int -> Int -> View -> ST s (Int,View)
        dKCD_ins i nk new = do
          let ins ~(Just a) = amendPlan plan (insertAt (coerce e) a i)
          !n' <- unsafeIOToST (build' mounted Nothing new)
          ins (getHost n')
          return (nk,n')
