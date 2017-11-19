{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}
module Pure.DOM where

import Ef.Base hiding (Construct,Updated,Update,embed)

import Pure.Data hiding (append,replace)
import Pure.Data.JSV
import Pure.Types hiding (build)
import Pure.Lifted
import qualified Pure.Types as Types

import Control.Arrow
import Control.Concurrent
import Data.Coerce
import Data.IORef
import Data.List as List
import Data.Maybe
import Data.Proxy
import Data.Traversable
import Data.Foldable
import Data.Typeable

import GHC.Exts hiding (build)
import GHC.Prim

import qualified Data.HashMap.Lazy as HM

import System.IO.Unsafe
import Unsafe.Coerce

#ifdef __GHCJS__
import GHCJS.Foreign.Callback as CB
import GHCJS.Marshal.Pure
#endif

import qualified Data.Map.Strict as M

import Debug.Trace

import Control.Monad.ST as Export
import Control.Monad.ST.Unsafe as Export
import Data.STRef as Export

type P = [ IO () ]
type Plan s = STRef s P

{-# INLINE newPlan #-}
newPlan :: ST s (Plan s)
newPlan = newSTRef []

{-# INLINE buildPlan #-}
buildPlan :: (forall s. Plan s -> ST s a) -> (P,a)
buildPlan f = runST $ do
  p <- newPlan
  a <- f p
  p <- readSTRef p
  return (p,a)

{-# INLINE amendPlan #-}
amendPlan :: Plan s -> IO () -> ST s ()
amendPlan plan !f = modifySTRef' plan (f:)

{-# INLINE runPlan #-}
runPlan :: P -> IO ()
runPlan = sequence_ . reverse

type Diff a = a -> a -> a -> IO a
type Diff' s a = a -> a -> a -> ST s a

build f mounted mparent (NullView _) = do
  element <- create "template"
  for_ mparent (`append` element)
  return $ NullView (Just element)

build f mounted mparent (TextView _ t) = do
  tn <- createText t
  for_ mparent (`append` tn)
  return $ TextView (Just tn) t

build f mounted mparent (HTMLView _ t fs cs) = do
  e   <- create t
  let n = Just (toNode e)
  fs' <- for fs (addFeature f e)
  cs' <- for cs (build f mounted n)
  for_ mparent (`append` e)
  return $ HTMLView (Just e) t fs' cs'

build f mounted mparent (KHTMLView _ t fs cs _) = do
  e   <- create t
  let n = Just (toNode e)
  fs' <- for fs (addFeature f e)
  cs' <- for cs (traverse (build f mounted n))
  for_ mparent (`append` e)
  return $ KHTMLView (Just e) t fs' cs' $ HM.fromList cs'

build f mounted mparent (SomeView r) = build f mounted mparent (render r)

build f mounted mparent (RawView _ t fs c) = do
  e   <- create t
  setInnerHTML e c
  let n = Just (toNode e)
  fs' <- for fs (addFeature f e)
  for_ mparent (`append` e)
  return $ RawView n t fs' c

build f mounted mparent (SVGView _ t fs cs) = do
  e   <- createNS "http://www.w3.org/2000/svg" t
  let n = Just (toNode e)
  fs' <- for fs (addFeature f e)
  cs' <- for cs (build f mounted n)
  for_ mparent (`append` e)
  return $ SVGView (Just e) t fs' cs'

build f mounted mparent (KSVGView _ t fs cs _) = do
  e   <- createNS "http://www.w3.org/2000/svg" t
  let n = Just (toNode e)
  fs' <- for fs (addFeature f e)
  cs' <- for cs (traverse (build f mounted n))
  for_ mparent (`append` e)
  return $ KSVGView (Just e) t fs' cs' $ HM.fromList cs'

build f mounted mparent c@ComponentView{} = buildComponentView f mounted mparent c

build f mounted mparent m@ManagedView{} = buildManagedView f mounted mparent m

buildComponentView :: forall e n. Dispatcher e -> IORef (IO ()) -> Maybe Node -> View e -> IO (View e)
buildComponentView f mtd mparent ComponentView {..} = do
    stq   <- newPatchQueue
    props_ <- newIORef componentProps
    state_ <- newIORef undefined
    live_ <- newIORef undefined
    let c = componentView cr
        cr  = Ref live_ props_ state_ f c stq
    state1 <- Types.construct c
    writeIORef state_ state1
#ifndef __GHCJS__
    state1 <- Types.initialize c state1
    writeIORef state_ state1
#else
    mount c
#endif
    let new = renderer c componentProps state1
    live <- build f mtd mparent new
    writeIORef live_ live
#ifdef __GHCJS__
    modifyIORef mtd (>> mounted c)
#endif
    componentThread cr live componentProps state1
    return $ ComponentView componentProps (Just cr) componentView

buildManagedView :: Dispatcher e -> IORef (IO ()) -> Maybe Node -> View e -> IO (View e)
buildManagedView f mounted mparent m@ManagedView {..} = do
  -- the assumptions here need reviewing; keep an eye out
  case controller of
    Controller_ a -> do
      let bld elementHost = do
            MVCRecord {..} <- mkController mounted Build a
            MVCView {..} <- liftIO $ readIORef mvcrView
            for_ mvcvCurrentLive $ \live ->
              for_ elementHost ((`embed` live) . toNode)
            return ManagedView {..}

      case elementHost of
        Nothing -> do
          el <- create tag
          features <- for features (addFeature f el)
          mi_ <- lookupController (key a)
          let elementHost = Just el
          case mi_ of
            Nothing -> do
              mv <- bld elementHost
              for_ mparent (`append` el)
              return mv
            Just MVCRecord {..} -> do
              MVCView {..} <- liftIO $ readIORef mvcrView
              let mv = ManagedView {..}
              rebuild mv
              for_ mvcvCurrentLive $ embed (toNode el) -- doesn't rebuild do this?
              for_ mparent (`append` el)
              return mv

        Just e -> do
          mi_ <- lookupController (key a)
          case mi_ of
            Nothing -> do
              mv <- bld elementHost
              return mv
            Just MVCRecord {..} -> do
              MVCView {..} <- liftIO $ readIORef mvcrView
              rebuild m
              for_ mvcvCurrentLive $ embed (toNode e)
              return m

cleanup :: View e -> IO ()
cleanup (HTMLView _ _ fs cs) = do
  for_ fs cleanupFeature
  for_ cs cleanup
cleanup (KHTMLView _ _ fs cs _) = do
  for_ fs cleanupFeature
  for_ cs (cleanup . snd)
cleanup (SVGView _ _ fs cs) = do
  for_ fs cleanupFeature
  for_ cs cleanup
cleanup (KSVGView _ _ fs cs _) = do
  for_ fs cleanupFeature
  for_ cs (cleanup . snd)
cleanup (ComponentView _ r _) = do
  for_ r $ \cr -> do
    componentCleanup <- newEmptyMVar
    unmountComponent cr (\p v -> unsafeIOToST (cleanup v),componentCleanup)
    join $ takeMVar componentCleanup
cleanup (ManagedView _ _ fs _) =
  for_ fs cleanupFeature
cleanup _ = return ()

cleanupDeferred :: Plan s -> View e -> ST s ()
cleanupDeferred plan = go
  where
    go (HTMLView _ _ fs cs) = do
      for_ fs (cleanupFeatureDeferred plan)
      for_ cs go
    go (KHTMLView _ _ fs cs _) = do
      for_ fs (cleanupFeatureDeferred plan)
      for_ cs (go . snd)
    go (SVGView _ _ fs cs) = do
      for_ fs (cleanupFeatureDeferred plan)
      for_ cs go
    go (KSVGView _ _ fs cs _) = do
      for_ fs (cleanupFeatureDeferred plan)
      for_ cs (go . snd)
    go stv@(ComponentView _ r _) = do
      for_ r $ \cr -> do
        componentCleanup <- unsafeIOToST newEmptyMVar
        unsafeIOToST $ unmountComponent cr (cleanupDeferred,componentCleanup)
        cc <- unsafeIOToST $ takeMVar componentCleanup
        amendPlan plan cc
    go (ManagedView _ _ fs _) =
      for_ fs (cleanupFeatureDeferred plan)
    go _ = return ()

remove :: View e -> IO ()
remove v = do
  for_ (getHost v) removeNode
  cleanup v

removeDeferred :: Plan s -> View e -> ST s ()
removeDeferred plan v = do
  for_ (getHost v) (amendPlan plan . removeNode)
  cleanupDeferred plan v

getHost :: View e -> Maybe Node
getHost (NullView  n)           = fmap toNode n
getHost (TextView  n _)         = fmap toNode n
getHost (HTMLView  n _ _ _)     = fmap toNode n
getHost (KHTMLView n _ _ _ _)   = fmap toNode n
getHost (SVGView   n _ _ _)     = fmap toNode n
getHost (KSVGView  n _ _ _ _)   = fmap toNode n
getHost (ComponentView _ r _)   = join $ for r (getHost . unsafePerformIO . readIORef . crView)
getHost (ManagedView n _ _ _)   = fmap toNode n
getHost _                       = Nothing

embed :: Node -> View e -> IO Bool
embed parent (getHost -> Just child) = do
  append parent child
  return True
embed _ _ = return False

diffDeferred :: Dispatcher e -> IORef (IO ()) -> Plan s -> Diff' s (View e)
diffDeferred f mounted plan old mid new =
  case reallyUnsafePtrEquality# mid new of
    1# -> do
      -- unsafeIOToST $ putStrLn "Fully short-circuited a view diff."
      return old
    _  ->
      let replace = unsafeIOToST (build f mounted Nothing new) >>= replaceDeferred plan old
      in
        case (mid,new) of
          (NullView{},NullView{})              -> return old
          (TextView{},TextView{})              -> replaceTextContentDeferred plan old new
          (SomeView m,SomeView n)
            -> if reallyVeryUnsafeEq m n -- shouldn't the above rUPE catch this?
                then return old
                else diffDeferred f mounted plan old (render m) (render n)
          (HTMLView{},HTMLView{})
            | tag mid == tag new               -> diffElementDeferred f mounted plan old mid new
            | otherwise                        -> replace
          (SVGView{},SVGView{})
            | tag mid == tag new               -> diffElementDeferred f mounted plan old mid new
            | otherwise                        -> replace
          (KHTMLView{},KHTMLView{})
            | tag mid == tag new               -> diffKeyedElementDeferred f mounted plan old mid new
            | otherwise                        -> replace
          (KSVGView{},KSVGView{})
            | tag mid == tag new               -> diffKeyedElementDeferred f mounted plan old mid new
            | otherwise                        -> replace
          (ManagedView{},ManagedView{})
            | controller old == controller new -> diffManagedDeferred f plan old mid new
            | otherwise                        -> replace
          (ComponentView{},ComponentView{})                  ->
            case (old,new) of
              (ComponentView p r v,ComponentView p' _ v')
                | reallyVeryUnsafeEq p p' -> do
                    -- unsafeIOToST $ putStrLn "No change in component."
                    return old
                | typeOf p == typeOf p' -> unsafeIOToST $ do
                    -- putStrLn "Component views equal, but propeties changed."
                    let cr = fromJust (unsafeCoerce r)
                    setProps cr (unsafeCoerce p')
                    return (ComponentView (unsafeCoerce p') r v)
                | otherwise -> do
                    -- unsafeIOToST $ putStrLn "Components different."
                    new' <- unsafeIOToST $ build f mounted Nothing new
                    let cr = fromJust (unsafeCoerce r)
                    cb <- unsafeIOToST newEmptyMVar
                    unsafeIOToST $ unmountComponent cr (\p live -> void $ replaceDeferred p live new',cb)
                    plan' <- unsafeIOToST $ takeMVar cb
                    amendPlan plan plan'
                    return new'

          _ -> replace

replaceDeferred :: Plan s -> View e -> View e -> ST s (View e)
replaceDeferred plan old@(getHost -> oldHost) new@(getHost -> newHost) =
  case oldHost of
    Nothing -> error "Expected old host in replaceDeferred; got nothing."
    Just oh ->
      case newHost of
        Nothing -> error "Expected new host in replaceDeferred; got nothing."
        Just nh -> do
          amendPlan plan (replaceNodes oh nh)
          cleanupDeferred plan old
          return new

replaceTextContentDeferred :: Plan s -> View e -> View e -> ST s (View e)
replaceTextContentDeferred plan old@(textHost -> Just oh) new = do
  amendPlan plan (oh `replaceText` content new)
  return old { content = content new }

diffKeyedElementDeferred :: Dispatcher e -> IORef (IO ()) -> Plan s -> Diff' s (View e)
diffKeyedElementDeferred f mounted plan old@(elementHost &&& childMap -> (Just e,cm)) mid new = do
  fs      <- diffFeaturesDeferred      e f         plan    (fs old) (fs mid) (fs new)
  (cm,cs) <- diffKeyedChildrenDeferred e f mounted plan cm (cs old) (cs mid) (cs new)
  return old { features = fs, keyedChildren = cs, childMap = cm }
  where
    fs = features
    cs = keyedChildren

diffElementDeferred :: Dispatcher e -> IORef (IO ()) -> Plan s -> Diff' s (View e)
diffElementDeferred f mounted plan old@(elementHost -> Just e) mid new = do
  fs <- diffFeaturesDeferred e f         plan (fs old) (fs mid) (fs new)
  cs <- diffChildrenDeferred e f mounted plan (cs old) (cs mid) (cs new)
  return old { features = fs, children = cs }
  where
    fs = features
    cs = children

diffManagedDeferred :: Dispatcher e -> Plan s -> Diff' s (View e)
diffManagedDeferred f plan old@(elementHost -> Just e) mid new = do
  fs <- diffFeaturesDeferred e f plan (fs old) (fs mid) (fs new)
  return old { features = fs }
  where
    fs = features


diffKeyedChildrenDeferred :: forall s e. Element -> Dispatcher e -> IORef (IO ()) -> Plan s -> HM.HashMap Int (View e) -> [(Int,View e)] -> [(Int,View e)] -> [(Int,View e)] -> ST s (HM.HashMap Int (View e),[(Int,View e)])
diffKeyedChildrenDeferred (toNode -> e) f mounted plan keys olds mids news = do
  dc                  <- newSTRef (e,f,keys,mempty)
  news'               <- start dc olds mids news
  (_,_,keys,removals) <- readSTRef dc
  ks                  <- newSTRef keys
  plan'               <- newSTRef []
  for_ (HM.toList removals) $ \(i,r) -> do
    modifySTRef ks (HM.delete i)
    removeDeferred plan' r
  p' <- readSTRef plan'
  amendPlan plan (sequence_ $ List.reverse p')
  keys' <- readSTRef ks
  return (keys',news')
  where
    start dc [] _ []        = return []
    start dc [] _ news      = dKCD_new dc mounted plan news
    start dc olds _ []      = dKCD_rm dc plan olds
    start dc olds mids news = dKCD_upd dc mounted plan olds mids news

type Keys e = HM.HashMap Int (View e)
type Removals e = HM.HashMap Int (View e)
type DiffCtx s e = STRef s (Node,Dispatcher e,Keys e,Removals e)

dKCD_new :: DiffCtx s e -> IORef (IO ()) -> Plan s -> [(Int,View e)] -> ST s [(Int,View e)]
dKCD_new dc mounted plan news = do
  -- unsafeIOToST $ print ("dKCD_new",map fst news)
  (e,f,_,removals) <- readSTRef dc
  plan' <- newSTRef []
  keys <- newSTRef mempty
  news' <- for news $ \(i,n) -> do
    new' <- unsafeIOToST $ build f mounted Nothing n
    for_ (getHost new') $ amendPlan plan' . append e
    modifySTRef keys (HM.insert i new')
    return (i,new')
  ks <- readSTRef keys
  p <- readSTRef plan'
  writeSTRef dc (e,f,ks,removals)
  amendPlan plan $! sequence_ (List.reverse p)
  return news'

dKCD_rm :: DiffCtx s e -> Plan s -> [(Int,View e)] -> ST s [(Int,View e)]
dKCD_rm dc plan olds = do
  -- unsafeIOToST $ print ("dKCD_rm",map fst olds)
  plan' <- newSTRef []
  for_ olds $ traverse (removeDeferred plan')
  p <- readSTRef plan'
  amendPlan plan $! sequence_ (List.reverse p)
  modifySTRef dc $ \(e,f,_,_) -> (e,f,mempty,mempty)
  return []

dKCD_ins :: DiffCtx s e -> IORef (IO ()) -> Plan s -> Int -> Int -> View e -> ST s (Int,View e)
dKCD_ins dc mounted plan i nk new = do
  (e,f,keys,removals) <- readSTRef dc
  let ins i ~(Just a) = amendPlan plan (insertAt (coerce e) a i)
  case HM.lookup nk removals of
    Nothing -> do
      n' <- unsafeIOToST (build f mounted Nothing new)
      writeSTRef dc (e,f,HM.insert nk n' keys,removals)
      ins i (getHost n')
      return (nk,n')
    Just n -> do
      writeSTRef dc (e,f,keys,HM.delete nk removals)
      ins (i + 1) (getHost n)
      return (nk,n)

dKCD_upd :: DiffCtx s e -> IORef (IO ()) -> Plan s -> Diff' s [(Int,View e)]
dKCD_upd dc mounted plan = go 0
  where
    go !i olds mids news = -- traceShow ("dKCD_upd",map fst olds,map fst news) $
      case reallyUnsafePtrEquality# mids news of
        1# -> do
          -- unsafeIOToST $ putStrLn "Fully short circuited a keyed element"
          return olds
        _  -> dKCD_slow dc mounted plan i olds mids news

dKCD_slow :: DiffCtx s e -> IORef (IO ()) -> Plan s -> Int -> Diff' s [(Int,View e)]
dKCD_slow dc mounted plan = go
  where
    go !_ olds _ [] = do
      -- unsafeIOToST $ print (1,map fst olds)
      -- for_ olds $ \(ok,_) -> unsafeIOToST $ print ("removing",ok)
      for_ olds $ \(ok,o) -> modifySTRef dc $ \(e,f,keys,removals) -> (e,f,HM.delete ok keys,HM.insert ok o removals)
      return []

    go _ [] _ news = do
      -- unsafeIOToST $ print (2,map fst news)
      for news $ \(i,new) -> do
        (e,f,keys,removals) <- readSTRef dc
        case HM.lookup i removals of
          Nothing -> do
            new' <- unsafeIOToST (build f mounted Nothing new)
            writeSTRef dc (e,f,HM.insert i new' keys,removals)
            for (getHost new') $ amendPlan plan . append e
            return (i,new')
          Just r  -> do
            writeSTRef dc (e,f,keys,HM.delete i removals)
            for (getHost r) $ amendPlan plan . append e
            return (i,r)

    go i [o@(ok,old)] ~[m@(mk,mid)] (n0@(nk0,new0):n1@(nk1,new1):ns) = do
      -- unsafeIOToST $ print (3,ok,(nk0:nk1:map fst ns))
      if mk == nk0
        then do
          n' <- dKCD_helper dc mounted plan o m n0
          ns' <- go (i + 1) [] [] (n1:ns)
          return (n':ns')
        else
          if mk == nk1
            then do
              n'  <- dKCD_ins dc mounted plan i nk0 new0
              ns' <- go (i + 1) [o] [m] (n1:ns)
              return (n':ns')
            else do
              modifySTRef dc $ \(e,f,keys,removals) -> (e,f,keys,HM.insert mk old removals)
              n' <- dKCD_ins dc mounted plan i nk0 new0
              ns' <- go (i + 1) [] [] (n1:ns)
              return (n':ns')

    go i [o@(ok,old)] ~[m@(mk,mid)] (n@(nk,new):ns) = do
      -- unsafeIOToST $ print (4,ok,(nk:map fst ns))
      if mk == nk
        then do
          n' <- dKCD_helper dc mounted plan o m n
          ns' <- go (i + 1) [] [] ns
          return (n':ns')
        else do
          modifySTRef dc $ \(e,f,keys,removals) -> (e,f,keys,HM.insert mk old removals)
          n' <- dKCD_ins dc mounted plan i nk new
          ns' <- go (i + 1) [] [] ns
          return (n':ns')

    go i ~(o@(ok,old):os) ~(m@(mk,mid):ms) ns@[n@(nk,new)] = do
      -- unsafeIOToST $ print (5,ok:map fst os,nk)
      if mk == nk
        then do
          -- unsafeIOToST $ print 51
          n' <- dKCD_helper dc mounted plan o m n
          ns' <- go (i + 1) os ms []
          return (n':ns')
        else do
          -- unsafeIOToST $ print 52
          modifySTRef dc $ \(e,f,keys,removals) -> (e,f,keys,HM.insert mk old removals)
          go i os ms ns

    go i ~os0@(o0@(ok0,old0):os1@(o1@(ok1,old1):os2)) ~ms0@(m0@(mk0,mid0):ms1@(m1@(mk1,mid1):ms2)) ~ns@(n0@(nk0,new0):ns1@(n1@(nk1,new1):ns2))
      | mk0 == nk0 = do
          -- unsafeIOToST $ print (6,ok0:ok1:map fst os2,nk0:nk1:map fst ns2)
          n  <- dKCD_helper dc mounted plan o0 m0 n0
          case reallyUnsafePtrEquality# ms1 ns1 of
            1# -> do
              -- unsafeIOToST $ putStrLn "Short 6"
              return (n:os1)
            _  -> do
              ns <- go (i + 1) os1 ms1 ns1
              return (n:ns)

      | mk0 == nk1 && mk1 == nk0 = do
          -- unsafeIOToST $ print (7,ok0:ok1:map fst os2,nk0:nk1:map fst ns2)
          -- swap mk0 mk1
          (e,_,_,_) <- readSTRef dc
          let ins ~(Just b) = amendPlan plan (insertAt (coerce e) b i)
          ins (getHost old1)
          case reallyUnsafePtrEquality# ms2 ns2 of
            1# -> do
              -- unsafeIOToST $ putStrLn "Short 7"
              return (o1:o0:os2)
            _  -> do
              ns <- go (i + 2) os2 ms2 ns2
              return (o1:o0:ns)

      | mk0 == nk1 = do
          -- unsafeIOToST $ print (8,ok0:ok1:map fst os2,nk0:nk1:map fst ns2)
          -- insert nk0
          n0 <- dKCD_ins dc mounted plan i nk0 new0
          case reallyUnsafePtrEquality# ms0 ns1 of
            1# -> do
              -- unsafeIOToST $ putStrLn "Short 8"
              return (n0:os0)
            _  -> do
              ns <- go (i + 1) os0 ms0 ns1
              return (n0:ns)

      | mk1 == nk0 = do
          -- unsafeIOToST $ print (9,ok0:ok1:map fst os2,nk0:nk1:map fst ns2)
          -- delete mk0
          modifySTRef dc $ \(e,f,keys,removals) -> (e,f,keys,HM.insert mk0 old0 removals)
          case reallyUnsafePtrEquality# ms1 ns of
            1# -> do
              -- unsafeIOToST $ putStrLn "Short 9"
              return os1
            _  -> go i os1 ms1 ns

      | otherwise = do
          -- unsafeIOToST $ print (10,ok0:ok1:map fst os2,nk0:nk1:map fst ns2)
          -- remove mk0, insert nk0, diff mk1 nk1 or recurse
          modifySTRef dc $ \(e,f,keys,removals) -> (e,f,keys,HM.insert mk0 old0 removals)
          n0 <- dKCD_ins dc mounted plan i nk0 new0
          if mk1 == nk1
            then do
              n1 <- dKCD_helper dc mounted plan o1 m1 n1
              case reallyUnsafePtrEquality# ms2 ns2 of
                1# -> do
                  -- unsafeIOToST $ putStrLn "Short 10a"
                  return (n0:n1:os2)
                _  -> do
                  ns <- go (i + 2) os2 ms2 ns2
                  return (n0:n1:ns)
            else
              case reallyUnsafePtrEquality# ms1 ns1 of
                1# -> do
                  -- unsafeIOToST $ putStrLn "Short 10b"
                  return (n0:os1)
                _  -> do
                  ns <- go (i + 1) os1 ms1 ns1
                  return (n0:ns)

dKCD_helper :: DiffCtx s e -> IORef (IO ()) -> Plan s -> (Int,View e) -> (Int,View e) -> (Int,View e) -> ST s (Int,View e)
dKCD_helper dc mounted plan (ok,old) (mk,mid) (nk,new) = do
  (e,f,keys,removals) <- readSTRef dc
  new' <- diffDeferred f mounted plan old mid new
  case reallyUnsafePtrEquality# old new' of
    1# -> return ()
    _  -> writeSTRef dc (e,f,HM.insert nk new' keys,removals)
  return (nk,new')

diffChildrenDeferred :: forall s e. Element -> Dispatcher e -> IORef (IO ()) -> Plan s -> Diff' s [View e]
diffChildrenDeferred (toNode -> e) f mounted plan = start
  where
    start :: Diff' s [View e]
    start olds mids news =
      case reallyUnsafePtrEquality# mids news of
        1# -> return olds
        _  -> go olds mids news

    go :: Diff' s [View e]
    go olds _ [] = do
      plan' <- newSTRef []
      for_ olds (removeDeferred plan')
      p <- readSTRef plan'
      amendPlan plan $! sequence_ (List.reverse p)
      return []

    go [] _ news = do
      plan' <- newSTRef []
      news' <- for news $ \n -> do
        new' <- unsafeIOToST $ build f mounted Nothing n
        for_ (getHost new') $ amendPlan plan' . append e
        return new'
      p <- readSTRef plan'
      amendPlan plan $! sequence_ (List.reverse p)
      return news'

    go (old:olds) (mid:mids) (new:news) = do
      new'  <- diffDeferred f mounted plan old mid new
      news' <- start olds mids news
      return (new' : news')

replace :: View e -> View e -> IO (View e)
replace old@(getHost -> oldHost) new@(getHost -> newHost) =
  case oldHost of
    Nothing -> error "Expected old host in replace; got nothing."
    Just oh ->
      case newHost of
        Nothing -> error "Expected new host in replace; got nothing."
        Just nh -> do
          replaceNodes oh nh
          cleanup old
          return new

forceToFromTxt :: (ToTxt t, FromTxt t) => Txt -> t
forceToFromTxt = fromTxt

-- rebuild finds managed nodes and re-embeds them in case they were
-- removed for other uses
rebuild :: forall e. View e -> IO ()
rebuild h =
#ifndef __GHCJS__
    return ()
#else
    go h
  where
    go :: View e -> IO ()
    go ComponentView {..}  = for_ componentRecord ((=<<) rebuild . readIORef . crView . unsafeCoerce)
    go HTMLView {..}  = mapM_ go children
    go SVGView {..}   = mapM_ go children
    go KHTMLView {..} = mapM_ (go . snd) keyedChildren
    go KSVGView {..}  = mapM_ (go . snd) keyedChildren
    go m@ManagedView {..} = do
      case controller of
        Controller_ c -> do
          mi_ <- lookupController (key c)
          case mi_ of
            Nothing -> liftIO $ print "lookupController: Nothing"
            Just MVCRecord {..} -> do
              MVCView {..} <- readIORef mvcrView
              case mvcvCurrentLive of
                Nothing -> do
                  let f = void . runAs mvcrAs
                  mounted <- newIORef (return ())
                  live <- build f mounted Nothing mvcvCurrent
                  writeIORef mvcrView MVCView { mvcvCurrentLive = Just live, .. }
                  for_ elementHost ((`embed` live) . toNode)
                  join $ readIORef mounted
                Just live -> do
                  rebuild live
                  for_ elementHost ((`embed` live) . toNode)
    go _ =
      return ()
#endif

reflect :: forall ts ms m c.
           ( IsMVC' ts ms m
           , MonadIO c
           )
        => Controller' ts ms m
        -> c (Promise (View ms))
reflect c =
  with c $ do
    MVCState {..} :: MVCState m <- get
    MVCView  {..} <- liftIO $ readIORef mvcsView
    return (unsafeCoerce mvcvCurrentLive)

{-# NOINLINE controllerShutdownSyndicate #-}
controllerShutdownSyndicate :: Syndicate ()
controllerShutdownSyndicate = unsafePerformIO syndicate

{-# NOINLINE controllerVault__ #-}
controllerVault__ :: Vault
controllerVault__ = Vault (unsafePerformIO (newMVar HM.empty))

lookupController :: (MonadIO c) => Key phantom -> c (Maybe phantom)
lookupController = liftIO . vaultLookup controllerVault__

getControllerName :: IsMVC' ts ms m => Controller' ts ms m -> Txt
getControllerName = toTxt . key

addController :: (MonadIO c) => Key phantom -> phantom -> c ()
addController k = liftIO . vaultAdd controllerVault__ k

deleteController :: (MonadIO c) => Key phantom -> c ()
deleteController = liftIO . vaultDelete controllerVault__

instance IsMVC' ts ms m
  => With (Controller' ts ms m)
          (Ef ms IO)
          IO
  where
    using_ c = usingController c NoBuild
    with_ c m = do
      run <- using_ c
      run m
    shutdown_ c = do
      -- this method should 1. destroy the view 2. syndicate a shutdown event 3. poison the context
      -- so that unmount events that call with on the context do not fail
      miohhm <- lookupController (key c)
      case miohhm of
        Just MVCRecord {..} -> do
          killThread (mvcpThreadId mvcrPatcher)
          MVCView {..} <- liftIO $ readIORef mvcrView
          for mvcvCurrentLive remove
          void $ runAs mvcrAs $ do
            buf <- get
            Shutdown sdn <- get
            publish sdn ()
            -- this is where things get iffy... what should this look like?
            delay 0 $ do
              deleteController (key c)
              liftIO $ do
                killBuffer buf
                myThreadId >>= killThread
        _ -> return ()

usingController c bld = do
  -- FIXME: likely a bug here with double initialization in multithreaded contexts!
  mi_ <- lookupController (key c)
  case mi_ of
    Just (MVCRecord {..}) -> return (runAs mvcrAs)
    Nothing -> do
      mounted <- newIORef (return ())
      mkController mounted bld c
      usingController c bld

data MkControllerAction
  = ClearAndAppend Node
  | forall e. Replace (View e)
  | Append Node
  | Build
  | NoBuild
  | Preinit

controllerPatcher :: (Pure a ms) => Dispatcher ms -> IORef (MVCView ms m) -> (m ms -> a ms) -> IO (ThreadId,MVar ())
controllerPatcher f view rndr = do
  awaiting <- newEmptyMVar
  tid <- forkIO (patch awaiting)
  return (tid,awaiting)
  where
    patch awaiting = forever $ do
#ifdef __GHCJS__
      -- await the controller triggering an update
      takeMVar awaiting

      -- read the current view
      mvcv@MVCView {..} <- readIORef view

      -- render the current view which should be an update to the exisiting view in mvcvCurrent/Live
      let new = render $ rndr mvcvModel
      mounted <- newIORef (return ())
      let (plan,newLive) =
            case mvcvCurrentLive of
              Nothing -> ([],Nothing)
              Just live -> fmap Just $ buildPlan (\p -> diffDeferred f mounted p live mvcvCurrent new)
      mtd <- plan `seq` readIORef mounted
      unless (List.null plan) $ do
        barrier <- newEmptyMVar
        addAnimation $ runPlan (putMVar barrier ():plan)
        takeMVar barrier
      mtd
      atomicModifyIORef' view $ \v ->
        (v { mvcvCurrent = new, mvcvCurrentLive = newLive },())
#else
      mvcv@MVCView {..} <- readIORef view
      let new = render $ rndr mvcvModel
      mounted <- newIORef (return ())
      let (plan,newLive) =
            case mvcvCurrentLive of
                Nothing -> ([],Nothing)
                Just live -> fmap Just $ buildPlan (\p -> diffDeferred f mounted p live mvcvCurrent new)
      mtd <- plan `seq` readIORef mounted
      runPlan plan
      mtd
      atomicModifyIORef' view $ \v ->
        (v { mvcvCurrent = new, mvcvCurrentLive = newLive },())
#endif

mkController :: forall ms ts m.
          ( IsMVC' ts ms m
          , ms <: Base m
          )
       => IORef (IO ())
       -> MkControllerAction
       -> Controller' ts ms m
       -> IO (MVCRecord ms m)
mkController mounted mkControllerAction c@Controller {..} = do
  let raw = render $ view model
  buf <- newEvQueue
  us  <- syndicate
  sdn <- Shutdown <$> syndicate
  as  <- unsafeConstructAs buf
  let sendEv = void . runAs as
  i <- case mkControllerAction of
         ClearAndAppend n -> do
           i <- Pure.DOM.build sendEv mounted Nothing raw
           clear (toNode n)
           let mn = getHost i
           for_ mn (addAnimation . append n)
           return (Just i)
         Replace as -> do
           i <- Pure.DOM.build sendEv mounted Nothing raw
           addAnimation $ replace as (unsafeCoerce i)
           return (Just i)
         Append en -> do
           i <- Pure.DOM.build sendEv mounted (Just en) raw
           return (Just i)
         Build -> do
           i <- Pure.DOM.build sendEv mounted Nothing raw
           return (Just i)
         NoBuild ->
           return Nothing
         Preinit -> do
           frag <- createFrag
           i <- Pure.DOM.build sendEv mounted (Just (toNode frag)) raw
           return (Just i)
  ds   <- newIORef Eager
  mvcv <- newIORef (MVCView raw i model)
  (patcherThread,patcherBarrier) <- controllerPatcher sendEv mvcv view
  let mvcp = MVCPatcher patcherThread (void $ tryPutMVar patcherBarrier ())
      mvcr = MVCRecord as mvcv mvcp

  -- keep out of forkIO to prevent double-initialization
  addController key mvcr

  forkIO $ do
    built <- Types.build c $ Ef.Base.state (MVCState mvcp ds us mvcv)
                         *:* Ef.Base.state sdn
                         *:* Ef.Base.state ds
                         *:* Ef.Base.state mvcp
                         *:* Ef.Base.state buf
                         *:* Empty
    (obj',_) <- Ef.Base.Object built Ef.Base.! do
      connect controllerShutdownSyndicate $ const (Ef.Base.lift shutdownSelf)
      prime
#if (defined __GHCJS__) || (defined DEVEL)
    driverPrintExceptions (" Controller exception (" ++ show key ++ "): ")
#else
    driver
#endif
        buf obj'
  return mvcr

forceDiff :: ms <: '[State () MVCPatcher] => Ef ms IO ()
forceDiff = do
  MVCPatcher {..} <- get
  liftIO mvcpAddPatch

setEagerDiff :: ms <: '[State () (IORef DiffStrategy)] => Ef ms IO ()
setEagerDiff = do
  ds_ <- get
  liftIO $ writeIORef ds_ Eager

setManualDiff :: ms <: '[State () (IORef DiffStrategy)] => Ef ms IO ()
setManualDiff = do
  ds_ <- get
  liftIO $ writeIORef ds_ Manual

currentHTML :: (IsMVC' ts ms m, MonadIO c) => Controller' ts ms m -> c (Promise (View ms))
currentHTML c = with c $ ownHTML c

ownHTML :: forall ts ms c m.
           ( IsMVC' ts ms m
           , MonadIO c
           , ms <: Base m
           )
        => Controller' ts ms m
        -> Ef ms c (View ms)
ownHTML _ = do
  MVCState {..} :: MVCState m <- get
  MVCView {..} <- liftIO $ readIORef mvcsView
  return (unsafeCoerce mvcvCurrentLive)

onModel :: forall ts ms ms' m c e.
          ( IsMVC' ts ms m
          , MonadIO c
          , ms <: Base m
          , ms' <: '[Evented]
          , e ~ Ef ms' c
          )
        => Controller' ts ms m
        -> (m ms -> Ef '[Event (m ms)] e ())
        -> e (Promise (IO ()))
onModel c f = do
  connectWith c getter f
  where
    getter = do
      MVCState {..} :: MVCState m <- get
      return (unsafeCoerce mvcsUpdates :: Syndicate (m ms))

onOwnModel :: forall ts ms m. ms <: Base m
           => (m ms -> Ef '[Event (m ms)] (Ef ms IO) ())
           -> Ef ms IO (IO ())
onOwnModel f = do
  MVCState {..} :: MVCState m <- get
  connect (unsafeCoerce mvcsUpdates :: Syndicate (m ms)) f

getModel :: forall m ms. ms <: '[State () (MVCState m)] => Ef ms IO (m ms)
getModel = do
  MVCState {..} :: MVCState m <- get
  mvcvModel <$> liftIO (readIORef (unsafeCoerce mvcsView))

putModel :: forall ms m. ms <: Base m => m ms -> Ef ms IO ()
putModel !new = do

  -- clever, but a little ugly - the result below is pleasant, though
  MVCState
    { mvcsView     = unsafeCoerce          -> view
    , mvcsUpdates  = unsafeCoerce          -> updates
    , mvcsStrategy = liftIO . readIORef    -> getStrategy
    , mvcsPatcher  = liftIO . mvcpAddPatch -> patch
    } :: MVCState m                        <- get

  old <- liftIO $ atomicModifyIORef' view $ \mvcv@MVCView { mvcvModel = old } ->
           (mvcv { mvcvModel = new },old)

  -- this is a little funky; not all updates are seen in the projected view because of batching,
  -- but all updates are seen in the updates network. What should the expected behavior be?
  -- Should I have the patcher syndicate only the projected models?
  publish updates new

  ds <- getStrategy
#ifdef __GHCJS__
  case reallyUnsafePtrEquality# old new of
    1# -> return ()
    _  ->
      case ds of
        Eager  -> patch
        Manual -> return ()
#else
  patch
#endif

modifyModel :: forall e ms m. ms <: Base m => (m ms -> m ms) -> Ef ms IO ()
modifyModel f = do
  MVCState
    { mvcsView     = unsafeCoerce          -> view
    , mvcsUpdates  = unsafeCoerce          -> updates
    , mvcsStrategy = liftIO . readIORef    -> getStrategy
    , mvcsPatcher  = liftIO . mvcpAddPatch -> patch
    } :: MVCState m                        <- get

  (old,new) <- liftIO $ atomicModifyIORef' view $ \mvcv@MVCView { mvcvModel = old } ->
    let !new = f old
    in (mvcv { mvcvModel = new },(old,new))

  -- this is a little funky; not all updates are seen in the projected view because of batching,
  -- but all updates are seen in the updates network. What should the expected behavior be?
  -- Should I have the patcher syndicate only the projected models?
  publish updates new

  ds <- getStrategy
#ifdef __GHCJS__
  case reallyUnsafePtrEquality# old new of
    1# -> return ()
    _  ->
      case ds of
        Eager  -> patch
        Manual -> return ()
#else
  patch
#endif

_rAF f = void $ do
#ifdef __GHCJS__
  callback <- CB.syncCallback1 CB.ContinueAsync $ \_ -> f
  requestAnimationFrame callback
#else
  f
#endif

newPatchQueue :: IO (IORef (Maybe (Queue (ComponentPatch parent props state))))
newPatchQueue = newIORef . Just =<< newQueue

getState :: MonadIO m => Ref parent props state -> m state
getState = liftIO . readIORef . crState

getProps :: MonadIO m => Ref parent props state -> m props
getProps = liftIO . readIORef . crProps

getView :: MonadIO m => Ref parent props state -> m (View parent)
getView = liftIO . readIORef . crView

parent :: Ref parent props state -> Dispatcher parent
parent = crDispatcher

setState :: MonadIO m => Ref parent props state -> (props -> state -> IO (state,IO ())) -> m Bool
setState cr = liftIO . queueComponentUpdate cr . UpdateState

-- Probably don't use this....
setProps :: MonadIO m => Ref parent props state -> props -> m Bool
setProps cr = liftIO . queueComponentUpdate cr . UpdateProperties

-- Definitely don't use this....
unmountComponent :: MonadIO m => Ref parent props state -> (Plan s -> View parent -> ST s (),MVar (IO ())) -> m Bool
unmountComponent cr (f,cb) = liftIO . queueComponentUpdate cr $ Unmount f cb

queueComponentUpdate :: Ref parent props state -> ComponentPatch parent props state -> IO Bool
queueComponentUpdate crec cp = do
  mq <- readIORef (crPatchQueue crec)
  case mq of
    Nothing -> return False
    Just q  -> do
      arrive q cp
      return True

componentThread :: forall parent props state. Ref parent props state -> View parent -> props -> state -> IO ()
componentThread Ref { crComponent = c, ..} live props state = void $ forkIO $ wrapper (renderer c) live props state props state [] []
  where
    {-# INLINE wrapper #-}
    wrapper rndr live props state newProps newState = worker
      where
        {-# INLINE worker #-}
        worker :: [(IO (),View parent -> IO (),IO ())] -> [ComponentPatch parent props state] -> IO ()
        worker [] [] = do
          mq <- readIORef crPatchQueue
          for_ mq (worker [] <=< collect)

        worker acc [] = do
          dus <- for (List.reverse acc) $ \(willUpd,didUpd,callback) -> do
            willUpd
            return (didUpd,callback)
          mtd <- newIORef (return ())
          let new =
                case reallyUnsafePtrEquality# props newProps of
                  1# ->
                    case reallyUnsafePtrEquality# state newState of
                      1# -> rndr props state
                      _  -> rndr props newState
                  _ ->
                    case reallyUnsafePtrEquality# state newState of
                      1# -> rndr newProps state
                      _  -> rndr newProps newState
          let old = rndr props state
              (plan,new_live) = buildPlan (\p -> diffDeferred crDispatcher mtd p live old new)
          writeIORef crView new_live
          mtd <- plan `seq` readIORef mtd
          unless (List.null plan) $ do
            barrier <- newEmptyMVar
            addAnimation (runPlan (putMVar barrier ():plan))
            takeMVar barrier
          mtd
          cbs <- for dus $ \(du,c) -> do
            du new_live
            return c
          sequence_ cbs
          wrapper rndr new_live newProps newState newProps newState [] []

        worker acc (cp : cps) = do
          case cp of
            Unmount f plan -> do
              unmount c
              writeIORef crPatchQueue Nothing
              barrier <- newEmptyMVar
              let (p,_) = buildPlan (flip (unsafeCoerce f) live)
              putMVar plan (runPlan (putMVar barrier ():p))
              takeMVar barrier
              destruct c
            UpdateProperties newProps -> do
              newState      <- receiveProps c newProps state
              shouldUpdate  <- forceUpdate  c newProps newState
              let writeRefs = writeIORef crProps newProps >> writeIORef crState newState
              if shouldUpdate || not (List.null acc) then
                let
                  will = update  c newProps newState
                  did  = updated c props    state
                in
                  wrapper rndr live props state newProps newState ((will >> writeRefs,did,def) : acc) cps
              else do
                writeRefs
                wrapper rndr live props state newProps newState acc cps
            UpdateState f -> do
              (newState,updatedCallback) <- f props state
              shouldUpdate               <- forceUpdate c props newState
              let writeRef = writeIORef crState newState
              if shouldUpdate || not (List.null acc) then
                let
                  will = update  c props newState
                  did  = updated c props state
                in
                  wrapper rndr live props state newProps newState ((will >> writeRef,did,updatedCallback) : acc) cps
              else do
                writeRef
                wrapper rndr live props state newProps newState acc cps


-- componentPatcher
--   :: forall parent props state.
--      Ref parent props state
--   -> IO ()
-- componentPatcher crec@Ref { crComponent = c, ..} = do
--   run
--   where

--     run = do
--       ps <- modifyMVar crPatchQueue $ \mpq ->
--         return $
--           case mpq of
--             Nothing -> (Nothing,[])
--             Just (ComponentPatchQueue ps self) -> (Just $ ComponentPatchQueue [] self,List.reverse ps)
--       CView {..} <- readIORef crCView
--       go cvProps cvState [] ps

--     continue :: IO ()
--     continue =
--       join $ modifyMVar crPatchQueue $ \mpq ->
--         return $
--           case mpq of
--             Nothing -> (Nothing,def)
--             Just (ComponentPatchQueue ps patchThread) ->
--               case ps of
--                 [] -> (Just $ ComponentPatchQueue [] Nothing    ,def)
--                 _  -> (Just $ ComponentPatchQueue ps patchThread,run)

--     go props state = go'
--       where
--         go' [] [] =
--           continue

--         go' acc [] = do
--           dus <- for (List.reverse acc) $ \(willUpd,didUpd,callback) -> do
--             willUpd
--             return (didUpd,callback)

--           cv <- readIORef crCView

--           mtd <- newIORef (return ())

--           let new = cvApplied cv state
--               (plan,new_live) = buildPlan (\p -> diffDeferred crDispatcher mtd p (cvLive cv) (cvCurrent cv) new)
--               cv' = cv { cvProps = props, cvState = state, cvLive = new_live, cvCurrent = new }

--           writeIORef crCView cv'
--           mtd <- plan `seq` readIORef mtd

--           -- should we use crPatchQueue as a second barrier here to protect against mid-patch shutdown?
--           unless (List.null plan) $ do
--             barrier <- newEmptyMVar
--             addAnimation (runPlan plan >> putMVar barrier ())
--             takeMVar barrier

--           mtd

--           cbs <- for dus $ \(du,c) -> do
--             du
--             return c

--           sequence_ cbs
--           continue

--         go' acc (Unmount f plan : _ ) = do
--           -- Not sinking the rest of the list here

--           cv <- readIORef crCView

--           unmount c

--           modifyMVar crPatchQueue $ \_ -> return (Nothing,())

--           barrier <- newEmptyMVar

--           -- not good; f is coercing the rank2 s from ST s
--           let (p,_) = buildPlan (\p -> (unsafeCoerce f) p (cvLive cv) >> unsafeIOToST (putMVar barrier ()))
--           p `seq` putMVar plan (runPlan p)

--           takeMVar barrier

--           destruct c

--         go' acc (UpdateProperties !newProps : ps ) = do
--           -- putStrLn "Updated properties"

--           cv <- readIORef crCView

--           newState      <- receiveProps c newProps (cvState cv)
--           shouldUpdate  <- forceUpdate  c newProps newState

--           let cv'   = cv { cvProps = newProps, cvState = newState, cvApplied = renderer c newProps }

--           writeIORef crCView cv'

--           if shouldUpdate || not (List.null acc) then
--             let
--               will = update  c newProps     newState
--               did  = updated c (cvProps cv) (cvState cv)
--             in
--               go newProps newState ((will,did,def) : acc) ps
--           else
--             go newProps newState acc ps

--         go' acc (UpdateState f : ps ) = do
--           -- putStrLn "Updated state"

--           cv <- readIORef crCView

--           let before = cvApplied cv (cvState cv)

--           (newState,updatedCallback) <- f (cvProps cv) (cvState cv)
--           shouldUpdate <- forceUpdate c (cvProps cv) newState

--           cv <- readIORef crCView

--           let cv' = cv { cvState = newState }

--           writeIORef crCView cv'

--           if shouldUpdate || not (List.null acc) then
--             let
--               will = update  c (cvProps cv) newState
--               did  = updated c (cvProps cv) (cvState cv)
--             in
--               go props newState ((will,did,updatedCallback) : acc) ps
--           else
--             go props newState acc ps

addAnimation a = do
  atomicModifyIORef' animationQueue $ \as -> (a:as,())
  tryPutMVar animationsAwaiting ()

{-# NOINLINE animationsAwaiting #-}
animationsAwaiting = unsafePerformIO newEmptyMVar

{-# NOINLINE animationQueue #-}
animationQueue = unsafePerformIO (newIORef [])

{-# NOINLINE animator #-}
animator = unsafePerformIO $ void $ forkIO await
  where
    await = do
      takeMVar animationsAwaiting
      as <- atomicModifyIORef' animationQueue $ \as -> ([],as)
      animate as

    animate [] = await
    animate as = do
      barrier <- newEmptyMVar
      _rAF $ do
        bs <- atomicModifyIORef' animationQueue $ \bs -> ([],bs)
        sequence_ (List.reverse as)
        sequence_ (List.reverse bs)
        putMVar barrier ()
      takeMVar barrier
      await

onRaw :: Node -> Txt -> Options -> (IO () -> Obj -> IO ()) -> IO (IO ())
onRaw n nm os f = do
#ifdef __GHCJS__
  stopper <- newIORef undefined
  cb <- CB.syncCallback1 CB.ContinueAsync $ \ev -> do
    when (preventDef os) (preventDefault ev)
    when (stopProp os) (stopPropagation ev)
    f (join $ readIORef stopper) (pFromJSVal ev)
  writeIORef stopper $ do
    removeEventListener n nm cb
    CB.releaseCallback cb
  addEventListener n nm cb (passive os)
  return (join $ readIORef stopper)
#else
  return (return ())
#endif

{-# INLINE addFeature #-}
addFeature :: Dispatcher e -> Element -> Feature e -> IO (Feature e)
addFeature f e = go
  where
    go NullFeature    = return NullFeature

    go a@Attribute {..} = do
      setAttribute e name value
      return a

    go p@Property {..} = do
      setProperty e name value
      return p

    go sl@StyleList {..} = do
      M.foldMapWithKey (setStyle e) stylePairs
      return sl

#ifdef __GHCJS__
    go (On n t o a _ _) = do
      let target = case t of
                    ElementTarget  -> toJSV e
                    WindowTarget   -> toJSV window
                    DocumentTarget -> toJSV document
      (cb,stopper) <- do

        stopper <- newIORef undefined

        let stpr = join $ readIORef stopper

        cb <- CB.syncCallback1 CB.ContinueAsync $ \jsv -> do
          when (preventDef o) (preventDefault jsv)
          when (stopProp o) (stopPropagation jsv)
          mef <- a (Evt (pFromJSVal jsv) stpr target)
          for_ mef f

        writeIORef stopper $ do
          removeEventListener target n cb
          CB.releaseCallback cb

        return (cb,stpr)

      addEventListener target n cb (passive o)
      return (On n t o a (Just cb) stopper)
#else
    go f@On {..} = return f
#endif

    go ref@(HostRef g) = do
      mef <- g (toNode e)
      case mef of
        Nothing -> return ref
        Just ef -> do
          f ef
          return ref

#ifdef __GHCJS__
    go l@Link {..} = do
      setAttribute e "href" link
      stopper <- newIORef undefined
      callback <- CB.syncCallback1 CB.ContinueAsync $ \jsv -> do
                    preventDefault jsv
                    pushState link
                    popState
                    scrollToTop
      writeIORef stopper $ do
        removeEventListener e "click" callback
        releaseCB callback
      addEventListener e "click" callback False
      return $ Link link (join $ readIORef stopper)
#else
    go l@Link {..} = return l
#endif

#ifdef __GHCJS__
    go l@SVGLink {..} = do
      setAttributeNS e "http://www.w3.org/1999/xlink" "xlink:href" link
      stopper <- newIORef undefined
      callback <- CB.syncCallback1 CB.ContinueAsync $ \jsv -> do
                    preventDefault jsv
                    pushState link
                    popState
                    scrollToTop
      writeIORef stopper $ do
        removeEventListener e "click" callback
        releaseCB callback
      addEventListener e "click" callback False
      return $ SVGLink link (join $ readIORef stopper)
#else
    go l@SVGLink {..} = return l
#endif

    go l@XLink {..} = do
      setAttributeNS e "http://www.w3.org/1999/xlink" name value
      return l

styleDiff :: Element -> M.Map Txt Txt -> M.Map Txt Txt -> M.Map Txt (IO ())
styleDiff e = M.mergeWithKey diff remove add
  where
    diff nm val1 val2
      | val1 == val2           = Nothing
      | otherwise              = Just $ setStyle e nm val2
    remove = M.mapWithKey (\nm  _    -> removeStyle e nm)
    add    = M.mapWithKey (\nm val   -> setStyle e nm val)

cleanupFeature :: Feature e -> IO ()
-- cleanupFeature Link {..} = eventStopper
-- cleanupFeature SVGLink {..} = eventStopper
-- cleanupFeature On {..} = eventStopper
cleanupFeature _ = return ()

cleanupFeatureDeferred :: Plan s -> Feature e -> ST s ()
-- cleanupFeatureDeferred plan Link {..} = amendPlan plan eventStopper
-- cleanupFeatureDeferred plan SVGLink {..} = amendPlan plan eventStopper
-- cleanupFeatureDeferred plan On {..} = amendPlan plan eventStopper
cleanupFeatureDeferred _ _ = return ()

removeFeature :: Element -> Feature e -> IO ()
removeFeature e = go
  where
    go NullFeature = return ()

    go Attribute {..} =
      removeAttribute e name

    go Property {..}  =
      removeProperty e name

    go StyleList {..} =
      for_ (M.keys stylePairs) (removeStyle e)

    go On {..} =
      eventStopper

    go HostRef {} =
      return ()

    go Link {..} = do
      removeAttribute e "href"
      eventStopper

    go SVGLink {..} = do
      removeAttributeNS e "http://www.w3.org/1999/xlink" "xlink:href"
      eventStopper

    go XLink {..} =
      removeAttributeNS e "http://www.w3.org/1999/xlink" name

{-# INLINE addFeatureDeferred #-}
addFeatureDeferred :: forall e s. Element -> Dispatcher e -> Plan s -> Feature e -> ST s (Feature e)
addFeatureDeferred e f plan = go
  where
    go :: Feature e -> ST s (Feature e)
    go NullFeature    = return NullFeature

    go a@Attribute {..} = do
      amendPlan plan (setAttribute e name value)
      return a

    go p@Property {..} = do
      amendPlan plan (setProperty e name value)
      return p

    go sl@StyleList {..} = do
      amendPlan plan (M.foldMapWithKey (setStyle e) stylePairs)
      return sl

#ifdef __GHCJS__
    go (On n t o a _ _) = do
      let target = case t of
                    ElementTarget  -> toJSV e
                    WindowTarget   -> toJSV window
                    DocumentTarget -> toJSV document
      (cb,stopper) <- unsafeIOToST $ do

              stopper <- newIORef undefined

              let stpr = join $ readIORef stopper

              cb <- CB.syncCallback1 CB.ContinueAsync $ \jsv -> do
                when (preventDef o) (preventDefault jsv)
                when (stopProp o) (stopPropagation jsv)
                mef <- a (Evt (pFromJSVal jsv) stpr target)
                for_ mef f

              writeIORef stopper $ do
                removeEventListener target n cb
                CB.releaseCallback cb

              return (cb,stpr)

      amendPlan plan (addEventListener target n cb (passive o))
      return (On n t o a (Just cb) stopper)
#else
    go f@On {..} = return f
#endif

    go ref@(HostRef g) = do
      amendPlan plan $ do
        mef <- g (toNode e)
        for_ mef f
      return ref

#ifdef __GHCJS__
    go l@Link {..} = do
      (cb,l) <- unsafeIOToST $ do

        stopper <- newIORef undefined

        cb <- CB.syncCallback1 CB.ContinueAsync $ \jsv -> do
          preventDefault jsv
          pushState link
          popState
          scrollToTop

        writeIORef stopper $ do
          removeEventListener e "click" cb
          releaseCB cb

        return (cb,l { eventStopper = join $ readIORef stopper })

      amendPlan plan $ do
        setAttribute e "href" link
        addEventListener e "click" cb False
      return l
#else
    go l@Link {..} = return l
#endif

#ifdef __GHCJS__
    go l@SVGLink {..} = do
      (cb,l) <- unsafeIOToST $ do

        stopper <- newIORef undefined

        cb <- CB.syncCallback1 CB.ContinueAsync $ \jsv -> do
          preventDefault jsv
          pushState link
          popState
          scrollToTop

        writeIORef stopper $ do
          removeEventListener e "click" cb
          releaseCB cb

        return (cb,l { eventStopper = join $ readIORef stopper })

      amendPlan plan $ do
        setAttributeNS e "http://www.w3.org/1999/xlink" "xlink:href" link
        addEventListener e "click" cb False
      return l
#else
    go l@SVGLink {..} = return l
#endif

    go l@XLink {..} = do
      amendPlan plan (setAttributeNS e "http://www.w3.org/1999/xlink" name value)
      return l

{-# INLINE diffFeaturesDeferred #-}
diffFeaturesDeferred :: Element -> Dispatcher e -> Plan s -> Diff' s [Feature e]
diffFeaturesDeferred e f plan = start
  where
    start old mid new =
      case reallyUnsafePtrEquality# mid new of
        1# -> do
          -- unsafeIOToST $ putStrLn "Features rUE"
          return old
        _  -> go old mid new

    go [] _ news = mapM (addFeatureDeferred e f plan) news
    go olds _ [] = do
      mapM (\o -> amendPlan plan (removeFeature e o)) olds
      return []
    go (old:olds) (mid:mids) (new:news) =
      case reallyUnsafePtrEquality# mid new of
        1# -> do
          news' <- start olds mids news
          return (old:news')
        _  -> do
          new' <- diffFeatureDeferred old mid new
          news' <- start olds mids news
          return (new':news')

    diffFeatureDeferred old mid@NullFeature NullFeature = return old

    diffFeatureDeferred old mid@Attribute{} new@Attribute{} = do
      if prettyUnsafeEq (name mid) (name new)
        then unless (prettyUnsafeEq (value mid) (value new)) $
               amendPlan plan $ setAttribute e (name new) (value new)
        else amendPlan plan $ do
               removeAttribute e (name mid)
               setAttribute e (name new) (value new)
      return new

    diffFeatureDeferred old mid@Property{} new@Property{} = do
      if prettyUnsafeEq (name mid) (name new)
        then unless (prettyUnsafeEq (value mid) (value new)) $
               amendPlan plan $ setProperty e (name new) (value new)
        else amendPlan plan $ do
               removeProperty e (name mid)
               setProperty e (name new) (value new)
      return new

    diffFeatureDeferred old mid@StyleList{} new@StyleList{} = do
      sequence_ $ plannedStyleDiff e plan (stylePairs mid) (stylePairs new)
      return new

    diffFeatureDeferred old (On n t o g _ _) new@(On n' t' o' g' _ _) =
      case reallyUnsafePtrEquality# g g' of
        1# | prettyUnsafeEq n n' && prettyUnsafeEq t t' && prettyUnsafeEq o o' -> return old
           | otherwise -> do
              amendPlan plan $ eventStopper old
              addFeatureDeferred e f plan new
        _ -> do
          -- unsafeIOToST $ putStrLn $ show ("Listeners differ",n,n',reallyUnsafeEq n n',t,t',reallyUnsafeEq t t',o,o',reallyUnsafeEq o o',reallyUnsafeEq g g')
          amendPlan plan $ eventStopper old
          addFeatureDeferred e f plan new

    diffFeatureDeferred old mid@HostRef{} new@HostRef{} = do
      case reallyUnsafePtrEquality# (withHost mid) (withHost new) of
        1# -> return old
        _  -> do
          amendPlan plan $ do
            mef <- withHost new (toNode e)
            case mef of
              Nothing -> return ()
              Just ef -> f ef
          return new

    diffFeatureDeferred old mid@Link{} new@Link{} =
      if (prettyUnsafeEq (link mid) (link new))
        then return old
        else do
          amendPlan plan $ eventStopper old
          addFeatureDeferred e f plan new

    diffFeatureDeferred old mid@SVGLink{} new@SVGLink{} =
      if (prettyUnsafeEq (link mid) (link new))
        then return old
        else do
          amendPlan plan $ eventStopper old
          addFeatureDeferred e f plan new

    diffFeatureDeferred old mid@XLink{} new@XLink{} = do
      if prettyUnsafeEq (name mid) (name new)
        then unless (link mid == link new) $
               amendPlan plan $
                 setAttributeNS e "http://www.w3.org/1999/xlink" (name new) (link new)
        else amendPlan plan $ do
          removeAttributeNS e "http://www.w3.org/1999/xlink" (name mid)
          setAttributeNS e "http://www.w3.org/1999/xlink" (name new) (link new)
      return new

    diffFeatureDeferred old _ new = do
      amendPlan plan $ removeFeature e old
      addFeatureDeferred e f plan new

plannedStyleDiff :: Element -> Plan s -> M.Map Txt Txt -> M.Map Txt Txt -> M.Map Txt (ST s ())
plannedStyleDiff e plan = M.mergeWithKey diff remove add
  where
    diff nm val1 val2
      | val1 == val2           = Nothing
      | otherwise              = Just $ amendPlan plan (setStyle e nm val2)
    remove = M.mapWithKey (\nm  _    -> amendPlan plan (removeStyle e nm))
    add    = M.mapWithKey (\nm val   -> amendPlan plan (setStyle e nm val))

