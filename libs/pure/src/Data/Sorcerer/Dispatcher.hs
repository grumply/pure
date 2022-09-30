{-# language RankNTypes, FlexibleContexts, UndecidableInstances, ScopedTypeVariables #-}
module Data.Sorcerer.Dispatcher 
  ( Listener(..), addListener, removeListener
  , StreamListener(..), addStreamListener, removeStreamListener
  , StreamManager(..), removeStreamManager, getListeners
  , dispatchWith, dispatchManyWith
  , Unlisten(..)
  ) where

import Data.Sorcerer.Streamable

import Data.Map as Map

import Control.Concurrent
import Data.Foldable
import Data.IORef
import Data.Typeable
import Data.Unique
import GHC.Exts
import System.IO.Unsafe
import Unsafe.Coerce

type Listeners = [(Either Unique ThreadId,[Event] -> IO ())]
type Sink = MVar ([Event] -> IO ())

-- Not using a strict map so the CAS of atomicModifyIORef' can be faster.
-- Since everything inside `Streams` is simple and static, this should be okay.
type Streams ev = Map (Stream ev) (Listeners,Maybe Sink)

{-# NOINLINE dispatchers #-}
dispatchers :: IORef (Map TypeRep (Listeners,Streams Any))
dispatchers = unsafePerformIO (newIORef Map.empty)

newtype Listener ev = Listener (Either Unique ThreadId)
newtype StreamListener ev = StreamListener (Stream ev,Either Unique ThreadId)
newtype StreamManager ev = StreamManager (Stream ev,MVar ([Event] -> IO ()))

class Unlisten a where
  unlisten :: a -> IO ()

instance Typeable ev => Unlisten (Listener ev) where
  unlisten = removeListener

instance (Typeable ev, Ord (Stream ev)) => Unlisten (StreamListener ev) where
  unlisten = removeStreamListener

instance (Typeable ev, Ord (Stream ev)) => Unlisten (StreamManager ev) where
  unlisten = removeStreamManager

addListener :: forall ev. Typeable ev => Either Unique ThreadId -> ([Event] -> IO ()) -> IO (Listener ev)
addListener eut f = do
  let 
    ty = typeOf (undefined :: ev)
    l = [(eut,f)]

    add :: Maybe (Listeners,Streams Any) -> Maybe (Listeners,Streams Any)
    add Nothing = Just (l,Map.empty)
    add (Just (untargeted,streams)) =
      let
        untargeted' = untargeted ++ l
      in
        Just (untargeted',streams)

  atomicModifyIORef' dispatchers (\ds -> (Map.alter add ty ds,()))

  pure (Listener eut)

removeListener :: forall ev. Typeable ev => Listener ev -> IO ()
removeListener (Listener u) = do
  let 
    ty = typeOf (undefined :: ev)

    remove :: Maybe (Listeners,Streams Any) -> Maybe (Listeners,Streams Any)
    remove Nothing = Nothing
    remove (Just (untargeted,streams)) = 
      let
        untargeted' = Prelude.filter ((/= u) . fst) untargeted
      in
        Just (untargeted',streams)

  atomicModifyIORef' dispatchers (\ds -> (Map.alter remove ty ds,()))

addStreamListener :: forall ev. (Typeable ev, Ord (Stream ev)) => Either Unique ThreadId -> Stream ev -> ([Event] -> IO ()) -> IO (StreamListener ev)
addStreamListener eut s f = do
  let 
    ty = typeOf (undefined :: ev)

    l = [(eut,f)]

    insert :: Maybe (Listeners,Streams ev) -> Maybe (Listeners,Streams ev)
    insert Nothing = Just ([],Map.singleton s (l,Nothing))
    insert (Just (untargeted,streams)) =
      let
        add :: Maybe (Listeners,Maybe Sink) -> Maybe (Listeners,Maybe Sink)
        add Nothing = Just (l,Nothing)
        add (Just (ls,mgr)) = Just (ls ++ l,mgr)
      in
        Just (untargeted,Map.alter add s streams)

  atomicModifyIORef' dispatchers $ \ds -> 
    let ds' = unsafeCoerce (Map.alter insert ty (unsafeCoerce ds))
    in (ds',())
    
  pure (StreamListener (s,eut))

removeStreamListener :: forall ev. (Typeable ev, Ord (Stream ev)) => StreamListener ev -> IO ()
removeStreamListener (StreamListener (s,eut)) = do
  let 
    ty = typeOf (undefined :: ev)

    remove :: Maybe (Listeners,Streams ev) -> Maybe (Listeners,Streams ev)
    remove Nothing = Nothing
    remove (Just (untargeted,streams)) =
      let
        delete :: Maybe (Listeners,Maybe Sink) -> Maybe (Listeners,Maybe Sink)
        delete Nothing = Nothing
        delete (Just (ls,mgr)) = Just (Prelude.filter ((/= eut) . fst) ls,mgr)
      in
        Just (untargeted,Map.alter delete s streams)
        
  atomicModifyIORef' dispatchers $ \ds -> 
    let ds' = unsafeCoerce (Map.alter remove ty (unsafeCoerce ds))
    in (ds',())
    
removeStreamManager :: forall ev. (Typeable ev, Ord (Stream ev)) => StreamManager ev -> IO ()
removeStreamManager (StreamManager (s,mv)) = do
  let
    ty = typeOf (undefined :: ev)

    remove :: Maybe (Listeners,Streams ev) -> Maybe (Listeners,Streams ev)
    remove Nothing = Nothing
    remove (Just (untargeted,streams)) =
      let
        delete :: Maybe (Listeners,Maybe Sink) -> Maybe (Listeners,Maybe Sink)
        delete (Just (ls,mgr)) | mgr == Just mv = 
          if Prelude.null ls then 
              Nothing 
            else 
              Just (ls,Nothing)
        delete x = x
      in
        Just (untargeted,Map.alter delete s streams)
    
  atomicModifyIORef' dispatchers $ \ds -> 
    let ds' = unsafeCoerce (Map.alter remove ty (unsafeCoerce ds))
    in (ds',())

getListeners 
  :: forall ev. 
    ( Typeable ev
    , Ord (Stream ev)
    ) => Stream ev -> IO ([Event] -> IO (),Either (StreamManager ev) (StreamManager ev))
getListeners s = do
  let ty = typeOf (undefined :: ev)
  ds <- readIORef dispatchers
  case Map.lookup ty (unsafeCoerce ds) of
    Just (untargeted,streams) | Just (ls,Just mv) <- Map.lookup s streams ->
      -- The fast path. A single readIORef and two map lookups for an open stream.
      let composed evs = traverse_ (($ evs) . snd) (untargeted ++ ls)
      in pure (composed,Right (StreamManager (s,mv)))
    _ -> do
      -- The slow path. Requires a compare and swap.
      mv <- newEmptyMVar
      atomicModifyIORef' dispatchers $ \ds ->

        -- Switch to alterF? We can't use alter since we need to
        -- carry information from the lookups out of the atomicModifyIORef',
        -- but alterF may allow us to smuggle something out.
        case Map.lookup ty (unsafeCoerce ds) of

          -- No listeners or streams
          Nothing -> 
            let 
              streams = Map.singleton s ([],Just mv)
              composed _ = pure ()
              ds' = unsafeCoerce (Map.insert ty ([],streams) (unsafeCoerce ds))
              new = Left (StreamManager (s,mv))
            in
              (ds',(composed,new))

          -- Existing listeners and streams
          Just (untargeted,streams) -> 
            case Map.lookup s streams of

              -- Stream open, listeners may exist 
              -- we must have been slightly early for the fast path
              Just (ls,Just mv) -> 
                let 
                  composed evs = traverse_ (($ evs) . snd) (untargeted ++ ls)
                  existing = Right (StreamManager (s,mv))
                in
                  (ds,(composed,existing))

              -- Stream not open, listeners may exist
              Just (ls,Nothing) -> 
                let 
                  composed evs = traverse_ (($ evs) . snd) (untargeted ++ ls)
                  streams' = Map.insert s (ls,Just mv) streams
                  ds' = unsafeCoerce (Map.insert ty (untargeted,streams') (unsafeCoerce ds))
                  new = Left (StreamManager (s,mv))
                in 
                  (ds',(composed,new))

              Nothing -> 
                let
                  composed evs = traverse_ (($ evs) . snd) untargeted
                  streams' = Map.insert s ([],Just mv) streams
                  ds' = unsafeCoerce (Map.insert ty (untargeted,streams') (unsafeCoerce ds))
                  new = Left (StreamManager (s,mv))
                in
                  (ds',(composed,new))

dispatchWith :: forall ev. (Typeable ev, Ord (Stream ev)) => ([Event] -> StreamManager ev -> IO ()) -> Stream ev -> Event -> IO ()
dispatchWith initialize s ev = dispatchManyWith initialize s [ev]

dispatchManyWith :: forall ev. (Typeable ev, Ord (Stream ev)) => ([Event] -> StreamManager ev -> IO ()) -> Stream ev -> [Event] -> IO ()
dispatchManyWith initialize s evs = go
  where
    go = do
      (untargeted,targeted) <- getListeners s
      case targeted of
        Left tl@(StreamManager (s,mv)) -> do
          -- Newly-created StreamManager requires initialization 
          -- since the callback MVar is empty. 
          initialize evs tl
          untargeted evs

        Right (StreamManager (s,mv)) -> do
          -- presumably this will eventually succeed?
          mf <- tryTakeMVar mv
          case mf of
            Nothing -> do
              yield >> go
            Just f  -> do
              f evs
              putMVar mv f
              untargeted evs

