{-# language RankNTypes, ScopedTypeVariables, FlexibleContexts, BlockArguments, TypeOperators, MagicHash, PatternSynonyms, RecordWildCards, NamedFieldPuns, MultiWayIf #-}
module Effect.Poll (Async,poll,await) where

import Control.Concurrent (MVar,newEmptyMVar,putMVar,readMVar,forkIO,ThreadId,killThread,newMVar,myThreadId)
import Control.Dynamic
import Control.Exception
import Control.Monad
import qualified Control.Reader as Reader (Reader,ask)
import Control.State (manage)
import Data.Coerce (coerce)
import Data.Exists (with)
import Data.Time
import Data.Typeable (Typeable)
import Data.View (pattern Component,Comp(..),View,eager,ask,modifyM,get)
import Effect.Fork
import GHC.Exts
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace
import Data.Default (Default(def))
import Effect.Async (Async,await)

data Polling a = Polling Time (IO a) (Async a :=> View)

data Model a = Model
  { interval :: Time
  , begin :: Time
  , action :: IO a 
  , view :: Async a :=> View
  , thread :: ThreadId
  , result :: MVar a
  }

{-# INLINE poll #-}
-- Note that only the asynchrony of the first action can be witnessed in View
poll :: forall a. Typeable a => Time -> IO a -> (Async a => View) -> View
poll t f v = go (Polling t f (dynamic v))
  where
    go = Component \self -> def
      { deferred = True

      , onConstruct = do
          begin <- time
          Polling interval action view <- ask self
          result <- newEmptyMVar
          thread <- forkIO do

            mask $ \unmask -> do
              a <- action 
              unmask (putMVar result a)

            forever do
              delay interval
              mask $ \unmask -> do
                a <- action
                unmask do
                  tid <- myThreadId
                  modifyM self $ \_ Model {..} ->
                    if tid == thread then do
                      begin <- time
                      result <- newMVar a
                      pure (Model {..},def)
                    else
                      pure (Model {..},killThread tid)

          pure Model {..}

      , onForce = \(Polling new_interval new_action new_view) old@Model {..} -> do
          let
            sameInterval = new_interval == interval
            sameAction   = isTrue# (reallyUnsafePtrEquality# new_action action)
            sameView     = isTrue# (reallyUnsafePtrEquality# new_view view)
          pure (not sameInterval || not sameAction || not sameView)

      , onReceive = \(Polling new_interval new_action new_view) old@Model {..} -> do
          let
            sameInterval = new_interval == interval
            sameAction   = isTrue# (reallyUnsafePtrEquality# new_action action) 
            sameView     = isTrue# (reallyUnsafePtrEquality# new_view view)
          now <- time
          if 
            | not sameInterval || not sameAction -> do
              let interval = new_interval
              let begin = now
              let action = new_action
              let view = new_view
              thread <-
                forkIO do
                  when (not sameInterval && sameAction) do
                    delay ((begin + new_interval) - now)
                  forever do
                    mask $ \unmask -> do
                      a <- action
                      unmask do
                        tid <- myThreadId
                        modifyM self $ \_ Model {..} ->
                          if tid == thread then do
                            begin <- time
                            result <- newMVar a
                            pure (Model {..},def)
                          else
                            pure (Model {..},killThread tid)
                        delay interval
              pure Model {..}

            | not sameView -> do
              pure old { view = new_view }
              
            | otherwise ->
              pure old

      , onUnmounted = get self >>= \Model { thread } -> killThread thread

      , render = \_ Model { view, result } -> 
          with (unsafePerformIO (readMVar result)) (fromDynamic view)
      }
