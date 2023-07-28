{-# language RankNTypes, ScopedTypeVariables, ConstraintKinds, BlockArguments, FlexibleContexts, TypeFamilies, MagicHash, PatternSynonyms, TypeApplications, NamedFieldPuns, RecordWildCards, TypeOperators #-}
module Effect.Async (Async,async,await) where

import Control.Concurrent (MVar,newEmptyMVar,putMVar,readMVar,forkIO,ThreadId,myThreadId,newMVar,forkIOWithUnmask)
import Control.Dynamic
import Control.Exception
import qualified Control.Reader as Reader (Reader,ask)
import Data.Coerce (coerce)
import Data.Exists (with,it)
import Data.Typeable (Typeable)
import Data.View (pattern Component,Comp(..),View,ask,modifyM)
import Effect.Fork
import GHC.Exts
import System.IO.Unsafe (unsafePerformIO)
import Debug.Trace
import Data.Default (Default(def))

type Async a = Reader.Reader a

data Asynchronous a = Asynchronous (IO a) (Async a :=> View)

data Model a = Model 
  { action :: IO a
  , view :: Async a :=> View
  , thread :: ThreadId 
  , result :: MVar a
  }

{-# INLINE async #-}
-- Note that only the asynchrony of the first action can be witnessed in View
async :: forall a. Typeable a => IO a -> (Async a => View) -> View
async io v = go (Asynchronous io (dynamic v))
  where
    go = Component \self -> def
      { deferred = True

      , onConstruct = do
          Asynchronous action view <- ask self
          result <- newEmptyMVar
          thread <- forkIO (action >>= putMVar result)
          pure Model {..}

      , onForce = \(Asynchronous new_action new_view) old@Model {..} -> do
          let
            sameAction = isTrue# (reallyUnsafePtrEquality# new_action action) 
            sameView   = isTrue# (reallyUnsafePtrEquality# new_view view)
          pure (not sameAction || not sameView)

      , onReceive = \(Asynchronous new_action new_view) old@Model {..} -> do
          let
            sameAction = isTrue# (reallyUnsafePtrEquality# new_action action) 
            sameView   = isTrue# (reallyUnsafePtrEquality# new_view view)
          case (sameAction,sameView) of
            (False,_) -> do
              let action = new_action
              let view = new_view
              thread <- forkIO do
                mask $ \unmask -> do
                  a <- action 
                  unmask do
                    tid <- myThreadId
                    modifyM self $ \_ Model {..} ->
                      if tid == thread then do
                        result <- newMVar a
                        pure (Model {..},def)
                      else
                        pure (Model {..},def)
                    pure ()
              pure Model {..}

            (_,False) -> do
              pure old { view = new_view }

            _ -> do
              pure old

      , render = \_ Model { view, result } -> 
          with (unsafePerformIO (readMVar result)) (fromDynamic view)

      }

await :: forall a. Async a => a
await = it
