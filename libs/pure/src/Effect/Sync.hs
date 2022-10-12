{-# language RankNTypes, MagicHash, TypeFamilies, TypeOperators, RecordWildCards, PatternSynonyms, NamedFieldPuns, ConstraintKinds, FlexibleContexts #-}
module Effect.Sync (Sync,sync,demand) where

import Control.Dynamic
import qualified Control.Reader as Reader
import Data.Default
import Data.Exists
import Data.Typeable
import Data.View (pattern Component,Comp(..),View,eager,ask)
import GHC.Exts (isTrue#,reallyUnsafePtrEquality#)
import System.IO.Unsafe

type Sync a = Reader.Reader a

data Synchronous a = Synchronous (IO a) (Sync a :=> View)
data Model a = Model 
  { action :: IO a
  , result    :: a
  , view   :: Sync a :=> View
  }

{-# INLINE sync #-}
sync :: Typeable a => IO a -> (Sync a => View) -> View
sync io v = go (Synchronous io (dynamic v))
  where
    go = Component $ \self -> def
      { onConstruct = do
        Synchronous action view <- ask self
        result <- action
        pure Model {..}

      , onForce = \(Synchronous new_action new_view) Model {..} -> do
          let
            sameAction = isTrue# (reallyUnsafePtrEquality# new_action action) 
            sameView   = isTrue# (reallyUnsafePtrEquality# new_view view)
          pure (not sameAction || not sameView)

      , onReceive = \(Synchronous new_action new_view) old@Model {..} -> do
          let
            sameAction = isTrue# (reallyUnsafePtrEquality# new_action action) 
            sameView   = isTrue# (reallyUnsafePtrEquality# new_view view)
          case (sameAction,sameView) of
            (False,_) -> do
              let action = new_action
              let view = new_view
              result <- action
              pure Model {..}
            (_,False) -> do
              pure old { view = new_view }
            _ -> do
              pure old

      , render = \_ Model { view, result } -> with result (fromDynamic view)
      }

demand :: Sync a => a
demand = it