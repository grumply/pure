{-# language RankNTypes, ScopedTypeVariables, ConstraintKinds, BlockArguments, FlexibleContexts, TypeFamilies, MagicHash, PatternSynonyms #-}
module Effect.Lifecycles (Lifecycles(..),lifecycles) where

import Control.Applicative
import Control.Monad (join,unless,void)
import Data.Default (Default(def))
import Data.View (pattern Component,Comp(..),View,ask)
import GHC.Exts

-- A record of listeners to lifecycle events.
--
-- This approach reduces the component overhead.
--
data Lifecycles = Lifecycles
  { onStart :: IO ()
  , onLoad :: IO ()
  , onBefore :: IO ()
  , onAfter :: IO ()
  , onStop :: IO ()
  }

instance Semigroup Lifecycles where
  (<>) lc1 lc2 = Lifecycles 
    { onStart = onStart lc1 >> onStart lc2
    , onLoad = onLoad lc1 >> onLoad lc2
    , onBefore = onBefore lc1 >> onBefore lc2
    , onAfter = onAfter lc1 >> onAfter lc2
    , onStop = onStop lc1 >> onStop lc2
    }

instance Monoid Lifecycles where
  mempty = def

instance Default Lifecycles where
  def = Lifecycles def def def def def

{-# RULES 
  "lifecyles lc1 (lifecycles lc2) => lifecycles (lc1 <> lc2)"
    forall lc1 lc2 v. lifecycles lc1 (lifecycles lc2 v) = lifecycles (lc1 <> lc2) v
 #-}

{-# INLINE [1] lifecycles #-}
lifecycles :: Lifecycles -> View -> View
lifecycles ls v = Component go (ls,v)
  where
    go self = def
      { onConstruct = ask self >>= onStart . fst
      , onMounted = ask self >>= onLoad . fst
      , onUpdate = \(ls',v') _ -> do
          (_,v) <- ask self
          unless (isTrue# (reallyUnsafePtrEquality# v v')) (onBefore ls')
      , onUpdated = \(ls,v) _ -> onAfter ls
      , onUnmounted = ask self >>= onStop . fst
      , render = \(_,v) _ -> v
      }

