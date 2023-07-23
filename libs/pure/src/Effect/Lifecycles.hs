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

instance Default Lifecycles where
  def = Lifecycles def def def def def

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

{-


{-# INLINE onStop #-}
onStop :: IO a -> View -> View
onStop f v = Component go (void f,v)
  where
    {-# INLINE go #-}
    go self = def
      { onConstruct = pure ()
      , onUnmounted = ask self >>= fst
      , render = \(_,v) _ -> v
      }
-}
