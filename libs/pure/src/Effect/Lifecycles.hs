{-# language RankNTypes, ScopedTypeVariables, ConstraintKinds, BlockArguments, FlexibleContexts, TypeFamilies, MagicHash, PatternSynonyms #-}
module Effect.Lifecycles (onStart,onLoad,onChange,onChanged,onStop) where

import Control.Monad (unless,void)
import Data.Default (Default(def))
import Data.View (pattern Component,Comp(..),View,ask)
import GHC.Exts

{-# INLINE onStart #-}
onStart :: IO a -> View -> View
onStart f v = Component go (void f,v)
  where
    {-# INLINE go #-}
    go self = def
      { onConstruct = pure ()
      , onMount = \m -> ask self >>= fst >> pure m
      , render = \(_,v) _ -> v
      }

{-# INLINE onLoad #-}
onLoad :: IO a -> View -> View
onLoad f v = Component go (void f,v)
  where
    {-# INLINE go #-}
    go self = def
      { onConstruct = pure ()
      , onMounted = ask self >>= fst
      , render = \(_,v) _ -> v
      }
  
{-# INLINE onChange #-}
onChange :: IO a -> View -> View
onChange f v = Component go (void f,v)
  where
    {-# INLINE go #-}
    go self = def
      { onConstruct = pure ()
      , onReceive = \(new_change,new_view) old -> do
        (_,view) <- ask self
        unless (isTrue# (reallyUnsafePtrEquality# view new_view)) new_change
        pure old
      , render = \(_,v) _ -> v
      }

{-# INLINE onChanged #-}
onChanged :: IO a -> View -> View
onChanged f v = Component go (void f,v)
  where
    {-# INLINE go #-}
    go self = def
      { onConstruct = pure ()
      , onUpdated = \(changed,_) _ -> changed
      , render = \(_,v) _ -> v
      }

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