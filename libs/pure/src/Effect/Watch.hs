{-# language RankNTypes, ScopedTypeVariables, ConstraintKinds, BlockArguments, FlexibleContexts, TypeFamilies, MagicHash, PatternSynonyms #-}
module Effect.Watch where

import Control.Applicative
import Control.Monad (join,unless,void)
import Data.Default (Default(def))
import Data.View (pattern Component,Comp(..),View,ask)
import GHC.Exts

watch :: IO () -> View
watch = Component go
  where
    go self = def
      { onConstruct = join (ask self)
      , onUpdate    = const
      , onUnmounted = join (ask self)
      }
  

