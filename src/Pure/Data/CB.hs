{-# LANGUAGE CPP #-}
module Pure.Data.CB
  ( module Pure.Data.CB
#ifdef __GHCJS__
  , module CB
#endif
  ) where

#ifdef __GHCJS__
import GHCJS.Foreign.Callback as CB
#endif

type CB a =
#ifdef __GHCJS__
  CB.Callback a
#else
  a
#endif

