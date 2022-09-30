{-# language CPP #-}
module Data.Fetch (module Export) where

#ifdef __GHCJS__
import Data.Fetch.GHCJS as Export 
#else
import Data.Fetch.GHC as Export 
#endif