{-# language CPP #-}
module Data.Websocket.Internal (module Export) where

#ifdef __GHCJS__
import Data.Websocket.Internal.GHCJS as Export
#else
import Data.Websocket.Internal.GHC   as Export
#endif