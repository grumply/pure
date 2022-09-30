{-# language PatternSynonyms, TypeApplications #-}
module Effect.Fork (fork) where

import Control.State (manage,put)
import Control.Reader (ask)
import Data.Default
import Data.View (View,Comp(deferred,render),pattern Component)

import Debug.Trace

data Fork

-- Fork an asynchronous rendering context. 
--
-- Consider:
-- 
-- > async action do
-- >   fork do
-- >     { ... await ... }
--
-- Without fork, await is render-blocking. With fork, the await is performed
-- within a new thread and, therefore, only blocks the fork.
--
{-# INLINE fork #-}
fork :: View -> View
fork = Component @View @Fork (const def { deferred = True, render = const })
