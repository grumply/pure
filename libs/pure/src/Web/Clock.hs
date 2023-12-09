{-# LANGUAGE RankNTypes, BangPatterns, TypeApplications, FlexibleContexts #-}
module Web.Clock where

import Data.Exists
import Data.Time
import Data.View
import System.IO.Unsafe
import Data.List as List
import Control.Parallel

{-# INLINE every #-}
every :: forall a. Time -> (Exists Time => a) -> a
every t a = go 
  where 
    {-# NOINLINE go #-}
    go | !a <- delayed t (let t = unsafePerformIO time in t `pseq` with t a)
       = go

{-# INLINE at #-}
at :: forall a. [Time] -> (Exists Time => a) -> a
at = atWith False 

{-# INLINE dot #-}
-- dot, as in: on-the-dot
dot :: forall a. Time -> (Exists Time => a) -> a
dot = dotWith False

{-# INLINE dotWith #-}
dotWith :: forall a. Bool -> Time -> (Exists Time => a) -> a
dotWith truth t = 
  let 
    fromInt = fromIntegral @Int
    now = unsafePerformIO time
    d = fromInt (floor (now / t) :: Int)
    start = d * t
  in 
    atWith truth [ start + fromInt n * t | n <- [1 :: Int ..] ]

{-# INLINE atWith #-}
atWith :: forall a. Bool -> [Time] -> (Exists Time => a) -> a
atWith truth [] a = with (unsafePerformIO time) a
atWith truth ts a = go ts
  where 
    {-# NOINLINE go #-}
    go ~(t:ts)
      | !s <- unsafePerformIO time
      , () <- unsafePerformIO (delay (t - s))
      , !n  <- if truth then unsafePerformIO time else t
      , Milliseconds ms _ <- n - t
      = if List.null ts then with n a else with n a `pseq` go ts

