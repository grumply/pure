module Atomic.Limit (limit) where

import Ef.Base
import Data.Millis

import Control.Concurrent
import Data.IORef
import System.IO.Unsafe

limitify :: IORef (Int,[Millis]) -> Millis -> Int -> IO () -> Bool
limitify limiter duration count m = unsafePerformIO $ do
  (recents,times) <- readIORef limiter
  now <- millis
  if recents == count then do
    let recents' = cull (now - duration) times
        times' = take recents' times
    writeIORef limiter (recents',now:times')
    if recents' == count then do
      m
      return True
    else
      return False
  else do
    let !recents' = recents + 1
    writeIORef limiter (recents',now:times)
    m
    return True
  where
    cull :: Millis -> [Millis] -> Int
    cull cutoff = go 0
      where
        go accN [] = accN
        go accN (x:xs) =
          if x < cutoff
          then accN
          else go (accN + 1) xs

{-# NOINLINE limit #-}
limit :: MonadIO c => Millis -> Int -> IO () -> c Bool
limit durationMillis count m = do
  x <- liftIO $
    unsafePerformIO $ do
      limiter <- newIORef (0,[])
      return $ return $ limitify limiter durationMillis count m
  x `seq` return x
