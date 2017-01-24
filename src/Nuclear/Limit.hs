module Nuclear.Limit (limit) where

import Ef.Base
import Data.JSTime

import Control.Concurrent
import Data.IORef
import System.IO.Unsafe

limitify :: IORef (Int,[JSTime]) -> JSTime -> Int -> IO () -> Bool
limitify limiter duration count m = unsafePerformIO $ do
  (recents,times) <- readIORef limiter
  now <- jstime
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
    cull :: JSTime -> [JSTime] -> Int
    cull cutoff = go 0
      where
        go accN [] = accN
        go accN (x:xs) =
          if x < cutoff
          then accN
          else go (accN + 1) xs

{-# NOINLINE limit #-}
limit :: (Monad super, MonadIO super) => JSTime -> Int -> IO () -> super Bool
limit durationMillis count m = do
  x <- liftIO $
    unsafePerformIO $ do
      limiter <- newIORef (0,[])
      return $ return $ limitify limiter durationMillis count m
  x `seq` return x
