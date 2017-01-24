module Nuclear.Memo (memo,memo2,memo3) where

import Data.Hashable
import Data.IORef
import System.IO.Unsafe

import Data.HashMap.Strict as Map

memoizeOne :: (Eq a,Hashable a) => IORef (Map.HashMap a b) -> (a -> b) -> a -> b
memoizeOne memoTable f a = unsafePerformIO $ do
  mt <- readIORef memoTable
  case Map.lookup a mt of
    Nothing -> do
      let b = f a
      writeIORef memoTable $ Map.insert a b mt
      return b

    Just b ->
      return b

memoizeTwo :: (Eq a,Eq b,Hashable a,Hashable b)
           => IORef (Map.HashMap (a,b) c) -> (a -> b -> c) -> a -> b -> c
memoizeTwo memoTable f a b = unsafePerformIO $ do
  mt <- readIORef memoTable
  case Map.lookup (a,b) mt of
    Nothing -> do
      let c = f a b
      writeIORef memoTable $ Map.insert (a,b) c mt
      return c

    Just c ->
      return c

memoizeThree :: (Eq a,Eq b,Eq c,Hashable a,Hashable b,Hashable c)
             => IORef (Map.HashMap (a,b,c) d)
             -> (a -> b -> c -> d)
             -> a
             -> b
             -> c
             -> d
memoizeThree memoTable f a b c = unsafePerformIO $ do
  mt <- readIORef memoTable
  case Map.lookup (a,b,c) mt of
    Nothing -> do
      let d = f a b c
      writeIORef memoTable $ Map.insert (a,b,c) d mt
      return d

    Just d ->
      return d

memo :: (Eq a,Hashable a) => (a -> b) -> (a -> b)
memo f = unsafePerformIO $ do
  memoTable <- newIORef Map.empty
  return $ memoizeOne memoTable f

memo2 :: (Eq a,Eq b,Hashable a,Hashable b) => (a -> b -> c) -> (a -> b -> c)
memo2 f = unsafePerformIO $ do
  memoTable <- newIORef Map.empty
  return $ memoizeTwo memoTable f

memo3 :: (Eq a,Eq b,Eq c,Hashable a,Hashable b,Hashable c)
      => (a -> b -> c -> d) -> (a -> b -> c -> d)
memo3 f = unsafePerformIO $ do
  memoTable <- newIORef Map.empty
  return $ memoizeThree memoTable f
