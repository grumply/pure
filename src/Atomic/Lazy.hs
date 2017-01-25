{-# language MagicHash #-}
module Atomic.Lazy (lazy,lazy2,lazy3,lazy',lazy2',lazy3') where

import Data.IORef
import GHC.Prim
import System.IO.Unsafe

lazifyOne :: IORef (Maybe (a,b)) -> (a -> b) -> a -> b
lazifyOne recent f a = unsafePerformIO $ do
  mab <- readIORef recent
  let upd = do
        let b = f a
        writeIORef recent $ Just (a,b)
        return b
  case mab of
    Nothing -> upd
    Just (a',b) ->
      case reallyUnsafePtrEquality# a a' of
        1# -> return b
        _  -> upd

lazifyTwo :: IORef (Maybe (a,b,c)) -> (a -> b -> c) -> a -> b -> c
lazifyTwo recent f a b = unsafePerformIO $ do
  mabc <- readIORef recent
  let upd = do
        let c = f a b
        writeIORef recent $ Just (a,b,c)
        return c
  case mabc of
    Nothing -> upd
    Just (a',b',c) ->
      case reallyUnsafePtrEquality# a a' of
        1# ->
          case reallyUnsafePtrEquality# b b' of
            1# -> return c
            _  -> upd
        _  -> upd

lazifyThree :: IORef (Maybe (a,b,c,d)) -> (a -> b -> c -> d) -> a -> b -> c -> d
lazifyThree recent f a b c = unsafePerformIO $ do
  mabcd <- readIORef recent
  let upd = do
        let d = f a b c
        writeIORef recent $ Just (a,b,c,d)
        return d
  case mabcd of
    Nothing -> upd
    Just (a',b',c',d) ->
      case reallyUnsafePtrEquality# a a' of
        1# ->
          case reallyUnsafePtrEquality# b b' of
            1# ->
              case reallyUnsafePtrEquality# c c' of
                1# -> return d
                _  -> upd
            _ -> upd
        _ -> upd

lazifyOne' :: Eq a => IORef (Maybe (a,b)) -> (a -> b) -> a -> b
lazifyOne' recent f a = unsafePerformIO $ do
   mab <- readIORef recent
   case mab of
     Nothing -> do
       let b = f a
       writeIORef recent $ Just (a,b)
       return b
     Just (a',b) ->
       case reallyUnsafePtrEquality# a a' of
         1# -> return b
         _  -> if a == a'
               then return b
               else do
                 let b' = f a
                 writeIORef recent $ Just (a,b')
                 return b'

lazifyTwo' :: (Eq a,Eq b) => IORef (Maybe (a,b,c)) -> (a -> b -> c) -> a -> b -> c
lazifyTwo' recent f a b = unsafePerformIO $ do
   mabc <- readIORef recent
   let write c = writeIORef recent $ Just (a,b,c)
   case mabc of
     Nothing -> do
       let c = f a b
       write c
       return c
     Just (a',b',c) ->
       case reallyUnsafePtrEquality# a a' of
         1# ->
           case reallyUnsafePtrEquality# b b' of
             1# -> return c
             _ -> if b == b'
                  then return c
                  else do
                    let c' = f a b
                    write c'
                    return c'
         _  -> if a == a' && b == b'
               then return c
               else do
                 let c' = f a b
                 write c'
                 return c'

lazifyThree' :: (Eq a,Eq b,Eq c)
            => IORef (Maybe (a,b,c,d)) -> (a -> b -> c -> d) -> a -> b -> c -> d
lazifyThree' recent f a b c = unsafePerformIO $ do
  mabcd <- readIORef recent
  let write d = writeIORef recent $ Just (a,b,c,d)
  case mabcd of
    Nothing -> do
      let d = f a b c
      write d
      return d
    Just (a',b',c',d) ->
      case reallyUnsafePtrEquality# a a' of
        1# ->
          case reallyUnsafePtrEquality# b b' of
            1# ->
              case reallyUnsafePtrEquality# c c' of
                1# -> return d
                _ ->
                  if c == c'
                  then return d
                  else do
                    let d' = f a b c
                    write d'
                    return d'
            _ ->
              if b == b' && c == c'
              then return d
              else do
                let d' = f a b c
                write d'
                return d'
        _ ->
          if a == a' && b == b' && c == c'
          then return d
          else do
            let d' = f a b c
            write d'
            return d'

lazy :: (a -> b) -> (a -> b)
lazy f = unsafePerformIO $ do
    recent <- newIORef Nothing
    return $ lazifyOne recent f

lazy2 :: (a -> b -> c) -> (a -> b -> c)
lazy2 f = unsafePerformIO $ do
  recent <- newIORef Nothing
  return $ lazifyTwo recent f

lazy3 :: (a -> b -> c -> d) -> (a -> b -> c -> d)
lazy3 f = unsafePerformIO $ do
  recent <- newIORef Nothing
  return $ lazifyThree recent f

lazy' :: Eq a => (a -> b) -> (a -> b)
lazy' f = unsafePerformIO $ do
    recent <- newIORef Nothing
    return $ lazifyOne' recent f

lazy2' :: (Eq a, Eq b) => (a -> b -> c) -> (a -> b -> c)
lazy2' f = unsafePerformIO $ do
  recent <- newIORef Nothing
  return $ lazifyTwo' recent f

lazy3' :: (Eq a,Eq b,Eq c) => (a -> b -> c -> d) -> (a -> b -> c -> d)
lazy3' f = unsafePerformIO $ do
  recent <- newIORef Nothing
  return $ lazifyThree' recent f
