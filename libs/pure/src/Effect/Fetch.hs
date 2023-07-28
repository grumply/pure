{-# language BlockArguments, RankNTypes, FlexibleContexts, ScopedTypeVariables, AllowAmbiguousTypes, TypeApplications, OverloadedStrings, ConstraintKinds, PatternSynonyms #-}
module Effect.Fetch (XHRError(..),Fetch,fetchIO,fetch,fetchWith,postIO,post,postWith,formIO,form,formWith,response,err,pattern MaxAge,pattern Uncached) where

import Control.Concurrent (newEmptyMVar,putMVar,takeMVar)
import Control.Reader (Reader,reader,ask)
import Control.Monad (join,when)
import Effect.Async (Async,async,await)
import Data.Fetch (XHRError(..))
import qualified Data.Fetch as Fetch
import Data.Foldable (traverse_)
import Data.JSON (Value,FromJSON(..),fromJSON,ToJSON,Result(..))
import Data.Maybe (maybe)
import Data.Txt (Txt)
import Data.Time (Time,time)
import Data.Typeable (Typeable)
import Data.IORef (IORef,newIORef,readIORef,atomicModifyIORef')
import qualified Data.Map as Map (Map,lookup,insert)
import qualified Data.Set as Set (Set,fromList)
import Data.View (View,weak)
import System.IO.Unsafe (unsafePerformIO)

type Fetch a = Async (Either XHRError a)

pattern MaxAge t = Just t
pattern Uncached = Nothing

{-# complete MaxAge, Uncached #-}

{-# INLINE response #-}
response :: forall a b. (Reader XHRError => b) -> (Reader a => b) -> (Fetch a => b)
response failure success = 
  case await @(Either XHRError a) of
    Left  e -> reader e failure
    Right a -> reader a success

{-# INLINE err #-}
err :: Reader XHRError => XHRError
err = ask

{-# INLINE fetchIO #-}
fetchIO :: forall a. (Typeable a, FromJSON a) => Maybe Time -> [(Txt,Txt)] -> Txt -> IO (Either XHRError a)
fetchIO mt hs url = do
  maybe 
    (Fetch.getWith @a hs url) 
    (\t -> fmap (convert @a url) (fetchInternal t hs url)) 
    mt

{-# INLINE fetch #-}
fetch :: forall a. (Typeable a, FromJSON a) => Maybe Time -> Txt -> (Fetch a => View) -> View
fetch t = fetchWith @a t [("Content-Type","application/json"),("Accept","*/*")] 

{-# INLINE fetchWith #-}
fetchWith :: forall a. (FromJSON a, Typeable a) => Maybe Time -> [(Txt,Txt)] -> Txt -> (Fetch a => View) -> View
fetchWith mt hs url v = 
  weak (mt,hs,url) do
    async (maybe (Fetch.getWith @a hs url) (\t -> fmap (convert @a url) (fetchInternal t hs url)) mt) do
      v

{-# INLINE postIO #-}
postIO :: forall b a. (ToJSON a, FromJSON b) => [(Txt,Txt)] -> Txt -> a -> IO (Either XHRError b)
postIO = Fetch.postWith @a @b

{-# INLINE post #-}
post :: forall b a. (ToJSON a, FromJSON b, Typeable a, Typeable b) => Txt -> a -> (Fetch b => View) -> View
post = postWith @b [("Content-Type","application/json"),("Accept","application/json")]

{-# INLINE postWith #-}
postWith :: forall b a. (ToJSON a, FromJSON b, Typeable a, Typeable b) => [(Txt,Txt)] -> Txt -> a -> (Fetch b => View) -> View
postWith hs url a v =
  weak (hs,url,a) do
    async (Fetch.postWith @a @b hs url a) do
      v

{-# INLINE formIO #-}
formIO :: forall a. FromJSON a => [(Txt,Txt)] -> Txt -> [(Txt,Txt)] -> IO (Either XHRError a)
formIO = Fetch.postFormWith @a

{-# INLINE form #-}
form :: forall a. (FromJSON a, Typeable a) => Txt -> [(Txt,Txt)] -> (Fetch a => View) -> View
form = formWith @a [("Content-Type","application/x-www-form-urlencoded"),("Accept","application/json")]

{-# INLINE formWith #-}
formWith :: forall a. (FromJSON a, Typeable a) => [(Txt,Txt)] -> Txt -> [(Txt,Txt)] -> (Fetch a => View) -> View
formWith hs url frm v = 
  weak (hs,url,frm) do
    async (Fetch.postFormWith @a hs url frm) do
      v

--------------------------------------------------------------------------------
-- Internal

{-# INLINE convert #-}
convert :: FromJSON a => Txt -> Either XHRError Value -> Either XHRError a
convert _ (Left e) = Left e
convert url (Right v) =
  case fromJSON v of
    Success a -> Right a
    Error e   -> Left (ParseError url e)

{-# NOINLINE fetchInternal #-}
fetchInternal :: Time -> [(Txt,Txt)] -> Txt -> IO (Either XHRError Value)
fetchInternal d hs url =
  case d of
    0 -> fetch
    _ -> do
      now <- time
      mtv <- lookup
      case mtv of
        Just (t,r) | t + d > now -> pure r
        _ -> fetch
  where
    fetch :: IO (Either XHRError Value)
    fetch = do
      mv <- newEmptyMVar
      empty <- queue (putMVar mv)
      when empty (Fetch.getWith hs url >>= satisfy)
      takeMVar mv

    lookup :: IO (Maybe (Time,Either XHRError Value))
    lookup = do
      rs <- readIORef (responses __cache)
      case Map.lookup (url,Set.fromList hs) rs of
        Just (Right tv) -> pure (Just tv)
        _ -> pure Nothing

    queue :: (Either XHRError Value -> IO ()) -> IO Bool
    queue cb = do
      let k = (url,Set.fromList hs)
      atomicModifyIORef' (responses __cache) $ \rs ->
        case Map.lookup k rs of
          Just (Left cbs) -> (Map.insert k (Left (cbs ++ [cb])) rs,False)
          _ -> (Map.insert k (Left [cb]) rs,True)

    satisfy :: Either XHRError Value -> IO ()
    satisfy v = do
      now <- time
      let k = (url,Set.fromList hs)
      join $ atomicModifyIORef' (responses __cache) $ \rs ->
        case Map.lookup k rs of
          Just (Left cbs) -> (Map.insert k (Right (now,v)) rs,traverse_ ($ v) cbs)
          _ -> (Map.insert k (Right (now,v)) rs,pure ())

newtype Requests = Requests { responses :: IORef (Map.Map (Txt,Set.Set (Txt,Txt)) (Either [Either XHRError Value -> IO ()] (Time,Either XHRError Value))) }
{-# NOINLINE __cache #-}
__cache :: Requests
__cache = Requests (unsafePerformIO (newIORef mempty))


