{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
module Pure.Services.LocalStorage where

import Ef.Base

import Pure.Service
import Pure.Signals
import Pure.Data
import Pure.Lifted
import Pure.Data.JSV
import Pure.Data.Txt (Txt,ToTxt(..),FromTxt(..))
import Pure.WebSocket

import qualified Pure.Data.Txt as Txt

#ifdef __GHCJS__
import qualified GHCJS.Marshal as M
import qualified GHCJS.Marshal.Pure as M
import qualified JavaScript.Object as O
import qualified JavaScript.Object.Internal as O
import qualified JavaScript.Array as JSA
import qualified JavaScript.JSON.Types.Internal as JSON
#else
import Data.Text.Lazy.Encoding as TL
#endif

import qualified Data.HashMap.Strict as Map

import Data.Typeable
import Data.Maybe
import Data.List

-- import Control.Lens as L

#ifdef __GHCJS__
-- multi-line strings and CPP = pain
foreign import javascript unsafe
  "var arr = []; for (var i = 0, len = window.localStorage.length; i < len; i++) { var key = window.localStorage.key(i); var value = window.localStorage[key]; arr.push({ key: key, value: value }); }; $r = arr;"
  readLocalStorage_js :: IO JSA.JSArray

foreign import javascript unsafe
  "$r = window.localStorage.getItem($1)" getItem_js :: Txt -> IO JSV

foreign import javascript unsafe
  "LZString.compress($1)" compress :: Txt -> IO Txt

foreign import javascript unsafe
  "LZString.decompress($1)" decompress :: Txt -> IO Txt
#endif

localStorageS = Service {..}
  where
    key = "pure.localStorage"
    build base = return (state (Map.empty :: Map.HashMap Txt Value) *:* base)
    prime = return ()

readLocalStorage = do
#ifdef __GHCJS__
  jsa <- liftIO readLocalStorage_js
  let jsvs = JSA.toList jsa
  kvs :: [(Txt,Value)] <- liftIO $ fmap catMaybes $ forM jsvs $ \jsv -> do
    let o = O.Object jsv
    k <- O.unsafeGetProp "key" o
    v <- O.unsafeGetProp "value" o
    let key = M.pFromJSVal k
        txt = M.pFromJSVal v
    value <- fmap js_JSON_parse $ decompress txt
    return $ Just (key,value)
  put (Map.fromList kvs :: Map.HashMap Txt Value)
#else
  return ()
#endif

putLocalItemIfNotExists :: (MonadIO c, ToJSON a) => Txt -> a -> c (Promise Bool)
putLocalItemIfNotExists key val = do
  with localStorageS $ do
    kvs :: Map.HashMap Txt Value <- get
    case Map.lookup key kvs of
      Nothing -> do
#ifdef __GHCJS__
        mi <- liftIO $ getItem_js key
        if isNull mi
          then putLocalItem key val >> return True
          else return False
#else
        put $ Map.insert key (toJSON val) kvs
        return True
#endif
      Just _ -> return False

itemExists :: MonadIO c => Txt -> c (Promise Bool)
itemExists key = do
  with localStorageS $ do
    kvs :: Map.HashMap Txt Value <- get
    case Map.lookup key kvs of
      Nothing -> do
#ifdef __GHCJS__
        mi <- liftIO $ getItem_js key
        return $ isNull mi
#else
        return False
#endif
      Just _ -> return True

purgeExpired :: MonadIO c => Millis -> c (Promise ())
purgeExpired expiration = do
  with localStorageS $ do
    readLocalStorage
    kvs :: [(Txt,Value)] <- Map.toList <$> get
    let expiredKVs = findExpired kvs
    put $ Map.fromList (kvs \\ expiredKVs)
    for_ expiredKVs $ \(k,_) -> removeLocalItem k
  where
    findExpired [] = []
    findExpired ((k,val):kvs) =
      case fromJSON val of
        Success (t,v :: Value) | t < expiration -> (k,val):findExpired kvs
                               | otherwise      -> findExpired kvs
        _                                       -> findExpired kvs

getLocalItem :: (MonadIO c, FromJSON a) => Txt -> c (Promise (Maybe (Millis,a)))
getLocalItem k = do
  with localStorageS $ do
    hm <- get
    case Map.lookup k hm of
      Just val  ->
        case fromJSON val of
          Error   _ -> return Nothing
          Success a -> return $ Just a
      Nothing -> do
#ifdef __GHCJS__
        v <- liftIO $ getItem_js k
        if isNull v
          then return Nothing
          else do
            val <- liftIO $ fmap js_JSON_parse $ decompress $ M.pFromJSVal v
            put $ Map.insert k val hm
            case fromJSON val of
              Error   _ -> return Nothing
              Success a -> return $ Just a
#else
        return Nothing
#endif

putLocalItem :: (MonadIO c, ToJSON a) => Txt -> a -> c (Promise Bool)
putLocalItem k v = do
  now <- millis
  let value = toJSON (now,v)
  with localStorageS $ do
    hm :: Map.HashMap Txt Value <- get
    put $ Map.insert k value hm
#ifdef __GHCJS__
    txt <- liftIO $ compress (toTxt value)
    res <- liftIO $ js_setItem_catch k txt
    case res of
      0 -> return False
      _ -> return True
#else
    return True
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "try { window.localStorage.setItem($1, $2); $r = 1; } catch (e) { $r = 0; }"
  js_setItem_catch :: Txt -> Txt -> IO Int

foreign import javascript unsafe
  "window.localStorage.removeItem($1);" removeItem :: Txt -> IO ()

foreign import javascript unsafe
  "window.localStorage.clear()" clear_localStorage_js :: IO ()
#endif

removeLocalItem :: (MonadIO c) => Txt -> c (Promise ())
removeLocalItem k = do
  with localStorageS $ do
    hm :: Map.HashMap Txt Value <- get
    put $ Map.delete k hm
#ifdef __GHCJS__
    liftIO $ removeItem k
#else
    return ()
#endif

clearLocalStorage :: (MonadIO c) => c (Promise ())
clearLocalStorage = do
  with localStorageS $ do
    put (Map.empty :: Map.HashMap Txt Value)
#ifdef __GHCJS__
    liftIO clear_localStorage_js
#else
    return ()
#endif

-- This needs a home; it captures both localstorage events and sessionstorage events.
onStorage :: (MonadIO c, ms <: '[Evented], e ~ Ef ms c)
          => (Obj -> Ef '[Event Obj] e ()) -> e (IO ())
onStorage = onWindowSyndicate "storage"

localMessage :: (MonadIO c, Typeable mTy, Message mTy, M mTy ~ m, ToJSON m)
             => Proxy mTy -> m -> c (Promise Bool)
localMessage mty_proxy = putLocalItem (messageHeader mty_proxy)

proxyLocalMessage :: forall ts ms ms' c m mTy.
                      ( MonadIO c
                      , ms <: '[State () EvQueue,WS]
                      , IsService' ts ms
                      , Typeable mTy
                      , Message mTy
                      , M mTy ~ m
                      , ToJSON m
                      , FromJSON m
                      )
                  => Service' ts ms -> Proxy mTy -> c (IO ())
proxyLocalMessage s mty_proxy = do
  let header = messageHeader mty_proxy
  Right a <- (=<<) demand $ with s $
    onStorage $ \se -> do
      let mres = flip parseMaybe se $ \o -> do
            key <- se .: "key"
            if key == header then do
              nv <- se .: "newValue"
              pure $ do
#ifdef __GHCJS__
                liftIO $ removeItem key
                case fromJSON (js_JSON_parse nv) of
                  Error _ -> liftIO $ Prelude.putStrLn "Bad message from storage event."
                  Success (m :: m) -> void $ lift $ sendSelfMessage s mty_proxy m
#else
                case eitherDecode' (TL.encodeUtf8 nv) of
                  Left _ -> liftIO $ Prelude.putStrLn "Bad message from storage event."
                  Right (m :: m) -> return ()
#endif
            else
              pure (return ())
      forM_ mres id
  return a

onLocalMessage :: forall ms c m mTy e.
                  ( MonadIO c
                  , Typeable mTy
                  , Typeable m
                  , Message mTy
                  , M mTy ~ m
                  , FromJSON m
                  , ms <: '[Evented]
                  , e ~ Ef ms c
                  )
               => Proxy mTy -> (m -> e ()) -> e (IO ())
onLocalMessage mty_proxy f = do
  slf <- asSelf
  onStorage $ \se -> do
    let mres = flip parseMaybe se $ \o -> do
          key <- se .: "key"
          if key == messageHeader mty_proxy then do
            nv <- se .: "newValue"
            pure $ do
#ifdef __GHCJS__
              liftIO $ removeItem key
              case fromJSON (js_JSON_parse nv) of
                Error _   -> liftIO $ Prelude.putStrLn $ "Bad message from storage event: " ++ show (key,nv)
                Success m -> void $ runAs slf (f m)
#else
              case eitherDecode' (TL.encodeUtf8 nv) of
                Left _ -> liftIO $ Prelude.putStrLn $ "Bad message from storage event: " ++ show (key,nv)
                Right m -> void $ runAs slf (f m)
#endif
          else
            pure (return ())
    forM_ mres id
