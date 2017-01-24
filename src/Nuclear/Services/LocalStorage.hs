{-# language CPP #-}
{-# language OverloadedStrings #-}
module Nuclear.Services.LocalStorage where

import Ef.Base

import Data.JSText as AE
import Data.ByteString

import Nuclear.Atom (Win,getWindow)
import Nuclear.Revent
import Nuclear.Service
import Nuclear.Signals
import Nuclear.ToText
import Nuclear.FromText
import Nuclear.FromBS
import Nuclear.Strict
import Nuclear.With
import Nuclear.Message
import Nuclear.TypeRep

import Nuclear.WebSocket

import qualified Data.JSText as JSText

#ifdef __GHCJS__
import qualified GHCJS.DOM.EventM as Ev
import qualified GHCJS.DOM.EventTargetClosures as Ev
import qualified GHCJS.DOM.Storage as S
import qualified GHCJS.Marshal as M
import qualified GHCJS.Marshal.Pure as M
import qualified JavaScript.Object as O
import qualified JavaScript.Object.Internal as O
import qualified JavaScript.Array as JSA
import qualified JavaScript.JSON.Types.Internal as JSON

import qualified GHCJS.DOM.Window as W
#endif

import qualified Data.HashMap.Strict as Map

import Data.Typeable
import Data.Maybe

#ifdef __GHCJS__
-- multi-line strings and CPP = pain
foreign import javascript unsafe
  "var arr = []; for (var i = 0, len = localStorage.length; i < len; i++) { var key = localStorage.key(i); var value = localStorage[key]; arr.push({ key: key, value: value }); }; $r = arr;"
  readLocalStorage :: IO JSA.JSArray
#endif

type LocalStorageS = (State () (Map.HashMap JSText Value)) ': Service_

localStorageS :: S '[State () (Map.HashMap JSText Value)]
localStorageS = Service {..}
  where
    key = "Fusion.LocalStorage"

    build base = do
#ifdef __GHCJS__
      jsa <- readLocalStorage
      let jsvs = JSA.toList jsa
      kvs :: [(JSText,AE.Value)] <- fmap catMaybes $ forM jsvs $ \jsv -> do
        let o = O.Object jsv
        k <- O.unsafeGetProp "key" o
        v <- O.unsafeGetProp "value" o
        let key = M.pFromJSVal k
            value = js_JSON_parse (M.pFromJSVal v)
        return $ Just (key,value)
#else
      let kvs = []
#endif
      return (state (Map.fromList kvs) *:* base)

    prime = return ()

getLocalItem :: (MonadIO c, AE.FromJSON a) => JSText -> c (Promise (Maybe a))
getLocalItem k = do
  with localStorageS $ do
    hm <- get
    let mv = Map.lookup k hm
    case mv of
      Nothing -> return Nothing
      Just v  ->
        case AE.fromJSON v of
          AE.Error _ -> return Nothing
          AE.Success a -> return $ Just a

putLocalItem :: (MonadIO c, AE.ToJSON a) => JSText -> a -> c (Promise Bool)
putLocalItem k v = do
  let value = AE.toJSON v
  with localStorageS $ do
    hm :: Map.HashMap JSText AE.Value <- get
    put $ Map.insert k value hm
    win <- getWindow
#ifdef __GHCJS__
    Just ls <- W.getLocalStorage win
    res <- liftIO $ js_setItem_catch ls k (toText value)
    case res of
      0 -> return False
      _ -> return True
#else
    return True
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "try { $1[\"setItem\"]($2, $3); $r = 1; } catch (e) { $r = 0; }"
  js_setItem_catch :: S.Storage -> JSText -> JSText -> IO Int
#endif

removeLocalItem :: (MonadIO c) => JSText -> c (Promise ())
removeLocalItem k = do
  with localStorageS $ do
    hm :: Map.HashMap JSText AE.Value <- get
    put $ Map.delete k hm
    win <- getWindow
#ifdef __GHCJS__
    Just ls <- W.getLocalStorage win
    S.removeItem ls k
#else
    return ()
#endif

clearLocalStorage :: (MonadIO c) => c (Promise ())
clearLocalStorage = do
  with localStorageS $ do
    put (Map.empty :: Map.HashMap JSText AE.Value)
    win <- getWindow
#ifdef __GHCJS__
    Just ls <- W.getLocalStorage win
    S.clear ls
#else
    return ()
#endif

-- This needs a home; it captures both localstorage events and sessionstorage events.
onStorage :: ( MonadIO c
             , '[Revent] <: ms
             )
          => (JSText.Object -> Code '[Event JSText.Object] (Code ms c) ())
          -> Code ms c (Subscription ms c JSText.Object,Periodical ms c JSText.Object)
onStorage =
  onWindowNetwork
#ifdef __GHCJS__
    (Ev.unsafeEventName "storage" :: EVName Win JSText.Object)
#else
    ("storage" :: EVName Win JSText.Object)
#endif

localMessage :: ( MonadIO c
                , Typeable messageType
                , Message messageType
                , M messageType ~ message
                , ToJSON message
                )
            => Proxy messageType -> message -> c (Promise Bool)
localMessage mty_proxy = putLocalItem (messageHeader mty_proxy)


proxyLocalMessage :: forall traits ms c msg message messageType.
                      ( MonadIO c
#ifdef __GHCJS__
                      , '[WebSocket] <: ms
#else
                      , '[State () WebSocket] <: ms
#endif
                      , IsService traits ms
                      , Typeable messageType
                      , Message messageType
                      , M messageType ~ message
                      , ToJSON message
                      , FromBS message
                      , FromJSON message
                      )
                  => Service traits ms
                  -> Proxy messageType
                  -> c (Subscription ms IO JSText.Object,Periodical ms IO JSText.Object)
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
                win <- getWindow
                Just ls <- W.getLocalStorage win
                S.removeItem ls key
                case fromBS (lazify (fromText nv :: ByteString)) of
                  Left _ -> liftIO $ Prelude.putStrLn "Bad message from storage event."
                  Right (m :: message) ->
                    void $ lift $ sendSelfMessage s mty_proxy m
#else
                case fromBS (lazify (fromText nv :: ByteString)) of
                  Left _ -> liftIO $ Prelude.putStrLn "Bad message from storage event."
                  Right (m :: message) -> return ()
#endif
            else
              pure (return ())
      forM_ mres id
  return a

onLocalMessage :: forall ms c msg message messageType.
                  ( MonadIO c
                  , Typeable messageType
                  , Typeable message
                  , Message messageType
                  , M messageType ~ message
                  , FromJSON message
                  , FromBS message
                  , '[Revent] <: ms
                  )
               => Proxy messageType
               -> (message -> Code ms c ())
               -> Code ms c (Subscription ms c JSText.Object,Periodical ms c JSText.Object)
onLocalMessage mty_proxy f = do
  slf <- asSelf
  onStorage $ \se -> do
    let mres = flip parseMaybe se $ \o -> do
          key <- se .: "key"
          if key == messageHeader mty_proxy then do
            nv <- se .: "newValue"
            pure $ do
#ifdef __GHCJS__
              win <- getWindow
              Just ls <- W.getLocalStorage win
              S.removeItem ls key
#endif
              case fromBS (lazify (fromText nv :: ByteString)) of
                Left _ -> liftIO $ Prelude.putStrLn $ "Bad message from storage event: " ++ show (key,nv)
                Right (m :: message) -> void $ runAs slf (f m)
          else
            pure (return ())
    forM_ mres id
