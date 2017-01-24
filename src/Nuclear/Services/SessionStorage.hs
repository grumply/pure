{-# language CPP #-}
{-# language OverloadedStrings #-}
module Nuclear.Services.SessionStorage where

import Ef.Base

import Data.JSText

import Nuclear.Atom (Win,getWindow)
import Nuclear.Service
import Nuclear.With
import Nuclear.FromText
import Nuclear.ToText
import Nuclear.FromBS

#ifdef __GHCJS__
import Fusion.WebSocket
#endif

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
import Data.String

#ifdef __GHCJS__
foreign import javascript unsafe
  "var arr = []; for (var i = 0, len = sessionStorage.length; i < len; i++) { var key = sessionStorage.key(i); var value = sessionStorage[key]; arr.push({ key: key, value: value }); }; $r = arr;"
  readSessionStorage :: IO JSA.JSArray
#endif

type SessionStorageS = (State () (Map.HashMap JSText Value)) ': Service_

sessionStorageS :: S '[State () (Map.HashMap JSText Value)]
sessionStorageS = Service {..}
  where
    key = "Fusion.SessionStorage"

    build base = do
#ifdef __GHCJS__
      jsa <- readSessionStorage
      let jsvs = JSA.toList jsa
      kvs :: [(JSText,Value)] <- fmap catMaybes $ forM jsvs $ \jsv -> do
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

getSessionItem :: (MonadIO c, FromJSON a) => JSText -> c (Promise (Maybe a))
getSessionItem (k :: JSText) = do
  with sessionStorageS $ do
    hm <- get
    let mv = Map.lookup k hm
    case mv of
      Nothing -> return Nothing
      Just v  ->
        case fromJSON v of
          Error _ -> return Nothing
          Success a -> return $ Just a

getTypedSessionItem :: forall c a.
                       ( MonadIO c
                       , FromJSON a
                       , Typeable a
                       )
                    => c (Promise (Maybe a))
getTypedSessionItem = do
  let ty = fromString $ show $ typeRepTyCon $ typeOf (undefined :: a)
  getSessionItem ty

putSessionItem :: (MonadIO c, ToJSON a) => JSText -> a -> c (Promise Bool)
putSessionItem (k :: JSText) v = do
  let value = toJSON v
  with sessionStorageS $ do
    hm :: Map.HashMap JSText Value <- get
    put $ Map.insert k value hm
    win <- getWindow
#ifdef __GHCJS__
    Just ls <- W.getSessionStorage win
    res <- liftIO $ js_setItem_catch ls k (toText value)
    case res of
      0 -> return False
      _ -> return True
#else
    return True
#endif

putTypedSessionItem :: ( MonadIO c
                       , ToJSON a
                       , Typeable a
                       )
                    => a -> c (Promise Bool)
putTypedSessionItem a = do
  let ty = fromString $ show $ typeRepTyCon $ typeOf a
  putSessionItem ty a

#ifdef __GHCJS__
foreign import javascript unsafe
  "try { $1[\"setItem\"]($2, $3); $r = 1; } catch (e) { $r = 0; }"
  js_setItem_catch :: S.Storage -> JSText -> JSText -> IO Int
#endif

removeSessionItem :: (MonadIO c) => JSText -> c (Promise ())
removeSessionItem (k :: JSText) = do
  with sessionStorageS $ do
    hm :: Map.HashMap JSText Value <- get
    put $ Map.delete k hm
    win <- getWindow
#ifdef __GHCJS__
    Just ls <- W.getSessionStorage win
    S.removeItem ls k
#else
    return ()
#endif

clearSessionStorage :: (MonadIO c) => c (Promise ())
clearSessionStorage = do
  with sessionStorageS $ do
    put (Map.empty :: Map.HashMap JSText Value)
    win <- getWindow
#ifdef __GHCJS__
    Just ls <- W.getSessionStorage win
    S.clear ls
#else
    return ()
#endif

