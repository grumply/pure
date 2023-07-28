{-# language OverloadedStrings #-}
module Data.Fetch.GHCJS 
  (XHRError(..)
  ,get,getWith,getRaw
  ,post,postWith,postRaw
  ,postForm,postFormWith,postFormRaw
  ,patch,patchWith,patchRaw
  ,delete,deleteWith,deleteRaw
  ,put,putWith,putRaw
  ) where

import Data.DOM
import Data.JSON
import Data.Txt hiding (foldl')
import Data.URI

import Control.Concurrent
import Control.Exception
import Data.Foldable

-- XHRErrors contain the target URL
data XHRError 
  = StatusError Txt Int Txt
  | ParseError Txt String
  | InvalidURLError Txt Txt
  | OtherError Txt SomeException
  deriving (Show)

instance Exception XHRError
instance ToTxt XHRError where
  toTxt = toTxt . show

xhrErrorURL :: XHRError -> Txt
xhrErrorURL (StatusError     u _ _) = u
xhrErrorURL (ParseError      u _) = u
xhrErrorURL (InvalidURLError u _) = u
xhrErrorURL (OtherError      u _) = u

newtype XHR = XHR JSV

foreign import javascript unsafe
  "$r = new XMLHttpRequest()"
    new_xhr_js :: IO XHR

foreign import javascript unsafe
  "$1.onreadystatechange = $2"
    on_ready_js :: XHR -> Callback (JSV -> IO ()) -> IO ()

foreign import javascript unsafe
  "$1.open($2, $3, true)"
    open_js :: XHR -> Txt -> Txt -> IO ()

foreign import javascript unsafe
  "$1.setRequestHeader($2,$3)"
    set_request_header_js :: XHR -> Txt -> Txt -> IO ()

foreign import javascript unsafe
  "$1.send()"
    send_js :: XHR -> IO ()

foreign import javascript unsafe
  "$1.send($2)"
    send_with_js :: XHR -> Txt -> IO ()

foreign import javascript unsafe
  "$r = $1.readyState"
    ready_js :: XHR -> IO Int
    
foreign import javascript unsafe
  "$r = $1.status"
    status_js :: XHR -> IO Int

foreign import javascript unsafe
  "$r = $1.responseText" 
    response_text_js :: XHR -> IO Txt

get :: FromJSON a => Txt -> IO (Either XHRError a)
get = getWith [("Content-Type","application/json"),("Accept","*/*")] 

getWith :: FromJSON a => [(Txt,Txt)] -> Txt -> IO (Either XHRError a)
getWith headers url = do
  ext <- getRaw headers url
  pure $
    case ext of
      Left e  -> Left e
      Right t -> either (Left . ParseError url) Right (decodeEither t)

getRaw :: [(Txt,Txt)] -> Txt -> IO (Either XHRError Txt)
getRaw headers url = do
  xhr <- new_xhr_js
  mv  <- newEmptyMVar
  cb  <- syncCallback1 ContinueAsync $ \_ -> do
    r <- ready_js xhr
    case r of
      4 -> do
        s <- status_js xhr
        case s of
          _ | s >= 200 && s < 300 -> do
              t <- response_text_js xhr
              putMVar mv (Right t)
            | otherwise -> do
              t <- response_text_js xhr
              putMVar mv $ Left (StatusError url s t)
      _ -> pure ()
  on_ready_js xhr cb
  open_js xhr "GET" url
  for_ headers $ \(h,v) -> set_request_header_js xhr h v
  send_js xhr
  ma <- takeMVar mv
  ma `seq` releaseCallback cb
  pure ma

post :: (ToJSON a, FromJSON b) => Txt -> a -> IO (Either XHRError b)
post = postWith [("Content-Type","application/json"),("Accept","application/json")]

postWith :: (ToJSON a, FromJSON b) => [(Txt,Txt)] -> Txt -> a -> IO (Either XHRError b)
postWith headers url payload = do
  ext <- postRaw headers url (encode payload)
  pure $
    case ext of
      Left e  -> Left e
      Right t -> either (Left . ParseError url) Right (decodeEither t)

postRaw :: [(Txt,Txt)] -> Txt -> Txt -> IO (Either XHRError Txt)
postRaw headers url payload = do
  xhr <- new_xhr_js
  mv  <- newEmptyMVar
  cb  <- syncCallback1 ContinueAsync $ \_ -> do
    r <- ready_js xhr
    case r of
      4 -> do
        s <- status_js xhr
        case s of
          _ | s >= 200 && s < 300 -> do
              t <- response_text_js xhr
              putMVar mv (Right t)
            | otherwise -> do
              t <- response_text_js xhr
              putMVar mv $ Left (StatusError url s t)
      _ -> pure ()
  on_ready_js xhr cb
  open_js xhr "POST" url
  for_ headers $ \(h,v) -> set_request_header_js xhr h v
  send_with_js xhr payload
  ma <- takeMVar mv
  ma `seq` releaseCallback cb
  pure ma

postForm :: FromJSON a => Txt -> [(Txt,Txt)] -> IO (Either XHRError a)
postForm = postFormWith [("Content-Type","application/x-www-form-urlencoded"),("Accept","application/json")]

postFormWith :: FromJSON a => [(Txt,Txt)] -> Txt -> [(Txt,Txt)] -> IO (Either XHRError a)
postFormWith headers url payload = do
  ext <- postFormRaw headers url payload
  pure $
    case ext of
      Left e  -> Left e
      Right t -> either (Left . ParseError url) Right (decodeEither t)

postFormRaw :: [(Txt,Txt)] -> Txt -> [(Txt,Txt)] -> IO (Either XHRError Txt)
postFormRaw headers url payload = do
  xhr <- new_xhr_js
  mv  <- newEmptyMVar
  cb  <- syncCallback1 ContinueAsync $ \_ -> do
    r <- ready_js xhr
    case r of
      4 -> do
        s <- status_js xhr
        case s of
          _ | s >= 200 && s < 300 -> do
              t <- response_text_js xhr
              putMVar mv (Right t)
            | otherwise -> do
              t <- response_text_js xhr
              putMVar mv $ Left (StatusError url s t)
      _ -> pure ()
  on_ready_js xhr cb
  open_js xhr "POST" url
  for_ headers $ \(h,v) -> set_request_header_js xhr h v
  send_with_js xhr params
  ma <- takeMVar mv
  ma `seq` releaseCallback cb
  pure ma
  where
    params = foldl' (\ps (k,v) -> ps <> "&" <> encodeURIComponent k <> "=" <> encodeURIComponent v) mempty payload

patch :: (ToJSON a, FromJSON b) => Txt -> a -> IO (Either XHRError b)
patch = patchWith [("Content-Type","application/json"),("Accept","application/json")]

patchWith :: (ToJSON a, FromJSON b) => [(Txt,Txt)] -> Txt -> a -> IO (Either XHRError b)
patchWith headers url payload = do
  ext <- patchRaw headers url (encode payload)
  pure $
    case ext of
      Left e  -> Left e
      Right t -> either (Left . ParseError url) Right (decodeEither t)

patchRaw :: [(Txt,Txt)] -> Txt -> Txt -> IO (Either XHRError Txt)
patchRaw headers url payload = do
  xhr <- new_xhr_js
  mv  <- newEmptyMVar
  cb  <- syncCallback1 ContinueAsync $ \_ -> do
    r <- ready_js xhr
    case r of
      4 -> do
        s <- status_js xhr
        case s of
          _ | s >= 200 && s < 300 -> do
              t <- response_text_js xhr
              putMVar mv (Right t)
            | otherwise -> do
              t <- response_text_js xhr
              putMVar mv $ Left (StatusError url s t)
      _ -> pure ()
  on_ready_js xhr cb
  open_js xhr "PATCH" url
  for_ headers $ \(h,v) -> set_request_header_js xhr h v
  send_with_js xhr payload
  ma <- takeMVar mv
  ma `seq` releaseCallback cb
  pure ma

delete :: FromJSON b => Txt -> IO (Either XHRError b)
delete = deleteWith [("Content-Type","application/json"),("Accept","application/json")]

deleteWith :: FromJSON b => [(Txt,Txt)] -> Txt -> IO (Either XHRError b)
deleteWith headers url = do
  ext <- deleteRaw headers url
  pure $
    case ext of
      Left e  -> Left e
      Right t -> either (Left . ParseError url) Right (decodeEither t)

deleteRaw :: [(Txt,Txt)] -> Txt -> IO (Either XHRError Txt)
deleteRaw headers url = do
  xhr <- new_xhr_js
  mv  <- newEmptyMVar
  cb  <- syncCallback1 ContinueAsync $ \_ -> do
    r <- ready_js xhr
    case r of
      4 -> do
        s <- status_js xhr
        case s of
          _ | s >= 200 && s < 300 -> do
              t <- response_text_js xhr
              putMVar mv (Right t)
            | otherwise -> do
              t <- response_text_js xhr
              putMVar mv $ Left (StatusError url s t)
      _ -> pure ()
  on_ready_js xhr cb
  open_js xhr "DELETE" url
  for_ headers $ \(h,v) -> set_request_header_js xhr h v
  send_js xhr 
  ma <- takeMVar mv
  ma `seq` releaseCallback cb
  pure ma

put :: (ToJSON a, FromJSON b) => Txt -> a -> IO (Either XHRError b)
put = putWith [("Content-Type","application/json"),("Accept","application/json")]

putWith :: (ToJSON a, FromJSON b) => [(Txt,Txt)] -> Txt -> a -> IO (Either XHRError b)
putWith headers url payload = do
  ext <- putRaw headers url (encode payload)
  pure $
    case ext of
      Left e  -> Left e
      Right t -> either (Left . ParseError url) Right (decodeEither t)

putRaw :: [(Txt,Txt)] -> Txt -> Txt -> IO (Either XHRError Txt)
putRaw headers url payload = do
  xhr <- new_xhr_js
  mv  <- newEmptyMVar
  cb  <- syncCallback1 ContinueAsync $ \_ -> do
    r <- ready_js xhr
    case r of
      4 -> do
        s <- status_js xhr
        case s of
          _ | s >= 200 && s < 300 -> do
              t <- response_text_js xhr
              putMVar mv (Right t)
            | otherwise -> do
              t <- response_text_js xhr
              putMVar mv $ Left (StatusError url s t)
      _ -> pure ()
  on_ready_js xhr cb
  open_js xhr "PUT" url
  for_ headers $ \(h,v) -> set_request_header_js xhr h v
  send_with_js xhr payload
  ma <- takeMVar mv
  ma `seq` releaseCallback cb
  pure ma




