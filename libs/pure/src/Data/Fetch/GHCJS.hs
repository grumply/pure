{-# language OverloadedStrings, PatternSynonyms #-}
module Data.Fetch.GHCJS (Response(..), Data.Fetch.GHCJS.json, get, post, postForm, patch, delete, put, pattern Good, pattern Bad, pattern Ok, pattern JSON) where

import Data.Fetch.Response

import Data.DOM
import Data.JSON
import qualified Data.List as List
import Data.Txt hiding (foldl')
import Data.URI

import Control.Concurrent
import Control.Exception
import Data.Foldable

json :: [(Txt,Txt)]
json = [("Content-Type","application/json"),("Accept","application/json")]

get :: [(Txt,Txt)] -> Txt -> IO Response
get hs url = raw "GET" hs url ""

post :: [(Txt,Txt)] -> Txt -> Txt -> IO Response
post = raw "POST"

patch :: [(Txt,Txt)] -> Txt -> Txt -> IO Response
patch = raw "PATCH"

delete :: [(Txt,Txt)] -> Txt -> IO Response
delete hs url = raw "DELETE" hs url ""

put :: [(Txt,Txt)] -> Txt -> Txt -> IO Response
put = raw "PUT"

postForm :: [(Txt,Txt)] -> Txt -> [(Txt,Txt)] -> IO Response
postForm headers url payload = post (("Content-Type","application/x-www-form-urlencoded"): List.filter ((/= "Content-Type") . fst) headers) url params
  where
    params = foldl' (\ps (k,v) -> ps <> "&" <> encodeURIComponent k <> "=" <> encodeURIComponent v) mempty payload

raw :: Txt -> [(Txt,Txt)] -> Txt -> Txt -> IO Response
raw method headers url payload = do
  xhr <- new_xhr_js
  mv  <- newEmptyMVar
  cb  <- syncCallback1 ContinueAsync $ \_ -> do
    r <- ready_js xhr
    case r of
      4 -> do
        s <- status_js xhr
        t <- response_text_js xhr
        putMVar mv (Response s t)
      _ -> 
        pure ()
  on_ready_js xhr cb
  open_js xhr method url
  for_ headers $ \(h,v) -> set_request_header_js xhr h v
  send_with_js xhr payload
  ma <- takeMVar mv
  ma `seq` releaseCallback cb
  pure ma

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



