{-# language TypeApplications, OverloadedStrings, PatternSynonyms, CPP #-}
module Data.Fetch.GHC (Response(..), Data.Fetch.GHC.json, get, post, postForm , patch , delete, put, pattern Good, pattern Bad, pattern Ok, pattern JSON) where

import Data.Fetch.Response

import Data.JSON hiding (Result,Success,Failure)

import Control.Exception
import Control.Lens ((^.),(.~),(&))
import Data.ByteString
import qualified Data.List as List
import Data.Txt
import Data.String
import qualified Network.Wreq as Wreq
import Network.Wreq (headers,defaults,responseBody,responseStatus,statusCode,FormParam(..))

json :: [(Txt,Txt)]
json = [("Content-Type","application/json"),("Accept","application/json")]

get :: [(Txt,Txt)] -> Txt -> IO Response
get hs url = lift (\hs url _ -> Wreq.getWith hs url) hs url ""

post :: [(Txt,Txt)] -> Txt -> Txt -> IO Response
post = lift Wreq.postWith

patch :: [(Txt,Txt)] -> Txt -> Txt -> IO Response
#if MIN_VERSION_wreq(0,5,4)
patch = lift Wreq.patchWith
#else
patch = lift (Wreq.customPayloadMethodWith "PATCH")
#endif

delete :: [(Txt,Txt)] -> Txt -> IO Response
delete hs0 url = lift (\hs url _ -> Wreq.deleteWith hs url) hs0 url ("" :: Txt)

put :: [(Txt,Txt)] -> Txt -> Txt -> IO Response
put = lift Wreq.putWith

postForm :: [(Txt,Txt)] -> Txt -> [(Txt,Txt)] -> IO Response
postForm hs0 url payload = do
  let
    hs = fmap (\(h,v) -> (fromString (fromTxt h),fromString (fromTxt v))) hs0 
    opts = defaults & headers .~ (("Content-Type","application/x-www-form-urlencoded") : List.filter ((/= "Content-Type") . fst) hs)
  rsp <- Wreq.postWith opts (fromTxt url) params
  pure (Response (rsp ^. responseStatus . statusCode) (toTxt $ rsp ^. responseBody))
  where
    params = fmap (\(k,v) -> fromTxt k := v) payload

lift f hs0 url payload = do
  let
    hs = fmap (\(h,v) -> (fromString (fromTxt h),fromString (fromTxt v))) hs0 
    opts = defaults & headers .~ hs
  rsp <- f opts (fromTxt url) (fromTxt payload :: ByteString)
  pure (Response (rsp ^. responseStatus . statusCode) (toTxt $ rsp ^. responseBody))
