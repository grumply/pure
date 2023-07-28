{-# language TypeApplications, OverloadedStrings #-}
module Data.Fetch.GHC 
  (XHRError(..)
  ,get,getWith,getRaw
  ,post,postWith,postRaw
  ,postForm,postFormWith,postFormRaw
  ,patch,patchWith,patchRaw
  ,delete,deleteWith,deleteRaw
  ,put,putWith,putRaw
  ) where

import Data.JSON

import Control.Exception
import Control.Lens ((^.),(.~),(&))
import Data.ByteString
import Data.Txt
import Data.String
import qualified Network.Wreq as Wreq
import Network.Wreq (headers,defaults,responseBody,responseStatus,statusCode,FormParam(..))

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
xhrErrorURL (StatusError     u _ _ ) = u
xhrErrorURL (ParseError      u _) = u
xhrErrorURL (InvalidURLError u _) = u
xhrErrorURL (OtherError      u _) = u

get :: FromJSON a => Txt -> IO (Either XHRError a)
get = getWith [("Content-Type","application/json"),("Accept","*/*")] 

getWith :: FromJSON a => [(Txt,Txt)] -> Txt -> IO (Either XHRError a)
getWith hs url = liftWith (\hs url _ -> Wreq.getWith hs url) hs url ("" :: Txt)

getRaw :: [(Txt,Txt)] -> Txt -> IO (Either XHRError Txt)
getRaw hs url = liftRaw (\hs url _ -> Wreq.getWith hs url) hs url ""

post :: (ToJSON a,FromJSON b) => Txt -> a -> IO (Either XHRError b)
post = postWith [("Content-Type","application/json"),("Accept","application/json")]

postWith :: (ToJSON a, FromJSON b) => [(Txt,Txt)] -> Txt -> a -> IO (Either XHRError b)
postWith = liftWith Wreq.postWith

postRaw :: [(Txt,Txt)] -> Txt -> Txt -> IO (Either XHRError Txt)
postRaw = liftRaw Wreq.postWith

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
postFormRaw hs0 url payload = do
  let
    hs = fmap (\(h,v) -> (fromString (fromTxt h),fromString (fromTxt v))) hs0 
    opts = defaults & headers .~ hs
  rsp <- handle @SomeException (pure . Left) (Right <$> Wreq.postWith opts (fromTxt url) params)
  pure $
    case rsp of
      Left se -> Left (OtherError url se)
      Right r -> let code = r ^. responseStatus . statusCode in
        case code of
          _ | code >= 200 && code < 300 -> Right (toTxt $ r ^. responseBody)
            | otherwise                 -> Left (StatusError url code (toTxt $ r ^. responseBody))
  where
    params = fmap (\(k,v) -> fromTxt k := v) payload


patch :: (ToJSON a,FromJSON b) => Txt -> a -> IO (Either XHRError b)
patch = patchWith [("Content-Type","application/json"),("Accept","application/json")]

patchWith :: (ToJSON a, FromJSON b) => [(Txt,Txt)] -> Txt -> a -> IO (Either XHRError b)
patchWith = liftWith Wreq.patchWith

patchRaw :: [(Txt,Txt)] -> Txt -> Txt -> IO (Either XHRError Txt)
patchRaw = liftRaw Wreq.patchWith

delete :: FromJSON b => Txt -> IO (Either XHRError b)
delete = deleteWith [("Content-Type","application/json"),("Accept","application/json")]

deleteWith :: FromJSON b => [(Txt,Txt)] -> Txt -> IO (Either XHRError b)
deleteWith hs0 url = liftWith (\hs url _ -> Wreq.deleteWith hs url) hs0 url ("" :: Txt)

deleteRaw :: [(Txt,Txt)] -> Txt -> IO (Either XHRError Txt)
deleteRaw hs0 url = liftRaw (\hs url _ -> Wreq.deleteWith hs url) hs0 url ("" :: Txt)

put :: (ToJSON a,FromJSON b) => Txt -> a -> IO (Either XHRError b)
put = putWith [("Content-Type","application/json"),("Accept","application/json")]

putWith :: (ToJSON a, FromJSON b) => [(Txt,Txt)] -> Txt -> a -> IO (Either XHRError b)
putWith = liftWith Wreq.putWith

putRaw :: [(Txt,Txt)] -> Txt -> Txt -> IO (Either XHRError Txt)
putRaw = liftRaw Wreq.putWith

liftWith f hs0 url payload = do
  ext <- liftRaw f hs0 url (encode payload)
  pure $
    case ext of
      Left e  -> Left e
      Right t -> either (Left . ParseError url) Right (decodeEither t)

liftRaw f hs0 url payload = do
  let
    hs = fmap (\(h,v) -> (fromString (fromTxt h),fromString (fromTxt v))) hs0 
    opts = defaults & headers .~ hs
  rsp <- handle @SomeException (pure . Left) (Right <$> f opts (fromTxt url) (fromTxt payload :: ByteString))
  pure $
    case rsp of
      Left se -> Left (OtherError url se)
      Right r -> let code = r ^. responseStatus . statusCode in
        case code of
          _ | code >= 200 && code < 300 -> Right (toTxt $ r ^. responseBody)
            | otherwise                 -> Left (StatusError url code (toTxt $ r ^. responseBody))

 
