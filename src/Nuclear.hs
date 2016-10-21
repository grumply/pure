{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language TypeFamilies #-}
module Nuclear
  ( Msg(..)
  , encodeMsg, decodeMsg
  , Text
  , rep, qualRep, fullRep
  , simpleMsgHdr, qualMsgHdr, fullMsgHdr
  , simpleReqHdr, qualReqHdr, fullReqHdr
  , simpleRspHdr, qualRspHdr, fullRspHdr
  , Message (..)
  , Nuclear (..)
  , Indexed (..)
  , ToText (..)
  , ToBS (..)
  , FromBS (..)
  , module Data.Aeson
  , module Data.Typeable
  , module GHC.Generics
  , Proxy(..)
  ) where

import Data.Aeson
import Data.Monoid
import Data.Proxy
import Data.Text hiding (index)
import Data.Text.Encoding
import Data.Typeable
import Data.List as L

import GHC.Generics

import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Encoding as TL
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL

-- Note: This module, and, thus, its descendants, defaults to preferring
-- lazy bytestrings and strict text.

data Msg
  = Msg
    { name :: Text
    , body :: Value
    } deriving Generic
instance ToJSON Msg
instance FromJSON Msg
instance ToBS Msg
instance FromBS Msg
instance ToText Msg

{-# INLINE encodeMsg #-}
encodeMsg :: ToJSON a => Text -> a -> Msg
encodeMsg name a =
  let body = toJSON a
  in Msg {..}

{-# INLINE decodeMsg #-}
decodeMsg :: FromJSON a => Msg -> Maybe a
decodeMsg Msg {..} =
  case fromJSON body of
    Error _ -> Nothing
    Success a -> Just a

-- | rep is a type representation without package or module qualification.
{-# INLINE rep #-}
rep :: forall p. (Typeable p) => Proxy p -> Text
rep _ =
  pack $ go False (typeOf (undefined :: p))
  where
    go surround tr =
      let tc = typeRepTyCon tr
          trs = typeRepArgs tr
          r = tyConName tc
      in if L.null trs then
           r
         else if surround then
           " (" ++ L.intercalate " " (r:L.map (go True) trs) ++ ")"
         else
           r ++ L.intercalate " " (L.map (go True) trs)

-- | fullRep is a type representation with package and package version,
-- and full module qualification. Use this only when expecting to create
-- versioned APIs with full backwards compatibility; you'll know when you
-- want this, so assume you don't as the bytes-across-the-wire cost is high.
{-# INLINE fullRep #-}
fullRep :: forall p. (Typeable p) => Proxy p -> Text
fullRep _ =
  pack $ go False (typeOf (undefined :: p))
  where
    go surround tr =
      let tc = typeRepTyCon tr
          trs = typeRepArgs tr
          r = tyConPackage tc <> ('.':tyConModule tc) <> ('.':tyConName tc)
      in if L.null trs then
           r
         else if surround then
           " (" ++ L.intercalate " " (r:L.map (go True) trs) ++ ")"
         else
           r ++ L.intercalate " " (L.map (go True) trs)

-- | qualRep is a type representation with full module qualification.
{-# INLINE qualRep #-}
qualRep :: forall p. (Typeable p) => Proxy p -> Text
qualRep _ =
  pack $ go False (typeOf (undefined :: p))
  where
    go surround tr =
      let tc = typeRepTyCon tr
          trs = typeRepArgs tr
          r = tyConModule tc <> ('.':tyConName tc)
      in if L.null trs then
           r
         else if surround then
           " (" ++ L.intercalate " " (r:L.map (go True) trs) ++ ")"
         else
           r ++ L.intercalate " " (L.map (go True) trs)

class Typeable msgTy => Message msgTy where
  type M msgTy :: *
  {-# INLINE messageHeader #-}
  messageHeader :: Proxy msgTy -> Text
  default messageHeader :: Proxy msgTy -> Text
  messageHeader = qualMsgHdr

simpleMsgHdr :: Typeable msgTy => Proxy msgTy -> Text
simpleMsgHdr = append "Message :: " . rep

qualMsgHdr :: Typeable msgTy => Proxy msgTy -> Text
qualMsgHdr = append "Message :: " . qualRep

fullMsgHdr :: Typeable msgTy => Proxy msgTy -> Text
fullMsgHdr = append "Message :: " . fullRep

class Indexed a where
  type I a :: *
  index :: (I a ~ i) => a -> i

class (Typeable requestType) => Nuclear requestType rspTy | requestType -> rspTy where
  type Req requestType :: *
  type Rsp requestType :: *

  requestHeader :: Proxy requestType -> Text
  {-# INLINE requestHeader #-}
  default requestHeader :: Proxy requestType -> Text
  requestHeader = qualReqHdr

  responseHeader :: (Req requestType ~ request) => Proxy requestType -> request -> Text
  {-# INLINE responseHeader #-}
  default responseHeader :: ( Req requestType ~ request
                            , Indexed request
                            , I request ~ requestIndex
                            , ToText requestIndex
                            )
                        => Proxy requestType -> request -> Text
  responseHeader = qualRspHdr

simpleReqHdr :: Typeable requestType => Proxy requestType -> Text
simpleReqHdr = rep

qualReqHdr :: Typeable requestType => Proxy requestType -> Text
qualReqHdr = qualRep

fullReqHdr :: Typeable requestType => Proxy requestType -> Text
fullReqHdr = fullRep

simpleRspHdr :: ( Typeable requestType
             , Nuclear requestType responseType
             , Req requestType ~ request
             , Indexed request
             , I request ~ requestIndex
             , ToText requestIndex
             )
          => Proxy requestType -> request -> Text
simpleRspHdr rqty_proxy req = rep rqty_proxy <> " " <> toText (index req)

qualRspHdr :: ( Typeable requestType
              , Nuclear requestType responseType
              , Req requestType ~ request
              , Indexed request
              , I request ~ requestIndex
              , ToText requestIndex
              )
           => Proxy requestType -> request -> Text
qualRspHdr rqty_proxy req = qualRep rqty_proxy <> " " <> toText (index req)

fullRspHdr :: ( Typeable requestType
              , Nuclear requestType responseType
              , Req requestType ~ request
              , Indexed request
              , I request ~ requestIndex
              , ToText requestIndex
              )
           => Proxy requestType -> request -> Text
fullRspHdr rqty_proxy req = fullRep rqty_proxy <> " " <> toText (index req)

class ToBS a where
  toBS :: a -> BSL.ByteString
  {-# INLINE toBS #-}
  default toBS :: ToJSON a => a -> BSL.ByteString
  toBS = encode

instance ToBS BSL.ByteString where
  {-# INLINE toBS #-}
  toBS = id

class FromBS a where
  fromBS :: BSL.ByteString -> Either String a
  {-# INLINE fromBS #-}
  default fromBS :: FromJSON a => BSL.ByteString -> Either String a
  fromBS = eitherDecode' . BSL.takeWhile (/= 0)

instance FromBS Text where
  fromBS = Right . TL.toStrict . TL.decodeUtf8

instance FromBS TL.Text where
  fromBS = Right . TL.decodeUtf8

-- ToText is representational and is thus uni-directional. For a fully
-- bidirectional encoding, see ToBS/FromBS where the expectation is that
-- fromBS . toBS = id and toBS . fromBS = id
--
-- ToText is used to construct, possibly unique, resource identifiers.
--
-- Note the default instance uses a ToBS instance to construct a text value
-- from a full encoding of the term; this is slow since ToBS generates a
-- lazy bytestring and we must use lazy decoding and subsequent strictness
-- conversion. For small terms, which is the intended use-case for the
-- default instance, this won't matter much.
class ToText a where
  toText :: a -> Text
  {-# INLINE toText #-}
  default toText :: ToBS a => a -> Text
  toText = TL.toStrict . TL.decodeUtf8 . toBS

instance ToText B.ByteString where
  {-# INLINE toText #-}
  toText = decodeUtf8

instance ToText Text where
  {-# INLINE toText #-}
  toText = id

instance ToText TL.Text where
  {-# INLINE toText #-}
  toText = TL.toStrict
