{-# language OverloadedStrings #-}
module Nuclear.Message where

import Data.Text
import Data.Typeable

import Nuclear.TypeRep

class Typeable (msgTy :: *) => Message msgTy where
  type M msgTy :: *
  {-# INLINE messageHeader #-}
  messageHeader :: Proxy msgTy -> Text
  default messageHeader :: Proxy msgTy -> Text
  messageHeader = qualMsgHdr

simpleMsgHdr :: forall (msgTy :: *). Typeable msgTy => Proxy msgTy -> Text
simpleMsgHdr = append "Message :: " . rep

qualMsgHdr :: forall (msgTy :: *). Typeable msgTy => Proxy msgTy -> Text
qualMsgHdr = append "Message :: " . qualRep

fullMsgHdr :: forall (msgTy :: *). Typeable msgTy => Proxy msgTy -> Text
fullMsgHdr = append "Message :: " . fullRep

