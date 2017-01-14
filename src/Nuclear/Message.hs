{-# language OverloadedStrings #-}
module Nuclear.Message where

import Data.JSText
import Data.Typeable

import Nuclear.TypeRep

class Typeable (msgTy :: *) => Message msgTy where
  type M msgTy :: *
  {-# INLINE messageHeader #-}
  messageHeader :: Proxy msgTy -> JSText
  default messageHeader :: Proxy msgTy -> JSText
  messageHeader = qualMsgHdr

simpleMsgHdr :: forall (msgTy :: *). Typeable msgTy => Proxy msgTy -> JSText
simpleMsgHdr = ("M :: " <>) . rep

qualMsgHdr :: forall (msgTy :: *). Typeable msgTy => Proxy msgTy -> JSText
qualMsgHdr = ("M :: " <>) . qualRep

fullMsgHdr :: forall (msgTy :: *). Typeable msgTy => Proxy msgTy -> JSText
fullMsgHdr = ("M :: " <>) . fullRep

