{-# language OverloadedStrings #-}
module Atomic.Message where

import Data.Txt
import Data.Typeable

import Atomic.TypeRep

class Typeable (msgTy :: *) => Message msgTy where
  type M msgTy :: *
  {-# INLINE messageHeader #-}
  messageHeader :: Proxy msgTy -> Txt
  default messageHeader :: Proxy msgTy -> Txt
  messageHeader = qualMsgHdr

simpleMsgHdr :: forall (msgTy :: *). Typeable msgTy => Proxy msgTy -> Txt
simpleMsgHdr = ("M :: " <>) . rep

qualMsgHdr :: forall (msgTy :: *). Typeable msgTy => Proxy msgTy -> Txt
qualMsgHdr = ("M :: " <>) . qualRep

fullMsgHdr :: forall (msgTy :: *). Typeable msgTy => Proxy msgTy -> Txt
fullMsgHdr = ("M :: " <>) . fullRep

