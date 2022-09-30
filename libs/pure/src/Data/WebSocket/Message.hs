{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Websocket.Message where

import Data.Monoid
import Data.Txt
import Data.Typeable
import Data.Websocket.Identify
import Data.Websocket.TypeRep

class Typeable (msgTy :: *) => Message msgTy where
  type M msgTy :: *
  {-# INLINE messageHeader #-}
  messageHeader :: Proxy msgTy -> Txt
  default messageHeader :: Proxy msgTy -> Txt
  messageHeader = rep