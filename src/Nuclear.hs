{-# language DeriveGeneric #-}
{-# language OverloadedStrings #-}
{-# language CPP #-}
{-# language MultiParamTypeClasses #-}
{-# language FunctionalDependencies #-}
{-# language ScopedTypeVariables #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language TypeFamilies #-}
module Nuclear
  ( Msg(..), Name, Body
  , fromS, toS
  , encodeMsg, decodeMsg
  , Request(..), Response(..), Message(..)
  , module Data.Typeable
  , Proxy(..)
  ) where

import Data.Monoid

import Data.Typeable
import Data.Proxy

import Text.Read

type Name = String
type Body = String

data Msg
  = Msg
    { name :: !Name
    , body :: !Body
    } deriving (Read,Show,Eq,Ord)

fromS :: String -> Either String Msg
fromS = readEither

toS :: Msg -> String
toS = show

encodeMsg :: Show a => String -> a -> Msg
encodeMsg name a =
  let body = show a
  in Msg {..}

decodeMsg :: Read a => Msg -> Maybe a
decodeMsg Msg {..} = readMaybe body

class (Show a,Read a,Show b,Read b,Typeable a,Typeable b) => Response a b | a -> b, b -> a where
  requestName :: Proxy b -> String
  requestName p =
     "Req::" ++ show (typeRepTyCon $ typeOf (undefined :: b))

  responseName :: Proxy a -> String
  responseName p =
    "Res::" ++ show (typeRepTyCon $ typeOf (undefined :: a))

class (Show a,Read a,Show b,Read b,Typeable a,Typeable b) => Request b a | a -> b, b -> a
instance (Request b a) => Response a b

-- class of unsollicited message types
class (Show m,Read m,Typeable m) => Message m where
  messageName :: Proxy m -> String
  messageName p =
    "Msg::" ++ show (typeRepTyCon $ typeOf (undefined :: m))
