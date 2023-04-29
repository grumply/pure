{-# language DerivingStrategies, DeriveGeneric, DeriveAnyClass, DuplicateRecordFields #-}
module Data.Websocket.Events where

import Data.JSON
import Data.Time
import Data.Txt
import Data.Websocket.Callbacks
import GHC.Generics

data ConnectionEvent = ConnectionEvent
  { host    :: String
  , port    :: Int
  , secure  :: Bool
  , status  :: Status
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data MessageEvent 
  = DispatchedMessage
    { host     :: String
    , port     :: Int
    , endpoint :: Txt 
    , payload  :: Value
    }
  | SentMessage
    { host     :: String
    , port     :: Int
    , payload  :: Value
    }
  | UnknownMessage
    { host    :: String
    , port    :: Int
    , content :: Txt
    } 
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

