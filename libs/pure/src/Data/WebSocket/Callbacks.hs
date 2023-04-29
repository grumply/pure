{-# language DerivingStrategies, DeriveAnyClass, DeriveGeneric #-}
module Data.Websocket.Callbacks where

import Control.Exception
import Data.IORef
import Data.JSON
import Data.Txt
import Data.DOM
import Data.Websocket.Dispatch
import Data.ByteString.Lazy as Lazy
import GHC.Generics

data CloseReason = Disconnect | MessageLengthExceeded | InvalidMessage
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

data Status = Initialized | Connecting | Opened | Closed CloseReason
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

data DispatchCallback = DispatchCallback
  { dcRef :: IORef (Dispatch -> IO ())
  , dcCleanup :: IO ()
  }
instance Eq DispatchCallback where
  (==) (DispatchCallback dcr1 _) (DispatchCallback dcr2 _) = dcr1 == dcr2

data StatusCallback = StatusCallback
  { scRef :: IORef (Status -> IO ())
  , scCleanup :: IO ()
  }
instance Eq StatusCallback where
  (==) (StatusCallback scr1 _) (StatusCallback scr2 _) = scr1 == scr2

