module Data.Websocket.Callbacks where

import Control.Exception
import Data.IORef

import Data.DOM
import Data.Websocket.Dispatch

data CloseReason = MessageLengthExceeded | InvalidMessage | UnexpectedClosure
  deriving (Show)

instance Exception CloseReason

data Status = Unopened | Closed CloseReason | Opened | Errored JSV | Connecting

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

