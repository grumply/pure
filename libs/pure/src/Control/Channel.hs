{-# language RankNTypes, FlexibleContexts, ConstraintKinds #-}
module Control.Channel where

import Control.State
import Data.Exists
import Data.View

type Event a = Exists a
type Write a = Modify a

channel :: Typeable a => a -> (Event a => View)-> View
channel = state

write :: Write a => a -> IO ()
write = put

read :: Event a => a
read = it

