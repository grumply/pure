module Nuclear.Header where

-- NOTE: This module /MUST/ be imported qualified.

import Control.Category

import Data.Proxy

-- A few common header combinators.

----------------------------------------
-- Destination

data Manager a
withManager :: Proxy Manager
withManager = Proxy

data Server a
withServer :: Proxy Server
withServer = Proxy

data Client a
withClient :: Proxy Client
withClient = Proxy

----------------------------------------
-- Lifecycle

data Register a
register :: Proxy Register
register = Proxy

data Initialize a
initialize :: Proxy Initialize
initialize = Proxy

data Shutdown a
shutdown :: Proxy Shutdown
shutdown = Proxy

----------------------------------------
-- Verbs and Status/Updates

data Perform a
perform :: Proxy Perform
perform = Proxy

data Performed a
performed :: Proxy Performed
performed = Proxy

data Process a
process :: Proxy Process
process = Proxy

data Get a
get :: Proxy Get
get = Proxy

data Post a
post :: Proxy Post
post = Proxy

data Put a
put :: Proxy Put
put = Proxy

data Borrow a
borrow :: Proxy Borrow
borrow = Proxy

data Return a
return :: Proxy Return
return = Proxy

data Subscribe a
subscribe :: Proxy Subscribe
subscribe = Proxy

data Unsubscribe a
unsubscribe :: Proxy Unsubscribe
unsubscribe = Proxy

data Publish a
publish :: Proxy Publish
publish = Proxy
