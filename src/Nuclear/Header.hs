module Nuclear.Header where

-- NOTE: This module /MUST/ be imported qualified.

import Control.Category

import Data.Proxy

-- A few common header combinators.

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

data Ready
ready :: Proxy Ready
ready = Proxy
