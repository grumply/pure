module Pure.Media.Library.API where

import Pure.Media.Library.Data.Media (File,Media)
import Pure.Media.Library.Data.Library (Library)

import Pure.Auth.Data.Username
import Data.Txt (Txt)
import Data.Websocket as WS

import Data.Typeable (Typeable,Proxy(..))

data GetLibrary (domain :: *)
instance Identify (GetLibrary domain)
instance Typeable domain => Request (GetLibrary domain) where
  type Req (GetLibrary domain) = (Int,Username)
  type Rsp (GetLibrary domain) = Maybe (Library domain)

getLibrary :: Proxy (GetLibrary domain)
getLibrary = Proxy

data Upload (domain :: *)
instance Identify (Upload domain)
instance Typeable domain => Request (Upload domain) where
  type Req (Upload domain) = (Int,File)
  type Rsp (Upload domain) = Maybe (Media domain)

upload :: Proxy (Upload domain)
upload = Proxy

data Delete (domain :: *)
instance Identify (Delete domain) 
instance Typeable domain => Request (Delete domain) where
  type Req (Delete domain) = (Int,Media domain)
  type Rsp (Delete domain) = Bool

delete :: Proxy (Delete domain)
delete = Proxy

api :: forall domain. _ => _
api = WS.api msgs reqs
  where
    msgs = WS.non
    reqs = getLibrary @domain <:> upload @domain <:> delete @domain <:> WS.non