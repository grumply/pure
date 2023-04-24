module Pure.Media.Library.Browser (browser,input) where

import qualified Pure.Media.Library.API as API
import qualified Pure.Media.Library.Data.Media as Media
import qualified Pure.Media.Library.Data.Library as Library
import Pure.Auth.Access

import Pure hiding (reverse,user,Input,withInput)
import qualified Pure
import Data.DOM 
import Data.File as File (ByteTxt,getFile)
import qualified Data.Stream as S
import qualified Effect.Stream as S
import Effect.Websocket as WS

import Prelude hiding (max)
import Control.Monad (when)
import Control.Concurrent (newEmptyMVar,putMVar,takeMVar)
import Data.Typeable (Typeable)

newStream :: forall domain. (Websocket domain, Authenticated domain) => S.Stream IO (Media.Media domain)
newStream = S.chunksOf 12 (S.unfolds Nothing getLibrary)
  where
    getLibrary (Just l) = 
      case l of
        (m:rest) -> S.more m (Just rest)
        _        -> S.done

    getLibrary Nothing =
      req @domain Uncached (API.api @domain) (API.getLibrary @domain) (user @domain) >>= \case
        Just (Library.Library lib) -> getLibrary (Just (reverse lib))
        _                          -> S.done

withStream :: forall domain. (Websocket domain, Authenticated domain) => (State (S.Stream IO (Media.Media domain)) => View) -> View
withStream = state (newStream @domain)

newtype Input = Input View
input :: Exists Input => View
input = let Input i = it in i

withInput :: forall domain. (Websocket domain, Authenticated domain, State (S.Stream IO (Media.Media domain))) => (State Input => View) -> View
withInput = state (Input input)
  where
    input = Pure.Input <| OnChangeWith intercept select . Type "file" . Accept "image/*"

    select :: Evt -> IO ()
    select (Pure.target -> Just (coerce -> node)) = File.getFile node >>= maybe (pure ()) onUpload
    select _ = pure ()

    onUpload :: Media.File -> IO ()
    onUpload f =
      req @domain Uncached (API.api @domain) (API.upload @domain) f >>= \case
        Just _ -> put (newStream :: S.Stream IO (Media.Media domain))
        _      -> pure ()

type Browser domain = (State (S.Stream IO (Media.Media domain)), State Input)

browser :: forall domain. (Websocket domain, Authenticated domain) => (Browser domain => View) -> View
browser v = withStream @domain (withInput @domain v)
