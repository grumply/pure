{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Pure.Media.Library.GHC.Library (Config(..),library) where

import Pure.Media.Library.API as API
import Pure.Media.Library.Data.Library hiding (library)
import Pure.Media.Library.Data.Media

import Pure.Auth.Data.Username
import qualified Data.Bloom.Limiter as Limiter
import Data.Txt (Txt,FromTxt(..),ToTxt(..))
import Data.Time (time,pattern Minutes)
import Data.Websocket as WS
import Data.File (writeByteTxt)
import Data.Sorcerer as Sorcerer

import System.FilePath (takeDirectory,(</>))
import System.Directory (createDirectoryIfMissing,removeFile)

import Control.Monad.IO.Class (MonadIO(..))
import Data.Maybe (fromMaybe)
import Data.Typeable (Typeable)
import Prelude hiding (read)

data Config domain = Config
  { root      :: Txt
  , authorize :: Username -> IO Bool
  , validate  :: File -> IO (Maybe (Media domain))
  }

library :: forall domain. Typeable domain => Config domain -> Endpoints _ _ _ _
library config = Endpoints (API.api @domain) msgs reqs
  where
    msgs = WS.non
    reqs = handleGetLibrary config
       <:> handleUpload config 
       <:> handleDelete config
       <:> WS.non

allowed :: MonadIO m => Username -> m Bool
allowed un = liftIO (Limiter.allowed 20 "pure-media-library" (toTxt un) (Minutes 10 0))

handleGetLibrary :: forall domain. Typeable domain => Config domain -> RequestHandler (API.GetLibrary domain)
handleGetLibrary Config { authorize } = responding do
  un <- acquire
  authorized <- liftIO (authorize un)
  if authorized then do
    ml <- Sorcerer.read (LibraryStream un :: Stream (LibraryMsg domain))
    reply (Just (fromMaybe (Library []) ml))
  else do
    reply Nothing

handleUpload :: forall domain. Typeable domain => Config domain -> RequestHandler (API.Upload domain)
handleUpload Config { root, authorize, validate } = responding do
  file <- acquire
  liftIO (validate file) >>= \case
    Just m -> do
      allow <- allowed (owner m)
      if allow then
        let fp = fromTxt root <> fromTxt (path m)
        in
          Sorcerer.transact (LibraryStream (owner m) :: Stream (LibraryMsg domain)) (CreateMedia m) >>= \case
            Update (l :: Library domain) -> do
              liftIO do 
                createDirectoryIfMissing True (takeDirectory fp)
                writeByteTxt fp (snd file)
              reply (Just m)
            _ -> 
              reply Nothing
      else
        reply Nothing
    _ -> reply Nothing

handleDelete :: forall domain. Typeable domain => Config domain -> RequestHandler (API.Delete domain)
handleDelete Config { root, authorize, validate } = responding do
  media <- acquire
  authorized <- liftIO (authorize (owner media))
  if authorized then do
    Sorcerer.transact (LibraryStream (owner media) :: Stream (LibraryMsg domain)) (DeleteMedia media) >>= \case
      Update (l :: Library domain) -> do
        liftIO (removeFile (fromTxt root <> fromTxt (path media)))
        reply True
      _ -> 
        reply False
  else do
    reply False
