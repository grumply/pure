{-# language CPP #-}
module Pure.Media.Library where

import Endpoint
import Data.File
import Pure.Auth hiding (delete,Config)
#ifndef __GHCJS__
import Server
import Data.Marker
import System.Directory
import System.FilePath
#else
import Client
import Data.DOM
import Web.File as File
import Data.Events as Events (pattern OnChangeWith,target,intercept)
import Data.HTML (pattern Accept)
#endif

upload :: (forall method x. Endpoint method x) -> POST (Token app -> ByteTxt -> IO (Maybe (Marker ByteTxt)))
upload = (<> "/media/upload") . fromTxt . toTxt

delete :: (forall method x. Endpoint method x) -> PATCH (Token app -> Marker ByteTxt -> IO ())
delete = (<> "/media/delete") . fromTxt . toTxt

list :: (forall method x. Endpoint method x) -> GET (Token app -> IO [Marker ByteTxt]) 
list = (<> "/media/list") . fromTxt . toTxt

#ifndef __GHCJS__
data Config = Config
  { validate :: ByteTxt -> IO Bool
  , root :: FilePath
  }

media :: forall app. (Typeable app, Pool app, Secret app) => (forall method x. Endpoint method x) -> Config -> [Handler]
media ep Config {..} =
  [ lambda (upload ep) False False do
      authenticated @app \contents -> do
        valid <- validate contents
        if valid then do
          m <- markIO
          let dir = root </> fromTxt (toTxt (user @app)) 
              fp = dir </> fromTxt (toTxt m)
          createDirectoryIfMissing True dir
          writeByteTxt fp contents
          pure (Just m)
        else
          pure Nothing
        
  , lambda (delete ep) False False do
      authenticated @app \marker -> do
        let fp = root </> fromTxt (toTxt (user @app)) </> fromTxt (toTxt marker)
        exists <- doesFileExist fp
        when exists (removeFile fp)

  , lambda (list ep) False False do
      authenticated @app do
        let dir = root </> fromTxt (toTxt (user @app))
        exists <- doesDirectoryExist dir
        if exists then do
          contents <- getDirectoryContents dir
          let hidden ('.':_) = True
              hidden _ = False
          -- if there are any invalid markers, this will fail....
          pure (fmap (fromTxt . toTxt) (filter (not . hidden) contents))
        else
          pure []

  ]
#else
-- | Note: Endpoint app is needed to disambiguate the endpoints in case you want to have multiple
--         upload endpoints for different media libraries.
uploader :: forall api app. (API api, Typeable app, Authenticated app, Producer (Marker ByteTxt)) => (forall method x. Endpoint method x) -> View
uploader ep = Input <| OnChangeWith intercept select . Type "file" . Accept "image/*"
  where
    select :: Evt -> IO ()
    select (Events.target -> Just (coerce -> node)) = getFiles node >>= traverse_ onUpload
    select _ = pure ()

    onUpload :: (Txt,ByteTxt) -> IO ()
    onUpload (name,content) = post @api (upload ep) (token @app) content >>= traverse_ yield
#endif

