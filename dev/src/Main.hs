{-# language CPP, BlockArguments, OverloadedStrings, LambdaCase, ScopedTypeVariables #-}
module Main where

#ifndef __GHCJS__
import Control.Concurrent
import Control.Exception
import Control.Monad
import qualified Data.ByteString as BS
import Data.Foldable
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
import Network.Wai.Handler.WarpTLS
-- import Network.Wai.Middleware.Gzip
import System.Directory
import System.Exit
import System.FSNotify hiding (Action)
import System.Process
import System.IO
import System.IO.Error
import System.Posix.Signals

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  let 
    config = defaultConfig 
      { confDebounce = Debounce (realToFrac (0.5 :: Double)) 
      }

  blocker <- newEmptyMVar

  withManagerConf config $ \mgr -> do
    stopFrontend <- frontend mgr 
    stopBackend <- backend mgr 
    serving <- forkIO server
    installHandler keyboardSignal (Catch (stopFrontend >> stopBackend >> putMVar blocker ())) Nothing
    takeMVar blocker
    killThread serving

server :: IO ()
server = do
  fe <- doesFileExist "data/server.crt"
  let p = if fe then 443 else 80
  (if fe then runTLS (tlsSettings "data/server.crt" "data/server.key") else runSettings)
    (setServerName "pure-dev" (setPort p defaultSettings)) 
    app
  where
    -- compressing = gzip def { gzipFiles = GzipCacheFolder "dist/cache" }

    isBot x = 
         "bot"    `BS.isInfixOf` x 
      || "crawl"  `BS.isInfixOf` x 
      || "spider" `BS.isInfixOf` x

    app req send =
      case pathInfo req of
        []              -> fileServer req { pathInfo = ["index.html"] } send
        ["robots.txt"]  -> fileServer req send
        ["all.js"]      -> fileServer req send
        ["favicon.ico"] -> fileServer req send
        ( "static" : _) -> fileServer req send

        _
          -- | Just True <- fmap isBot (requestHeaderUserAgent req) ->
            -- If the UA contains 'bot', 'crawl', or 'spider', serve a
            -- statically rendered version. This allows us to serve content over
            -- a websocket for real users and still serve content to search
            -- engines. See https://webmasters.stackexchange.com/a/64805 for a
            -- justification.
          -- fileServer req { pathInfo = addHTMLExtension ("static" : pathInfo req) } send

          | otherwise -> 
            -- In all other cases, serve the index.html as a fallback.
            fileServer req { pathInfo = ["index.html"] } send

    fileServer = staticApp (defaultFileServerSettings "dist/")

    addHTMLExtension []     = []
    addHTMLExtension [file] = [file <> ".html"]
    addHTMLExtension (p:ps) = p:addHTMLExtension ps

frontend :: WatchManager -> IO (IO ())
frontend mgr = do
  
  buildTrigger <- newMVar ()

  builder <- 
    forkIO do
      forever do
        takeMVar buildTrigger
        let cmd = proc "js-unknown-ghcjs-cabal" ["build","frontend","--builddir=dist-newstyle/ghcjs"]
        print (cmdspec cmd)
        withCreateProcess cmd $ \_ _ _ ph ->
          waitForProcess ph >>= \case
            ExitSuccess -> do
              let cmd = shell "cp \"$(js-unknown-ghcjs-cabal list-bin exe:frontend --builddir=dist-newstyle/ghcjs).jsexe/all.js\" dist/all.js"
              print (cmdspec cmd)
              withCreateProcess cmd $ \_ _ _ -> void . waitForProcess
            _ ->
              pure ()

  watchTree mgr "app/frontend" (const True) $ \ev ->
    void (tryPutMVar buildTrigger ())

  watchTree mgr "app/shared" (const True) $ \ev ->
    void (tryPutMVar buildTrigger ())

  pure do
    killThread builder

backend :: WatchManager -> IO (IO ())
backend mgr = do

  first <- newMVar ()
  buildTrigger <- newMVar ()
  runTrigger <- newEmptyMVar

  let run = tryPutMVar runTrigger ()

  builder <- 
    forkIO do
      forever do
        mf <- tryTakeMVar first
        takeMVar buildTrigger
        let cmd = proc "cabal" ["build","backend","--builddir=dist-newstyle/ghc"]
        print (cmdspec cmd)
        withCreateProcess cmd $ \_ _ _ ph -> do
          waitForProcess ph >>= \case
            ExitSuccess -> void run
            _ -> maybe (pure ()) (const (void run)) mf

  runner <- 
    forkIO do
      takeMVar runTrigger
      forever do
        let cmd = shell "\"$(cabal list-bin exe:backend --builddir=dist-newstyle/ghc)\""
        print (cmdspec cmd)
        withCreateProcess cmd { create_group = True } $ \_ _ _ ph -> do
          takeMVar runTrigger
          mpid <- getPid ph
          for_ mpid \_ -> do
            tryJust (guard . isDoesNotExistError) (interruptProcessGroupOf ph)
            waitForProcess ph

  watchTree mgr "app/backend" (const True) $ \ev ->
    void (tryPutMVar buildTrigger ())

  watchTree mgr "app/shared" (const True) $ \ev ->
    void (tryPutMVar buildTrigger ())

  pure do
    killThread builder 
    killThread runner
#else
main = print "dev environment not supported on GHCJS"
#endif
