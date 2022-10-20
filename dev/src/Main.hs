{-# language CPP, BlockArguments, OverloadedStrings #-}
module Main where

#ifndef __GHCJS__
import Control.Concurrent
import Control.Monad
import qualified Data.ByteString as BS
import Data.Foldable
import Network.Wai
import Network.Wai.Application.Static
import Network.Wai.Handler.Warp
-- import Network.Wai.Middleware.Gzip
import System.Exit
import System.FSNotify hiding (Action)
import System.Process

main :: IO ()
main = do
  let 
    config = defaultConfig 
      { confDebounce = Debounce (realToFrac (0.5 :: Double)) 
      }

  withManagerConf config $ \mgr -> do
    frontend mgr 
    backend mgr 
    server

server :: IO ()
server = run 80 app -- (compressing app)
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
          | Just True <- fmap isBot (requestHeaderUserAgent req) ->
            -- If the UA contains 'bot', 'crawl', or 'spider', serve a
            -- statically rendered version. This allows us to serve content over
            -- a websocket for real users and still serve content to search
            -- engines. See https://webmasters.stackexchange.com/a/64805 for a
            -- justification.
            fileServer req { pathInfo = addHTMLExtension ("static" : pathInfo req) } send

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
        ec <- spawn "js-unknown-ghcjs-cabal build frontend" waitForProcess
        case ec of
          ExitSuccess -> do
            spawn "cp dist-newstyle/build/js-ghcjs/ghcjs-*/frontend-*/x/frontend/opt/build/frontend/frontend.jsexe/all.js dist/all.js" waitForProcess
            pure ()
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

  buildTrigger <- newMVar ()
  runTrigger <- newEmptyMVar

  builder <- 
    forkIO do
      forever do
        takeMVar buildTrigger
        ec <- spawn "cabal build backend" waitForProcess
        case ec of
          ExitSuccess -> 
            void (tryPutMVar runTrigger ())
          _ -> 
            pure ()

  runner <- 
    forkIO do
      thread <- newEmptyMVar
      forever do
        takeMVar runTrigger
        x <- tryTakeMVar thread >>= traverse_ terminateProcess 
        forkIO do
          void do
            spawn "cabal run backend" (putMVar thread)

  watchTree mgr "app/backend" (const True) $ \ev ->
    void (tryPutMVar buildTrigger ())

  watchTree mgr "app/shared" (const True) $ \ev ->
    void (tryPutMVar buildTrigger ())

  pure do
    killThread builder 
    killThread runner
    

spawn :: String -> (ProcessHandle -> IO a) -> IO a
spawn s wph = do
  Prelude.putStrLn s
  withCreateProcess (shell s) $ \_ _ _ -> wph
#else
main = print "dev environment not supported on GHCJS"
#endif