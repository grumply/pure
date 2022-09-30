{-# LANGUAGE CPP, RecordWildCards, OverloadedStrings, LambdaCase, BangPatterns, PatternSynonyms, ScopedTypeVariables #-}
module Server where

import Control.Exception
import Control.Concurrent
import Control.Monad
import Data.Default
import Data.IntMap as IntMap
import Data.Unique
import Data.View
import Data.Websocket hiding (handle)

data Server
  = Server_
    { ip         :: String
    , port       :: Int
    , connection :: Websocket -> View
    }
  | SecureServer_
    { ip         :: String
    , port       :: Int
    , sslKey     :: FilePath
    , sslCert    :: FilePath
    , sslChain   :: Maybe FilePath
    , connection :: Websocket -> View
    }

#ifndef __GHCJS__
pattern SecureServer :: String -> Int -> FilePath -> FilePath -> Maybe FilePath -> (Websocket -> View) -> View
pattern SecureServer ip port sslKey sslCert sslChain connection = View (SecureServer_ ip port sslKey sslCert sslChain connection)

pattern Server :: String -> Int -> (Websocket -> View) -> View
pattern Server ip port connection = View (Server_ ip port connection)

data ServerState = ServerState
  { ssListener    :: ThreadId
  , ssSocket      :: Socket
  , ssConnections :: !(IntMap Websocket)
  }

instance Pure Server where
  view =
      Component $ \self ->
          let
              updConnections f = modify_ self $ \_ ss -> ss { ssConnections = f (ssConnections ss) }

              handleConnections sock = forever $ handle (\(_ :: SomeException) -> return ()) $ do
                  (conn,sockAddr) <- accept sock
                  void $ forkIO $ do
                    ws <- serverWS conn
                    u <- hashUnique <$> newUnique
                    onStatus ws $ \case
                      Closed _ -> updConnections (IntMap.delete u)
                      _ -> return ()
                    updConnections (IntMap.insert u ws)
              handleSecureConnections ctx sock = forever $ handle (\(_ :: SomeException) -> return ()) $ do
                  (conn,sockAddr) <- accept sock
                  void $ forkIO $ do
                    ssl <- sslAccept conn
                    ws <- serverWSS conn ssl
                    u <- hashUnique <$> newUnique
                    onStatus ws $ \case
                      Closed _ -> updConnections (IntMap.delete u)
                      _        -> return ()
                    updConnections (IntMap.insert u ws)
          in
              def
                  { onConstruct = do
                      s <- ask self
                      case s of
                        Server_ {..} -> do
                          sock <- makeListenSocket ip port
                          tid <- forkIO $ handleConnections sock
                          return (ServerState tid sock IntMap.empty)
                        SecureServer_ {..} -> do
                          ctx <- sslSetupServer sslKey sslCert sslChain
                          sock <- makeListenSocket ip port
                          tid <- forkIO $ handleSecureConnections ctx sock
                          return (ServerState tid sock IntMap.empty)
                  , render = \s ServerState {..} ->
                      Keyed (SimpleHTML "clients") <||#>
                        (fmap (fmap (connection s)) (IntMap.toAscList ssConnections))
                  }
#endif