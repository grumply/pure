module Pure.Magician.Server.Server where

import Control.Log (Logging)
import Control.Exception
import Control.Concurrent
import Control.Monad
import Data.Default
import Data.IntMap as IntMap
import Data.Unique
import Data.View
import Data.Websocket hiding (handle)

#ifndef __GHCJS__
import qualified Network.Socket as S
import Network.WebSockets as WS

import OpenSSL as SSL
import OpenSSL.Session as SSL
import System.IO.Streams.SSL as Streams
import System.IO.Streams as Streams
#endif

data Server
  = Server
    { ip         :: String
    , port       :: Int
    , connection :: Websocket -> View
    }
  | SecureServer
    { ip         :: String
    , port       :: Int
    , sslKey     :: FilePath
    , sslCert    :: FilePath
    , sslChain   :: Maybe FilePath
    , connection :: Websocket -> View
    }

#ifndef __GHCJS__
data ServerState = ServerState
  { ssListener    :: ThreadId
  , ssSocket      :: S.Socket
  , ssConnections :: !(IntMap Websocket)
  }

server :: Logging => Server -> View
server =
  Component $ \self ->
      let
          updConnections f = modify_ self $ \_ ss -> ss { ssConnections = f (ssConnections ss) }

          handleConnections sock = forever $ handle (\(_ :: SomeException) -> return ()) $ do
              (conn,sockAddr) <- S.accept sock
              void $ forkIO $ do
                ws <- Data.Websocket.server WS.defaultConnectionOptions conn
                u <- hashUnique <$> newUnique
                onStatus ws $ \case
                  Closed _ -> updConnections (IntMap.delete u)
                  _ -> return ()
                updConnections (IntMap.insert u ws)
          handleSecureConnections ctx sock = forever $ handle (\(_ :: SomeException) -> return ()) $ do
              (conn,sockAddr) <- S.accept sock
              void $ forkIO $ do
                ssl <- sslAccept conn
                ws <- secureServer WS.defaultConnectionOptions conn ssl 
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
                    Server {..} -> do
                      sock <- makeListenSocket ip port
                      tid <- forkIO $ handleConnections sock
                      return (ServerState tid sock IntMap.empty)
                    SecureServer {..} -> do
                      ctx <- sslSetupServer sslKey sslCert sslChain
                      sock <- makeListenSocket ip port
                      tid <- forkIO $ handleSecureConnections ctx sock
                      return (ServerState tid sock IntMap.empty)
              , render = \s ServerState {..} ->
                  Keyed (SimpleHTML "clients") <||#>
                    fmap (fmap (Pure.Magician.Server.Server.connection s)) (IntMap.toAscList ssConnections)
              }

sslSetupServer keyFile certFile mayChainFile = SSL.withOpenSSL $ do
  ctx <- SSL.context
  SSL.contextSetPrivateKeyFile ctx keyFile
  SSL.contextSetCertificateFile ctx certFile
  forM_ mayChainFile (SSL.contextSetCertificateChainFile ctx)
  SSL.contextSetCiphers ctx "HIGH"
  SSL.contextSetVerificationMode ctx (VerifyPeer True True Nothing)
  return ctx

sslAccept conn = do
  ctx <- SSL.context
  ssl <- SSL.connection ctx conn
  SSL.accept ssl
  return ssl

sslConnect conn = do
  ctx <- SSL.context
  ssl <- SSL.connection ctx conn
  SSL.connect ssl
  return ssl

#endif
