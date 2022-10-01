{-# language AllowAmbiguousTypes, DuplicateRecordFields, UndecidableInstances #-}
module Pure.Magician.Server (module Pure.Magician.Server, Server(..), Limit(..), activate) where

import Pure.Magician.Resources 
import Pure.Magician.Server.Analytics
import Pure.Magician.Server.Cache
import qualified Pure.Magician.Server.Config as Config
import Pure.Magician.Server.Listen
import Pure.Magician.Server.Serve
import Pure.Magician.Server.Static
import Pure.Magician.Server.Limit

import Pure.Auth as Export (Config(..),Token(..),Username(..),Password,Email,auth,authDB,tryCreateUser) 
import Pure.Conjurer as Export hiding (Cache,cache)
import Pure.Conjurer.Analytics
import qualified Pure.Conjurer as Conjurer
import Pure.Convoker as Convoker
import Pure hiding (Server)
import qualified Pure
import Control.Component as Component
import qualified Data.Txt as Txt
import Data.Websocket ( Websocket, enact, repeal, clientWS, activate )
import qualified Data.Websocket as WS

import Control.Monad ( liftM2, forever, void )
import Data.Char
import Data.Foldable (for_)
import Data.List as List
import Data.Typeable ( Typeable, Proxy(..) )
import GHC.Generics hiding (Meta)
import System.IO

type UserConfig a = Effect (Msg (WithSocket a)) => Websocket -> SessionId -> Export.Config a 

type App = (Reader Websocket, Reader SessionId, Reader (Maybe Username))

serve
  :: forall a resources cache static. 
    ( Typeable a
    , Server a
    , Subset (Caches a) (Resources a) ~ True
    , Subset (Statics a) (Resources a) ~ True
    , Subset (Analyze a) (Resources a) ~ True
    , ListenMany a (Resources a)
    , ServeMany a (Resources a)
    , CacheMany a (Caches a)
    , StaticMany a (Statics a)
    , LimitMany a (Resources a)
    , Analyzeable (Analyze a)
    ) => UserConfig a -> (App => View) -> IO ()
serve userConfig v = do
  cfg@Config.Config {..} <- Config.getConfig
  hSetBuffering stdout LineBuffering 
  listenAll @a
  tryCreateUser @a admin email password 
  tryReadProduct fullPermissions def (AdminsContext :: Context (Admins a)) AdminsName >>= \case
    Just (Admins _) -> pure ()
    Nothing -> void (tryCreateAdmins @a [admin])
  cacheAll @a
  analyze @a (Milliseconds refresh 0)
  Pure.run do
    case (,) <$> key <*> cert of
      Just (k,c) -> Pure.server (Pure.SecureServer host port k c chain (Component.run . WithSocket userConfig v))
      _ -> Pure.server (Pure.Server host port (Component.run . WithSocket userConfig v))
  staticAll @a 
  forever (delay Minute)

data WithSocket a = WithSocket (Effect (Msg (WithSocket a)) => Websocket -> SessionId -> Config a) (App => View) Websocket
instance (Typeable a, Server a, ServeMany a (Resources a), LimitMany a (Resources a)) => Component (WithSocket a) where
  data Model (WithSocket a) = WithSocketModel (Maybe (Token a)) SessionId

  initialize (WithSocket _ _ socket) = do
    sid <- recordStart socket
    pure (WithSocketModel Nothing sid)

  data Msg (WithSocket a)
    = Startup 
    | Shutdown
    | GetUserToken (Maybe (Token a) -> IO ())
    | SetUserToken (Token a)
    | ClearUserToken

  startup = [Startup]
  shutdown = [Shutdown]
    
  upon msg (WithSocket cfg _ socket) (WithSocketModel userToken sid)  = 
    case msg of
    
      Shutdown -> do
        recordEnd sid 
        pure (WithSocketModel userToken sid)

      Startup -> do
        serveAll @a socket sid Nothing
        enact socket (auth socket (cfg socket sid))
        pure (WithSocketModel userToken sid)

      GetUserToken with -> do
        with userToken 
        pure (WithSocketModel userToken sid)

      ClearUserToken -> do
        for_ userToken $ \_ -> recordEnd sid
        pure (WithSocketModel Nothing sid)

      SetUserToken t@(Token (un,_)) -> do
        case userToken of
          Just t' 
            | t == t' -> 
              pure (WithSocketModel (Just t) sid)

            | otherwise -> do
              recordEnd sid
              sid' <- recordStart socket
              recordUser sid' un 
              pure (WithSocketModel (Just t) sid')

          Nothing -> do
            recordUser sid un 
            pure (WithSocketModel (Just t) sid)

  view (WithSocket _ v socket) (WithSocketModel token sid) | user <- fmap (\(Token (un,_)) -> un) token =
    reader socket do
      reader user do 
        reader sid do
          v

defaultUserConfig :: forall a. (Effect (Msg (WithSocket a)), Server a, ServeMany a (Resources a), LimitMany a (Resources a), RemoveMany a (Resources a)) => Websocket -> SessionId -> Config a 
defaultUserConfig socket sid = Config {..}
  where
    blacklist = []
    implicitlyWhitelisted = Prelude.not . (`elem` blacklist)

    validateUsername (coerce -> un) = 
      List.and 
        [ Txt.length un <= 20 
        , Txt.all ((&&) <$> isAscii <*> isAlphaNum) un
        , implicitlyWhitelisted un
        ] 

    onTokenChange mt = do
      command (maybe ClearUserToken SetUserToken mt)
      case mt of
        Nothing -> void do
          removeAll @a socket
          serveAll @a socket sid Nothing

        Just (Token (un,_)) -> void do
          removeAll @a socket
          serveAll @a socket sid (Just un)

    onDeleted username email = pure ()

    onRegister username email key activate = activate

    onRecover username email key = pure ()

    onDelete username email key = pure ()

