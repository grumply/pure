{-# LANGUAGE CPP, TypeOperators, GADTs, FlexibleContexts, DataKinds, TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, TypeFamilies, ScopedTypeVariables #-}
module Data.Websocket.Handlers where

import Data.Log
import Data.JSON
import Data.Proxy
import Data.Txt (Txt)
import Data.Websocket.API
import Data.Websocket.Callbacks
import Data.Websocket.Dispatch
import Data.Websocket.Endpoint
import Data.Websocket.Identify
import Data.Websocket.Message
import Data.Websocket.Request
import Data.Websocket.Internal
import Unsafe.Coerce

data ActiveImplementation es
  where
    ActiveImplementationNull
      :: ActiveImplementation '[]

    ActiveImplementationCons
      :: Proxy e
      -> Endpoint a
      -> ActiveImplementation es
      -> ActiveImplementation (e ': es)

class EnactImplementation api_ty hndlr es es' where
  -- we take api_ty's es to be our fixed basis
  enactImplementation :: Websocket
                 -> api_ty es
                 -> Implementation hndlr es'
                 -> IO (ActiveImplementation es)

instance EnactImplementation api_ty hndlr '[] '[] where
  enactImplementation _ _ _ = return ActiveImplementationNull

data ActiveAPI messages requests =
  ActiveAPI (ActiveImplementation messages) (ActiveImplementation requests)

type family Equal a b :: Bool
  where

    Equal a a = 'True

    Equal a b = 'False

data RequestHandler rqTy
  where
    RequestHandler
      :: (Request rqTy, Req rqTy ~ request, Identify request, I request ~ rqI, FromJSON request, Rsp rqTy ~ response, ToJSON response)
      => Proxy rqTy
      -> RequestCallback request response
      -> RequestHandler rqTy

-- | Construct a well-typed request handler.
--
-- Given a request type:
--
-- > data Ping = Ping
-- > data Pong = Pong
-- > ping = Proxy :: Proxy Ping
-- > instance Request Ping where
-- >   Req Ping = Ping
-- >   Rsp Ping = Pong
--
-- Or:
--
-- > mkMessage "Ping" [t|Ping -> Pong|]
--
-- Create a handler with:
--
-- > responds ping $ \done -> \case
-- >   Left dsp -> ...
-- >   Right (respond,Ping) -> liftIO $ respond (Right Pong)
responds :: (Request rqTy, Req rqTy ~ request, Identify request, I request ~ rqI, FromJSON request, Rsp rqTy ~ response, ToJSON response)
         => Proxy rqTy
         -> RequestCallback request response
         -> RequestHandler rqTy
responds = RequestHandler

data MessageHandler mTy
  where
    MessageHandler
      :: (Message mTy, M mTy ~ msg, FromJSON msg)
      => Proxy mTy
      -> (IO () -> Either Dispatch msg -> IO ())
      -> MessageHandler mTy

-- | Construct a well-typed message handler.
--
-- Given a message type:
--
-- > data HeartBeat
-- > heartbeat = Proxy :: Proxy (HeartBeat)
-- > instance Message HeartBeat where
-- >   type M HeartBeat = ()
--
-- Or:
--
-- > mkMessage "HeartBeat" [t|()|]
--
-- Create a handler with:
--
-- > accepts heartbeat $ \done -> \case
-- >   Left dsp -> ...
-- >   Right () -> ...
accepts :: (Message mTy, M mTy ~ msg, FromJSON msg)
        => Proxy mTy
        -> (IO () -> Either Dispatch msg -> IO ())
        -> MessageHandler mTy
accepts = MessageHandler

instance ( GetHandler MessageHandler message msgs'
         , Removed msgs' message ~ msgs''
         , DeleteHandler MessageHandler message msgs' msgs''
         , EnactImplementation (Interface Message) MessageHandler msgs msgs''
         , Message message
         , M message ~ msg
         , FromJSON msg
         )
  => EnactImplementation (Interface Message) MessageHandler (message ': msgs) msgs' where
  enactImplementation ws_ (InterfaceCons pm ms) mhs = do
    case getHandler mhs :: MessageHandler message of
      MessageHandler _ f -> do
        let p = Proxy :: Proxy message
            mhs' = deleteHandler p mhs :: Implementation MessageHandler msgs''
        amh <- onMessage ws_ p f
        ams <- enactImplementation ws_ ms mhs'
        return $ ActiveImplementationCons pm (Endpoint (messageHeader p) amh) ams

instance ( GetHandler RequestHandler request rqs'
         , Removed rqs' request ~ rqs''
         , DeleteHandler RequestHandler request rqs' rqs''
         , EnactImplementation (Interface Request) RequestHandler rqs rqs''
         , Request request
         , Req request ~ req
         , Rsp request ~ response
         , ToJSON req
         , FromJSON response
         , Logging
         )
  => EnactImplementation (Interface Request) RequestHandler (request ': rqs) rqs' where
  enactImplementation ws_ (InterfaceCons pm ms) mhs =
    case getHandler mhs :: RequestHandler request of
      RequestHandler _ f -> do
        let p = Proxy :: Proxy request
            mhs' = deleteHandler p mhs :: Implementation RequestHandler rqs''
        amh <- respond ws_ p f
        ams <- enactImplementation ws_ ms mhs'
        return $ ActiveImplementationCons pm (Endpoint (requestHeader p) amh) ams

data Endpoints msgs rqs msgs' rqs'
  where
    Endpoints
      :: ( EnactImplementation (Interface Message) MessageHandler msgs msgs'
         , EnactImplementation (Interface Request) RequestHandler rqs rqs'
         )
      => API msgs rqs
      -> Implementation MessageHandler msgs'
      -> Implementation RequestHandler rqs'
      -> Endpoints msgs rqs msgs' rqs'

-- | Given two distinct API implementations, combine them into one implementation.
--
-- The type is rather hairy. Note that `TListAppend` guarantees uniqueness.
--
-- (<++++>) :: (TListAppend (Implementation RequestHandler c) rqsl' rqsr' rqs'
--             ,TListAppend (Implementation MessageHandler c) msgsl' msgsr' msgs'
--             ,TListAppend (API Request) rqsl rqsr rqs
--             ,TListAppend (API Message) msgsl msgsr msgs
--             ,EnactImplementation (Interface Request) RequestHandler rqs rqs'
--             ,EnactImplementation (Interface Message) MessageHandler msgs msgs'
--             )
--          => Implementation msgsl rqsl msgsl' rqsl'
--          -> Implementation msgsr rqsr msgsr' rqsr'
--          -> Implementation msgs rqs msgs' rqs'
(Endpoints (API ml rl) eml erl) <++++> (Endpoints (API mr rr) emr err) =
  Endpoints (API (ml <+++> mr) (rl <+++> rr)) (eml <+++> emr) (erl <+++> err)

-- | Given an Implementation of an API, execute the implementation and return a corresponding ActiveAPI.
enact :: Websocket -> Endpoints msgs rqs msgs' rqs' -> IO (ActiveAPI msgs rqs)
enact ws_ (Endpoints local mhs rhs) = do
  let API mapi rapi = local
  amapi <- enactImplementation ws_ mapi mhs
  arapi <- enactImplementation ws_ rapi rhs
  let active = ActiveAPI amapi arapi
  return active

repeal :: ActiveAPI msgs rqs -> IO ()
repeal (ActiveAPI mimpl rimpl) = do
  deactivate mimpl 
  deactivate rimpl
  where
    deactivate :: ActiveImplementation es -> IO ()
    deactivate ActiveImplementationNull = pure ()
    deactivate (ActiveImplementationCons _ ep rest) = do
      dcCleanup (epDispatchCallback ep)
      deactivate rest 
