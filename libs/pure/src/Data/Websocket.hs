{-# LANGUAGE CPP, FlexibleContexts, RankNTypes, TypeApplications, LambdaCase,
             TypeOperators, ScopedTypeVariables, GADTs, DataKinds,
             FlexibleInstances, MultiParamTypeClasses, FunctionalDependencies,
             GeneralizedNewtypeDeriving, TypeApplications, RecordWildCards, 
             TemplateHaskell, ViewPatterns
  #-}
module Data.Websocket
  ( mkRequest,
    mkMessage,
    request,
    requestDebug,
    message,
    Stop(..),
    Acquire(..),
    Reply(..),
    Responding,
    responding,
    responding',
    respondWith,
    respondWithRaw,
    Awaiting,
    awaiting,
    awaiting',
    module Export
  ) where

import Control.Log hiding (error)
import Data.JSON (ToJSON,FromJSON,fromJSON,logJSON)
import qualified Data.JSON as JSON

import Data.Txt (Txt)
import Data.Time as Time (time)

import Data.ByteString.Lazy as Lazy


import Data.Websocket.API       as Export
import Data.Websocket.Callbacks as Export
import Data.Websocket.Dispatch  as Export
import Data.Websocket.Endpoint  as Export
import Data.Websocket.Handlers  as Export
import Data.Websocket.Internal  as Export hiding (request,message)
import Data.Websocket.Identify  as Export
import Data.Websocket.Message   as Export
import Data.Websocket.Request   as Export
import Data.Websocket.TypeRep   as Export

#ifndef __GHCJS__
import Network.Connection as Export (ConnectionParams(..))
import Network.WebSockets as Export (ConnectionOptions(..),defaultConnectionOptions)
#endif

import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Proxy
import Data.Unique

import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.Trans
import Control.Monad.Reader
import Control.Monad.IO.Class as Export

import Language.Haskell.TH
import Language.Haskell.TH.Lib

mapHead f [] = []
mapHead f (x:xs) = f x : xs

processName str = (mkName str,mkName $ mapHead toLower str)

mkRequest :: String -> TypeQ -> Q [Dec]
mkRequest (processName -> (dat,rq))  ty = do
  rr <- ty
  case rr of
    (AppT (AppT ArrowT req) rsp) -> do
      let dataDec = DataD [] dat [] Nothing [] []
          proxyFunTy  = SigD rq (ConT ''Proxy `AppT` ConT dat)
          proxyFunDec = FunD rq [ Clause [] (NormalB (ConE 'Proxy)) [] ]
          requestInstanceDec = InstanceD Nothing [] (ConT ''Request `AppT` ConT dat)
            [ TySynInstD (TySynEqn (Just [ PlainTV dat ]) (AppT (AppT (TupleT 2) (ConT ''Int)) req) (ConT ''Req))
            , TySynInstD (TySynEqn (Just [ PlainTV dat ]) rsp (ConT ''Rsp))
            ]
      return [dataDec,proxyFunTy,proxyFunDec,requestInstanceDec]
    _ -> error $ "Invalid request type for " ++ show dat

mkMessage :: String -> TypeQ -> Q [Dec]
mkMessage (processName -> (dat,msg)) ty = do
  message <- ty
  let dataDec = DataD [] dat [] Nothing [] []
      proxyFunTy  = SigD msg (ConT ''Proxy `AppT` ConT dat)
      proxyFunDec = FunD msg [ Clause [] (NormalB (ConE 'Proxy)) [] ]
      messageInstanceDec = InstanceD Nothing [] (ConT ''Message `AppT` ConT dat)
        [ TySynInstD (TySynEqn (Just [ PlainTV dat ]) message (ConT ''M))]
  return [dataDec,proxyFunTy,proxyFunDec,messageInstanceDec]

-- This works with the type of requests produced by `mkRequest`
request :: ( Request rqTy
          , Req rqTy ~ (Int,request)
          , ToJSON request
          , Rsp rqTy ~ response
          , FromJSON response
          , (rqTy Export.∈ rqs) ~ 'True
          , Logging
          )
       => API msgs rqs
       -> Websocket
       -> Proxy rqTy
       -> request
       -> (response -> IO ())
       -> IO ()
request api ws p rq f = do
  u <- hashUnique <$> newUnique
  void $ forkIO $ void $ do
    Export.apiRequest api ws p (u,rq) $ \_ rsp -> do
      traverse_ f rsp

-- This works with the type of requests produced by `mkRequest`
-- and conveniently prints the time the request took as well as
-- the request and response data. Drop this in as a first step 
-- in debugging a failed request; once in a while, the GHCJS 
-- and GHC JSON instances are mis-aligned.
requestDebug :: forall msgs rqTy request response rqs.
               ( Request rqTy
               , Req rqTy ~ (Int,request)
               , ToJSON request
               , ToJSON response
               , Rsp rqTy ~ response
               , FromJSON response
               , (rqTy Export.∈ rqs) ~ 'True
               , Logging
               )
            => API msgs rqs
            -> Export.Websocket
            -> Proxy rqTy
            -> request
            -> (response -> IO ())
            -> IO ()
requestDebug api ws p rq f = do
  u   <- hashUnique <$> newUnique
  void $ forkIO $ void $ do
    s <- Time.time
    logJSON ("sending",u,rq)
    Export.apiRequest api ws p (u,rq) $ \_ rsp -> do
      e <- Time.time
      case rsp of
        Left Dispatch {..} -> 
          case fromJSON pl of 
            JSON.Error err -> logJSON ("message parse failure",u,rsp,err,e - s)
            JSON.Success (a :: response) -> error "Invariant broken; twice parsed, once successful"
        Right r ->
          f r

message :: ( Message msgTy
          , M msgTy ~ message
          , ToJSON message
          , (msgTy Export.∈ msgs) ~ 'True
          , Logging
          )
       => API msgs rqs
       -> Websocket
       -> Proxy msgTy
       -> message
       -> IO ()
message api ws p msg =
  void $ forkIO $ void $ Export.apiMessage api ws p msg

type ResponseString =
#ifdef __GHCJS__
  Txt
#else
  Lazy.ByteString
#endif

class Stop m where
    stop :: m ()
    stopper :: m (IO ())

class Acquire a m | m -> a where
    acquire :: m a

class Reply a m | m -> a where
    reply :: a -> m ()
    replyRaw :: ResponseString -> m ()
    replyer :: m (Either ResponseString a -> IO ())

newtype Responding request response a = Responding { unResponding :: ReaderT (request,IO (),Either ResponseString response -> IO ()) IO a }
    deriving (Functor,Applicative,Monad,Alternative,MonadFail,MonadFix,MonadPlus)

instance MonadIO (Responding request response) where
    liftIO f = Responding (lift f)

instance Acquire request (Responding request response) where
    acquire = Responding $ do
      (request,_,_) <- ask
      return request

instance Reply response (Responding request response) where
    reply a = Responding $ do
      (_,_,send) <- ask
      lift $ send (Right a)
    replyRaw s = Responding $ do
      (_,_,send) <- ask
      lift $ send (Left s)
    replyer = Responding $ do
      (_,_,send) <- ask
      pure send

instance Stop (Responding request response) where
    stop = Responding $ do
      (_,f,_) <- ask
      lift f
    stopper = Responding $ do
      (_,f,_) <- ask
      pure f

responding :: forall rqTy request response.
              ( Request rqTy
              , Req rqTy ~ (Int,request)
              , Identify (Req rqTy)
              , I (Req rqTy) ~ Int
              , FromJSON request
              , Rsp rqTy ~ response
              , ToJSON response
              )
           => Responding request response () -> RequestHandler rqTy
responding rspndng = responds (Proxy @rqTy) $ \done -> \case
    Right (rsp,(_,rq)) -> runReaderT (unResponding rspndng) (rq,done,void . rsp)
    _ -> return ()

responding' :: forall rqTy request response.
              ( Request rqTy
              , Req rqTy ~ (Int,request)
              , Identify (Req rqTy)
              , I (Req rqTy) ~ Int
              , FromJSON request
              , Rsp rqTy ~ response
              , ToJSON response
              )
           => Awaiting Dispatch () -> Responding request response () -> RequestHandler rqTy
responding' errrng rspndng = responds (Proxy @rqTy) $ \done -> \case
    Left dsp           -> runReaderT (unAwaiting errrng) (dsp,done)
    Right (rsp,(_,rq)) -> runReaderT (unResponding rspndng) (rq,done,void . rsp)

respondWith :: ( Request rqTy
               , Req rqTy ~ (Int,request)
               , Identify (Req rqTy)
               , I (Req rqTy) ~ Int
               , FromJSON request
               , Rsp rqTy ~ response
               , ToJSON response
               ) => (request -> IO response) -> RequestHandler rqTy
respondWith f = responding $ acquire >>= liftIO . f >>= reply

respondWithRaw :: ( Request rqTy
                  , Req rqTy ~ (Int,request)
                  , Identify (Req rqTy)
                  , I (Req rqTy) ~ Int
                  , FromJSON request
                  , Rsp rqTy ~ response
                  , ToJSON response
                  ) => (request -> IO ResponseString) -> RequestHandler rqTy
respondWithRaw f = responding $ acquire >>= liftIO . f >>= replyRaw

newtype Awaiting message a = Awaiting { unAwaiting :: ReaderT (message,IO ()) IO a }
    deriving (Functor,Applicative,Monad,Alternative,MonadFail,MonadFix,MonadPlus)

instance MonadIO (Awaiting message) where
    liftIO f = Awaiting (lift f)

instance Acquire message (Awaiting message) where
    acquire = Awaiting (asks fst)

instance Stop (Awaiting message) where
    stop = Awaiting (asks snd >>= lift)
    stopper = Awaiting (asks snd)

awaiting :: forall msgTy message.
          ( Message msgTy
          , M msgTy ~ message
          , FromJSON message
          )
       => Awaiting message () -> MessageHandler msgTy
awaiting awtng = accepts (Proxy @msgTy) $ \done -> \case
    Right msg -> runReaderT (unAwaiting awtng) (msg,done)
    _ -> return ()

awaiting' :: forall msgTy message.
          ( Message msgTy
          , M msgTy ~ message
          , FromJSON message
          )
       => Awaiting Dispatch () -> Awaiting message () -> MessageHandler msgTy
awaiting' errrng awtng = accepts (Proxy @msgTy) $ \done -> \case
    Left dsp  -> runReaderT (unAwaiting errrng) (dsp,done)
    Right msg -> runReaderT (unAwaiting awtng) (msg,done)
