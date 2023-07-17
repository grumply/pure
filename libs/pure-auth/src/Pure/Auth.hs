{-# language CPP, TypeApplications, RankNTypes, BlockArguments, ScopedTypeVariables, FlexibleContexts, OverloadedStrings #-}
module Pure.Auth 
  ( module Export
  , Object
  , liveWith, live
  , Proofs
#ifndef __GHCJS__
  , Secret, Pool
  , Pure.Auth.object
#endif
  ) where

#ifdef __GHCJS__
import Client
#else
import Server
#endif
import Control.Concurrent
import Control.Dynamic
import Control.Exception as Exception 
import Control.Monad
import Control.Producer as Producer
import Control.Retry
import Data.Bool
import Data.Default
import Data.DOM
import Data.Exists
import Data.Function
import Data.IORef
import Data.JSON
import Data.List as List
import Data.Sorcerer as Sorcerer
import Data.Sorcerer.Sorcery
import Data.Time
import Data.Txt
import Data.Typeable
import Data.URI
import Data.View
import Endpoint
import Pure.Auth.Data as Export (Token(owner,claims))
import Pure.Auth.API as Export
import Pure.Auth.Data

#ifndef __GHCJS__
import Pure.Auth.GHC.Crypto 
import Pure.Auth.GHC as Export hiding (AuthEvent,activate,register,recover,startDelete,delete,recentAuthEvents,login,startRecover,updateEmail,updatePassword,logout,logoutAll)
#endif

type Object c e a = Endpoint (Token c -> Stream e -> IO (Token e,Int,Maybe a))

live :: forall c e a. (Typeable c, Typeable e, FromJSON a, FromJSON e, Typeable a, ToJSON a, ToJSON e, ToJSON (Stream e), Aggregable e a, Exists (Token c)) => Txt -> Object c e a -> Stream e -> ((Producer e, Exists a) => View) -> View
live api o s v = liveWith @c @e @a (jittered Second & limitDelay (Seconds 30 0)) api o s v

liveWith :: forall c e a. (Typeable c, Typeable e, FromJSON e, FromJSON a, Typeable a, ToJSON a, ToJSON e, ToJSON (Stream e), Exists (Token c), Aggregable e a) => Policy -> Txt -> Object c e a -> Stream e -> ((Producer e, Exists a) => View) -> View
liveWith policy api o s v = 
#ifdef __GHCJS__
  flip Component (dynamic @(Exists a, Producer e) v) \self -> def
    { onConstruct = do
        (t,i,ma) <- Client.get api (o <> "/read") (it :: Token c) s
        ir <- newIORef i
        let

          publisher = post api (fromTxt (toTxt o) <> "/write" :: Endpoint (Token e -> Stream e -> e -> IO ())) t s

          integrate :: (Int,e) -> IO ()
          integrate (j,e) = modifyM_ self \p (tid,pub,i,a) -> do
            case Sorcerer.update e a of
              Just a  -> pure ((tid,pub,j,a),writeIORef ir j)
              Nothing -> pure ((tid,pub,j,a),writeIORef ir j)
                
        rs <- newIORef def
        let stop = join (readIORef rs)

        tid <- forkIO do
          Producer.stream integrate do
            handle (\ThreadKilled -> stop) do
              void do
                retrying policy do
                  mv  <- newEmptyMVar
                  i <- readIORef ir
                  es  <- new_event_source_js (api <> toTxt o <> "/events?payload=" <> encodeURIComponent (btoa_js (encode (t,s,i))))

                  msgs <- onRaw es "message" def \_ msg ->
                    case msg .# "data" of
                      Just d | Just (i,e) <- decode d -> Producer.yield @(Int,e) (i,e)
                      _ -> putMVar mv stop

                  errs <- onRaw es "error" def \_ _ -> putMVar mv (stop >> retry)

                  writeIORef rs (msgs >> errs >> close_es_js es)

                  join (takeMVar mv)
        pure (tid,publisher,i,ma)

    , onUnmounted = Data.View.get self >>= \(tid,_,_,_) -> killThread tid

    , render = \v (tid,pub,i,ma) -> maybe Null (\a -> Producer.stream pub (with a (fromDynamic v))) ma
    }
#else
  Data.View.Null
#endif

newtype EventSource = EventSource JSV

#ifdef __GHCJS__
foreign import javascript unsafe
  "$r = new EventSource($1)" new_event_source_js :: Txt -> IO EventSource

foreign import javascript unsafe
  "$1.close()" close_es_js :: EventSource -> IO ()

foreign import javascript unsafe
  "btoa($1)" btoa_js :: Txt -> Txt
#endif

#ifndef __GHCJS__
object :: forall c e a. (Typeable c, Typeable e, Typeable a, FromJSON (Stream e), ToJSON a, ToJSON e, FromJSON e, Streamable e, Aggregable e a, Ord (Stream e), Pool c, Secret c, Pool e, Secret e) => Object c e a -> Time -> (Username c -> Stream e -> IO (Maybe Bool)) -> [Server.Handler]
object o dur f =
  [ Server.lambda (o <> "/read") do
      authenticated @c \s -> do
        permissions <- f it s
        let ps = ("id",toTxt (Sorcerer.stream s)) : maybe [] (bool [("read","")] [("read",""),("write","")]) permissions
        (i,a) <- Sorcerer.read' s
        now <- Data.Time.time
        let t = sign (fromTxt (toTxt (it :: Username c))) (now + dur) ps 
        pure (t,i,a)

  , Server.lambda (fromTxt (toTxt o) <> "/write") do
      authenticated @e \(s :: Stream e) (e :: e) -> do
        authorized @e "write" \_ -> do
          case List.lookup "id" (proofs @e) of
            Just x | Sorcerer.stream s == fromTxt x -> Sorcerer.write s e
            _ -> unauthorized

  , Server.channel @(Token e -> Stream e -> Int -> IO [(Int,e)]) (fromTxt (toTxt o) <> "/events") do
      authenticated @e \(s :: Stream e) n -> do
        authorized @e "read" \_ -> do
          case List.lookup "id" (proofs @e) of
            Just x | Sorcerer.stream s == fromTxt x -> do
              chan     <- newChan
              listener <- Producer.stream (writeChan chan) (subscribeStream s id)
              Exception.handle (\(se :: SomeException) -> unlisten listener >> Exception.throw se) do
                List.zip [n+1..] <$> liftM2 (++) (Sorcerer.events n s) (getChanContents chan)

            _ -> unauthorized

  ] 
#endif

