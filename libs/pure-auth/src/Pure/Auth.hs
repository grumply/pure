{-# language CPP, TypeApplications, RankNTypes, BlockArguments, ScopedTypeVariables, FlexibleContexts, OverloadedStrings, DataKinds #-}
module Pure.Auth 
  ( module Export
  , Object, liveWith, live
#ifndef __GHCJS__
  , Pure.Auth.object
#endif
  ) where

#ifdef __GHCJS__
import Client hiding (user)
#else
import Server
#endif
import Control.Concurrent hiding (yield)
import Control.Exception as Exception 
import Control.Monad
import Data.Retry
import Data.Bool
import Data.Default
import Data.DOM
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

import Pure.Auth.Data
import Pure.Auth.Auth as Export
#ifndef __GHCJS__
import Pure.Auth.API as Export
import Pure.Auth.GHC as Export hiding (AuthEvent,activate,register,recover,startDelete,delete,recentAuthEvents,login,startRecover,updateEmail,updatePassword,logout,logoutAll)
#else
import Pure.Auth.Access as Export
#endif

type Object c e a = GET (Token c -> Stream e -> IO (Token e,Int,Maybe a))

live :: forall c e a. (API c, Typeable c, Typeable e, FromJSON a, FromJSON e, Typeable a, ToJSON a, ToJSON e, ToJSON (Stream e), Aggregable e a, Exists (Token c)) => Object c e a -> Stream e -> ((Producer e, Exists a) => View) -> View
live = liveWith @c @e @a (jittered Second & limitDelay (Seconds 30 0))

liveWith :: forall c e a. (API c, Typeable c, Typeable e, FromJSON e, FromJSON a, Typeable a, ToJSON a, ToJSON e, ToJSON (Stream e), Exists (Token c), Aggregable e a) => Policy -> Object c e a -> Stream e -> ((Producer e, Exists a) => View) -> View
liveWith policy o s v = 
#ifdef __GHCJS__
  flip Component (Data.View.proof @(Exists a, Producer e) v) \self -> def
    { onConstruct = do
        (t,i,ma) <- Client.get_ @c (o <> "/read") (it :: Token c) s
        ir <- newIORef i
        let

          publisher :: e -> IO ()
          publisher = patch_ @c (fromTxt (toTxt o) <> "/write") t s

          integrate :: (Int,e) -> IO ()
          integrate (j,e) = modifyrefM_ self \p (tid,pub,i,a) -> do
            case Sorcerer.update e a of
              Just a  -> pure ((tid,pub,j,a),writeIORef ir j)
              Nothing -> pure ((tid,pub,j,a),writeIORef ir j)
                
        rs <- newIORef def
        let stop = join (readIORef rs)

        tid <- forkIO do
          Data.View.stream integrate do
            handle (\ThreadKilled -> stop) do
              void do
                retrying policy do
                  mv  <- newEmptyMVar
                  i <- readIORef ir
                  es  <- new_event_source_js (api @c <> toTxt o <> "/events?payload=" <> encodeURIComponent (btoa_js (encode (t,s,i))))

                  msgs <- onRaw es "message" def \_ msg ->
                    case msg .# "data" of
                      Just d | Just (i,e) <- decode d -> yield @(Int,e) (i,e)
                      _ -> putMVar mv stop

                  errs <- onRaw es "error" def \_ _ -> putMVar mv (stop >> retry)

                  writeIORef rs (msgs >> errs >> close_es_js es)

                  join (takeMVar mv)
        pure (tid,publisher,i,ma)

    , onUnmounted = getref self >>= \(tid,_,_,_) -> killThread tid

    , render = \v (tid,pub,i,ma) -> maybe Null (\a -> Data.View.stream pub (with a (prove v))) ma
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
  [ Server.lambda (o <> "/read") False False [] do
      authenticated @c \s -> do
        permissions <- f (name @c) s
        let ps = ("id",toTxt (Sorcerer.stream s)) : maybe [] (bool [("read","")] [("read",""),("write","")]) permissions
        (i,a) <- Sorcerer.read' s
        now <- Data.Time.time
        let t = sign (fromTxt (toTxt (name @c))) (now + dur) ps 
        pure (t,i,a)

  , Server.lambda (fromTxt (toTxt o) <> "/write" :: PATCH (Token e -> Stream e -> e -> IO ())) False False [] do
      authenticated @e \(s :: Stream e) (e :: e) -> do
        authorized @e "write" \_ -> do
          authorized @e "id" \x -> 
            if Sorcerer.stream s == fromTxt x 
              then Sorcerer.write s e 
              else unauthorized

  , Server.channel (fromTxt (toTxt o) <> "/events" :: GET (Token e -> Stream e -> Int -> IO [(Int,e)]) ) False do
      authenticated @e \(s :: Stream e) n -> do
        authorized @e "read" \_ -> do
          authorized @e "id" \x -> do
            if Sorcerer.stream s == fromTxt x then do
              chan     <- newChan
              listener <- Data.View.stream (writeChan chan) (subscribeStream s id)
              List.zip [n+1..] <$> liftM2 (++) (Sorcerer.events n s) (getChanContents chan) `onException` unlisten listener
            else
              unauthorized

  ] 
#endif
