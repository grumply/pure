{-# language RankNTypes, TypeApplications, ScopedTypeVariables, ConstraintKinds, FlexibleContexts, AllowAmbiguousTypes, BlockArguments, DataKinds, TypeOperators, RecordWildCards, TypeFamilies, ExistentialQuantification #-}
module Effect.Websocket (Policy(..),Websocket,Effect.Websocket.websocket,Effect.Websocket.onStatus,Cache(socket),req,msg,Effect.Websocket.request,flush,flushMany,flushAll) where

import Control.Concurrent
import Control.Reader
import Control.State
import Control.Fold
import Data.Foldable
import Data.IORef
import Data.JSON
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Proxy
import Data.Subscribe
import Data.Txt
import Data.Typeable
import Data.View hiding (modify,ask)
import Data.Websocket as WS hiding (Websocket)
import qualified Data.Websocket as WS
import Effect.Async
import Effect.Fork
import System.IO.Unsafe
import Unsafe.Coerce

data RequestMap 
  = forall rq pl. (WS.Request rq, WS.Req rq ~ (Int,pl), Ord pl) 
  => RequestMap (Proxy rq) (Map.Map pl (Either [WS.Rsp rq -> IO ()] (WS.Rsp rq)))

data Cache domain = Cache 
  { socket :: WS.Websocket
  , cache  :: Map TypeRep RequestMap
  }

type Websocket domain = Modify (Cache domain) 

websocket :: forall domain. Typeable domain => String -> Int -> (Websocket domain => View) -> View
websocket h p = manage (\_ -> pure) initial
  where
    initial :: Websocket domain => IO (Cache domain,Cache domain -> IO ())
    initial = do
      subscribe @(Cache domain -> IO (Cache domain))
      ws <- WS.clientWS h p
      pure (Cache ws mempty,\_ -> pure ())

onStatus :: forall domain. Websocket domain => (WS.Status -> IO ()) -> IO (IO ())
onStatus f = do
  mv <- newEmptyMVar
  modifyIO @(Cache domain) do
    let c@Cache { socket = s } = ask :: Cache domain
    putMVar mv s
    pure c
  s <- takeMVar mv
  st <- WS.wsStatus <$> readIORef s
  f st
  cb <- WS.onStatus s f
  pure (WS.scCleanup cb)

request' 
  :: forall domain rq rqs msgs pl. 
  ( WS.Request rq, WS.Req rq ~ (Int,pl), Ord pl, ToJSON pl, FromJSON (Rsp rq), rq ∈ rqs ~ True, Typeable domain )
  => Bool -> Bool -> WS.API msgs rqs -> Proxy rq -> pl -> (WS.Rsp rq -> IO (WS.Rsp rq)) -> (WS.Rsp rq -> IO ()) 
  -> IO ()
request' force bypass api p pl process cb
  | force = publish forced
  | otherwise = publish unforced
  where
    forced, unforced :: Cache domain -> IO (Cache domain)
    forced c = do
      WS.request api (socket c) p pl $ \rsp -> do
        rsp' <- process rsp
        if bypass then cb rsp' else satisfy @domain p pl rsp'
      pure c  
        { cache = 
          if bypass 
            then cache c
            else Map.insert (typeRep p) (RequestMap p (Map.singleton pl (Left [cb]))) (cache c)
        }

    unforced c =
      case Map.lookup (typeRep p) (cache c) of
        Nothing -> do
          WS.request api (socket c) p pl $ \rsp -> do
            rsp' <- process rsp
            if bypass
              then cb rsp'
              else satisfy @domain p pl rsp'
          pure 
            c { cache = 
                if bypass
                  then cache c
                  else Map.insert (typeRep p) (RequestMap p (Map.singleton pl (Left [cb]))) (cache c)
              }
            
        Just (RequestMap _ rm) ->
          -- that's a lot of coercions
          case Map.lookup pl (unsafeCoerce rm) of
            Just (Left cbs) -> do
              let 
                cbs' = cbs ++ [unsafeCoerce cb]
                rm' = RequestMap p (unsafeCoerce (Map.insert pl (Left cbs') (unsafeCoerce rm)))
              pure c { cache = Map.insert (typeRep p) rm' (cache c) }

            Just (Right rsp) -> do
              cb rsp
              pure c

            Nothing -> do
              WS.request api (socket c) p pl (satisfy @domain p pl)
              let 
                cbs = [cb]
                rm' = RequestMap p (unsafeCoerce (Map.insert pl (Left cbs) (unsafeCoerce rm)))
              pure c { cache = Map.insert (typeRep p) rm' (cache c) }

flush' 
  :: forall domain rq pl. 
  ( WS.Request rq, WS.Req rq ~ (Int,pl), Ord pl, Typeable domain )
  => Proxy rq -> pl -> IO ()
flush' p pl = publish go
  where
    go :: Cache domain -> IO (Cache domain)
    go c =
      case Map.lookup (typeRep p) (cache c) of
        Just (RequestMap p' rm) ->
          let 
            rm' = Map.delete pl (unsafeCoerce rm)
            cache' = Map.insert (typeRep p) (RequestMap p' (unsafeCoerce rm')) (cache c)
          in 
            pure c { cache = cache' }
        _ ->
          pure c

flushMany' 
  :: forall domain rq pl. 
  ( WS.Request rq, Typeable domain ) 
  => Proxy rq -> IO ()
flushMany' p = publish go
  where
    go :: Cache domain -> IO (Cache domain)
    go c = pure c { cache = Map.delete (typeRep p) (cache c) }

flushAll' :: forall domain. Typeable domain => IO ()
flushAll' = publish go
  where
    go :: Cache domain -> IO (Cache domain)
    go c = pure c { cache = Map.empty }

satisfy 
  :: forall domain rq pl rsp. 
  ( WS.Request rq, WS.Req rq ~ (Int,pl), WS.Rsp rq ~ rsp, Ord pl, Typeable domain )
  => Proxy rq -> pl -> rsp 
  -> IO ()
satisfy p pl rsp = publish go
  where
    go :: Cache domain -> IO (Cache domain)
    go c =
      case Map.lookup (typeRep p) (cache c) of
        Just (RequestMap _ rm) ->
          case Map.lookup pl (unsafeCoerce rm) of
            Just (Left cbs) -> do
              for_ @[] cbs ($ rsp)
              pure c
                { cache = Map.insert (typeRep p) (RequestMap p (Map.insert pl (Right rsp) (unsafeCoerce rm))) (cache c) } 

            Just (Right _) -> do
              pure c
              
            Nothing -> do
              -- huh?
              pure c
                { cache = Map.insert (typeRep p) (RequestMap p (Map.insert pl (Right rsp) (unsafeCoerce rm))) (cache c)
                } 

        Nothing -> do
          -- This case allows for pre-seeding the cache with cache.
          pure c
            { cache = Map.insert (typeRep p) (RequestMap p (Map.singleton pl (Right rsp))) (cache c)
            } 
        
data Policy = Cached | Fresh | Uncached

req 
  :: forall domain request msgs reqs payload response.
  ( Ord payload, WS.Request request, ToJSON payload, FromJSON response
  , (request ∈ reqs) ~ 'True
  , Req request ~ (Int, payload)
  , Rsp request ~ response
  , Typeable domain
  )
  => Policy -> WS.API msgs reqs -> Proxy request -> payload -> IO response
req Uncached api rq pl = do
  mv <- newEmptyMVar
  request' @domain True True api rq pl pure (putMVar mv)
  takeMVar mv
req Fresh api rq pl = do
  mv <- newEmptyMVar
  request' @domain True False api rq pl pure (putMVar mv)
  takeMVar mv
req _ api rq pl = do
  mv <- newEmptyMVar
  request' @domain False False api rq pl pure (putMVar mv)
  takeMVar mv

msg  
  :: forall domain message msgs reqs payload.
  ( WS.Message message, ToJSON payload
  , (message ∈ msgs) ~ 'True
  , M message ~ payload
  , Typeable domain
  )
  => WS.API msgs reqs -> Proxy message -> payload -> IO ()
msg api endpoint pl = publish go
  where
    go :: Cache domain -> IO (Cache domain)
    go c = do
      message api (socket c) endpoint pl
      pure c

request
  :: forall domain request msgs reqs payload response.
  ( Ord payload, WS.Request request, ToJSON payload, FromJSON response
  , (request ∈ reqs) ~ 'True
  , Req request ~ (Int, payload)
  , Rsp request ~ response
  , Typeable response
  , Typeable domain
  ) 
  => Policy -> WS.API msgs reqs -> Proxy request -> payload -> (Async response => View) -> View
request policy api rq pl v = async (req @domain policy api rq pl) v

flush 
  :: forall domain request msgs reqs payload.
  ( Ord payload, WS.Request request, ToJSON payload, Req request ~ (Int,payload), Typeable domain )
  => WS.API msgs reqs -> Proxy request -> payload -> IO ()
flush _ p pl = flush' @domain p pl

flushMany 
  :: forall domain request msgs reqs payload.
  ( WS.Request request, ToJSON payload, Typeable domain )
  => WS.API msgs reqs -> Proxy request -> IO ()
flushMany _ p = flushMany' @domain p

flushAll :: forall domain. Typeable domain => IO ()
flushAll = flushAll' @domain

