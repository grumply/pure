{-# language PatternSynonyms, ExplicitNamespaces, RankNTypes, LambdaCase, TypeOperators, DataKinds, TypeApplications, TypeFamilies, PartialTypeSignatures, FlexibleContexts, ExistentialQuantification, OverloadedStrings, ScopedTypeVariables, AllowAmbiguousTypes, TypeApplications #-}
{-# options_ghc -fno-warn-partial-type-signatures #-}
module Data.Websocket.Cache (Policy(..),Cache(..),req,with,flush,flushMany,flushAll) where

import Control.Controller hiding (Left,Right,start)
import Data.JSON (ToJSON,FromJSON)
import Data.Websocket as WS (request,API,type (∈),Websocket,Request(..))

import Data.Map as Map (Map,delete,empty,insert,lookup,singleton)

import Control.Concurrent (newEmptyMVar,putMVar,takeMVar)
import Data.Foldable (for_)
import Data.Proxy (Proxy)
import Data.Typeable (Typeable,typeRep,TypeRep)
import Unsafe.Coerce (unsafeCoerce)

data RequestMap
  = forall rq pl. (WS.Request rq, WS.Req rq ~ (Int,pl), Ord pl) 
  => RequestMap (Proxy rq) (Map.Map pl (Either [WS.Rsp rq -> IO ()] (WS.Rsp rq)))

data Cache _role = Cache Websocket
instance Typeable _role => Component (Cache _role) where
  data Model (Cache _role) = Model
    { responses :: Map.Map TypeRep RequestMap
    }
  
  model = Model Map.empty

  data Msg (Cache _role)
    = forall rq rqs msgs pl. (WS.Request rq, WS.Req rq ~ (Int,pl), Ord pl, ToJSON pl, FromJSON (Rsp rq), rq ∈ rqs ~ True)
    => Request Bool Bool (WS.API msgs rqs) (Proxy rq) pl (WS.Rsp rq -> IO (WS.Rsp rq)) (WS.Rsp rq -> IO ())

    | forall rq pl rsp. (WS.Request rq, WS.Req rq ~ (Int,pl), WS.Rsp rq ~ rsp, Ord pl)
    => Satisfy (Proxy rq) pl rsp
    
    | forall rq pl. (WS.Request rq, WS.Req rq ~ (Int,pl), Ord pl)
    => Flush (Proxy rq) pl
    
    | forall rq. (WS.Request rq) 
    => FlushMany (Proxy rq)

    | FlushAll

    | Startup

  startup = [Startup]

  upon = \case
    Startup -> \_ mdl -> subscribe @(Msg (Cache _role)) >> pure mdl
    Request f bp api p pl process cb -> request' f bp api p pl process cb
    Satisfy p pl rsp -> satisfy p pl rsp
    Flush pr pl -> flush' pr pl
    FlushMany pr -> flushMany' pr
    FlushAll -> flushAll'
    
  view _ _ = SimpleHTML "pure-websocket-cache"

request' :: forall _role rq rqs msgs pl
          . (WS.Request rq, WS.Req rq ~ (Int,pl), Ord pl, ToJSON pl, FromJSON (Rsp rq), rq ∈ rqs ~ True)
         => Bool -> Bool -> WS.API msgs rqs -> Proxy rq -> pl -> (WS.Rsp rq -> IO (WS.Rsp rq)) -> (WS.Rsp rq -> IO ()) 
         -> Update (Cache _role)
request' force bypass api p pl process cb (Cache ws) mdl 
  | force = do
    WS.request api ws p pl $ \rsp -> do
      rsp' <- process rsp
      if bypass
        then cb rsp'
        else command (Satisfy @_role p pl rsp')
    pure mdl 
      { responses = 
        if bypass 
          then responses mdl
          else Map.insert (typeRep p) (RequestMap p (Map.singleton pl ((Left [cb])))) (responses mdl)
      }

  | otherwise =
    case Map.lookup (typeRep p) (responses mdl) of
      Nothing -> do
        WS.request api ws p pl $ \rsp -> do
          rsp' <- process rsp
          if bypass
            then cb rsp'
            else command (Satisfy @_role p pl rsp')
        pure mdl 
          { responses = 
            if bypass
              then responses mdl
              else Map.insert (typeRep p) (RequestMap p (Map.singleton pl ((Left [cb])))) (responses mdl)
          }
          
      Just (RequestMap _ rm) ->
        -- that's a lot of coercions
        case Map.lookup pl (unsafeCoerce rm) of
          Just (Left cbs) -> do
            let 
              cbs' = cbs ++ [unsafeCoerce cb]
              rm' = RequestMap p (unsafeCoerce (Map.insert pl (Left cbs') (unsafeCoerce rm)))
            pure mdl
              { responses = Map.insert (typeRep p) rm' (responses mdl)
              }

          Just (Right rsp) -> do
            cb rsp
            pure mdl

          Nothing -> do
            WS.request api ws p pl (command . Satisfy @_role p pl)
            let 
              cbs = [cb]
              rm' = RequestMap p (unsafeCoerce (Map.insert pl (Left cbs) (unsafeCoerce rm)))
            pure mdl
              { responses = Map.insert (typeRep p) rm' (responses mdl)
              }

flush' :: forall _role rq pl. 
         (WS.Request rq, WS.Req rq ~ (Int,pl), Ord pl, _) 
      => Proxy rq -> pl -> Update (Cache _role)
flush' p pl _ mdl =
  case Map.lookup (typeRep p) (responses mdl) of
    Just (RequestMap p' rm) ->
      let 
        rm' = Map.delete pl (unsafeCoerce rm)
        responses' = Map.insert (typeRep p) (RequestMap p' (unsafeCoerce rm')) (responses mdl)
      in 
        pure mdl { responses = responses' }
    _ ->
      pure mdl

flushMany' :: forall _role rq pl. (WS.Request rq, _) => Proxy rq -> Update (Cache _role)
flushMany' p _ mdl = pure mdl { responses = Map.delete (typeRep p) (responses mdl) }

flushAll' :: Update (Cache _role)
flushAll' _ mdl = pure mdl { responses = Map.empty }

satisfy :: forall _role rq pl rsp
         . (WS.Request rq, WS.Req rq ~ (Int,pl), WS.Rsp rq ~ rsp, Ord pl)
        => Proxy rq -> pl -> rsp 
        -> Update (Cache _role)
satisfy p pl rsp _ mdl = do
  case Map.lookup (typeRep p) (responses mdl) of
    Just (RequestMap _ rm) ->
      case Map.lookup (unsafeCoerce pl) rm of
        Just (Left cbs) -> do
          for_ @[] (unsafeCoerce cbs) ($ rsp)
          pure mdl
            { responses = Map.insert (typeRep p) (RequestMap p (Map.insert pl (Right rsp) (unsafeCoerce rm))) (responses mdl)
            } 

        Just (Right _) -> do
          pure mdl
          
        Nothing -> do
          -- huh?
          pure mdl
            { responses = Map.insert (typeRep p) (RequestMap p (Map.insert pl (Right rsp) (unsafeCoerce rm))) (responses mdl)
            } 

    Nothing -> do
      -- This case allows for pre-seeding the cache with responses.
      pure mdl
        { responses = Map.insert (typeRep p) (RequestMap p (Map.singleton pl (Right rsp))) (responses mdl)
        } 
      
data Policy = Cached | Fresh | Uncached

req :: forall _role request msgs reqs payload response.
       (Ord payload, WS.Request request, ToJSON payload, FromJSON response, _) 
    => Policy -> WS.API msgs reqs -> Proxy request -> payload -> IO response
req Uncached api rq pl = do
  mv <- newEmptyMVar
  publish (Request @_role True True api rq pl pure (putMVar mv))
  takeMVar mv
req Fresh api rq pl = do
  mv <- newEmptyMVar
  publish (Request @_role True False api rq pl pure (putMVar mv))
  takeMVar mv
req _ api rq pl = do
  mv <- newEmptyMVar
  publish (Request @_role False False api rq pl pure (putMVar mv))
  takeMVar mv

flush :: forall _role request msgs reqs payload.
         (Ord payload, WS.Request request, ToJSON payload, _)
      => WS.API msgs reqs -> Proxy request -> payload -> IO ()
flush _ p pl = publish (Flush @_role p pl)

flushMany :: forall _role request msgs reqs payload.
             (WS.Request request, ToJSON payload, _)
          => WS.API msgs reqs -> Proxy request -> IO ()
flushMany _ p = publish (FlushMany @_role p)

flushAll :: forall _role. _ => IO ()
flushAll = publish (FlushAll @_role)

{-
-- not the best place for this
with :: forall _role request msgs reqs payload response. (Ord payload, WS.Rsp request ~ response, _ ) 
     => Policy
     -> WS.API msgs reqs
     -> Proxy request 
     -> payload 
     -> (payload -> Maybe response -> View) 
     -> View
with policy api rq pl f = producingKeyed pl (req @_role policy api rq) f
-}