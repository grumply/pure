{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
module Nuclear.Component (module Nuclear.Component) where

import Ef.Base hiding (Client,Server)

import Nuclear.Key
import Nuclear.Observable
import Nuclear.Revent
import Nuclear.Vault
import Nuclear.With

import Control.Concurrent
import Data.IORef

import Data.HashMap.Strict as Map hiding ((!))

import Unsafe.Coerce

type IsComponent ts ms =
  ( C <: ms
  , C <. ts
  , Delta (Modules ts) (Messages ms)
  )

type C =
  '[Revent
   ,State () Vault
   ,State () Shutdown
   ]

data Component ts ms
  = Component
    { key        :: !(Key (Code ms IO `As` IO))
    , build      :: !(    Modules C (Action ts IO)
                       -> IO (Modules ts (Action ts IO))
                     )
    , prime      :: !(Code ms IO ())
    }

instance (IsComponent ts ms, MonadIO c, '[Revent,State () Vault] <: ms')
  => With (Component ts ms) (Code ms IO) (Code ms' c)
  where
    using_ c = do
      lv <- get
      mi_ <- vaultLookup lv (key c)
      case mi_ of
        Nothing -> do
          let Key (_,i) = key c
          modifyVault lv $ \v ->
            case Map.lookup i v of
              Just as ->
                return (v,liftIO . runAs as)
              Nothing -> do
                rb <- newSignalBuffer
                sig :: Signal ms IO (Code ms IO ()) <- runner
                startComponent lv rb c
                let asComponent :: Code ms IO `As` IO
                    asComponent = constructAs rb sig
                    new_v = Map.insert i (unsafeCoerce asComponent) v
                return (new_v,liftIO . runAs asComponent)
        Just as ->
          return (liftIO . runAs as)
    with_ c m = do
      run <- using_ c
      run m
    shutdown_ c = do
      lv <- get
      with_ c $ do
        buf <- getReventBuffer
        Shutdown sdn <- get
        syndicate sdn ()
        delay 0 $
          liftIO $ do
            killBuffer buf
            myThreadId >>= killThread
      vaultDelete lv (key c)

startComponent :: forall ms' c ms ts.
             ( MonadIO c
             , IsComponent ts ms
             )
          => Vault
          -> Signaled
          -> Component ts ms
          -> c ()
startComponent lv rb Component {..} = do
  sdn <- network
  udn :: Network m <- network
  built <- liftIO $ build (revent rb
                          *:* state lv
                          *:* state (Shutdown sdn)
                          *:* Empty
                          )

  void $ liftIO $ forkIO $ void $ do
    (obj,_) <- Object built ! prime
    driver rb obj

