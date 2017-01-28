{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language MagicHash #-}
module Atomic.Component (module Atomic.Component) where

import Ef.Base hiding (Client,Server)

import Atomic.Key
import Atomic.Observable
import Atomic.Revent
import Atomic.Vault
import Atomic.With

import Control.Concurrent
import Data.IORef
import GHC.Prim

import Data.HashMap.Strict as Map hiding ((!))

import Unsafe.Coerce

type IsComponent' ts ms =
  ( ComponentBase <: ms
  , ComponentBase <. ts
  , Delta (Modules ts) (Messages ms)
  )
type IsComponent ms = IsComponent' ms ms

type ComponentBase =
  '[Revent
   ,State () Vault
   ,State () Shutdown
   ]

type ComponentKey' ms = Key (Code ms IO `As` IO)
type ComponentKey ms = ComponentKey' (Appended ms ComponentBase)
type ComponentBuilder' ts = Modules ComponentBase (Action ts IO) -> IO (Modules ts (Action ts IO))
type ComponentBuilder ts = ComponentBuilder' (Appended ts ComponentBase)
type ComponentPrimer' ms = Code ms IO ()
type ComponentPrimer ms = Code (Appended ms ComponentBase) IO ()
type Component ms = Component' (Appended ms ComponentBase) (Appended ms ComponentBase)

data Component' ts ms
  = Component
    { key        :: !(ComponentKey' ms)
    , build      :: !(ComponentBuilder' ts)
    , prime      :: !(ComponentPrimer' ms)
    }

instance Eq (Component' ts ms) where
  (==) (Component i _ _) (Component i' _ _) =
    let Key k1 = i
        Key k2 = i'
    in case reallyUnsafePtrEquality# i i' of
         1# -> True
         _  -> k1 == k2

instance (IsComponent' ts ms, MonadIO c, '[Revent,State () Vault] <: ms')
  => With (Component' ts ms) (Code ms IO) (Code ms' c)
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
             , IsComponent' ts ms
             )
          => Vault
          -> Signaled
          -> Component' ts ms
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

