{-# language ConstraintKinds #-}
{-# language FlexibleInstances #-}
{-# language UndecidableInstances #-}
{-# language MagicHash #-}
module Nuclear.Service where

import Ef.Base as Export hiding (Client,Server)

import Nuclear.Key
import Nuclear.Revent
import Nuclear.Vault
import Nuclear.With

import Control.Concurrent
import Data.IORef
import GHC.Prim

import Data.HashMap.Strict as Map hiding ((!))

import System.IO.Unsafe
import Unsafe.Coerce

instance (IsService ts ms, MonadIO c) =>
          With
          (Service ts ms)
          (Code ms IO)
          c
  where
    using_ s = do
      -- faster lookup followed by modify if necessary which will check to
      -- make sure the service was not added between the lookup and the modify.
      mas <- vaultLookup serviceVault__ (key s)
      case mas of
        Nothing -> do
          let Key (_,i) = key s
          modifyVault serviceVault__ $ \v ->
            case Map.lookup i v of
              Nothing -> do
                rb <- newSignalBuffer
                sig :: Signal ms IO (Code ms IO ()) <- runner
                startService rb s
                let asService :: Code ms IO `As` IO
                    asService = constructAs rb sig
                    new_v = Map.insert i (unsafeCoerce asService) v
                return (new_v,liftIO . runAs asService)
              Just as ->
                return (v,liftIO . runAs as)
        Just as ->
          return (liftIO . runAs as)
    with_ s m = do
      run <- using_ s
      run m
    shutdown_ s = do
      with_ s $ do
        buf <- getReventBuffer
        Shutdown sdn <- get
        syndicate sdn ()
        liftIO $ do
          killBuffer buf
          myThreadId >>= killThread
      deleteService (key s)


type IsService ts ms =
  ( Service_ <: ms
  , Service_ <. ts
  , Delta (Modules ts) (Messages ms)
  )

type Service_ =
  '[Revent
   ,State () Vault
   ,State () Shutdown
   ]

type S ms = Service (Appended ms Service_) (Appended ms Service_)

data Service ts ms
  = Service
      { key      :: !(Key (Code ms IO `As` IO))
      , build    :: !(    Modules Service_ (Action ts IO)
                        -> IO (Modules ts (Action ts IO))
                      )
      , prime     :: !(Code ms IO ())
      }

instance Eq (Service ts ms) where
  (==) (Service i _ _) (Service i' _ _) =
    let Key k1 = i
        Key k2 = i'
    in case reallyUnsafePtrEquality# i i' of
         1# -> True
         _  -> k1 == k2

startService :: forall ms ts c.
                ( MonadIO c
                , IsService ts ms
                )
              => Signaled
              -> Service ts ms
              -> c ()
startService rb Service {..} = do
  sdn :: Network () <- network
  lv <- createVault
  built <- liftIO $ build $ revent rb
                         *:* state lv
                         *:* state (Shutdown sdn)
                         *:* Empty
  void $ liftIO $ forkIO $ do
    (obj,_) <- Object built ! prime
    driverPrintExceptions
      ("Service "
          ++ show key
          ++ " blocked in eventloop; likely caused by cyclic with calls. The standard solution is a 'delay'ed call to 'demand'."
      ) rb obj

{-# NOINLINE serviceVault__ #-}
serviceVault__ = Vault (unsafePerformIO (newMVar Map.empty))

lookupService :: (Monad c, MonadIO c)
              => Key phantom -> c (Maybe phantom)
lookupService = vaultLookup serviceVault__

deleteService :: (Monad c, MonadIO c)
              => Key phantom -> c ()
deleteService = vaultDelete serviceVault__
