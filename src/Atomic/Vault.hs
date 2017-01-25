module Atomic.Vault where

import Ef.Base

import Atomic.Key

import Data.IORef
import Data.HashMap.Strict as Map

import Control.Concurrent
import Data.String
import Data.Monoid

import Unsafe.Coerce

data Vault = forall a. Vault (MVar (Map.HashMap Int a))

modifyVault :: MonadIO c => Vault -> (Map.HashMap Int a -> IO (Map.HashMap Int a,b)) -> c b
modifyVault (Vault v_) f = liftIO $ modifyMVar v_ (\v -> do { (v',b) <- f (unsafeCoerce v); return (unsafeCoerce v',b) })

vaultLookup :: MonadIO c => Vault -> Key v -> c (Maybe v)
vaultLookup (Vault v_) (Key (_,k)) = do
  v <- liftIO $ readMVar v_
  return $ Map.lookup k (unsafeCoerce v)

vaultAdd :: MonadIO c => Vault -> Key v -> v -> c ()
vaultAdd v_ (Key (_,k)) val =
  modifyVault v_ $ \v ->
    return (unsafeCoerce $! Map.insert k val (unsafeCoerce v),())

vaultDelete :: MonadIO c => Vault -> Key v -> c ()
vaultDelete v_ (Key (_,k)) =
  modifyVault v_ $ \v ->
    return (unsafeCoerce $ Map.delete k (unsafeCoerce v),())

createVault :: MonadIO c => c Vault
createVault = liftIO $ Vault <$> newMVar Map.empty
