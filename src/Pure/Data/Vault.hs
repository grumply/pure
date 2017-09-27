{-# LANGUAGE ExistentialQuantification #-}
module Pure.Data.Vault where

import Pure.Data.Key

import Data.HashMap.Strict as Map

import Control.Concurrent

import Unsafe.Coerce

data Vault = forall a. Vault (MVar (Map.HashMap Int a))

modifyVault :: Vault -> (Map.HashMap Int a -> IO (Map.HashMap Int a,b)) -> IO b
modifyVault (Vault v_) f = modifyMVar v_ (\v -> do { (v',b) <- f (unsafeCoerce v); return (unsafeCoerce v',b) })

vaultLookup :: Vault -> Key v -> IO (Maybe v)
vaultLookup (Vault v_) (Key (_,k)) = do
  v <- readMVar v_
  return $ Map.lookup k (unsafeCoerce v)

vaultAdd :: Vault -> Key v -> v -> IO ()
vaultAdd v_ (Key (_,k)) val =
  modifyVault v_ $ \v ->
    return (unsafeCoerce $! Map.insert k val (unsafeCoerce v),())

vaultDelete :: Vault -> Key v -> IO ()
vaultDelete v_ (Key (_,k)) =
  modifyVault v_ $ \v ->
    return (unsafeCoerce $ Map.delete k (unsafeCoerce v),())

createVault :: IO Vault
createVault = Vault <$> newMVar Map.empty
