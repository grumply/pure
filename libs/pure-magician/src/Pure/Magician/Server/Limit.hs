module Pure.Magician.Server.Limit (setPeriod,allowed,Limit(..),limiting,LimitMany) where

import Data.Bloom.Scalable
import Pure.Conjurer.Permissions
import Pure.Convoker
import Data.Time
import Data.Txt

import Control.Concurrent
import Control.Monad
import Data.IORef
import Data.Typeable
import System.IO.Unsafe

-- maximum delay between similar actions
{-# NOINLINE period #-}
period :: IORef Time
period = unsafePerformIO do
  newIORef (Seconds 15 0)

setPeriod :: Time -> IO ()
setPeriod = writeIORef period

{-# NOINLINE limiter #-}
limiter :: IORef Bloom
limiter = unsafePerformIO do
  let new = bloom 0.001
  b_ <- new >>= newIORef
  forkIO do
    forever do
      t <- readIORef period
      delay t

      b <- new
      atomicModifyIORef' b_ $ \_ -> (b,())

  pure b_

allowed :: Time -> Txt -> IO Bool
allowed t@(Milliseconds ms _) action = do
  Milliseconds x _ <- readIORef period
  let max = x / ms
  b <- readIORef limiter
  full <- test b (action <> "_" <> toTxt max)
  if full then
    -- fail fast
    pure False
  else
    go 1 max b
  where
    -- a binary search for the first empty slot
    go lo hi b
      | lo == hi = update b (action <> "_" <> toTxt lo)
      | otherwise = do
        let mid = (lo + hi) / 2
        test b (action <> "_" <> toTxt mid) >>= \case
          False -> go lo mid b
          True  -> go (mid + 1) hi b

class Limit a where
  createLimitPrefix :: Txt
  createLimitPrefix = "create"

  createLimit :: Maybe Time
  createLimit = Nothing

  readLimitPrefix :: Txt
  readLimitPrefix = "read"

  readLimit :: Maybe Time
  readLimit = Nothing

  updateLimitPrefix :: Txt
  updateLimitPrefix = "update"

  updateLimit :: Maybe Time
  updateLimit = Nothing

  amendLimitPrefix :: Txt
  amendLimitPrefix = "amend"

  amendLimit :: Maybe Time
  amendLimit = Nothing

  interactLimitPrefix :: Txt
  interactLimitPrefix = "interact"

  interactLimit :: Maybe Time
  interactLimit = Nothing

  deleteLimitPrefix :: Txt
  deleteLimitPrefix = "delete"

  deleteLimit :: Maybe Time
  deleteLimit = Nothing

  listLimitPrefix :: Txt
  listLimitPrefix = "list"

  listLimit :: Maybe Time
  listLimit = Nothing

  enumLimitPrefix :: Txt
  enumLimitPrefix = "enum"

  enumLimit :: Maybe Time
  enumLimit = Nothing

instance {-# INCOHERENT #-} Limit a

limiting :: forall a. (Typeable a, Limit a) => Txt -> Permissions a -> Permissions a
limiting prefix perms =
  let
    ty = toTxt (show (tyConName (typeRepTyCon (typeRep (Proxy :: Proxy a)))))

    limiting act x = \case
      Nothing -> x
      Just t -> do
        b <- allowed t (prefix <> "_" <> act <> "_" <> ty)
        if b then x else pure False
  in
    Permissions
      { canCreate   = \ctx nm res -> limiting (createLimitPrefix   @a) (canCreate   perms ctx nm res) (createLimit   @a)
      , canUpdate   = \ctx nm     -> limiting (updateLimitPrefix   @a) (canUpdate   perms ctx nm    ) (updateLimit   @a)
      , canAmend    = \ctx nm amd -> limiting (amendLimitPrefix    @a) (canAmend    perms ctx nm amd) (amendLimit    @a)
      , canInteract = \ctx nm act -> limiting (interactLimitPrefix @a) (canInteract perms ctx nm act) (interactLimit @a)
      , canDelete   = \ctx nm     -> limiting (deleteLimitPrefix   @a) (canDelete   perms ctx nm    ) (deleteLimit   @a)
      , canRead     = \ctx nm     -> limiting (readLimitPrefix     @a) (canRead     perms ctx nm    ) (readLimit     @a)
      , canList     = \ctx        -> limiting (listLimitPrefix     @a) (canList     perms ctx       ) (listLimit     @a)
      , canEnum     =                limiting (enumLimitPrefix     @a) (canEnum     perms           ) (enumLimit     @a)
      }

class LimitMany (a :: *) (xs :: [*])
instance (Limit x, Limit (Discussion a x), Limit (Mods a x), Limit (Meta a x), Limit (UserVotes a x), Limit (Comment a x), LimitMany a xs) => LimitMany (a :: *) (x : xs)
instance LimitMany a '[]