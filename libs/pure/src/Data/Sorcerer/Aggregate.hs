{-# language CPP, DerivingStrategies, DeriveAnyClass, DeriveGeneric, OverloadedStrings #-}
module Data.Sorcerer.Aggregate where

import Data.Sorcerer.JSON

import Data.JSON (ToJSON,FromJSON)

import qualified Data.ByteString as BS
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy.Char8 as BSLC

import qualified System.Posix.Files as P
import qualified System.Posix.IO as P
#ifndef __GHCJS__
import qualified System.Posix.IO.ByteString.Lazy as P (fdWrites)
#endif
import System.Posix.Types as P (Fd, FileOffset, ByteCount, COff)

import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (plusPtr)
import GHC.Generics (Generic)
import System.Directory (doesFileExist)

type TransactionId = Int

data Aggregate ag = Aggregate
  { current   :: TransactionId
  , aggregate :: Maybe ag
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

{-# INLINE writeAggregate #-}
writeAggregate :: ToJSON ag => FilePath -> TransactionId -> Maybe ag -> IO ()
writeAggregate fp tid mag = do
  fd <- P.openFd fp P.WriteOnly (Just $ P.unionFileModes P.ownerReadMode P.ownerWriteMode) P.defaultFileFlags

  -- Just in case a write previously failed.
  P.setFdSize fd 0

  P.setFdOption fd P.SynchronousWrites True
  let 
    stid = encode_ tid
    tidl = succ (round (logBase 10 (fromIntegral tid)))
    commit = BSB.lazyByteString stid <> BSB.lazyByteString (BSLC.replicate (12 - tidl) ' ')
    bsb = commit <> "\n" <> BSB.lazyByteString (encode_ mag)

#ifndef __GHCJS__
  P.fdWrites fd (BSB.toLazyByteString bsb)
#endif

  P.closeFd fd

{-# INLINE readAggregate #-}
readAggregate :: FromJSON ag => FilePath -> IO (Maybe (Aggregate ag))
readAggregate fp = do
  exists <- doesFileExist fp
  if exists then do
    cnts <- BSLC.readFile fp
    case BSLC.lines cnts of
      (ln:rest) -> pure $ Just (Aggregate (Prelude.read (BSLC.unpack ln)) (decode_ (head rest)))
      _ -> pure Nothing
  else 
    pure Nothing

{-# INLINE commitTransaction #-}
commitTransaction :: FilePath -> TransactionId -> IO ()
commitTransaction fp tid = do
  fd <- P.openFd fp P.WriteOnly (Just $ P.unionFileModes P.ownerReadMode P.ownerWriteMode) P.defaultFileFlags
  P.setFdOption fd P.SynchronousWrites True
#ifndef __GHCJS__
  P.fdWrites fd (BSB.toLazyByteString (BSB.intDec tid))
#endif
  P.closeFd fd
