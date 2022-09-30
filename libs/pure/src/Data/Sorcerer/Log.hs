{-# language CPP, RankNTypes, BangPatterns, BlockArguments, TypeApplications, ScopedTypeVariables, AllowAmbiguousTypes #-}
module Data.Sorcerer.Log 
  ( Log
  , resume
  , record
  , close
  ) where

import Data.Sorcerer.Aggregate
import Data.Sorcerer.JSON
import Data.Sorcerer.Streamable

import Data.JSON (Value)

import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy.Char8 as BSLC

import qualified System.Posix.Files              as P
import qualified System.Posix.IO                 as P
#ifndef __GHCJS__
import qualified System.Posix.IO.ByteString.Lazy as P (fdWritev)
#endif
import           System.Posix.Types              as P (Fd, FileOffset, ByteCount, COff)

import Control.Monad
import Foreign.Ptr
import Foreign.ForeignPtr
import Foreign.Marshal.Alloc
import Foreign.Storable
import System.IO
import Text.Read

newtype Log = Log P.Fd

resume :: forall ev. Streamable ev => FilePath -> IO (Log,TransactionId)
resume fp = do
  fd <- P.openFd fp P.ReadWrite (Just $ P.unionFileModes P.ownerReadMode P.ownerWriteMode) P.defaultFileFlags
  !tid <- resumeLog @ev fd fp 
  pure (Log fd,tid)

resumeLog :: forall ev. Streamable ev => P.Fd -> FilePath -> IO TransactionId
resumeLog fd fp = do
  off <- P.fdSeek fd SeekFromEnd 0
  if off == 0 then newStream else resume
  where
    -- Write statusline and return 0
    newStream :: IO TransactionId
    newStream = do
      commit 0
      pure 0

    -- Commit a transaction id to the statusline
    -- Has to write twice in case the transaction id is rolled back
    commit :: Int -> IO ()
    commit c = do
      P.fdSeek fd AbsoluteSeek 0
      P.fdWrite fd (replicate 13 ' ' ++ "\n")
      P.fdSeek fd AbsoluteSeek 0
      P.fdWrite fd ('1':show c)
      P.fdSeek fd SeekFromEnd 0
      pure ()

    resume :: IO TransactionId
    resume = do
      _ <- P.fdSeek fd AbsoluteSeek 0
      (ln,_) <- P.fdRead fd 13
      i <- 
        case ln of
          '1':tid | Just i <- readMaybe tid -> pure i
          _  :tid                           -> recover
      P.fdSeek fd SeekFromEnd 0
      pure i

    -- Recoverable failures:
    --   1. Power failed before transaction write completed; delete unfinished transaction and attempt to roll back to the previous transaction.
    --   2. Power failed during transaction commit; verify the latest transaction and attempt to commit it.
    recover :: IO TransactionId
    recover = do
      (o,ln) <- fdGetLnReverse (-1)
      case decode_ (BSLC.pack ln) :: Maybe (Int,Value) of
        Just (i,_) -> do
          commit i
          pure i
        Nothing -> do
          (_,ln) <- fdGetLnReverse o
          case decode_ (BSLC.pack ln) :: Maybe (Int,Value) of
            Just (i,_) -> do
              off <- P.fdSeek fd SeekFromEnd (o + 2) -- must not truncate the newline
              P.setFdSize fd off
              commit i
              pure i
            Nothing -> error $ Prelude.unlines
              [ "Data.Sorcerer.Log.resumeLog.recover:" 
              , ""
              ,     "\tUnrecoverable event stream: "
              , ""
              ,     "\t\t" ++ fp
              , ""
              ,     "\tProblem:"
              ,         "\t\tThe latest commit was partial and the previous commit was not valid JSON."
              , ""
              ,     "\tSolution:"
              ,         "\t\tUnknown. This should not be possible using aeson for encoding." 
              ,         "\t\tReview the transaction stream manually to determine a solution." 
              ]
      where
        fdGetLnReverse :: FileOffset -> IO (FileOffset,String)
        fdGetLnReverse i = alloca $ \p -> go p [] i
          where
            go p = go'
              where
                go' s i = do
                  P.fdSeek fd SeekFromEnd i
                  P.fdReadBuf fd p 1
                  w <- peek p
                  let c = toEnum (fromIntegral w)
                  if c == '\n'
                    then pure (i - 1,s)
                    else go' (c:s) (i-1)

record :: Log -> BSB.Builder -> TransactionId -> IO ()
record (Log fd) bsb tid = void do

  -- seek to beginning and tag event log as uncommited
  P.fdSeek fd AbsoluteSeek 0
#ifndef __GHCJS__
  P.fdWritev fd (BSB.toLazyByteString (BSB.intDec 0 <> BSB.intDec tid))
#endif

  -- seek to end and write events
  P.fdSeek fd SeekFromEnd 0
#ifndef __GHCJS__
  P.fdWritev fd (BSB.toLazyByteString bsb)
#endif

  -- seek to beginning and tag event log as commited 
  P.fdSeek fd AbsoluteSeek 0
#ifndef __GHCJS__
  P.fdWritev fd (BSB.toLazyByteString (BSB.intDec 1))
#endif

close :: Log -> IO ()
close (Log fd) = P.closeFd fd