{-# LANGUAGE CPP, DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}
module Data.File (ByteTxt(),unsafeByteTxtToTxt,unsafeTxtToByteTxt,writeByteTxt,readByteTxt,appendByteTxt) where

import Data.Default
import Data.Txt as Txt
import Data.JSON
import Data.DOM

import Control.Concurrent
import Data.Hashable
import qualified Data.Text.IO as TIO

import GHC.Generics
import System.IO

newtype ByteTxt = ByteTxt Txt deriving (Generic,ToJSON,FromJSON,Ord,Eq)

instance Hashable ByteTxt where hashWithSalt s (ByteTxt bt) = hashWithSalt s bt

unsafeByteTxtToTxt :: ByteTxt -> Txt
unsafeByteTxtToTxt (ByteTxt t) = t

unsafeTxtToByteTxt :: Txt -> ByteTxt
unsafeTxtToByteTxt = ByteTxt

readByteTxt :: FilePath -> IO ByteTxt
readByteTxt fp = 
#ifdef __GHCJS__
  pure (ByteTxt "")
#else
  ByteTxt <$> withBinaryFile fp ReadMode TIO.hGetContents
#endif

writeByteTxt :: FilePath -> ByteTxt -> IO ()
writeByteTxt fp (ByteTxt bt) = 
#ifdef __GHCJS__
  pure ()
#else
  withBinaryFile fp WriteMode (`TIO.hPutStr` bt)
#endif

appendByteTxt :: FilePath -> ByteTxt -> IO ()
appendByteTxt fp (ByteTxt bt) = 
#ifdef __GHCJS__
  pure ()
#else
  withBinaryFile fp AppendMode (`TIO.hPutStr` bt)
#endif
