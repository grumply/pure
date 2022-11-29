{-# LANGUAGE CPP, DeriveGeneric, DeriveAnyClass, OverloadedStrings #-}
module Data.File (ByteTxt(),unsafeByteTxtToTxt,unsafeTxtToByteTxt,getFileAtIndex,getFile,writeByteTxt,readByteTxt,appendByteTxt) where

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

#ifdef __GHCJS__
foreign import javascript unsafe
  "var file = $1.files[$2]; var reader = new FileReader(); reader.readAsBinaryString(file); $r = reader;" get_file_reader_js :: Node -> Int -> IO JSV

foreign import javascript unsafe
  "$r = $1.files[$2].name" get_file_name_js :: Node -> Int -> IO Txt

foreign import javascript unsafe
  "$r = $1.result" get_result_js :: JSV -> IO Txt
#endif

getFileAtIndex :: Node -> Int -> IO (Maybe (Txt,ByteTxt))
getFileAtIndex node n =
#ifdef __GHCJS__
  do
    rdr <- get_file_reader_js node n
    path <- get_file_name_js node n
    mv <- newEmptyMVar
    onRaw rdr "load" def $ \stop _ -> do
      result <- rdr ..# "result"
      putMVar mv result
      stop
    mresult <- takeMVar mv
    case mresult of
      Nothing -> pure Nothing
      Just x -> pure $ Just (path,ByteTxt x)
#else
  return Nothing
#endif

getFile :: Node -> IO (Maybe (Txt,ByteTxt))
getFile node = getFileAtIndex node 0

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