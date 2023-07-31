{-# language PatternSynonyms, RankNTypes, FlexibleContexts, OverloadedStrings, ViewPatterns, CPP, DuplicateRecordFields #-}
module Web.File where

import Control.Concurrent
import Control.Monad
import Data.Coerce
import Data.Default
import Data.DOM
import Data.Events (pattern OnWith)
import Data.JSON hiding (String)
import Data.Maybe
import Data.Time
import Data.Txt
import Data.View
import System.IO.Unsafe

#ifdef __GHCJS__
import GHCJS.Marshal.Internal
#endif

data File = File
  { fileObject :: JSV
  , lastModified :: Time
  , name :: Txt
  , size :: Int
  , mime :: Txt
  , content :: Txt 
  }

toFile :: JSV -> File
toFile o = let err = error "Invalid File." in
  File
    { fileObject = o 
    , lastModified = maybe err (`Milliseconds` 0) (o .# "lastModified")
    , name = fromMaybe err (o .# "name")
    , size = fromMaybe err (o .# "size")
    , mime = fromMaybe err (o .# "type")
    , content =
#ifdef __GHCJS__
      unsafePerformIO $ do
        mv <- newEmptyMVar
        cb <- asyncCallback1 $ \jsv -> 
          putMVar mv $ do
            fromMaybe err $ do
              t <- jsv .# "target" 
              t .# "result"
        read_file_to_string_js o cb
        takeMVar mv
#else
      def
#endif
    }

#ifdef __GHCJS__
foreign import javascript unsafe
  "var fr = new FileReader(); fr.onload = $2;fr.readAsText($1);" 
    read_file_to_string_js :: JSV -> Callback (JSV -> IO ()) -> IO ()
#endif
