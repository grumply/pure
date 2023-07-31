{-# language RankNTypes, FlexibleContexts, OverloadedStrings, CPP, DuplicateRecordFields, JavaScriptFFI, NamedFieldPuns #-}
module Web.DataTransfer where

import Data.Coerce
import Data.Default
import Data.DOM
import Data.Maybe
import Data.Txt as Txt
import qualified Web.File as File
import System.IO.Unsafe

#ifdef __GHCJS__
import GHCJS.Marshal.Internal
#endif

data DataTransfer = DataTransfer
  { eventObject :: JSV
  , dropEffect :: Txt
  , effectAllowed :: Txt
  , types :: [Txt]
  , files :: [File.File]
  , dat :: Txt -> Txt
  }

toDataTransfer :: JSV -> DataTransfer
toDataTransfer o = let err = error "Invalid DataTransfer." in
  DataTransfer
    { eventObject = o
    , dropEffect = fromMaybe err (o .# "dropEffect")
    , effectAllowed = fromMaybe err (o .# "effectAllowed")
#ifdef __GHCJS__
    , types = maybe err (unsafePerformIO . fromJSValUncheckedListOf) (o .# "types")
    , files = fmap File.toFile (maybe err (unsafePerformIO . fromJSValUncheckedListOf) (o .# "files"))
    , dat = get_data_js o
#else
    , types = []
    , files = []
    , dat = const def
#endif
    }

#ifdef __GHCJS__
foreign import javascript unsafe
  "$1.getAsFile()" get_as_file_js :: JSV -> IO JSV

foreign import javascript unsafe
  "$1.getData($2)" get_data_js :: JSV -> Txt -> Txt
#endif
