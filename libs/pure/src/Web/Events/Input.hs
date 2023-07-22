{-# language PatternSynonyms, RankNTypes, FlexibleContexts, OverloadedStrings, ViewPatterns, CPP, DuplicateRecordFields, TypeApplications, TupleSections, BlockArguments #-}
module Web.Events.Input where

import Control.Concurrent hiding (yield)
import Control.Producer
import Control.Monad
import Data.File
import Data.Foldable
import Data.Coerce
import Data.Default
import Data.DOM
import Data.Effect ((#))
import Data.Events (pattern OnWith)
import Data.Exists
import Data.JSON
import Data.Maybe
import Data.Time
import Data.Traversable
import Data.Txt
import Data.View
import System.IO.Unsafe
import Web.DataTransfer
import Web.Range

#ifdef __GHCJS__
import GHCJS.Marshal.Internal
#endif

data InputEvent = InputEvent
  { eventObject :: JSV
  , value :: Txt
  , inputType :: Txt
  , files :: [(Txt,ByteTxt)]
  }

toInputEvent :: Evt -> InputEvent
toInputEvent (evtObj -> o) = let err = error "Invalid InputEvent" in
  InputEvent
    { eventObject = o 
    , value = fromMaybe err (o .# "target" >>= (.# "value"))
    , inputType = fromMaybe err (o .# "inputType")
    , files = maybe err (unsafePerformIO . getFiles . (coerce :: JSV -> Node)) (o .# "target")
    }

newtype BeforeInput = BeforeInput InputEvent

beforeInputWith :: Options -> (BeforeInput -> IO ()) -> View -> View
beforeInputWith opts f = OnWith opts "beforeinput" (f . BeforeInput . toInputEvent)

beforeInput :: View -> (Producer BeforeInput => View)
beforeInput = beforeInputWith def yield

beforeInputs :: (Exists BeforeInput => IO ()) -> View -> View
beforeInputs f = events @BeforeInput f beforeInput

newtype Input = In InputEvent

inputWith :: Options -> (Input -> IO ()) -> View -> View
inputWith opts f = OnWith opts "input" (f . In . toInputEvent)

input :: View -> (Producer Input => View)
input = inputWith def yield

inputs :: (Exists Input => IO ()) -> View -> View
inputs f = events @Input f input

#ifdef __GHCJS__
foreign import javascript unsafe
  "var file = $1.files[$2]; var reader = new FileReader(); reader.readAsBinaryString(file); $r = reader;" get_file_reader_js :: Node -> Int -> IO JSV

foreign import javascript unsafe
  "$r = $1.files[$2].name" get_file_name_js :: Node -> Int -> IO Txt

foreign import javascript unsafe
  "$r = $1.result" get_result_js :: JSV -> IO Txt

foreign import javascript unsafe
  "$r = $1.files.length" get_file_count_js :: Node -> IO Int
#endif

getFiles :: Node -> IO [(Txt,ByteTxt)]
getFiles node = do
#ifdef __GHCJS__
  files <- get_file_count_js node
  if files > 0 then 
    unsafeInterleaveIO do
      catMaybes <$> for [0..files - 1] \n -> do
        rdr <- get_file_reader_js node n
        path <- get_file_name_js node n
        mv <- newEmptyMVar
        onRaw rdr "load" def $ \stop _ -> do
          result <- rdr ..# "result"
          putMVar mv result
          stop
        fmap (\x -> (path,unsafeTxtToByteTxt x)) <$> takeMVar mv
  else
    pure []
#else
  pure []
#endif


