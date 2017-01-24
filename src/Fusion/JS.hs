module Fusion.JS where

import Ef.Base

import Data.JSText
import qualified GHCJS.Types as T

import Data.Maybe

import GHC.Prim
import Unsafe.Coerce

foreign import javascript unsafe
  "$r = JSON.parse($1);" jsonParse :: JSText -> IO T.JSVal

foreign import javascript unsafe
  "$r = JSON.stringify($1);" jsonEncode :: T.JSVal -> IO JSText

foreign import javascript unsafe
  "console.log($1);console.log($2);"
  printAny_js :: JSText -> T.JSVal -> IO ()

debug :: (MonadIO c) => JSText -> a -> c ()
debug label a = liftIO $ printAny_js label $ unsafeCoerce (unsafeCoerce a :: GHC.Prim.Any)

