{-# language CPP, PatternSynonyms, RankNTypes, FlexibleContexts, OverloadedStrings, DuplicateRecordFields, TypeApplications #-}
module Web.Events.Resize (Resize(..),resize,resizes) where

import Control.Producer
import Control.Monad
import Data.Effect ((#))
import Data.Events (pattern OnWith)
import Data.Exists
import Data.Foldable
import Data.Function
import Data.IORef
import Data.Maybe
import System.IO.Unsafe
import Prelude hiding (Either(..))

import Data.DOM
import Data.View hiding (ask)

#ifdef __GHCJS__
import GHCJS.Marshal (FromJSVal(..))
#endif

data Resize = Resize
  { eventObject :: JSV
  , target :: JSV
  , borderBlockSize :: Double
  , contentBlockSize :: Double
  , devicePixelContentBlockSize :: Double
  , borderInlineSize :: Double
  , contentInlineSize :: Double
  , devicePixelContentInlineSize :: Double
  , width :: Double
  , height :: Double
  , x :: Double
  , y :: Double
  , top :: Double
  , right :: Double
  , bottom :: Double
  , left :: Double
  }

toResize :: JSV -> Resize
toResize o = let err = error "Invalid Resize Event." in
  fix $ \re -> do
    let r = fromMaybe err (o .# "contentRect")
        w = Web.Events.Resize.width
        h = Web.Events.Resize.height
    Resize
      { eventObject = o
      , target = fromMaybe err (o .# "target")
      , devicePixelContentBlockSize = fromMaybe (w re) (o .# "devicePixelContentBoxSize[0]" >>= (.# "blockSize"))
      , devicePixelContentInlineSize = fromMaybe (h re) (o .# "devicePixelContentBoxSize[0]" >>= (.# "inlineSize"))
      , borderBlockSize = fromMaybe (w re) (o .# "borderBoxSize[0]" >>= (.# "blockSize"))
      , borderInlineSize = fromMaybe (h re) (o .# "borderBoxSize[0]" >>= (.# "inlineSize"))
      , contentBlockSize = fromMaybe (w re) (o .# "contentBoxSize[0]" >>= (.# "blockSize"))
      , contentInlineSize = fromMaybe (h re) (o .# "contentSize[0]" >>= (.# "inlineSize"))
      , width = fromMaybe err (r .# "width")
      , height = fromMaybe err (r .# "height")
      , x = fromMaybe err (r .# "x")
      , y = fromMaybe err (r .# "y")
      , top = fromMaybe err (r .# "top")
      , right = fromMaybe err (r .# "right")
      , bottom = fromMaybe err (r .# "bottom")
      , left = fromMaybe err (r .# "left")
      }

resize :: View -> (Producer Resize => View)
resize v = unsafePerformIO (writeIORef ref yield) `seq` OnMounted go v
  where
    {-# NOINLINE ref #-}
    ref :: IORef (Resize -> IO ())
    ref = unsafePerformIO (newIORef undefined)

    {-# NOINLINE go #-}
    go :: Producer Resize => Node -> IO (IO ())
    go n = do
#ifdef __GHCJS__
      cb <- asyncCallback1 (\re -> let es = fmap toResize (fromMaybe [] (unsafePerformIO (fromJSValListOf re))) in readIORef ref >>= \f -> for_ es f)
      ro <- create_js cb
      connect_js ro n
      pure (unobserve_js ro n)
#else
      pure (pure ())
#endif

#ifdef __GHCJS__
newtype ResizeObserver = ResizeObserver JSV
foreign import javascript unsafe
  "new ResizeObserver($1)" create_js :: Callback (JSV -> IO ()) -> IO ResizeObserver
foreign import javascript unsafe
  "$1.observe($2)" connect_js :: ResizeObserver -> Node -> IO ()
foreign import javascript unsafe
  "$1.unobserver($2)" unobserve_js :: ResizeObserver -> Node -> IO ()
#endif

resizes :: (Exists Resize => IO ()) -> View -> View
resizes f = events @Resize f resize
