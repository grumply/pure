{-# language CPP, OverloadedStrings #-}
module Data.Scroll where

import Data.Txt

import Control.Monad (when)

scrollTop :: IO ()
scrollTop =
#ifdef __GHCJS__
  scroll_top_js
#else
  pure ()
#endif

data ScrollRestoration = ManualScrollRestoration | AutoScrollRestoration

setScrollRestoration :: ScrollRestoration -> IO ()
setScrollRestoration sr =
#ifdef __GHCJS__
  set_scroll_restoration_js $
    case sr of
      ManualScrollRestoration -> "manual"
      AutoScrollRestoration   -> "auto"
#else
  pure ()
#endif

setManualScrollRestoration :: IO ()
setManualScrollRestoration = 
  setScrollRestoration ManualScrollRestoration

describe :: Txt -> IO ()
describe d =
#ifdef __GHCJS__
  set_description_js d
#else
  pure ()
#endif

-- | Store current scroll position in the current `window.history.state`.
storeScrollPosition :: IO ()
storeScrollPosition = 
#ifdef __GHCJS__
  store_scroll_position_js
#else
  pure ()
#endif

-- | Handle a stored scroll position with a callback `f :: X-Offset -> Y-Offset -> IO ()`
-- If no non-zero scroll position is available, the callback is not called. 
withScrollPositionFromHistory :: (Int -> Int -> IO ()) -> IO ()
withScrollPositionFromHistory f = do
#ifdef __GHCJS__
  x <- recall_page_x_offset_js
  y <- recall_page_y_offset_js
  when (y /= 0 || x /= 0) (f x y)
#else
  pure ()
#endif

-- | Restore a scroll position immediately. 
--
-- Note: This method must be called after content is restored. In some cases
-- of dynamic content rendering, orchestration will be required to restore 
-- scroll position successfully.
--
-- Note: The approach taken in this module will not allow for restoring scroll 
-- positon when the browser's forward button is clicked.
restoreScrollPosition :: IO ()
restoreScrollPosition = withScrollPositionFromHistory setScrollPosition
  where
    setScrollPosition x y = do
#ifdef __GHCJS__
      scroll_to_js x y
#else
      pure ()
#endif

-- | Restore a scroll position smoothly. 
--
-- Note: This method must be called after content is restored. In some cases
-- of dynamic content rendering, orchestration will be required to restore 
-- scroll position successfully.
--
-- Note: The approach taken in this module will not allow for restoring scroll 
-- positon when the browser's forward button is clicked.
restoreScrollPositionSmooth :: IO ()
restoreScrollPositionSmooth = withScrollPositionFromHistory setScrollPositionSmooth
  where
    setScrollPositionSmooth x y = do
#ifdef __GHCJS__
      scroll_to_smooth_js x y
#else
      pure ()
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "var st = history.state || {}; st.elmScrollY = window.pageYOffset; st.elmScrollX = window.pageXOffset; history.replaceState(st,null,null);"
    store_scroll_position_js :: IO ()

foreign import javascript unsafe
  "$r = (history.state && history.state.elmScrollY) || 0" recall_page_y_offset_js :: IO Int

foreign import javascript unsafe
  "$r = (history.state && history.state.elmScrollX) || 0" recall_page_x_offset_js :: IO Int

foreign import javascript unsafe
  "window.scrollTo($1,$2)" scroll_to_js :: Int -> Int -> IO ()

foreign import javascript unsafe
  "window.scrollTo({ top: $2, left: $1, behavior: 'smooth' })" scroll_to_smooth_js :: Int -> Int -> IO ()

foreign import javascript unsafe
  "var a = document.querySelector('meta[name=\"description\"]'); if (a) { a.setAttribute('content', $1);} else { var meta = document.createElement('meta'); meta.setAttribute('name','description'); meta.setAttribute('content', $1); document.getElementsByTagName('head')[0].appendChild(meta)};"
    set_description_js :: Txt -> IO ()

foreign import javascript unsafe
  "if ('scrollRestoration' in history) { history.scrollRestoration = $1;}"
    set_scroll_restoration_js :: Txt -> IO ()

foreign import javascript unsafe
  "window.scrollTo({ top: 0 })" scroll_top_js :: IO ()
#endif

