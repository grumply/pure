{-# language CPP #-}
module Pure.Magician.Client.Restore where

import Control.Concurrent
import Control.Monad
import Data.Animation
import Data.Default
import Data.DOM (Node(..),clientHeight,clientWidth,onRaw,toNode,getWindow)
import Data.Scroll
import Data.Time

{-
This method is far from perfect, but is designed to work with most off-the-shelf
`pure-magician`-based applications.
-}

restoreWith :: Time -> Time -> IO ()
restoreWith max d = 
  void do
    forkIO do
      mv <- newEmptyMVar
      w <- getWindow
      release <- onRaw (toNode w) "popstate" def $ \_ _ -> void (tryPutMVar mv ())
      go release mv
  where
    go release mv = loop max
      where
        loop remaining 

            -- browser never loaded enough content to correctly restore
          | remaining <= 0 = release

          | otherwise = do
            mu <- tryTakeMVar mv
            case mu of

              -- route change; fail out
              Just _  -> release 

              _ -> do
                -- Should be sufficient for most devices?
                -- The failure mode is simply not restoring 
                -- the scroll position, which isn't too bad.
                -- Sadly, there's no way to know if all the 
                -- desired content has loaded, which is the
                -- reason for the delay.
                withScrollPositionFromHistory $ \ox oy -> do
                  ch <- clientHeight
                  cw <- clientWidth
                  sh <- pageScrollHeight
                  sw <- pageScrollWidth
                  if sw >= ox + cw && sh >= oy + ch then do
                    addAnimation restoreScrollPosition
                    release
                  else do
                    delay d
                    loop (remaining - d)

pageScrollHeight :: IO Int
pageScrollHeight =
#ifdef __GHCJS__
  page_scroll_height_js
#else
  pure 0
#endif

pageScrollWidth :: IO Int
pageScrollWidth =
#ifdef __GHCJS__
  page_scroll_width_js
#else
  pure 0
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "$r = document.documentElement.scrollHeight" 
    page_scroll_height_js :: IO Int

foreign import javascript unsafe
  "$r = document.documentElement.scrollWidth"
    page_scroll_width_js :: IO Int
#endif
