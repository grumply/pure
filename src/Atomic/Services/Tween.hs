{-# language QuasiQuotes #-}
{-# language CPP #-}
module Atomic.Services.Tween (scrollTo, easeScroll) where

import Ef.Base

import Data.Txt

import Atomic.Component
import Atomic.Ease
import Atomic.Service

import Atomic.Services.Frame

#ifdef __GHCJS__
import qualified GHCJS.DOM.Window as W hiding (getWindow)
import qualified GHCJS.DOM.Element as E
#endif

-- Move this functionality into Frame or some animation library.

scrollTo :: ( MonadIO c
            , '[Revent] <: ms
            )
         => Ease
         -> Double
         -> Atom (Code ms' IO ())
         -> Code ms c (Maybe (IO ()))
scrollTo ease duration {- milliseconds -} to = do
  me <- liftIO $ getElement to
  case me of
    Nothing -> return Nothing
    Just dest  -> fmap Just $ do
      win   <- getWindow
#ifdef __GHCJS__
      begin <- fmap fromIntegral $ W.getScrollY win
      end   <- E.getOffsetTop dest
      let delta = end - begin
      onFrameWithTime (\_ -> easeScroll win ease begin delta dest duration)
#else
      return (return ())
#endif

#ifdef __GHCJS__
-- W.scrollTo only takes 'Int's.
foreign import javascript unsafe
  "window.scrollTo(0,$1);"
  scroll_to_js :: Double -> IO ()
#endif

easeScroll :: (MonadIO c)
           => Win -> Ease -> Double -> Double -> ENode -> Double -> Double -> Code '[Event Double] c ()
easeScroll win ease begin delta dest duration start = go begin start
  where

    go currentY currentTime = do
#ifdef __GHCJS__
      currentY' <- fromIntegral <$> W.getScrollY win
      let c = ease duration (currentTime - start)
          d = c * delta
          n = begin + d
      if currentTime - start > duration then do
        -- slow animation; just jump to destination
        liftIO $ scroll_to_js (begin + delta)
        end
      else if abs (currentY' - (max 0 currentY)) > 10 then do
        -- user interrupt
        end
      else do
        -- standard ease step
        liftIO $ scroll_to_js n
        become (skip n)

    -- skip every other frame for 30 fps; some mobile browsers
    -- don't properly update scrollY, so we give them a little leeway
    skip n _ = become (go n)
#else
      end
#endif
