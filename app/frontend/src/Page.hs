module Page (P, run, page, fromPage) where

import Pure.Magician
import Shared

type P =
  ( Viewable (Preview Page),
    Viewable (Preview Post),
    Viewable (Product Page),
    Viewable (Product Post),
    App Blog () ()
  )

-- | Run a `P`-constrained `View`. Injects the default previewers and product
-- viewers and enabless routing, authentication, and a websocket connection to
-- the backend.
run :: (P :=> View) -> View
run p =
  with pagePreview do
    with pageProduct do
      with postPreview do
        with postProduct do
          client @Blog @() "127.0.0.1" 8081 (dispatch ()) do
            fromPage p

page :: (P => View) -> (P :=> View)
page = dynamic

fromPage :: (P :=> View) -> (P => View)
fromPage = fromDynamic
