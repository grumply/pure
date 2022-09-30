module Effect.Title (title) where

import Data.DOM (setTitle)
import Effect.Sync (sync)
import Data.Txt (Txt)
import Data.View (View)

title :: Txt -> View -> View
title t v = sync (setTitle t) v