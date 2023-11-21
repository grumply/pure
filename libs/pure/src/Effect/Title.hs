module Effect.Title (title) where

import Data.DOM (setTitle)
import Data.Txt (Txt)
import Data.View (View,eagerly)

title :: Txt -> View -> View
title t = eagerly (setTitle t)
