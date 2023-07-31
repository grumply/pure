module Effect.Title (title) where

import Data.DOM (setTitle)
import Data.Txt (Txt)
import Data.View (View,eager)

title :: Txt -> View -> View
title t = eager (setTitle t)
