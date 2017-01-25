module Atomic.Render where

import Atomic.FromTxt
import Atomic.ToTxt

render :: (ToTxt a,FromTxt b) => a -> b
render = fromTxt . toTxt

