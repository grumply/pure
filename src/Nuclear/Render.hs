module Nuclear.Render where

import Nuclear.FromText
import Nuclear.ToText

render :: (ToText a,FromText b) => a -> b
render = fromText . toText

