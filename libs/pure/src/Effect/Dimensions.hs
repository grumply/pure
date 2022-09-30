{-# language BlockArguments, ConstraintKinds, FlexibleContexts, RankNTypes, OverloadedStrings #-}
module Effect.Dimensions (Size(..),Dimensions,resizing,size,dimensional) where

import Control.Reader
import Control.State

import Data.Default
import Data.DOM
import Data.Exists
import Data.View hiding (ask)

data Size = Size 
  { width :: Int
  , height :: Int 
  }

type Dimensions = State Size

{-# INLINE dimensional #-}
dimensional :: (Dimensions => View) -> View
dimensional = state (Size 0 0)

{-# INLINE size #-}
size :: Reader Size => Size
size = ask

{-# INLINE resizing #-}
resizing :: Modify Size => View -> View
resizing = OnMounted withLiveNode
  where
    withLiveNode node = seed >> resizer
      where
        seed    = update
        resizer = onRaw window "resize" def $ \_ _ -> update

        update  = getSize >>= put
          where
            getSize = 
              Size 
                <$> offsetWidth node 
                <*> offsetHeight node