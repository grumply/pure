{-# language PatternSynonyms, RankNTypes, FlexibleContexts, OverloadedStrings, ViewPatterns, CPP, DuplicateRecordFields, RecordWildCards #-}
module Web.Iterable where

import Control.Producer
import Control.Monad
import Data.Coerce
import Data.Default
import Data.DOM
import Data.Effect ((#))
import Data.Events (pattern OnWith)
import Data.JSON
import Data.Maybe
import Data.Time
import Data.Txt
import Data.View
import System.IO.Unsafe

data Iterable = Iterable
  { length :: Int
  , items :: [JSV]
  }

toIterable :: JSV -> Iterable
toIterable o = Iterable l (go 0)
  where
    err = error "Invalid Iterable." 
    l = fromMaybe err (o .# "length")
#ifdef __GHCJS__
    go n 
      | n == l = []
      | otherwise = get_item_js o n : go (n + 1)
#else
    go _ = []
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "$1[$2]" get_item_js :: JSV -> Int -> JSV
#endif

fromIterableWith :: (JSV -> a) -> Iterable -> [a]
fromIterableWith f Iterable {..} = fmap f items
