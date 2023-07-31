{-# language PatternSynonyms, RankNTypes, FlexibleContexts, OverloadedStrings, ViewPatterns, CPP, DuplicateRecordFields #-}
module Web.Range where

import Control.Monad
import Data.Coerce
import Data.Default
import Data.DOM
import Data.Events (pattern OnWith)
import Data.JSON
import Data.Maybe
import Data.Time
import Data.Txt
import Data.View
import System.IO.Unsafe

data Range = Range
  { collapsed :: Bool
  , startContainer :: Node
  , startOffset :: Int
  , endContainer :: Node
  , endOffset :: Int
  }

toRange :: JSV -> Range
toRange r = let err = error "Invalid Range." in
  Range
    { collapsed = fromMaybe err (r .# "collapsed")
    , startContainer = maybe err (coerce :: JSV -> Node) (r .# "startContainer")
    , startOffset = fromMaybe err (r .# "startOffset")
    , endContainer = maybe err (coerce :: JSV -> Node) (r .# "endContainer")
    , endOffset = fromMaybe err (r .# "endOffset")
    }



