module Pure.Magician.Server.Analytics where

import Pure.Magician.Resources
import Pure.Magician.Server.Serve

import Pure.Conjurer hiding (cache)
import qualified Pure.Conjurer as Conjurer
import qualified Pure.Conjurer.Analytics as Conjurer
import Pure.Convoker as Convoker
import Data.JSON (ToJSON,FromJSON)
import Data.Time (Time,pattern Day,delay)

import Control.Concurrent
import Control.Monad
import Data.Typeable

import Data.Hashable

analyze :: forall a. (Server a, Subset (Analyze a) (Resources a) ~ True, Conjurer.Analyzeable (Analyze a)) => Time -> IO ()
analyze d = do
  void do
    forkIO do
      forever do
        analyzed <- Conjurer.analyzeAll Day 
        Conjurer.analyzeEach @(Analyze a) analyzed
        delay d