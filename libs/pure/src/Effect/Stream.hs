{-# language ConstraintKinds, RankNTypes, ScopedTypeVariables, FlexibleContexts, AllowAmbiguousTypes, BlockArguments, RecordWildCards, TypeApplications #-}
module Effect.Stream
  ( Step
  , Effect.Stream.step
  , Effect.Stream.stream
  , loaded
  , stepper
  ) where

import Control.Monad (when)
import Data.Default
import Data.Stream
import Data.Styles (px)
import Data.Typeable
import Data.Txt
import Data.View
import Effect.Intersection

type Step a = State (Stream IO a)

step :: forall a. Modify (Stream IO a) => Int -> IO ()
step n = modifyIO (steps @IO @a n)

stream :: Typeable a => Stream IO a -> (Step a => View) -> View
stream = state

loaded :: forall a. Exists (Stream IO a) => [a]
loaded = toList (it :: Stream IO a)

stepper :: forall a. Txt -> View -> (Step a => View)
stepper m v = 
  Data.View.stream (\i@Intersection {..} -> when isIntersecting (Effect.Stream.step @a 1)) do
    viewportIntersecting def { margin = m, thresholds = [1] } v
