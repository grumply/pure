{-# language ScopedTypeVariables, TypeApplications, ConstraintKinds, FlexibleContexts, RankNTypes, AllowAmbiguousTypes #-}
module Control.Producer (Producer,yield,stream,events,discard) where

import Data.Effect (Effect, Handler(Handler),effect,(#))
import Data.Exists (Exists,using)
import Data.View (View)

type Producer a = Effect a

{-# INLINE yield #-}
yield :: Producer a => a -> IO ()
yield = effect

{-# INLINE stream #-}
stream :: (a -> IO ()) -> (Producer a => b) -> b
stream f = using (Handler (\a after -> f a >> after >> pure True))

{-# INLINE events #-}
events :: forall a b. (Exists a => IO ()) -> (Producer a => b) -> b
events f = stream @a (`using` f)

{-# INLINE discard #-}
discard :: forall a b. (Producer a => b) -> b
discard = stream @a (\_ -> pure ())



