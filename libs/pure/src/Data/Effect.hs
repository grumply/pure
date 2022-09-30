{-# language ConstraintKinds, FlexibleContexts, RankNTypes #-}
module Data.Effect (Effect,Handler(..),effect,effect',map,(#)) where

import Control.Monad (void)
import Data.Exists
import GHC.Exts

import Prelude hiding (map)

newtype Handler eff = Handler { runHandler :: eff -> IO () -> IO Bool }

type Effect eff = Exists (Handler eff)

{-# INLINE effect' #-}
effect' :: Effect eff => eff -> IO () -> IO Bool
effect' = runHandler it

{-# INLINE effect #-}
effect :: Effect eff => eff -> IO ()
effect eff = void (effect' eff (pure ()))

{-# INLINE map #-}
map :: forall msg msg' a. (msg -> msg') -> (Effect msg => a) -> (Effect msg' => a)
map f = using (Handler (\m io -> effect' (f m) io)) 

-- {-# INLINE also #-}
-- also :: forall msg a. (msg -> IO ()) -> (Effect msg => a) -> (Effect msg => a)
-- also f = using (Handler (\m io -> effect' m (io >> f m)))

{-# INLINE (#) #-}
infixr 0 #
(#) :: forall a b x. (a -> b) -> (Effect a => x) -> (Effect b => x)
(#) = map

{-# RULES
  "Data.Effect.map id" forall x. map id x = x
  #-}