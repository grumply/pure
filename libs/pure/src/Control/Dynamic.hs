{-# language TypeOperators, ConstraintKinds, Rank2Types #-}
module Control.Dynamic ((:=>),dynamic,fromDynamic) where

newtype c :=> a = Dynamic { fromDynamic :: c => a }

dynamic :: (c => a) -> (c :=> a)
dynamic = Dynamic
{-# INLINE dynamic #-}
