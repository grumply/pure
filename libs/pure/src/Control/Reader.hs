{-# language ConstraintKinds, RankNTypes, TypeFamilies, ScopedTypeVariables, AllowAmbiguousTypes, DataKinds, TypeOperators #-}
module Control.Reader (Reader,Readers,ask,asks,reader,local) where

import Control.Concurrent
import Control.Monad
import Data.Exists
import Data.Time
import Data.View hiding (ask)

import Data.Kind

type Reader a = Exists a

type family Readers (xs :: [*]) :: Constraint where
  Readers (x ': xs) = (Reader x,Readers xs)
  Readers '[] = ()

{-# INLINE ask #-}
ask :: Reader a => a
ask = it

{-# INLINE asks #-}
asks :: (a -> b) -> (Reader a => b)
asks f = f ask

{-# INLINE reader #-}
reader :: a -> (Reader a => x) -> x
reader = using

{-# INLINE local #-}
local :: (a -> b) -> (Reader b => x) -> (Reader a => x)
local f = reader (f ask)
