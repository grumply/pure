{-# language TemplateHaskell #-}
module Atomic.Try where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Data.Monoid

import Data.Txt
import Data.JSON
import GHC.Generics

-- import Control.Lens.TH

data Try a
  = Trying
  | Failed
  | Done a
  deriving (Eq,Ord,Read,Show,Generic,ToJSON,FromJSON)
-- makePrisms ''Try

instance Functor Try where
  fmap f (Done a) = Done (f a)
  fmap f Trying = Trying
  fmap f Failed = Failed

instance Applicative Try where
  pure = Done
  (Done f) <*> (Done a) = Done (f a)
  (Done _) <*> Trying = Trying
  (Done _) <*> Failed = Failed
  Failed <*> _ = Failed
  Trying <*> _ = Trying

instance Monoid a => Monoid (Try a) where
  mempty = Failed
  mappend (Done a) (Done b) = Done (a <> b)
  mappend (Done a) _ = Done a
  mappend _ (Done a) = Done a
  mappend Failed x = x
  mappend x _ = x

instance Monad Try where
  return = Done
  (>>=) (Done a) f = f a
  (>>=) Failed _ = Failed
  (>>=) Trying _ = Trying

instance MonadFix Try where
  mfix f = let a = f (unDone a) in a
           where unDone (Done x) = x
                 unDone Failed = error "mfix Try: Failed"
                 unDone Trying = error "mfix Try: Trying"

instance Foldable Try where
  foldr f z (Done x) = f x z
  foldr _ z _ = z

  foldl f z (Done x) = f z x
  foldl _ z _ = z

instance Traversable Try where
  traverse f (Done x) = Done <$> f x
  traverse _ Trying = pure Trying
  traverse _ Failed = pure Failed

instance MonadPlus Try

instance Alternative Try where
  empty = Failed
  (Done a) <|> _ = Done a
  _ <|> x = x

isTrying :: Try a -> Bool
isTrying Trying = True
isTrying _ = False

isFailed :: Try a -> Bool
isFailed Failed = True
isFailed _ = False

isDone :: Try a -> Bool
isDone (Done _) = True
isDone _ = False
