{-# LANGUAGE DeriveGeneric, DeriveAnyClass, AllowAmbiguousTypes #-}
module Data.Try where

import Data.Default
import Data.JSON

import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import Control.Monad.Fix
import Control.Monad.Zip

import Data.Monoid hiding ((<>))
import Data.Semigroup

import GHC.Generics

data Try a
  = Trying
  | Failed
  | Done a
  deriving (Eq,Ord,Read,Show,Generic,ToJSON,FromJSON)

instance Functor Try where
  fmap f (Done a) = Done (f a)
  fmap f Trying = Trying
  fmap f Failed = Failed

instance Applicative Try where
  pure = Done
  Done f <*> x = fmap f x
  Trying <*> _ = Trying
  Failed <*> _ = Failed

-- Note: (def :: Try a) /= (mempty :: Try a)
instance Default (Try a) where
  def = Trying

instance Semigroup a => Monoid (Try a) where
  mempty = Failed
  mappend = (<>)

instance Semigroup a => Semigroup (Try a) where
  (<>) Failed r = r
  (<>) l Failed = l
  (<>) Trying r = r
  (<>) l Trying = l
  (<>) (Done l) (Done r) = Done (l <> r)

instance MonadFail Try where
  fail _ = Failed

instance Monad Try where
  return = pure
  (>>=) (Done a) f = f a
  (>>=)  Trying  _ = Trying
  (>>=)  Failed  _ = Failed

instance MonadZip Try where
  mzipWith = liftM2

instance MonadFix Try where
  mfix f =
      let a = f (unDone a)
      in a
    where
      unDone (Done x) = x
      unDone Failed = error "mfix Try: Failed"
      unDone Trying = error "mfix Try: Trying"

instance Foldable Try where
  foldMap = try mempty mempty

  foldr f z (Done x) = f x z
  foldr _ z _        = z

  foldl f z (Done x) = f z x
  foldl _ z _        = z

instance Traversable Try where
  traverse f (Done x) = Done <$> f x
  traverse _  Trying  = pure Trying
  traverse _  Failed  = pure Failed

instance MonadPlus Try

instance Alternative Try where
  empty = Failed
  Failed <|> r      = r
  l      <|> Failed = l
  Trying <|> r      = r
  l      <|> _      = l

try :: b -> b -> (a -> b) -> Try a -> b
try t f d x =
  case x of
    Trying -> t
    Failed -> f
    Done a -> d a

isTrying :: Try a -> Bool
isTrying Trying = True
isTrying _ = False

isFailed :: Try a -> Bool
isFailed Failed = True
isFailed _ = False

isDone :: Try a -> Bool
isDone (Done _) = True
isDone _ = False

eitherToTry :: Either a b -> Try b
eitherToTry (Left _) = Failed
eitherToTry (Right a) = Done a

maybeToTry :: Maybe a -> Try a
maybeToTry Nothing = Failed
maybeToTry (Just a) = Done a