{-# language AllowAmbiguousTypes, ScopedTypeVariables, RankNTypes, FlexibleContexts, ConstraintKinds, BlockArguments, PatternSynonyms, ViewPatterns, TypeFamilies, TypeApplications, InstanceSigs #-}
module Control.Form (Form,formlet,form,liftIO) where

import Control.Applicative
import Control.Cont
import Control.Producer
import Control.State
import Data.Default
import Data.Exists
import Data.HTML (pattern Span,pattern Div)
import Data.Typeable
import Data.View hiding (lifecycles)
import Effect.Lifecycles
import GHC.Exts
import Unsafe.Coerce

newtype Form ctx a = Form (Producer a => Dynamic ctx)

formlet :: forall a ctx. ((ctx, Producer a) => View) -> Form ctx a
formlet v = Form (dynamic v)

form :: forall ctx a. (Typeable ctx, Producer a, ctx) => Form ctx a -> View
form (Form f) = cont f

instance Functor (Form ctx) where
  fmap f (Form g) = Form (dynamic (stream (yield . f) (fromDynamic g)))

instance Applicative (Form ctx) where
  pure a = formlet (lifecycles def { onStart = yield a } Null)
  liftA2 = liftF2

instance Monad (Form ctx) where
  return = pure
  (Form f) >>= g = Form (dynamic (stream (\a -> let Form f = g a in unify f) (fromDynamic f)))

instance MonadFail (Form ctx) where
  fail str = Form (dynamic (txt str))

liftIO :: (ctx => IO a) -> Form ctx a
liftIO io = Form (dynamic (lifecycles def { onStart = io >>= yield } Null))

data Any2 = Any2 Any Any
liftF2 :: forall ctx a b c. (a -> b -> c) -> Form ctx a -> Form ctx b -> Form ctx c
liftF2 f (Form fa) (Form fb) =
  Form do
    let 
      commitA :: State Any2 => a -> IO ()
      commitA a =
        case it of
          Any2 _ (unsafeCoerce -> Just b) -> do
            yield (f a b)
            put (Any2 (unsafeCoerce (Just a)) (unsafeCoerce (Just b)))
          Any2 _ b -> 
            put (Any2 (unsafeCoerce (Just a)) b)
            
      commitB :: State Any2 => b -> IO ()
      commitB b =
        case it of
          Any2 (unsafeCoerce -> Just a) _ -> do
            yield (f a b)
            put (Any2 (unsafeCoerce (Just a)) (unsafeCoerce (Just b)))
          Any2 a _ -> 
            put (Any2 a (unsafeCoerce (Just b)))

    dynamic do
      state (Any2 (unsafeCoerce Nothing) (unsafeCoerce Nothing)) do
        Span <||>
          [ stream commitA (fromDynamic fa)
          , stream commitB (fromDynamic fb)                
          ]
