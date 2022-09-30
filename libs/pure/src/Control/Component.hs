{-# language TypeApplications, KindSignatures, ScopedTypeVariables, TypeFamilies, RankNTypes, ConstrainedClassMethods, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}
module Control.Component 
  ( module Control.Controller
  , Update
  , Render
  , Handler
  , Component(..)
  , HasCallStack
  , timed
  ) where

import Control.Controller hiding (run,receive,initialize)
import qualified Control.Controller
import Data.Effect hiding (Handler)
import Data.DOM
import Data.View hiding (receive,initialize,view)

import Data.Typeable ( Typeable, typeOf )

import GHC.Stack (HasCallStack)

type Update  a = (HasCallStack, Component a, Effect (Msg a)) => a -> Model a -> IO (Model a)
type Render  a = (HasCallStack, Component a, Effect (Msg a)) => Model a -> View
type Handler a = (HasCallStack, Component a, Effect (Msg a)) => Evt -> IO ()

class Typeable (a :: *) => Component a where
  data Model a
  data Msg a
 
  startup :: Effect (Msg a) => [Msg a]
  startup = []

  receive :: Effect (Msg a) => [Msg a]
  receive = []

  shutdown :: Effect (Msg a) => [Msg a]
  shutdown = []

  model :: Effect (Msg a) => Model a
  model = error ("No default model defined for " ++ show (typeOf (undefined :: a)))

  -- GHC doesn't let me do this, but it would be nice if GHC would emit this warning for any overriding implementation.
  -- {-# WARNING initialize "Component.initialize is a render-blocking initialization method! Consider moving long-running and long-tail effects to a startup event." #-}
  initialize :: Effect (Msg a) => a -> IO (Model a)
  initialize _ = pure model

  upon :: Msg a -> Update a
  upon _ _ mdl = pure mdl

  view :: a -> Render a
  view _ _ = Null

  {-# INLINE app #-}
  app :: a -> Controller a (Model a) (Msg a)
  app a = Controller startup receive shutdown (initialize a) upon view 

  {-# INLINE run #-}
  run :: a -> View
  run a = Control.Controller.run (app a) a

