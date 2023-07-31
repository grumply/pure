{-# LANGUAGE ImplicitParams, ConstraintKinds, RankNTypes, RecordWildCards,
   ScopedTypeVariables, TypeApplications, BangPatterns, MagicHash, 
   AllowAmbiguousTypes, PatternSynonyms, ViewPatterns,
   FlexibleContexts #-}
module Data.Controller (Controller(..),run,command,commandWith) where

import Control.Concurrent (myThreadId,ThreadId)
import Control.Monad
import Data.Function
import Data.Typeable

import Prelude hiding (map)

import Data.Default
import Data.Subscribe
import Data.View

import Data.Coerce

import Data.IORef
import System.IO.Unsafe

data Controller env st msg = Controller 
  { _startup  :: Effect msg => [msg]
  , _receive  :: Effect msg => [msg]
  , _shutdown :: Effect msg => [msg]
  , _model    :: Effect msg => IO st
  , _update   :: Effect msg => msg -> env -> st -> IO st
  , _view     :: Effect msg => env -> st -> View
  }

instance (Typeable env, Typeable st, Typeable msg) => Default (Controller env st msg) where
  def = 
    let 
      tr = typeOf (undefined :: Controller env st msg) 
      tm = typeOf (undefined :: st)
    in
      Controller [] [] []
        (error $ "Control.Controller.def: No default model supplied to " ++ show tr ++ " of type " ++ show tm) 
        (\_ _ -> pure) 
        (\_ _ -> Null)

newtype ControllerEnv msg env = Env env

-- | Turn an `Controller st msg` into a component with `msg` property.
{-# INLINE run #-}
run :: forall env st msg. (Typeable env, Typeable st, Typeable msg) => Controller env st msg -> env -> View
run Controller {..} = Component app . (Env @msg) 
  where
    app :: Ref (ControllerEnv msg env) st -> Comp (ControllerEnv msg env) st 
    app self = 
      let
        {-# INLINE upd #-}
        upd :: msg -> IO () -> IO Bool
        upd msg after = modifyM self $ \env mdl -> do
          mdl' <- using (Handler upd) (_update msg (coerce env) mdl)
          pure (mdl',after)
      in
        using (Handler upd) $
          let
            {-# INLINE update #-}
            update :: env -> st -> [msg] -> IO st
            update env mdl msgs = go mdl msgs
              where
                go :: Effect msg => st -> [msg] -> IO st
                go mdl [] = pure mdl
                go mdl (msg:msgs) = do
                  mdl' <- _update msg env mdl
                  go mdl' msgs
          in 
            def 
              { onConstruct = _model
              , onExecuting = \mdl -> do
                env <- ask self
                update (coerce env) mdl _startup
              , onReceive = \env mdl ->
                update (coerce env) mdl _receive
              , onUnmounted = void $ do
                env <- ask self
                mdl <- get self
                update (coerce env) mdl _shutdown
              , render = _view . coerce
              }

{-# INLINE command #-}
command :: Effect msg => msg -> IO ()
command msg = effect msg

{-# INLINE commandWith #-}
commandWith :: Effect msg => msg -> IO () -> IO ()
commandWith msg after = void (effect' msg after)

