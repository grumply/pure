{-# language BlockArguments, ScopedTypeVariables, RankNTypes, DuplicateRecordFields, NamedFieldPuns, ConstraintKinds, FlexibleContexts, GADTs, TypeApplications, BangPatterns, AllowAmbiguousTypes, CPP, PatternSynonyms, OverloadedStrings, RecordWildCards, LambdaCase #-}
module Web (Web,play,play',play_,become,become_,Web.error,void,web,web',fix,fix',run,run',Web.read,Web.show,prompt,Web.reader,Parser) where

import Control.Concurrent hiding (yield)
import qualified Data.Function as F
import Data.Default
import Data.DOM
import Data.Foldable
import Data.Either (isRight)
import Data.Events (pattern OnSubmitWith, intercept)
import Data.Maybe
import Data.View.Build
import Data.View
import Data.Void
import Data.Txt as Txt
import Data.Typeable
import Data.HTML hiding (pattern Output)
import System.IO.Unsafe
import Text.Read
import GHC.Show as Show
import Web.Events
import Control.Exception (SomeException(..),handle)
import Control.DeepSeq
import Data.Animation
import System.Mem
import Data.View.Build (diffDeferred)
import Data.List as List
import Data.IORef
import Control.Monad (when)
import Data.Coerce

data OI = Output View | Display (MVar View) View
type Web = Producer OI

{-# INLINE play #-}
play :: forall a. (Web => a) -> View
play a = discard @a (play_ @a False a) 

{-# INLINE play' #-}
play' :: (Web => a) -> (Producer a => View)
play' = play_ False

{-# INLINE play_ #-}
play_ :: Bool -> (Web => a) -> (Producer a => View)
play_ synchronous a = SimpleHTML "template" <| OnMounted go
  where
    go n = do
      mv <- newMVar (NullView (Just (coerce n)),NullView Nothing) 
      tid <- forkIO do
        let x = runner synchronous mv a
        -- this seq is important!
        x `seq` yield x
      pure do
        (old,_) <- takeMVar mv
        killThread tid
        cleanup old

{-# NOINLINE become #-}
become :: (Producer a => View) -> (Web => a)
become v = unsafePerformIO do
  mv <- newEmptyMVar
  -- this deepseq is important!
  let send v = v `deepseq` yield (Output v) 
  v' <- handle (\(se :: SomeException) -> send (txt (Show.show se)))
          (send (consume (putMVar mv) v))
  a <- takeMVar mv
  pure a

{-# NOINLINE become_ #-}
become_ :: View -> (Web => ())
become_ v = unsafePerformIO do
  mv <- newEmptyMVar
  -- this deepseq is important!
  let send v = v `deepseq` yield (Display mv v)
  handle (\(se :: SomeException) -> send (txt (Show.show se))) (send v)
  takeMVar mv
  -- performMajorGC
  pure ()

error :: Txt -> (Web => a)
error = become . fromTxt

void :: View -> (Web => Void)
void = become

{-# INLINE web' #-}
web' :: Bool -> Node -> (Web => a) -> a
web' synchronous n a = builder `seq` (consume (writeChan chan) a)
  where
    {-# NOINLINE chan #-}
    chan :: Chan OI
    chan = unsafePerformIO newChan

    {-# NOINLINE builder #-}
    builder :: ()
    builder = unsafePerformIO (run start)
      where
        run f = do
          forkIO f
          pure ()

        start = do
          mu <- readChan chan
          case mu of
            Output v -> do
              v' <- inject n v
              go v v'
            Display mv v -> do
              v' <- inject n v
              putMVar mv v'
              go v v'

        go mid current = do
          mu <- readChan chan
          case mu of
            Output v -> do
              v' <- diff current mid v
              go v v'
            Display mv v -> do
              v' <- diff current mid v
              putMVar mv v'
              go v v'

        runPlan :: [IO ()] -> IO ()
        runPlan plan = let !p = sequence_ (List.reverse plan) in p

        diff old mid new = do
          mtd <- newIORef []
          let
            (!plan,!plan',!new_old) = buildPlan $ \p p' -> 
                diffDeferred mtd p p' old mid new

            hasAnimations = not (List.null plan)
            hasIdleWork = not (List.null plan')
            
          mounts <- plan `seq` plan' `seq` readIORef mtd

#ifdef __GHCJS__
          when hasAnimations $ do
            if synchronous then do
              barrier <- newEmptyMVar
              addAnimation $ do
                runPlan plan
                putMVar barrier ()
              takeMVar barrier
            else
              runPlan plan
#else
          runPlan plan
#endif

          runPlan plan'

          runPlan mounts
          
          pure new_old

{-# NOINLINE runner #-}
runner :: Bool -> MVar (View,View) -> (Web => a) -> a
runner synchronous st a = consume builder a
  where
    {-# INLINE builder #-}
    builder :: OI -> IO ()
    builder (Output new) =
      takeMVar st >>= \(old,mid) -> do
        v <- diff old mid new
        putMVar st (v,new)
    builder (Display mv new) =
      takeMVar st >>= \(old,mid) -> do
        v <- diff old mid new
        putMVar mv v
        putMVar st (v,new)

    runPlan :: [IO ()] -> IO ()
    runPlan plan = let !p = sequence_ (List.reverse plan) in p

    diff old mid new = do
      mtd <- newIORef []
      let
        (!plan,!plan',!new_old) = buildPlan $ \p p' -> 
            diffDeferred mtd p p' old mid new

        hasAnimations = not (List.null plan)
        hasIdleWork = not (List.null plan')
        
      mounts <- plan `seq` plan' `seq` readIORef mtd

#ifdef __GHCJS__
      when hasAnimations $ do
        if synchronous then do
          barrier <- newEmptyMVar
          addAnimation $ do
            runPlan plan
            putMVar barrier ()
          takeMVar barrier
        else
          runPlan plan
#else
      runPlan plan
#endif

      runPlan plan'

      runPlan mounts
      
      pure new_old

{-# INLINE web #-}
web :: Bool -> Node -> (Web => a) -> a
web synchronous n a = consume builder a
  where
    {-# NOINLINE st #-}
    st = unsafePerformIO (newMVar Nothing)

    {-# NOINLINE builder #-}
    builder :: OI -> IO ()
    builder (Output new) =
      takeMVar st >>= \case
        Nothing -> do
          v <- inject n new
          putMVar st (Just (v,new))
        Just (old,mid) -> do
          v <- diff old mid new
          putMVar st (Just (v,new))
    builder (Display mv new) =
      takeMVar st >>= \case
        Nothing -> do
          v <- inject n new
          putMVar mv v
          putMVar st (Just (v,new))
        Just (old,mid) -> do
          v <- diff old mid new
          putMVar mv v
          putMVar st (Just (v,new))

    runPlan :: [IO ()] -> IO ()
    runPlan plan = let !p = sequence_ (List.reverse plan) in p

    diff old mid new = do
      mtd <- newIORef []
      let
        (!plan,!plan',!new_old) = buildPlan $ \p p' -> 
            diffDeferred mtd p p' old mid new

        hasAnimations = not (List.null plan)
        hasIdleWork = not (List.null plan')
        
      mounts <- plan `seq` plan' `seq` readIORef mtd

#ifdef __GHCJS__
      when hasAnimations $ do
        if synchronous then do
          barrier <- newEmptyMVar
          addAnimation $ do
            runPlan plan
            putMVar barrier ()
          takeMVar barrier
        else
          runPlan plan
#else
      runPlan plan
#endif

      runPlan plan'

      runPlan mounts
      
      pure new_old

fix :: (Web => a -> a) -> a
fix f = F.fix \a -> web False (toNode body) (f a) 

run :: (Web => a) -> a
run a = fix (const a)

fix' :: (Web => a -> a) -> a
fix' f = F.fix \a -> web True (toNode body) (f a)

run' :: (Web => a) -> a
run' a = fix' (const a)

data PromptState = PromptState 
  { currentInput :: Txt
  , errorMsg :: Maybe View
  }

type Parser a = Either Txt Txt -> Either View a

prompt :: forall a. View -> Parser a -> (Web => a)
prompt p parse = go (PromptState mempty Nothing)
  where
    go :: PromptState -> a
    go PromptState {..} = 
      become do
        Data.HTML.Form <| OnSubmitWith intercept (\_ -> submit) |> 
          [ P <||> [ p , inputField ]
          , maybe Null id errorMsg
          , Button <| Type "submit" |> [ "Submit" ]
          ]
      where
        inputField :: Producer a => View
        inputField =
            Input <| keyDowns keyDown . inputs input . Value currentInput . OnMounted mounted
          where
            mounted :: Node -> IO (IO ())
            mounted n = focusNode n >> pure (pure ())
            
            keyDown :: Exists KeyDown => IO ()
            keyDown 
              | KeyDown KeyboardEvent { key } <- it
              , editingKey key 
              = yield (go PromptState { errorMsg = Nothing, .. })

              | otherwise 
              = pure ()

            input :: Exists Input => IO ()
            input | In InputEvent { value = currentInput } <- it = 
              let errorMsg = either Just (const Nothing) (parse (Left currentInput))
              in yield (go PromptState {..})

        submit :: Producer a => IO ()
        submit =
          case parse (Right currentInput) of
            Right a -> yield a
            Left v  -> yield (go PromptState { errorMsg = Just v, .. })

reader :: forall a. Read a => Bool -> Parser a
reader live ett
  | live || isRight ett = 
      either (Left . txt) Right (readEither @a (either fromTxt fromTxt ett))
  | otherwise = 
      Left Null

read :: forall a. (Typeable a, Read a) => (Web => a)
read = prompt (txt (rep @a)) (Web.reader False)

{-# NOINLINE rep #-}
rep :: forall p. (Typeable p) => Txt
rep = go (typeRep (Proxy @p))
  where
    go tr =
      let tc = toTxt (Show.show (typeRepTyCon tr))
          trs = typeRepArgs tr
      in Txt.intercalate " " (tc : fmap go trs)

dup :: a -> (a,a)
dup !a = (a,a)

show :: Show a => a -> (Web => b)
show !a = become (txt (Show.show a))
