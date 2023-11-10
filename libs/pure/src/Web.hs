{-# language PatternSynonyms, DerivingVia, ViewPatterns, BlockArguments, ScopedTypeVariables, RankNTypes, DuplicateRecordFields, NamedFieldPuns, ConstraintKinds, FlexibleContexts, GADTs, TypeApplications, BangPatterns, AllowAmbiguousTypes, CPP, PatternSynonyms, OverloadedStrings, RecordWildCards, LambdaCase, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, DefaultSignatures, TypeOperators, TupleSections #-}
{-# OPTIONS_GHC -O2 #-}
module Web (pattern Route,Routed,withRoute,Web.route,routed,Stencil(),ToPath(..),FromPath(..),ui,ui_,Web.clear,done,Void,Web,play',play,fixp,play_,fixp_,animate,become,become_,Web.error,Web.void,Web.fix,run,Web.read,Web.show,prompt,Web.reader,Parser,link) where

import Control.Concurrent hiding (yield)
import qualified Data.Function as F
import Data.Default
import Data.DOM
import Data.Foldable
import Data.Either (isRight)
import Data.Events (pattern OnSubmitWith, intercept)
import Data.Maybe
import Data.Router
import Data.View.Build
import Data.View
import Data.Void
import Data.Txt as Txt
import Data.Typeable
import Data.HTML hiding (pattern Output,pattern Input)
import qualified Data.HTML as HTML
import System.IO.Unsafe
import Text.Read
import GHC.Show as Show
import Web.Events
import Control.Exception (Exception(..),SomeException(..),catch,handle,AsyncException(ThreadKilled))
import Control.DeepSeq
import Data.Animation
import System.Mem
import Data.View.Build (diffDeferred)
import Data.List as List
import Data.IORef
import Control.Monad (when,void)
import Data.Coerce
import GHC.Exts
import Data.String
import Control.Parallel (pseq)
import qualified Pure

import Control.Applicative
import Control.Monad.Cont
import Control.Monad.Cont.Class
import Control.Monad.Except
import Control.Monad.Error.Class
import Control.Monad.Fix
import Control.Monad.IO.Class
import Control.Monad.Reader
import Control.Monad.Reader.Class
import Control.Monad.State
import Control.Monad.State.Class
import Control.Monad.Trans.Class as T
import Control.Monad.Writer
import Control.Monad.Writer.Class
import Data.Functor.Identity
import Data.JSON (ToJSON,FromJSON)
import Data.Time


data OI = Input View | Output (MVar ()) View
type Web = Producer OI

{-# INLINE fixp_ #-}
fixp_ :: Typeable a => (Web => a -> a) -> View
fixp_ f = play_ (F.fix f)

{-# INLINE fixp #-}
fixp :: Typeable a => (Web => a -> a) -> (Producer a => View)
fixp f = play (F.fix f)

{-# INLINE play_ #-}
play_ :: forall a. Typeable a => (Web => a) -> View
play_ a = discard @a (play' @a a) 

{-# INLINE play #-}
play :: Typeable a => (Web => a) -> (Producer a => View)
play a = play' a


{-# NOINLINE play' #-}
play' :: forall a. Typeable a => (Web => a) -> (Producer a => View)
play' (Proof -> a) = flip component a $ \self -> 
  let 
    setView :: OI -> IO ()
    setView = Control.Monad.void . \case
      Input v -> modifyref self (\_ (_,tid) -> (v,tid))
      Output mv v -> modifyref self (\_ (_,tid) -> unsafePerformIO (putMVar mv ()) `pseq` (v,tid))

    setter :: Producer a => Web |- a -> IO ThreadId
    setter a = forkIO do
      Control.Exception.catch (yield $! consume setView (prove a)) \(se :: SomeException) -> 
        case fromException se of
          Just ThreadKilled -> pure ()
          _ -> setView (Input (txt (Show.show se)))
  in 
    def
      { construct = do
        a <- askref self
        tid <- setter a
        pure (Null,tid)
      , onReceive = \a (v,old) -> do
        killThread old
        tid <- setter a
        pure (v,tid)
      , render = \a (v,_) -> v
      }

{-# NOINLINE animate #-}
animate :: a -> a
animate a = 
  unsafePerformIO do
    mv <- newEmptyMVar
    addAnimation do
      a `seq` putMVar mv ()
    takeMVar mv
    pure a

{-# NOINLINE become #-}
become :: (Producer a => View) -> (Web => a)
become v = unsafePerformIO do
  mv <- newEmptyMVar
  -- this deepseq is important!
  let send v = v `deepseq` yield (Input v) 
  v' <- handle (\(se :: SomeException) -> send (txt (Show.show se)))
          (send (consume (putMVar mv) v))
  a <- takeMVar mv
  pure a

{-# NOINLINE become_ #-}
become_ :: View -> (Web => ())
become_ v = unsafePerformIO do
  mv <- newEmptyMVar
  -- this deepseq is important!
  let send v = v `deepseq` yield (Output mv v)
  handle (\(se :: SomeException) -> send (txt (Show.show se))) (send v)
  x <- takeMVar mv
  x `seq` pure ()

{-# INLINE error #-}
error :: Txt -> (Web => a)
error = become . fromTxt

{-# INLINE void #-}
void :: View -> (Web => Void)
void = become

runner :: MVar (View,View) -> (Web => a) -> a
runner st a = consume builder a
  where
    builder :: OI -> IO ()
    builder (Input new) =
      takeMVar st >>= \(old,mid) -> do
        v <- diff old mid new
        putMVar st (v,new)
    builder (Output mv new) =
      takeMVar st >>= \(old,mid) -> do
        v <- diff old mid new
        putMVar mv ()
        putMVar st (v,new)

    runPlan :: [IO ()] -> IO ()
    runPlan plan = let !p = sequence_ (List.reverse plan) in p

    diff old mid new = do
      mtd <- newIORef []
      let
        (!plan,!plan',!new_old) = buildPlan $ \p p' -> 
            diffDeferred mtd p p' old mid new
        
      mounts <- plan `seq` plan' `seq` readIORef mtd

      runPlan plan
      runPlan plan'
      runPlan mounts
      
      pure new_old

{-# INLINE anon #-}
anon :: Node -> (Web => a) -> a
anon n a = consume builder a
  where
    {-# NOINLINE st #-}
    st :: MVar (Maybe (View,View))
    st = unsafePerformIO (newMVar Nothing)
      
    builder :: OI -> IO ()
    builder (Input new) =
      takeMVar st >>= \case
        Nothing -> do
          v <- inject n new
          putMVar st (Just (v,new))
        Just (old,mid) -> do
          v <- diff old mid new
          putMVar st (Just (v,new))
    builder (Output mv new) =
      takeMVar st >>= \case
        Nothing -> do
          v <- inject n new
          putMVar mv ()
          putMVar st (Just (v,new))
        Just (old,mid) -> do
          v <- diff old mid new
          putMVar mv ()
          putMVar st (Just (v,new))

    runPlan :: [IO ()] -> IO ()
    runPlan plan = let !p = sequence_ (List.reverse plan) in p

    diff old mid new = do
      mtd <- newIORef []
      let
        (!plan,!plan',!new_old) = buildPlan $ \p p' -> 
            diffDeferred mtd p p' old mid new
        
      mounts <- plan `seq` plan' `seq` readIORef mtd

      runPlan plan
      runPlan plan'
      runPlan mounts
      
      pure new_old

{-# INLINE fix #-}
fix :: (Web => a -> a) -> a
fix f = F.fix \a -> anon (toNode body) (f a) 

-- this inline is important
{-# INLINE run #-}
run :: (Web => IO a) -> IO a
run wa = do
  let n = toNode body
      v0 = Null
  v <- inject n v0
  mv <- newMVar (v,v0) 
  a <- runner mv wa
  () <- a `seq` (pure $! runner mv Web.clear)
  pure a

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
        HTML.Form <| OnSubmitWith intercept (\_ -> submit) |> 
          [ P <||> [ p , inputField ]
          , maybe Null id errorMsg
          , Button <| Type "submit" |> [ "Submit" ]
          ]
      where
        inputField :: Producer a => View
        inputField =
            HTML.Input <| keyDowns keyDown . inputs input . Value currentInput . OnMounted mounted
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

{-# INLINE ui #-}
ui :: ((Web, Producer (m a)) => View) -> (Web => m a)
ui v = become v

{-# INLINE ui_ #-}
ui_ :: Applicative m => (Web => View) -> (Web => m ())
ui_ v = pure $! become_ v

{-# INLINE clear #-}
clear :: Web => ()
clear = let !x = become_ Null in x `pseq` ()

{-# INLINE done #-}
done :: Web => ()
done = Web.clear

newtype Stencil = Stencil Txt
  deriving (ToTxt,FromTxt,ToJSON,FromJSON,IsString) via Txt
instance Eq Stencil where
  (==) (Stencil s1) (Stencil s2) = simplify s1 == simplify s2
    where
      simplify = fmap (Txt.dropWhile (/= '/')) . Txt.splitOn "/:"

instance FromPath a => IsString (Txt -> Maybe (Txt,a)) where
  fromString s = fromPath (fromString s)

instance FromPath b => IsString (Maybe (Txt,a) -> Maybe (Txt,b)) where
  fromString s (Just (t,_)) = fromString s t
  fromString _ _ = Nothing

instance ToPath a => IsString (a -> Txt) where
  fromString s = snd . toPath (fromString s)

class FromPath a where
  fromPath :: Stencil -> Txt -> Maybe (Txt,a)
  default fromPath :: FromTxt a => Stencil -> Txt -> Maybe (Txt,a)
  fromPath (Stencil s) u =
    case stencil s u of
      Just (rest,[(_,a)]) -> Just (rest,fromTxt (Pure.decodeURIComponent a))
      Just res -> Prelude.error "fromPath(txt): Stencil and type mismatch."
      _ -> Nothing

instance FromPath () where
  fromPath s x = Just (x,())
instance FromPath Txt
instance FromPath String

instance (FromPath x, FromPath y) => FromPath (x,y) where
  fromPath (Stencil s) u =
    case stencil s u of
      Just (rest,[(_,y),(_,x)]) 
        | Just (_,x') <- fromPath "/:x" ("/" <> x)
        , Just (_,y') <- fromPath "/:y" ("/" <> y)
        -> Just (rest,(x',y'))

      Just _ 
        -> Prelude.error "fromPath(x,y): Stencil and type mismatch."

      _ -> Nothing

instance (FromPath x, FromPath y, FromPath z) => FromPath (x,y,z) where
  fromPath (Stencil s) u =
    case stencil s u of
      Just (rest,[(_,z),(_,y),(_,x)]) 
        | Just (_,x') <- fromPath "/:x" ("/" <> x)
        , Just (_,y') <- fromPath "/:y" ("/" <> y)
        , Just (_,z') <- fromPath "/:z" ("/" <> z)
        -> Just (rest,(x',y',z'))

      Just _ 
        -> Prelude.error "fromPath(x,y,z): Stencil and type mismatch."

      _ -> Nothing

class ToPath a where
  toPath :: Stencil -> a -> (Stencil,Txt)
  default toPath :: ToTxt a => Stencil -> a -> (Stencil,Txt)
  toPath s a = fromMaybe (s,def) (Web.fill s (Pure.encodeURIComponent (toTxt a)))

instance ToPath Txt
instance ToPath String
instance (ToPath x, ToPath y) => ToPath (x,y) where
  toPath s0 (x,y) =
    let (s1,tx) = toPath s0 x 
        (s2,ty) = toPath s1 y
    in (s2,tx <> ty)
instance (ToPath x, ToPath y, ToPath z) => ToPath (x,y,z) where
  toPath s0 (x,y,z) =
    let (s1,txy) = toPath s0 (x,y) 
        (s2,tz) = toPath s1 z
    in (s2,txy <> tz)

fill :: Stencil -> Txt -> Maybe (Stencil,Txt)
fill (Stencil s) t =
  case Txt.breakOn "/:" s of
    (x,"") -> Just (Stencil "","/" <> t)
    (before,Txt.dropWhile (/= '/') . Txt.tail -> after) -> 
      Just (Stencil after,before <> "/" <> t)

pattern Route :: r -> Maybe (Txt,r)
pattern Route r <- Just (_,r)

newtype R = R Txt
withRoute :: (Pure.State R => View) -> View
withRoute = stateWith (const pure) initialize
  where
    initialize :: Pure.Modify R => IO (R,R -> IO ())
    initialize = do
      w    <- getWindow
      stop <- onRaw (toNode w) "popstate" def \_ _ -> getPathname >>= Data.View.put . R
      path <- getPathname
      pure (R path,\_ -> stop)

type Routed = Exists R
route :: Routed => Txt
route = let R r = it in r

routed :: (Routed => View) -> IO ()
routed v = Pure.run (withRoute v)

link :: Txt -> View -> View
link = clicks . goto