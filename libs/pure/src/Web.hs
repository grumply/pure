{-# language PatternSynonyms, DerivingVia, ViewPatterns, BlockArguments, ScopedTypeVariables, RankNTypes, DuplicateRecordFields, NamedFieldPuns, ConstraintKinds, FlexibleContexts, GADTs, TypeApplications, BangPatterns, AllowAmbiguousTypes, CPP, PatternSynonyms, OverloadedStrings, RecordWildCards, LambdaCase, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, DefaultSignatures, TypeOperators, TupleSections #-}
{-# OPTIONS_GHC -O2 #-}
module Web (Web.dot,dot',dotWith,Web.at,at',atWith,Web.every,url,Web.path,Path(..),embed,pattern Route,Routed,routed,Stencil(),ToPath(..),FromPath(..),Web.clear,done,Void,Web,play_,play,playIO,animate,become,become_,Web.error,Web.void,Web.fix,run,Web.read,Web.show,prompt,Web.reader,Parser,link) where

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
import Data.JSON (ToJSON,FromJSON,traceJSON)
import Data.Time
import Debug.Trace
import Data.Default
import qualified Data.Variance as Var

{-# NOINLINE embed #-}
embed :: (Web => a) -> (Producer a => View)
embed wa = SimpleHTML "template" <| OnMounted go 
  where
    go node = do
      om <- newMVar (NullView (Just (coerce node)),Null)
      tid <- forkIO do
        tid <- myThreadId
        let !a = consume (work om) wa
        yield a
        (old,mid) <- takeMVar om
        v <- diff old mid Null
        putMVar om (v,Null)
      pure do
        killThread tid 
        (old,_) <- takeMVar om 
        traverse_ removeNode (getHost old)
        cleanup old
        
    work :: MVar (View,View) -> OI -> IO ()
    work mv = \case
      Input new -> do
        (old,mid) <- takeMVar mv
        v <- diff old mid new
        putMVar mv (v,new)
      Output mr new -> do
        (old,mid) <- takeMVar mv
        v <- diff old mid new
        putMVar mv (v,new)
        putMVar mr ()

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


data OI = Input View | Output (MVar ()) View
type Web = Producer OI

{-# NOINLINE play_ #-}
play_ :: forall a. Typeable a => (Web => a) -> View
play_ (Proof -> a) = flip component (a :: Web |- a) $ \self -> 
  let 
    setView :: OI -> IO ()
    setView oi = modifyrefM_ self \_ (_,tid) -> 
      pure $ case oi of
        Input v     -> ((v,tid),def)
        Output mv v -> ((v,tid),putMVar mv ())

  in 
    def
      { construct = pure (Null,Nothing)
      , onExecuting = \_ -> do
        Proof a <- askref self 
        tid <- forkIO (consume setView (a :: Web => a) `seq` pure ())
        pure (Null,Just tid)
      , onReceive = \(Proof a) (v,old) -> do
        traverse_ killThread old
        tid <- forkIO (consume setView (a :: Web => a) `seq` pure ())
        pure (v,Just tid)
      , onUnmounted = getref self >>= \(_,mtid) -> traverse_ killThread mtid 
      , render = \_ -> fst
      }


{-# NOINLINE play #-}
play :: forall a. Typeable a => (Web => a) -> (Producer a => View)
play (Proof -> a) = flip component (a :: Web |- a) $ \self -> 
  let 
    setView :: OI -> IO ()
    setView oi = modifyrefM_ self \_ (_,tid) -> 
      pure $ case oi of
        Input v     -> ((v,tid),def)
        Output mv v -> ((v,tid),putMVar mv ())

  in 
    def
      { construct = pure (Null,Nothing)
      , onExecuting = \_ -> do
        Proof a <- askref self 
        tid <- forkIO (yield $ consume setView (a :: Web => a))
        pure (Null,Just tid)
      , onReceive = \(Proof a) (v,old) -> do
        traverse_ killThread old
        tid <- forkIO (yield $ consume setView (a :: Web => a))
        pure (v,Just tid)
      , onUnmounted = getref self >>= \(_,mtid) -> traverse_ killThread mtid 
      , render = \_ -> fst
      }

{-# NOINLINE playIO #-}
playIO :: forall a. Typeable a => (Web => IO a) -> (Producer a => View)
playIO (Proof -> a) = flip component (a :: Web |- IO a) $ \self -> 
  let 
    setView :: OI -> IO ()
    setView oi = modifyrefM_ self \_ (_,tid) -> 
      pure $ case oi of
        Input v     -> ((v,tid),def)
        Output mv v -> ((v,tid),putMVar mv ())
  in 
    def
      { construct = pure (Null,Nothing)
      , onExecuting = \_ -> do
        Proof a <- askref self 
        tid <- forkIO (consume setView (a :: Web => IO a) >>= yield)
        pure (Null,Just tid)
      , onReceive = \(Proof a) (v,_) -> do
        tid <- forkIO (consume setView (a :: Web => IO a) >>= yield)
        pure (v,Just tid)
      , onUnmounted = getref self >>= \(_,mtid) -> traverse_ killThread mtid 
      , render = \_ -> fst
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
  handle (\(se :: SomeException) -> send (txt (Show.show se))) (send (consume (putMVar mv) v))
  a <- takeMVar mv
  a `pseq` pure a

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

runner :: IORef (View,View) -> (Web => a) -> a
runner st a = consume builder a
  where
    builder :: OI -> IO ()
    builder (Input new) =
      readIORef st >>= \(old,mid) -> do
        v <- diff old mid new
        writeIORef st (v,new)
    builder (Output mv new) =
      readIORef st >>= \(old,mid) -> do
        v <- diff old mid new
        -- putMVar mv ()
        writeIORef st (v,new)

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
    st :: IORef (Maybe (View,View))
    st = unsafePerformIO (newIORef Nothing)
      
    builder :: OI -> IO ()
    builder (Input new) =
      readIORef st >>= \case
        Nothing -> do
          v <- inject n new
          writeIORef st (Just (v,new))
        Just (old,mid) -> do
          v <- diff old mid new
          writeIORef st (Just (v,new))
    builder (Output mv new) =
      readIORef st >>= \case
        Nothing -> do
          v <- inject n new
          putMVar mv ()
          writeIORef st (Just (v,new))
        Just (old,mid) -> do
          v <- diff old mid new
          putMVar mv ()
          writeIORef st (Just (v,new))

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
{-# NOINLINE run #-}
run :: forall a. (Web => a) -> a
run wa = unsafePerformIO do
  let n = toNode body
      v0 = Null
  v <- inject n v0
  mv <- newIORef (v,v0) 
  let !a = runner mv wa
  let !b = runner mv Web.clear
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

-- instance FromPath b => IsString (Maybe (Txt,a) -> Maybe (Txt,b)) where
--   fromString s (Just (t,_)) = fromString s (R t)
--   fromString _ _ = Nothing

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
  fromPath (Stencil s) x =
    case stencil s x of
      Just (rest,_) -> Just (rest,())
      _ -> Nothing
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

instance ToPath () where
  toPath s () = fromMaybe (s,def) (Web.fill s "")
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
    (x,"") -> Just (Stencil "",x <> t)
    (before,Txt.dropWhile (/= '/') . Txt.tail -> after) -> 
      Just (Stencil after,before <> "/" <> t)

pattern Route :: Path -> r -> Maybe (Path,r)
pattern Route rest r <- Just (rest,r) where
  Route rest r = Just (rest,r)

data Path = Path { _path :: {-# UNPACK #-}!Txt }
data R a = R { _unconsumed :: {-# UNPACK #-}!Path, _active :: a }
-- withRoute :: (Exists R => a) -> a
-- withRoute = stateWith (const pure) initialize
--   where
--     initialize :: Pure.Modify R => IO (R,R -> IO ())
--     initialize = do
--       w    <- getWindow
--       stop <- onRaw (toNode w) "popstate" def \_ _ -> getPathname >>= Data.View.put . R
--       path <- getPathname
--       pure (R path,\_ -> stop)

instance FromPath a => IsString (Path -> Maybe (Path,a)) where
  fromString s (Path r) = fmap (\(t,a) -> (Path t,a)) $! fromPath (fromString s) r

url :: Path
url = Path (unsafePerformIO getPathname)

path :: Exists Path => Path
path = it

type Routed = Exists Path

-- We have to constrain the type somehow or GHC's evaluation won't be
-- productive. To that end, we use `Producer a => View`.
-- Keep an eye on this, as we might need `a -> (Web => IO a)`
{-# NOINLINE routed #-}
routed :: forall a. ((Routed,Producer a) => View) -> (Web => a)
routed ra = unsafePerformIO do
  w <- getWindow 
  mv <- newEmptyMVar
  stop <- onRaw (toNode w) "popstate" def \_ _ -> do
    pn <- getPathname 
    let a = become (with (Path pn) ra)
    a `pseq` putMVar mv a
  tid <- forkIO do
    pn <- getPathname
    let a = become (with (Path pn) ra)
    a `pseq` putMVar mv a
  a <- takeMVar mv
  stop
  killThread tid
  pure a

link :: Txt -> View -> View
link = clicks . goto

{-# INLINE every #-}
every :: forall a. Time -> (Exists Time => a) -> a
every t a = go 
  where 
    {-# NOINLINE go #-}
    go | !a <- delayed t (let t = unsafePerformIO time in t `seq` with t a)
       = go

{-# INLINE at #-}
at :: forall a. [Time] -> (Exists Time => a) -> a
at = atWith False False 

{-# INLINE at' #-}
at' :: forall a. [Time] -> (Exists Time => a) -> a
at' = atWith True False 
 
{-# INLINE dot #-}
-- dot, as in: on-the-dot
dot :: forall a. Time -> (Exists Time => a) -> a
dot = dotWith False False

{-# INLINE dot' #-}
dot' :: forall a. Time -> (Exists Time => a) -> a
dot' = dotWith True False

{-# INLINE dotWith #-}
dotWith :: forall a. Bool -> Bool -> Time -> (Exists Time => a) -> a
dotWith compensate truth t = 
  let 
    fromInt = fromIntegral @Int
    now = unsafePerformIO time
    d = fromInt (floor (now / t) :: Int)
    start = d * t
  in 
    atWith compensate truth [ start + fromInt n * t | n <- [1 :: Int ..] ]

{-# INLINE atWith #-}
atWith :: forall a. Bool -> Bool -> [Time] -> (Exists Time => a) -> a
atWith compensate truth ts a = go ts 0
  where 
    {-# NOINLINE go #-}
    go (t:ts) !err
      | !s <- unsafePerformIO time
      , () <- unsafePerformIO (delay (t - s - Milliseconds err 0))
      , !e <- if compensate then unsafePerformIO time else t
      , Milliseconds ms _ <- e - t
      , e' <- if compensate then err / 2 + ms / 2 else err
      , n  <- if truth then e else t
      = if List.null ts then with n a else with n a `seq` go ts e'

