{-# language PatternSynonyms, DerivingVia, ViewPatterns, BlockArguments, ScopedTypeVariables, RankNTypes, DuplicateRecordFields, NamedFieldPuns, ConstraintKinds, FlexibleContexts, GADTs, TypeApplications, BangPatterns, AllowAmbiguousTypes, CPP, PatternSynonyms, OverloadedStrings, RecordWildCards, LambdaCase, FlexibleInstances, MultiParamTypeClasses, UndecidableInstances, DefaultSignatures, TypeOperators, TupleSections #-}
{-# OPTIONS_GHC -O2 #-}
module Web (Web.dot,dotWith,Web.at,atWith,Web.every,url,Web.path,Path(..),pattern Route,Routed,routed,Stencil(),ToPath(..),FromPath(..),Web.clear,Void,Web,play_,play,playIO,animate,become,become_,Web.error,halt,Web.fix,run,Web.read,Web.show,prompt,Web.reader,Parser,link) where

import Control.Concurrent hiding (yield)
import qualified Data.Function as F
import Data.Default
import Data.DOM
import Data.Foldable
import Data.Either (isRight)
import Data.Events (pattern OnSubmitWith, intercept)
import Data.Key
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
import Data.List as List
import Data.IORef
import Control.Monad (when,void)
import GHC.Exts
import Control.Parallel (pseq)
import qualified Pure

import Control.Applicative
import Data.JSON (ToJSON,FromJSON,traceJSON)
import Data.Time
import qualified Data.Variance as Var

-- The view-embedding commands. `Input` is used for `become`, which
-- has an associated `Producer` context, and `Output` is used for
-- `become_` which returns as soon as the view has been reconciled.
data OI = Input View | Output (MVar ()) View

-- | The driving context that allows for embedding of `View`s via `become`.
-- Declaring a `Web` constraint is equivalent to saying that there must be
-- a context in which `View`s can be displayed in order to evaluate the 
-- constrained code. The `Web` context is introduced via either `run` or
-- `play` (or its specializations, `play_` and `playIO`), and used via
-- `become` and `become_` and their derivatives (`clear`, `halt`, `show`,
-- etc....).
type Web = Producer OI

-- | Embed a `Web =>` context to construct a producing `View`.
--
-- ## Example usage
--
-- > someView :: Producer () => View
-- > someView = play (countdown 10 ())
-- >
-- > countdown :: forall a. Web => Int -> a -> a
-- > countdown n a = go n
-- >   where
-- >     go n | n < 1 = a 
-- >          | otherwise = become (Button <| onClick (go (n - 1)) |> [ txt n ])
-- 
-- In this example, `countdown` displays a button that, after being clicked `n` 
-- times, becomes the supplied `a`. Due to parametricity, the `a` can only be 
-- modified with respect to its evaluation, not its value. That is, only 
-- parametrically-polymorphic functions like `id` or `seq` can be applied to
-- the `a`. While seemingly immaterial, this is a powerful feature that allows
-- for a generic continuation-based interface where the `a` is the result of
-- some other view (perhaps, even, another `countdown`).
--
-- Via constraints, the return value can be modified:
--
-- > incrementer :: (Web, MonadState Int m) => m a -> m a
-- > incrementer ma = become do
-- >   Button <| onClick (modify (+(1::Int)) >> ma)) |> 
-- >     [ "Increment" ]
--
-- For views that don't produce values:
--
-- > clock :: Web => a
-- > clock = every Second (become (txt (RFC3339 it)))
-- 
-- Note that `clock` says it can be any value you want, as long as you give it
-- a `Web` context. Since arbitrary values cannot be witnessed (other than via
-- bottom), this is equivalent to saying that `clock` never returns. A common
-- way to use this is to specialize `a` to `Void`:
--
-- > _ :: View
-- > _ = consume absurd (play clock)
--
-- ## Careful use
-- Careful use of `play` is important because nested uses can result in `Web =>` 
-- constraint satisfaction before the value is sent to `play`. For instance, in:
--
-- > Web.run (play (become "some view"))
--
-- Note that `Web.run` is somewhat like `Pure.run (Web.play ...)`, but gives
-- pure access to the return value by specializing the embedding context to
-- <body>. The `become` in this case yields to `run`, not `play`, so you must
-- encapsulate the `Web =>` context:
--
-- > someView :: View
-- > someView = play (become "some view")
--
-- `someView` doesn't expose a `Web =>` dependency, so `become` is satisfied by
-- the local `play`.
-- 
-- In the failing example `Web.run ...`, if the `...` were not a direct call to 
-- `play . become`, and instead were a complex `View` tree, that tree would be
-- overwritten by the `become` call.
--
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

-- | A convenient specialization of `play` that discards the result. 
{-# NOINLINE play_ #-}
play_ :: forall a. Typeable a => (Web => a) -> View
play_ a = consume (\(!(_ :: a)) -> pure ()) (play a)

-- | A convenient specialization of `play` to a type of `IO a` such that the
-- `View` still produces an `a`, rather than an `IO a`.
{-# NOINLINE playIO #-}
playIO :: forall a. Typeable a => (Web => IO a) -> (Producer a => View)
playIO ioa = consume (>>= yield @a) (play ioa)

-- | Force a value to WHNF in an animation frame. This is useful for
-- wrapping `become` to guarantee that reconciliation is performed within
-- an animation frame.
{-# NOINLINE animate #-}
animate :: a -> a
animate a = 
  unsafePerformIO do
    mv <- newEmptyMVar
    addAnimation do
      a `seq` putMVar mv ()
    takeMVar mv
    pure a

-- | Display the given `View` in the local `Web =>` context. The result is
-- the result of the first `yield` within the `View`. This is the magic in `Web`
-- that allows a producing `View` to be treated as the first value that it
-- produces. This results in arbitrary composability - `become` can be performed
-- anywhere, and the result (and, thus, the effects) is produced on-demand. 
-- `become`, therefore, allows for inverting the usual scheme of designing
-- applications within the context of a UI, and instead allows for designing
-- UI in the context of applications. The usual caveats about laziness and space
-- apply, but Pure implicitly limits some of its impact by decoupling consumers
-- and producers: consumers supply a callback rather than waiting around for a
-- produced value. That is, a recursively-called `become` doesn't keep a live
-- chain of contexts. This results in a sort of tail-call optimization, in some
-- cases.
--
-- ## Evaluation
-- `become` fully forces the `View` before yielding, which forces the evaluation
-- of all associated `Web =>` effects, like displaying views that produce results
-- that the `become`ing view depends upon. This is how we achieve safe and well-
-- ordered dependency resolution, but note that we do not guarantee any particular
-- ordering, just a guarantee of productivity (up to bottom). 
--
{-# NOINLINE become #-}
become :: (Producer a => View) -> (Web => a)
become v = unsafePerformIO do
  mv <- newEmptyMVar
  -- this deepseq is important!
  let send v = v `deepseq` yield (Input v) 
  handle (\(se :: SomeException) -> send (txt (Show.show se))) (send (consume (putMVar mv) v))
  a <- takeMVar mv
  a `pseq` pure a

-- | Unlike `become`, `unsafeLazyBecome` is more lazy (and potentially 
-- application-breakingly so) than `become` by not forcing any `Web =>` thunks
-- out of the producing `View` before yielding it to the reconciler. This is a
-- low-level optimization and should only be used when you're absolutely
-- certain that the `View` doesn't use `Web =>` before being embedded. All this
-- function does is omit the `deepseq` of the given `View`, which, in some
-- cases, could be an optimization.
{-# NOINLINE unsafeLazyBecome #-}
unsafeLazyBecome :: (Producer a => View) -> (Web => a)
unsafeLazyBecome v = unsafePerformIO do
  mv <- newEmptyMVar
  let send v = yield (Input v) 
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

-- | Display some textual error. This effectively halts any computations being
-- performed within the `Web =>` context. Equivalent to `halt . fromTxt`.
{-# INLINE error #-}
error :: Txt -> (Web => a)
error = halt . fromTxt

-- | Turn a `View` into any desired type. This must be the last view that this
-- context displays.
{-# INLINE halt #-}
halt :: View -> (Web => a)
halt = become

-- | Show a `Show`-able value in a `Web =>` context.
{-# INLINE show #-}
show :: Show a => a -> (Web => b)
show !a = halt (txt (Show.show a))

-- | Clear the `Web =>` context.
{-# INLINE clear #-}
clear :: Web => ()
clear = let !x = become_ Null in x `pseq` ()

-- I'm not sure about this one, yet. Perhaps it isn't even necessary, since we
-- have `run` and `Data.Function.fix`. But, the existence is here to remind me
-- that this approach might be possible, and to keep the idea in mind when 
-- developing.
{-# INLINE fix #-}
fix :: (Web => a -> a) -> a
fix f = F.fix \a -> anon (toNode body) (f a) 
  where
    {-# INLINE anon #-}
    anon :: forall a. Node -> (Web => a) -> a
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

-- | Evaluate (up to WHNF) an `a` within <body>. This is expected to force out
-- any uses of `become`. When the value is fully evaluated to WHNF, any
-- associated view will be removed from the <body> before the `a` is returned.
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
  where
    runner :: forall a. IORef (View,View) -> (Web => a) -> a
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

data PromptState = PromptState 
  { currentInput :: Txt
  , errorMsg :: Maybe View
  }

type Parser a = Either Txt Txt -> Either View a

-- | Read an `a` from an <input> within some `Web =>` context.
-- 
-- ## Usage
--
-- > go :: Web => _
-- > go | n <- prompt "Number" (reader @Int False) = _
--
-- In this example, `go` will display a prompt for a number and then continue
-- evaluation of `_` after the user either presses the submit button or `Enter`
-- with a valid number entered. On read failure, the read error will be displayed.
--
-- The input is automatically focused on mount. Submit is only possible with a valid
-- value.
--
-- This is a quick-and-dirty approach to writing interactive utilities and exists,
-- partly, as a demonstration of `Web =>`.
--
prompt :: forall a. View -> Parser a -> (Web => a)
prompt p parse = go (PromptState mempty Nothing)
  where
    go :: PromptState -> a
    go PromptState {..} = 
      become do
        HTML.Form <| OnSubmitWith intercept (const submit) |> 
          [ P <||> [ p , inputField ]
          , fromMaybe Null errorMsg
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

-- | A basic `Read`-based `Parser` for use with `prompt`. If the supplied `Bool`
-- is true, the input value is parsed on every change. If false, parsing is only
-- attempted before submission.
reader :: forall a. Read a => Bool -> Parser a
reader live ett
  | live || isRight ett = 
      either (Left . txt) Right (readEither @a (either fromTxt fromTxt ett))
  | otherwise = 
      Left Null

-- | A composed version of `prompt` that uses a type-based representation of
-- the prompt label and uses a non-live `reader`.
--
-- # Usage
--
-- > go :: Web => _
-- > go | n <- read @Int = _
-- 
-- Or:
--
-- > someMultiArgumentFunction :: Int -> Double -> MyReadableType -> _
-- >
-- > test :: Web => _
-- > test = someMultiArgumentFunction read read read
--
-- In this case, each `read` will be embedded based on evaluation order, which,
-- without strictness annotations, is purely up to the compiler, and changes to
-- optimization level or application code can change the ordering.
--
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

newtype Stencil = Stencil Txt
  deriving (ToTxt,FromTxt,ToJSON,FromJSON,IsString) via Txt
instance Eq Stencil where
  (==) (Stencil s1) (Stencil s2) = simplify s1 == simplify s2
    where
      simplify = fmap (Txt.dropWhile (/= '/')) . Txt.splitOn "/:"

instance ToPath a => IsString (a -> Txt) where
  fromString s = snd . toPath (fromString s)

class FromPath a where
  fromPath :: Stencil -> Txt -> Maybe (Txt,a)
  default fromPath :: Read a => Stencil -> Txt -> Maybe (Txt,a)
  fromPath (Stencil s) u = do
    (rest,[(_,t)]) <- stencil s u 
    a <- readMaybe (fromTxt t)
    pure (rest,a)
instance FromPath () where
  fromPath (Stencil s) u = do
    (rest,_) <- stencil s u
    pure (rest,())
instance FromPath Txt where
  fromPath (Stencil s) u = do
    (rest,[(_,t)]) <- stencil s u
    pure (rest,t)
instance FromPath String where
  fromPath (Stencil s) u = do
    (rest,[(_,t)]) <- stencil s u
    pure (rest,fromTxt t)
instance FromPath (Marker a) where
  fromPath (Stencil s) u = do
    (rest,[(_,t)]) <- stencil s u
    pure (rest,fromTxt t)
instance FromPath Key where
  fromPath (Stencil s) u = do
    (rest,[(_,t)]) <- stencil s u
    pure (rest,fromTxt t)

instance (FromPath l, FromPath r) => FromPath (Either l r) where
  fromPath s u = do (fmap Right <$> fromPath s u) <|> (fmap Left <$> fromPath s u)
instance FromPath i => FromPath [i] where
  fromPath s u = go u [] where
    go u xs
      | Just (rest,a) <- fromPath s u = go rest (a : xs)
      | otherwise = Just (u,List.reverse xs)

instance (FromPath x, FromPath y) => FromPath (x,y) where
  fromPath (Stencil s) u = do
    (rest,[(_,y),(_,x)]) <- stencil s u 
    (_,x') <- fromPath "/:x" ("/" <> x)
    (_,y') <- fromPath "/:y" ("/" <> y)
    pure (rest,(x',y'))
instance (FromPath x, FromPath y, FromPath z) => FromPath (x,y,z) where
  fromPath (Stencil s) u = do
    (rest,[(_,z),(_,y),(_,x)]) <- stencil s u
    (_,x') <- fromPath "/:x" ("/" <> x)
    (_,y') <- fromPath "/:y" ("/" <> y)
    (_,z') <- fromPath "/:z" ("/" <> z)
    pure (rest,(x',y',z'))
instance (FromPath w, FromPath x, FromPath y, FromPath z) => FromPath (w,x,y,z) where
  fromPath (Stencil s) u = do
    (rest,[(_,z),(_,y),(_,x),(_,w)]) <- stencil s u
    (_,w') <- fromPath "/:w" ("/" <> w)
    (_,x') <- fromPath "/:x" ("/" <> x)
    (_,y') <- fromPath "/:y" ("/" <> y)
    (_,z') <- fromPath "/:z" ("/" <> z)
    pure (rest,(w',x',y',z'))
instance (FromPath v, FromPath w, FromPath x, FromPath y, FromPath z) => FromPath (v,w,x,y,z) where
  fromPath (Stencil s) u = do
    (rest,[(_,z),(_,y),(_,x),(_,w),(_,v)]) <- stencil s u
    (_,v') <- fromPath "/:v" ("/" <> v)
    (_,w') <- fromPath "/:w" ("/" <> w)
    (_,x') <- fromPath "/:x" ("/" <> x)
    (_,y') <- fromPath "/:y" ("/" <> y)
    (_,z') <- fromPath "/:z" ("/" <> z)
    pure (rest,(v',w',x',y',z'))
instance (FromPath u, FromPath v, FromPath w, FromPath x, FromPath y, FromPath z) => FromPath (u,v,w,x,y,z) where
  fromPath (Stencil s) u = do
    (rest,[(_,z),(_,y),(_,x),(_,w),(_,v),(_,u)]) <- stencil s u
    (_,u') <- fromPath "/:u" ("/" <> u)
    (_,v') <- fromPath "/:v" ("/" <> v)
    (_,w') <- fromPath "/:w" ("/" <> w)
    (_,x') <- fromPath "/:x" ("/" <> x)
    (_,y') <- fromPath "/:y" ("/" <> y)
    (_,z') <- fromPath "/:z" ("/" <> z)
    pure (rest,(u',v',w',x',y',z'))
instance (FromPath t, FromPath u, FromPath v, FromPath w, FromPath x, FromPath y, FromPath z) => FromPath (t,u,v,w,x,y,z) where
  fromPath (Stencil s) u = do
    (rest,[(_,z),(_,y),(_,x),(_,w),(_,v),(_,u),(_,t)]) <- stencil s u
    (_,t') <- fromPath "/:t" ("/" <> t)
    (_,u') <- fromPath "/:u" ("/" <> u)
    (_,v') <- fromPath "/:v" ("/" <> v)
    (_,w') <- fromPath "/:w" ("/" <> w)
    (_,x') <- fromPath "/:x" ("/" <> x)
    (_,y') <- fromPath "/:y" ("/" <> y)
    (_,z') <- fromPath "/:z" ("/" <> z)
    pure (rest,(t',u',v',w',x',y',z'))
instance (FromPath r, FromPath t, FromPath u, FromPath v, FromPath w, FromPath x, FromPath y, FromPath z) => FromPath (r,t,u,v,w,x,y,z) where
  fromPath (Stencil st) u = do
    (rest,[(_,z),(_,y),(_,x),(_,w),(_,v),(_,u),(_,t),(_,r)]) <- stencil st u
    (_,r') <- fromPath "/:r" ("/" <> r)
    (_,t') <- fromPath "/:t" ("/" <> t)
    (_,u') <- fromPath "/:u" ("/" <> u)
    (_,v') <- fromPath "/:v" ("/" <> v)
    (_,w') <- fromPath "/:w" ("/" <> w)
    (_,x') <- fromPath "/:x" ("/" <> x)
    (_,y') <- fromPath "/:y" ("/" <> y)
    (_,z') <- fromPath "/:z" ("/" <> z)
    pure (rest,(r',t',u',v',w',x',y',z'))

class ToPath a where
  toPath :: Stencil -> a -> (Stencil,Txt)
  default toPath :: Show a => Stencil -> a -> (Stencil,Txt)
  toPath s a = fromMaybe (s,def) (Web.fill s (Pure.encodeURIComponent (toTxt (Show.show a))))

instance ToPath () where
  toPath s () = fromMaybe (s,def) (Web.fill s "")
instance ToPath Txt where
  toPath s t = fromMaybe (s,def) (Web.fill s (Pure.encodeURIComponent t))
instance ToPath String where
  toPath s t = fromMaybe (s,def) (Web.fill s (Pure.encodeURIComponent (toTxt t)))
instance ToPath (Marker a) where
  toPath s m = fromMaybe (s,def) (Web.fill s (Pure.encodeURIComponent (toTxt m)))
instance ToPath Key where
  toPath s k = fromMaybe (s,def) (Web.fill s (Pure.encodeURIComponent (toTxt k)))

instance (ToPath l, ToPath r) => ToPath (Either l r) where
  toPath s (Left l) = toPath s l
  toPath s (Right r) = toPath s r
instance (ToPath i) => ToPath [i] where
  toPath s = go
    where 
      go [] 
        | Just (s,_) <- Web.fill s "" = (s,mempty)
        | otherwise                   = (s,mempty)

      go (i:is) 
        | (_,x)  <- toPath s i
        , (s,xs) <- go is
        = (s,x <> xs)
          
instance (ToPath y, ToPath z) => ToPath (y,z) where
  toPath s (y,z) =
    let (sy,ty) = toPath s  y
        (sz,tz) = toPath sy z
    in (sz,ty <> tz)
instance (ToPath x, ToPath y, ToPath z) => ToPath (x,y,z) where
  toPath s (x,y,z) =
    let (sx,tx) = toPath s  x
        (sy,ty) = toPath sx y
        (sz,tz) = toPath sy z
    in (sz,tx <> ty <> tz)
instance (ToPath w, ToPath x, ToPath y, ToPath z) => ToPath (w,x,y,z) where
  toPath s (w,x,y,z) =
    let (sw,tw) = toPath s  w
        (sx,tx) = toPath sw x
        (sy,ty) = toPath sx y
        (sz,tz) = toPath sy z
    in (sz,tw <> tx <> ty <> tz)
instance (ToPath v, ToPath w, ToPath x, ToPath y, ToPath z) => ToPath (v,w,x,y,z) where
  toPath s (v,w,x,y,z) =
    let (sv,tv) = toPath s  v
        (sw,tw) = toPath sv w
        (sx,tx) = toPath sw x
        (sy,ty) = toPath sx y
        (sz,tz) = toPath sy z
    in (sz,tv <> tw <> tx <> ty <> tz)
instance (ToPath u, ToPath v, ToPath w, ToPath x, ToPath y, ToPath z) => ToPath (u,v,w,x,y,z) where
  toPath s (u,v,w,x,y,z) =
    let (su,tu) = toPath s  u
        (sv,tv) = toPath su v
        (sw,tw) = toPath sv w
        (sx,tx) = toPath sw x
        (sy,ty) = toPath sx y
        (sz,tz) = toPath sy z
    in (sz,tu <> tv <> tw <> tx <> ty <> tz)
instance (ToPath t, ToPath u, ToPath v, ToPath w, ToPath x, ToPath y, ToPath z) => ToPath (t,u,v,w,x,y,z) where
  toPath s (t,u,v,w,x,y,z) =
    let (st,tt) = toPath s  t
        (su,tu) = toPath st u
        (sv,tv) = toPath su v
        (sw,tw) = toPath sv w
        (sx,tx) = toPath sw x
        (sy,ty) = toPath sx y
        (sz,tz) = toPath sy z
    in (sz,tt <> tu <> tv <> tw <> tx <> ty <> tz)
instance (ToPath r, ToPath t, ToPath u, ToPath v, ToPath w, ToPath x, ToPath y, ToPath z) => ToPath (r,t,u,v,w,x,y,z) where
  toPath s (r,t,u,v,w,x,y,z) =
    let (sr,tr) = toPath s  r
        (st,tt) = toPath sr t
        (su,tu) = toPath st u
        (sv,tv) = toPath su v
        (sw,tw) = toPath sv w
        (sx,tx) = toPath sw x
        (sy,ty) = toPath sx y
        (sz,tz) = toPath sy z
    in (sz,tr <> tt <> tu <> tv <> tw <> tx <> ty <> tz)

fill :: Stencil -> Txt -> Maybe (Stencil,Txt)
fill (Stencil s) t =
  case Txt.breakOn "/:" s of
    (x,"") -> Just (Stencil "",x <> t)
    (before,Txt.dropWhile (/= '/') . Txt.tail -> after) -> 
      Just (Stencil after,before <> "/" <> t)

pattern Route :: Path -> r -> Maybe (Path,r)
pattern Route rest r <- Just (rest,r) where
  Route rest r = Just (rest,r)

newtype Path = Path { _path :: Txt }
data R a = R { _unconsumed :: {-# UNPACK #-}!Path, _active :: a }

instance FromPath a => IsString (Path -> Maybe (Path,a)) where
  fromString s (Path r) = fmap (\(t,a) -> (Path t,a)) $! fromPath (fromString s) r

url :: Path
url = Path (unsafePerformIO getPathname)

path :: Exists Path => Path
path = it

type Routed = Exists Path

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
at = atWith False 

{-# INLINE dot #-}
-- dot, as in: on-the-dot
dot :: forall a. Time -> (Exists Time => a) -> a
dot = dotWith False

{-# INLINE dotWith #-}
dotWith :: forall a. Bool -> Time -> (Exists Time => a) -> a
dotWith truth t = 
  let 
    fromInt = fromIntegral @Int
    now = unsafePerformIO time
    d = fromInt (floor (now / t) :: Int)
    start = d * t
  in 
    atWith truth [ start + fromInt n * t | n <- [1 :: Int ..] ]

{-# INLINE atWith #-}
atWith :: forall a. Bool -> [Time] -> (Exists Time => a) -> a
atWith truth [] a = with (unsafePerformIO time) a
atWith truth ts a = go ts
  where 
    {-# NOINLINE go #-}
    go ~(t:ts)
      | !s <- unsafePerformIO time
      , () <- unsafePerformIO (delay (t - s))
      , !n  <- if truth then unsafePerformIO time else t
      , Milliseconds ms _ <- n - t
      = if List.null ts then with n a else with n a `seq` go ts

