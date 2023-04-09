{-# language ScopedTypeVariables, TypeApplications, ConstraintKinds, FlexibleContexts, RankNTypes, TypeFamilies, AllowAmbiguousTypes, PatternSynonyms, BlockArguments #-}
module Control.State (Modify,State,state,state',stateWith,stateWith',modifyIO,modify,modifyIt,modifyItIO,get,put,zoom,zoomIO,ignore,flat,flatBy,toState,toStateWith) where

import Control.Concurrent (forkIO,killThread,ThreadId,MVar,newEmptyMVar,putMVar,readMVar)
import Control.Dynamic (dynamic,fromDynamic)
import Control.Producer (Producer,stream,yield,discard)
import Control.Fold (foldM)
import Control.Monad (forever,void)
import Data.Effect ((#))
import Data.Exists (Exists,it,using)
import Data.Time (delay,Time)
import Data.View (View,pattern Null,eager)
import Data.Typeable (Typeable)
import GHC.Exts (IsList(..))
import System.IO.Unsafe (unsafePerformIO)

type Modify a = Producer (a -> IO a)
type State a = (Modify a, Exists a)

{-# INLINE state #-}
state :: Typeable a => (Modify a => a) -> (State a => View) -> View
state a = foldM ($) (pure (a,\_ -> pure ()))

{-# INLINE state' #-}
state' :: forall a. Typeable a => (Modify a => a) -> (State a => View) -> View
state' a v = eager (dynamic @(Modify a) a) (state a v)

{-# INLINE stateWith #-}
stateWith :: forall a. Typeable a => (Modify a => a -> a -> IO a) -> (Modify a => IO (a,a -> IO ())) -> (State a => View) -> View
stateWith f = foldM (\g a -> g a >>= f a) 

{-# INLINE stateWith' #-}
stateWith' :: forall a. Typeable a => (Modify a => a -> a -> IO a) -> (Modify a => IO (a,a -> IO ())) -> (State a => View) -> View
stateWith' f i v = eager (dynamic @(Modify a) f,dynamic @(Modify a) i) (stateWith f i v)

{-# INLINE modifyIO #-}
modifyIO :: forall a. Modify a => (a -> IO a) -> IO ()
modifyIO = yield 

{-# INLINE modify #-}
modify :: forall a. Modify a => (a -> a) -> IO ()
modify f = yield (\a -> pure @IO (f a))

{-# INLINE modifyIt #-}
modifyIt :: forall a. Modify a => (Exists a => a) -> IO ()
modifyIt a = modifyItIO (pure a) 

{-# INLINE modifyItIO #-}
modifyItIO :: forall a. Modify a => (Exists a => IO a) -> IO ()
modifyItIO ioa = yield (\a -> using (a :: a) ioa)

{-# INLINE put #-}
put :: Modify a => a -> IO ()
put = modify . const

{-# INLINE get #-}
get :: Exists a => a
get = it

{-# INLINE zoom #-}
zoom :: forall a b x. (a -> b) -> (b -> a -> a) -> (State b => x) -> (State a => x)
zoom f g v = using (f get) ((\h a -> h (f a) >>= \b -> pure @IO (g b a)) # v)

{-# INLINE zoomIO #-}
zoomIO :: forall a b x. (a -> b) -> (b -> a -> IO a) -> (State b => x) -> (State a => x)
zoomIO f g v = using (f get) ((\h a -> h (f a) >>= \b -> g b a) # v)

{-# INLINE ignore #-}
ignore :: forall b x. (Modify b => x) -> x
ignore = discard @(b -> IO b)

{-# INLINE toState #-}
toState :: forall a. (Producer a => View) -> (State a => View)
toState = stream @a put

{-# INLINE toStateWith #-}
toStateWith :: forall a b. (a -> b) -> (Producer a => View) -> (State b => View)
toStateWith f = stream @a (put . f)

{-# INLINE flat #-}
flat :: forall l a x. (IsList l, Item l ~ a, State l) => (State a => x) -> [x]
flat v = flatBy @l @a (const True) v

{-# INLINE flatBy #-}
flatBy :: forall l a x. (IsList l, Item l ~ a, State l) => (a -> Bool) -> (State a => x) -> [x]
flatBy pred v =
  [ zoom (const a) (replace n) v
  | (n,a) <- zip [0..] (toList (get :: l))
  , pred a
  ]
  where
    replace :: Int -> a -> l -> l
    replace n a l = fromList (go n (toList l))
      where
        go _ []     = []
        go 0 (_:as) = a : as
        go n (a:as) = a : go (n - 1) as
