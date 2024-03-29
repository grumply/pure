{-# language RankNTypes, ScopedTypeVariables, TypeFamilies, BangPatterns #-}
module Data.Stream
  ( Stream()
  , unfolds
  , folds
  , builds 
  , cons, nil, suspended, singleton
  , more, done
  , step, steps
  , force, forceAll
  , suspends, suspendsEvery
  , stepsOf, chunksOf
  , toList, toListM
  , fromList, fromListM
  , append, concat
  , merge
  , repeat, repeatM
  , cycle, infinite
  , take, drop
  , null
  , tail
  , reverse
  , filter
  ) where

import Control.Monad (join,ap)
import Data.Monoid
import Data.Semigroup
import Data.Foldable hiding (toList,concat,null,length)
import Data.Traversable
import Data.Function (fix)
import GHC.Exts (build,IsList(),inline)
import qualified GHC.Exts as List (IsList(..))
import qualified Data.List as List hiding (length)
import Prelude hiding (concat,repeat,take,drop,null,tail,reverse,filter,cycle,zip)

data Stream f a = Nil | Suspended (f (Stream f a)) | Cons a (Stream f a)

{-# INLINE [1] suspended #-}
suspended :: Functor f => f (Stream f a) -> Stream f a
suspended stream = builds $ \e c s -> c (fmap (folds e c s) stream)

{-# INLINE [1] cons #-}
cons :: Functor f => a -> Stream f a -> Stream f a
cons a stream = builds $ \e c s -> s a (folds e c s stream)

{-# INLINE [1] nil #-}
nil :: Stream f a
nil = builds $ \e c s -> e

{-# INLINE [1] singleton #-}
singleton :: a -> Stream f a
singleton a = builds $ \e c s -> s a e

instance Functor f => Monoid (Stream f a) where
  {-# INLINE mempty #-}
  mempty = nil
  {-# INLINE mappend #-}
  mappend = (<>)
  {-# INLINE mconcat #-}
  mconcat xs = 
    builds $ \e c s ->
      foldr (\as rest -> folds rest c s as) e xs

instance Functor f => Semigroup (Stream f a) where
  {-# INLINE (<>) #-}
  (<>) = append

instance Functor f => Functor (Stream f) where
  {-# INLINE fmap #-}
  fmap f xs = builds $ \e c s -> folds e c (s . f) xs

instance Functor f => Applicative (Stream f) where
  {-# INLINE pure #-}
  pure = return
  {-# INLINE (<*>) #-}
  (<*>) = ap

instance Functor f => Monad (Stream f) where
  {-# INLINE return #-}
  return a = builds $ \e _ s -> s a e
  {-# INLINE (>>=) #-}
  sa >>= asb = concat (fmap asb sa) 

instance (Functor f, Foldable f) => Foldable (Stream f) where
  {-# INLINE foldr #-}
  foldr f st = folds st (foldl' (flip const) undefined) f

instance (Functor f, Traversable f) => Traversable (Stream f) where
  {-# INLINE traverse #-}
  traverse f = go
    where
      -- a shame, really, but at least it is possible with this approach
      -- even if it breaks builds/folds fusion
      go Nil = pure Nil
      go (Suspended fs) = Suspended <$> traverse go fs
      go (Cons a fs) = Cons <$> f a <*> go fs

instance Functor f => IsList (Stream f a) where
  type Item (Stream f a) = a
  fromList = fromList
  toList = toList

{-# INLINE [1] augments #-}
augments :: (forall b. b -> (f b -> b) -> (a -> b -> b) -> b) -> Stream f a -> Stream f a
augments f s = f s Suspended Cons

{-# INLINE [1] builds #-}
builds :: (forall b. b -> (f b -> b) -> (a -> b -> b) -> b) -> Stream f a
builds f = f Nil Suspended Cons

{-# INLINE [0] folds #-}
folds :: Functor f => b -> (f b -> b) -> (element -> b -> b) -> Stream f element -> b
folds e c s = go
  where
    go Nil = e
    go (Suspended fs) = c (fmap go fs)
    go (Cons a sa) = s a (go sa)
 
{-# INLINE unfolds #-}
unfolds :: Functor f => state -> (state -> f (Maybe (element, state))) -> Stream f element
unfolds initial f = 
  builds $ \e c s ->
    flip fix initial $ \loop st -> 
      c (fmap (maybe e (\(a,st) -> s a (loop st))) (f st))

{-# INLINE more #-}
more :: Applicative f => element -> state -> f (Maybe (element,state))
more e s = pure (Just (e,s))

{-# INLINE done #-}
done :: Applicative f => f (Maybe (element,state))
done = pure Nothing

{-# INLINE suspends #-}
suspends :: Applicative f => Stream f a -> Stream f a
suspends as = builds $ \e c s -> folds e c (\a as -> c (pure (s a as))) as

{-# INLINE suspendsEvery #-}
suspendsEvery :: Monad f => Int -> Stream f a -> Stream f a
suspendsEvery n = chunksOf n . suspends 

{-# RULES
"folds/builds" forall e c s (f :: forall b. b -> (f b -> b) -> (a -> b -> b) -> b).
               folds e c s (builds f) = f e c s

"folds/augments" forall e c s xs (f :: forall b. b -> (f b -> b) -> (a -> b -> b) -> b).
                 folds e c s (augments f xs) = f (folds e c s xs) c s 

"folds/id" folds Nil Suspended Cons = \x -> x

"folds/app" [1] forall ys. folds ys Suspended Cons = \xs -> append xs ys

"folds/cons" forall e c s a as. folds e c s (Cons a as) = s a (folds e c s as)

"folds/cons"       forall e c s a.  folds e c s (Cons a Nil) = s a e
"folds/suspended"  forall e c s fa. folds e c s (Suspended fa) = c (fmap (folds e c s) fa)
"folds/nil"        forall e c s.    folds e c s Nil = e

"folds/cons/builds" forall e c s x (f :: forall b. b -> (f b -> b) -> (a -> b -> b) -> b).
                    folds e c s (Cons x (builds f)) = s x (f e c s)

"augments/builds" forall (f :: forall b. b -> (f b -> b) -> (a -> b -> b) -> b) 
                         (g :: forall b. b -> (f b -> b) -> (a -> b -> b) -> b).
                  augments g (builds f) = builds (\e c s -> g (f e c s) c s)

"augments/nil" forall (f :: forall b. b -> (f b -> b) -> (a -> b -> b) -> b).
               augments f Nil = builds f

"augments/augments" forall (f :: forall b. b -> (f b -> b) -> (a -> b -> b) -> b) 
                           (g :: forall b. b -> (f b -> b) -> (a -> b -> b) -> b)
                           t. 
                    augments f (augments g t) = augments (\e c s -> f (g e c s) c s) t


  #-}

{-# INLINE step #-}
step :: Monad f => Stream f a -> f (Stream f a)
step = go
  where
    go (Cons a rest) = Cons a <$> go rest
    go (Suspended fsa) = fsa
    go end = pure end

{-# INLINE uncons #-}
uncons :: Monad f => Stream f a -> f (Maybe (a,Stream f a))
uncons (Cons a s) = pure (Just (a,s))
uncons (Suspended f) = f >>= uncons
uncons Nil = pure Nothing

{-# INLINE steps #-}
steps :: Monad f => Int -> Stream f a -> f (Stream f a)
steps n stream = step $ force n stream

{-# INLINE force #-}
-- Collect `n` monadic stream effects into a single wrapper that,
-- when evaluated via `step` or similar, evaluates all of the 
-- wrapped effects. Subsequent calls to, for instance, `toList` 
-- will see any elements materialized between those `n` stream 
-- effects.
force :: Monad f => Int -> Stream f a -> Stream f a
force n stream
  | n <= 0    = stream
  | otherwise = 
    builds $ \e c s -> c $
      folds
        (\_ -> pure e)
        (\fns m ->
          let r n = join $ fmap ($ n) fns
          in if m == 0
            then pure (c (r 0))
            else r (m - 1)
        )
        (\a stream n -> fmap (s a) (stream n))
        stream
        n

-- Collect all of the monadic stream effects into a single wrapper that, 
-- when evaluated, forces the entire stream into memory and evaluates all
-- of the effects. Somewhat subject to fusion, depending on how it is handled.
-- Subsequent calls to, for instance, `toList`, will see all of the elements of
-- the stream.
{-# INLINE forceAll #-}
forceAll :: Monad f => Stream f a -> Stream f a
forceAll stream =
  builds $ \e c s -> 
    c $ folds (pure e) join (fmap . s) stream

{-# INLINE stepsOf #-}
-- make `step` always force `n` suspended frames; subtly different than chunksOf
stepsOf :: Monad f => Int -> Stream f a -> Stream f a
stepsOf n stream =
  builds $ \e c s -> c $
    folds 
      (\_ -> pure e) 
      (\fns m ->
        let r n = join $ fmap ($ n) fns
        in if m == 0 then pure (c (r n)) else r (m - 1)
      ) 
      (\a stream n -> fmap (s a) $ stream n)
      stream 
      n

{-# INLINE chunksOf #-}
-- make `step` always yield `n` elements; subtly different than stepSize
chunksOf :: Monad f => Int -> Stream f a -> Stream f a
chunksOf n stream =
  builds $ \e c s -> c $
    folds 
      (\_ -> pure e) 
      (\fns m -> join $ fmap ($ m) fns
      )
      (\a stream m -> 
        let r n = s a <$> stream n
        in if m == 0 then pure (c (r n)) else r (m - 1)
      )
      stream 
      n

{-# RULES
"append" [~1] forall xs ys. append xs ys = augments (\e c s -> folds e c s xs) ys
  #-}

{-# NOINLINE [2] append #-}
append :: Functor f => Stream f a -> Stream f a -> Stream f a
append l r = builds $ \e c s -> folds (folds e c s r) c s l

{-# INLINE interleave #-}
interleave :: Monad f => Stream f a -> Stream f a -> Stream f a
interleave as as' = builds $ \e c s -> folds e c (\(a,a') rest -> s a (s a' rest)) (zip as as')

{-# INLINE zip #-}
zip :: Monad f => Stream f a -> Stream f b -> Stream f (a,b)
zip la ra = unfolds (la,ra) $ \(la,ra) -> do
  lr <- (,) <$> uncons la <*> uncons ra
  case lr of
    (Just (l,lrest),Just (r,rrest)) -> pure (Just ((l,r),(lrest,rrest)))
    _                               -> pure Nothing

{-# INLINE fromList #-}
fromList :: [a] -> Stream f a
fromList xs = 
  builds $ \e _ s ->
    foldr (\a c -> s a . c) id xs e

{-# INLINE fromListM #-}
fromListM :: Functor f => [f a] -> Stream f a
fromListM xs = 
  builds $ \e c s ->
    foldr (\fa cont as -> c $ fmap (\a -> s a $ cont as) fa) id xs e

{-# INLINE toList #-}
toList :: Functor f => Stream f a -> [a]
toList xs = build $ \cons nil -> 
  folds 
    id 
    (\_ rest -> rest) 
    (\a c rest -> cons a (c rest))
    xs 
    nil

{-# INLINE toListM #-}
toListM :: Monad f => Stream f a -> f [a]
toListM xs = 
  folds 
    pure 
    (\fs rest -> join $ fmap ($ rest) fs) 
    (\a c rest -> fmap (a :) (c rest)) 
    xs 
    []

{-# INLINE concat #-}
concat :: Functor f => Stream f (Stream f a) -> Stream f a
concat xs = 
  builds $ \e c s -> 
    folds e c (\a xs -> folds xs c s a) xs

{-# INLINE merge #-}
merge :: Functor f => Stream f [a] -> Stream f a
merge as = builds $ \e c s -> 
  folds e c (\as rest -> foldr s rest as) as

{-# INLINE cycle #-}
cycle :: Functor f => Stream f a -> Stream f a
cycle as = 
  builds $ \_ c s ->
    fix $ \loop ->
      folds loop c s as

{-# INLINE infinite #-}
-- un-delimited
infinite :: Functor f => a -> Stream f a
infinite a = 
  builds $ \_ _ s -> 
    fix $ \loop -> 
      s a loop

{-# INLINE repeat #-}
-- delimited
repeat :: Applicative f => a -> Stream f a
repeat a =
  builds $ \_ c s ->
    fix $ \loop ->
      c $ pure $ s a loop

{-# INLINE repeatM #-}
-- delimited
repeatM :: Functor f => f a -> Stream f a
repeatM fa = 
  builds $ \_ c s ->
    fix $ \loop ->
      c (fmap (\a -> s a loop) fa)

{-# INLINE take #-}
take :: Functor f => Int -> Stream f a -> Stream f a
take n as =
  builds $ \e c s ->
    folds 
      (const e) 
      (\f n -> if n <= 0 then e else c (fmap ($ n) f))
      (\a rest n -> if n <= 0 then e else s a (rest (n - 1))) 
      as 
      n

{-# INLINE drop #-}
drop :: Functor f => Int -> Stream f a -> Stream f a
drop n as =
  builds $ \e c s ->
    folds
      (const e)
      (\f n -> c (fmap ($ n) f))
      (\a rest n -> if n <= 0 then s a (rest n) else rest (n - 1))
      as
      n

{-# INLINE null #-}
null :: Functor f => Stream f a -> Bool
null = folds True (\_ -> False) (\_ _ -> False)

{-# INLINE tail #-}
tail :: Functor f => Stream f a -> Stream f a
tail xs =
  builds $ \e c s ->
    folds
    (const e)
    (\f b -> c (fmap ($ b) f))
    (\a rest b -> if b then s a (rest b) else rest True)
    xs
    False

{-# INLINE reverse #-}
reverse :: Functor f => Stream f a -> Stream f a
reverse as =
  builds $ \e c s ->
    folds
      id
      (\f as -> c $ fmap ($ as) f)
      (\a rest as -> rest (s a as))
      as
      e

{-# INLINE filter #-}
filter :: Functor f => (a -> Bool) -> Stream f a -> Stream f a
filter p as = builds $ \e c s -> 
  folds e c (\a as -> if p a then s a as else as) as

{- 

GHC does an exceptional job with the stream rewrite rules in the general case. 
Note in the following Core how GHC fully merged the toListM/take/drop/unfolds. 
The stream is never even materialized. GHC even eliminated the Maybe!

print =<< toListM (take 1 (drop 10000000 (unfolds (0 :: Int) $ \(!n) -> more n (n + 1))))

Rec {
test_$s$wx
  = \ sc sc1 sc2 sc3 sc4 ->
      case <=# sc2 0# of {
        __DEFAULT ->
          case <=# sc3 0# of {
            __DEFAULT -> test_$s$wx sc sc1 sc2 (-# sc3 1#) (+# sc4 1#);
            1# ->
              case test_$s$wx sc sc1 (-# sc2 1#) sc3 (+# sc4 1#) of
              { (# ipv, ipv1 #) ->
              (# ipv, : (I# sc4) ipv1 #)
              }
          };
        1# -> (# sc, sc1 #)
      }
end Rec }

test1
  = \ s ->
      case test_$s$wx s [] 1# 10000000# 0# of { (# ipv, ipv1 #) ->
      ((hPutStr' stdout ($fShow[]_$s$cshow ipv1) True) `cast` <Co:2>) ipv
      }

-}
