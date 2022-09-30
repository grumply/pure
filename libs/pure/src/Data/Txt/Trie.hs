{-# LANGUAGE CPP, DeriveGeneric, DeriveAnyClass, ScopedTypeVariables, BangPatterns, ViewPatterns, OverloadedStrings #-}
module Data.Txt.Trie
  (
  -- * Trie Types
  TxtTrie, TxtSet
  -- * Operators
  , (!), (!?), (\\)
  -- * Query
  , size
  , null
  , lookup
  , member
  , notMember
  , find
  , findWithDefault
  , lookupPrefix
  -- * Construction
  , empty
  , singleton
  , fromList
  -- * Insert
  , insert
  -- * Union
  , union
  , unions
  -- * Delete
  , delete
  , difference
  , deletePrefix
  -- * Folds
  , foldr
  , foldl
  , foldrWithKey
  , foldlWithKey
  , foldMapWithKey
  -- * Traversable
  -- ** Map
  , map
  , mapWithKey
  , traverseWithKey
  , mapAccum
  , mapAccumWithKey
  , mapAccumRWithKey
  , mapKeys
  -- * Strict Folds
  , foldr'
  , foldl'
  , foldrWithKey'
  , foldlWithKey'
  -- * Conversion
  , elems
  , keys
  , assocs
  , toList
  , toAscList
  , toDescList
  -- * Optimization
  , recompress
  , splitRoot
  ) where

-- from base
import Control.Monad
import Data.Char
import Data.Coerce
import qualified Data.Foldable as Foldable
import Data.Maybe
import Data.Monoid hiding ((<>))
import Data.Semigroup
import Data.Traversable hiding (mapAccumL)
import GHC.Generics
import GHC.Exts (build)
import Prelude hiding (lookup,map,foldr,foldl,foldl',null,find)
import qualified Prelude

import Data.Txt (Txt,ToTxt(..),FromTxt(..))
import qualified Data.Txt as Txt
#ifndef __GHCJS__
import qualified Data.Text (copy)
#endif

import Data.JSON (ToJSON,FromJSON)

import qualified Data.IntMap.Strict as IntMap

import Control.DeepSeq

{- |
Pure.Data.Txt.Trie implements a Trie structure that is, in the majority of cases, a drop-in replacement for `Map Txt a`.

* Pros:
Trie is useful for reducing memory usage and improving lookup speed when many keys share a prefix.

* Cons:
There is overhead in toList and fromList when reconstructing and deconstructing keys, respectively.

* Implementation
Internally, conceptually and for ease of implementation, the Trie is structured as nested IntMaps.

`TxtSet` is a type synonym for `TxtTrie ()`.

The unmerge that delete, difference and deletePrefix are based on are non-optimal in that they do not recompress after deletion.  To combat this, in the case of a deletion-heavy use-case, you can use `recompress` which is a synonym for `fromList . toList`.

Note that the `Eq` and `Ord` instances are structural.

A generalization of the Key type with a deconstructing/reconstructing class (like IsList) could be interesting, but would add overhead.  An on-demand deconstruction might be able to avoid the overhead.

TODO:
* switch to a universal merge
* use IntMap merges where appropriate
-}

data Key
    = None
    | Suffix {-# UNPACK #-}!Txt
    deriving (Show,Generic,Eq,Ord,ToJSON,FromJSON)

instance NFData Key where
    rnf None = ()
    rnf (Suffix t) = rnf t

{-# INLINE key #-}
key :: a -> (Txt -> a) -> Key -> a
key none _ None = none
key _ suffix (Suffix s) = suffix s

{-# INLINE fromKey #-}
fromKey :: Txt -> Key -> Txt
fromKey t None = t
fromKey _ (Suffix s) = s

{-# INLINE mapKey #-}
mapKey :: (Txt -> Txt) -> Key -> Key
mapKey _ None = None
mapKey f (Suffix s) = Suffix (f s)

{-# INLINE isNone #-}
isNone :: Key -> Bool
isNone None = True
isNone _ = False

{-# INLINE isSuffix #-}
isSuffix :: Key -> Bool
isSuffix None = False
isSuffix _ = True

{-# INLINE fromSuffix #-}
fromSuffix :: Key -> Txt
fromSuffix (Suffix s) = s
fromSuffix _ = error "fromSuffix: expecting Suffix constructor; got None constructor"

newtype DL a = DL { unDL :: [a] -> [a] }

fromDL :: DL a -> [a]
fromDL dl = unDL dl []

{-# INLINE dl #-}
dl :: [a] -> DL a
dl = DL . (++)

{-# INLINE snoc #-}
snoc :: DL a -> a -> DL a
snoc xs x = DL (unDL xs . (x:))

{-# INLINE append #-}
append :: DL a -> DL a -> DL a
append xs ys = DL (unDL xs . unDL ys)

{-# INLINE foldlStrict #-}
foldlStrict :: (a -> b -> a) -> a -> [b] -> a
foldlStrict f = go
  where
    go z []     = z
    go z (x:xs) = let z' = f z x in z' `seq` go z' xs

type TxtTrieInternal a = IntMap.IntMap (TxtTrie a)

data TxtTrie a
    = Empty
    | Leaf !Key a
    | Branch {-# UNPACK #-}!Int !(TxtTrieInternal a)
    deriving (Show,Generic,Eq,Ord,ToJSON,FromJSON)

type TxtSet = TxtTrie ()

instance Monoid (TxtTrie a) where
    mempty = empty
    mconcat = unions
    mappend = (<>)

instance Semigroup (TxtTrie a) where
    (<>) = union
    stimes = stimesIdempotentMonoid

instance Functor TxtTrie where
    fmap f t = map f t

instance Traversable TxtTrie where
    traverse f = traverseWithKey (\_ -> f)

instance Foldable.Foldable TxtTrie where
    fold = foldr mappend mempty
    {-# INLINABLE fold #-}
    foldr = foldr
    {-# INLINE foldr #-}
    foldl = foldl
    {-# INLINE foldl #-}
    foldMap f = foldMapWithKey (\_ -> f)
    {-# INLINABLE foldMap #-}
    foldl' = foldl'
    {-# INLINE foldl' #-}
    foldr' = foldr'
    {-# INLINE foldr' #-}
    length = size
    {-# INLINE length #-}
    null = null
    {-# INLINE null #-}
    toList = elems
    {-# INLINE toList #-}
    maximum = Foldable.foldr1 max
    {-# INLINABLE maximum #-}
    minimum = Foldable.foldr1 min
    {-# INLINABLE minimum #-}
    sum = foldl' (+) 0
    {-# INLINABLE sum #-}
    product = foldl' (*) 1
    {-# INLINABLE product #-}

instance NFData a => NFData (TxtTrie a) where
    rnf Empty = ()
    rnf (Leaf k v) = rnf k `seq` rnf v
    rnf (Branch c b) = rnf c `seq` rnf b

{-# INLINE leaf #-}
leaf :: Txt -> a -> TxtTrie a
leaf k = Leaf (if Txt.null k then None else Suffix k)

{-# INLINE ttiNoCopy #-}
ttiNoCopy :: Key -> a -> TxtTrieInternal a
ttiNoCopy None v = IntMap.fromList [((-1),Leaf None v)]
ttiNoCopy (Suffix k) v =
  case Txt.uncons k of
    Just (h,t) -> IntMap.fromList [(ord h,Leaf (if Txt.null t then None else Suffix t) v)]
    _          -> IntMap.fromList [((-1),Leaf None v)]

{-# INLINE tti #-}
tti :: Key -> a -> TxtTrieInternal a
tti None v = IntMap.fromList [((-1),Leaf None v)]
tti (Suffix k) v =
  case Txt.uncons k of
    Just (h,t) ->
      let t' = if Txt.null t then None else Suffix $
#if (defined FAST || defined __GHCJS__)
               t
#else
               Txt.copy t
#endif
      in IntMap.fromList [(ord h,Leaf t' v)]
    _ -> IntMap.fromList [((-1),Leaf None v)]

{-# INLINE toTTINoCopy #-}
toTTINoCopy :: TxtTrie a -> TxtTrieInternal a
toTTINoCopy (Leaf k v) = ttiNoCopy k v
toTTINoCopy (Branch _ bs) = bs
toTTINoCopy _ = IntMap.empty

{-# INLINE toTTI #-}
toTTI :: TxtTrie a -> TxtTrieInternal a
toTTI (Leaf k v) = tti k v
toTTI (Branch _ bs) = bs
toTTI _ = IntMap.empty

-- returns the increase in size (as a result of inserts of elements of the right
-- into the left) and the new TxtTrie
{-# INLINE merge #-}
merge :: Int -> TxtTrie a -> TxtTrie a -> (Int,TxtTrie a)

merge _ x Empty = (0,x)

-- merge is called with curDepth == 1 initially, so we can
-- know if the key we're working with was already in the Trie
-- (curDepth > 0) or if we are just now adding it (curDepth == 0), so
-- we can call Txt.copy.
merge curDepth Empty (Leaf mk v)
    | curDepth == 0 = (1,Leaf mk v)
    | otherwise =
#if (defined FAST || defined __GHCJS__)
        (1,Leaf mk v)
#else
        (1,Leaf (mapKey Txt.copy mk) v)
#endif

merge _ Empty x = (size x,x)

merge curDepth l@(Leaf mk _) r@(Leaf mk' v')
    | mk == mk' = (0,Leaf mk v')
    | otherwise =
        let (added,merged) = mergeBranches (curDepth + 1) (toTTINoCopy l) (toTTI r)
        in (added,Branch (1 + added) merged)

merge curDepth l r =
    let (added,merged) = mergeBranches (curDepth + 1) (toTTINoCopy l) (toTTI r)
    in (added,Branch (size l + added) merged)

-- returns increaase in size (as a result of inserting elements of the rigth
-- into the left) and the new TxtTrieInternal
{-# INLINE mergeBranches #-}
mergeBranches :: forall a. Int -> TxtTrieInternal a -> TxtTrieInternal a -> (Int,TxtTrieInternal a)
mergeBranches !curDepth bs0 = IntMap.foldrWithKey mergeBranches' (0,bs0)
    where
        {-# INLINE mergeBranches' #-}
        mergeBranches' :: Int -> TxtTrie a -> (Int,TxtTrieInternal a) -> (Int,TxtTrieInternal a)
        mergeBranches' c t (n,bs) =
#if MIN_VERSION_containers(0,5,10)
            let (!added,bs') = IntMap.alterF go c bs
                !n' = n + added
                go Nothing = (size t,Just t)
                go (Just t') =
                    let (!added,merged) = merge curDepth t' t
                    in (added,Just merged)
            in (n',bs')
#else
          case IntMap.lookup c bs of
              Nothing ->
                  let !n' = size t + n
                      bs' = IntMap.insert c t bs
                  in (n',bs')
              Just t' ->
                  let (added,merged) = merge curDepth t' t
                      !n' = added + n
                      bs' = IntMap.insert c merged bs
                  in (n',bs')
#endif

{-# INLINE unmergePrefix #-}
unmergePrefix :: TxtTrie a -> Txt -> (Int,TxtTrie a)
unmergePrefix Empty !_     = (0,Empty)
unmergePrefix (Leaf mk v) p
    | isNone mk && Txt.null p     = (1,Empty)
    | isSuffix mk
    , p `Txt.isPrefixOf` fromSuffix mk = (1,Empty)
    | otherwise                      = (0,Leaf mk v)
unmergePrefix (Branch n bs) p =
    let (removed,unmerged) = unmergePrefixBranches bs p
        result | IntMap.size unmerged == 0 = (n,Empty)
               | otherwise                 = (removed,Branch (n - removed) unmerged)
    in result

{-# INLINE unmergePrefixBranches #-}
unmergePrefixBranches :: TxtTrieInternal a -> Txt -> (Int,TxtTrieInternal a)
unmergePrefixBranches bs !p =
    case Txt.uncons p of
        Nothing -> (0,IntMap.empty) -- the caller knows how many were removed in this case
        Just (ord -> h,t) ->
            case IntMap.lookup h bs of
                Nothing -> (0,bs)
                Just v ->
                  let (removed,unmerged) = unmergePrefix v t
                      bs' = IntMap.insert h unmerged bs
                  in (removed,bs')

-- unmerge; dual to merge
-- returns the decrease in size (as a result of removing elements of the right
-- from the left) and the new TxtTrie
{-# INLINE unmerge #-}
unmerge :: TxtTrie a -> TxtTrie a -> (Int,TxtTrie a)
unmerge Empty _ = (0,Empty)

unmerge t Empty = (0,t)

unmerge (Leaf mk v) (Leaf mk' _)
    | mk == mk' = (1,Empty)
    | otherwise = (0,Leaf mk v) -- use old key to avoid need to copy

unmerge (Leaf mk v) t
    | member (fromKey "" mk) t = (1,Empty)
    | otherwise                = (0,Leaf mk v)

unmerge (Branch n bs) r =
    let (removed,unmerged) = unmergeBranches bs (toTTINoCopy r)
        result | IntMap.size unmerged == 0 = Empty
               | otherwise                 = Branch (n - removed) unmerged
    in (removed,result)

-- unmergeBranches; dual to mergeBranches
-- returns the decrease in size (as a result of removing elements of the right
-- from the left) and the new TxtTrie
-- NOTE: not an optimal unmerge as we don't walk back up compressing on an empty trie
{-# INLINE unmergeBranches #-}
unmergeBranches :: TxtTrieInternal a -> TxtTrieInternal a -> (Int,TxtTrieInternal a)
unmergeBranches bs0 = IntMap.foldrWithKey unmergeBranches' (0,bs0)
    where
        {-# INLINE unmergeBranches' #-}
        unmergeBranches' c t (n,bs) =
#if MIN_VERSION_containers(0,5,10)
            let (removed,bs') = IntMap.alterF go c bs
                !n' = n + removed
                go Nothing = (0,Nothing)
                go (Just t') =
                    let (!removed,unmerged) = unmerge t' t
                        result =
                            case unmerged of
                                Empty -> Nothing
                                _     -> Just unmerged
                    in (removed,result)
            in (n',bs')
#else
            case IntMap.lookup c bs of
                Nothing -> (n,bs)
                Just t' ->
                    let (!removed,unmerged) = unmerge t' t
                        !n' = n + removed
                        bs' =
                            case unmerged of
                                Empty -> IntMap.delete c bs
                                _     -> IntMap.insert c unmerged bs
                    in (n',bs')
#endif

(!) :: TxtTrie a -> Txt -> a
(!) = flip find

(!?) :: TxtTrie a -> Txt -> Maybe a
(!?) = flip lookup

(\\) :: TxtTrie a -> TxtTrie a -> TxtTrie a
(\\) = difference

infixl 9 !?,\\{- -}

{-# INLINE null #-}
null :: TxtTrie a -> Bool
null Empty = True
null _ = False

{-# INLINE empty #-}
empty :: TxtTrie a
empty = Empty

{-# INLINE singleton #-}
singleton :: Txt -> a -> TxtTrie a
singleton = leaf

{-# INLINABLE size #-}
size :: TxtTrie a -> Int
size Empty = 0
size (Leaf _ _) = 1
size (Branch n _) = n

{-# INLINABLE insert #-}
insert :: Txt -> a -> TxtTrie a -> TxtTrie a
insert k v t =
    let (_,t') = merge 0 t (leaf k v)
    in t'

{-# INLINABLE union #-}
union :: TxtTrie a -> TxtTrie a -> TxtTrie a
union t0 t1 =
    let (_,t) = merge 0 t0 t1
    in t

{-# INLINABLE unions #-}
unions :: [TxtTrie a] -> TxtTrie a
unions xs = foldlStrict union empty xs

{-# INLINABLE delete #-}
delete :: Txt -> TxtTrie a -> TxtTrie a
delete x t =
    let (_,t') = unmerge t (leaf x undefined)
    in t'

{-# INLINABLE difference #-}
difference :: TxtTrie a -> TxtTrie a -> TxtTrie a
difference t0 t1 =
    let (_,t) = unmerge t0 t1
    in t

{-# INLINABLE deletePrefix #-}
deletePrefix :: Txt -> TxtTrie a -> TxtTrie a
deletePrefix x t =
    let (_,t') = unmergePrefix t x
    in t'

{-# INLINABLE lookup #-}
lookup :: Txt -> TxtTrie a -> Maybe a
lookup !k0 = go k0
    where
        go k Empty = Nothing
        go k (Leaf mk v)
            | Txt.null k && isNone mk  = Just v
            | Suffix k == mk           = Just v
            | otherwise                = Nothing
        go k (Branch _ bs) =
           case Txt.uncons k of
               Just (h,t) ->
                   case IntMap.lookup (ord h) bs of
                       Nothing -> Nothing
                       Just r  -> go t r
               Nothing ->
                   case IntMap.lookup (-1) bs of
                       Nothing -> Nothing
                       Just r  -> go k r

{-# INLINABLE find #-}
find :: Txt -> TxtTrie a -> a
find !k0 = go k0
    where
        go k Empty = not_found
        go k (Leaf mk v)
            | Txt.null k && isNone mk  = v
            | Suffix k == mk           = v
            | otherwise                = not_found
        go k (Branch _ bs) =
           case Txt.uncons k of
               Just (h,t) ->
                   case IntMap.lookup (ord h) bs of
                       Nothing -> not_found
                       Just r  -> go t r
               Nothing ->
                   case IntMap.lookup (-1) bs of
                       Nothing -> not_found
                       Just r  -> go k r

        not_found = error ("Pure.Data.Txt.Trie.!: key " ++ show k0 ++ " is not an element of the trie")

{-# INLINABLE findWithDefault #-}
findWithDefault :: a -> Txt -> TxtTrie a -> a
findWithDefault def !k0 = go k0
    where
        go k Empty = def
        go k (Leaf mk v)
            | Txt.null k && isNone mk  = v
            | Suffix k == mk           = v
            | otherwise                = def
        go k (Branch _ bs) =
           case Txt.uncons k of
               Just (h,t) ->
                   case IntMap.lookup (ord h) bs of
                       Nothing -> def
                       Just r  -> go t r
               Nothing ->
                   case IntMap.lookup (-1) bs of
                       Nothing -> def
                       Just r  -> go k r

{-# INLINABLE member #-}
member :: Txt -> TxtTrie a -> Bool
member k = isJust . lookup k

{-# INLINABLE notMember #-}
notMember :: Txt -> TxtTrie a -> Bool
notMember k = not . member k

{-# INLINABLE lookupPrefix #-}
lookupPrefix :: TxtTrie a -> Txt -> TxtTrie a
lookupPrefix Empty _ = Empty
lookupPrefix (Leaf k v) p
    | p `Txt.isPrefixOf` (fromKey ""  k) = Leaf k v
    | otherwise                          = Empty
lookupPrefix (Branch n bs) p =
    case Txt.uncons p of
        Nothing -> Branch n bs
        Just (h,t) ->
            case IntMap.lookup (ord h) bs of
                Just tt -> lookupPrefix tt t
                Nothing -> Empty

map :: (a -> b) -> TxtTrie a -> TxtTrie b
map f = go
    where
        go Empty = Empty
        go (Leaf k a) = Leaf k (f a)
        go (Branch n bs) = Branch n (IntMap.map go bs)

{-# NOINLINE [1] map #-}
{-# RULES
"map/map" forall f g xs. map f (map g xs) = map (f . g) xs
  #-}

{-# RULES
"map/coerce" map coerce = coerce
  #-}

mapWithKey :: (Txt -> a -> b) -> TxtTrie a -> TxtTrie b
mapWithKey f = go (dl [])
    where
        go k Empty = Empty
        go k (Leaf k' a) = Leaf k' (f (toTxt (fromDL k) `Txt.append` fromKey "" k') a)
        go k (Branch n bs) = Branch n (IntMap.mapWithKey go' bs)
            where
                go' c = go (k `snoc` (chr c))

{-# NOINLINE [1] mapWithKey #-}
{-# RULES
"mapWithKey/mapWithKey" forall f g xs. mapWithKey f (mapWithKey g xs) =
  mapWithKey (\k a -> f k (g k a)) xs
"mapWithKey/map" forall f g xs. mapWithKey f (map g xs) =
  mapWithKey (\k a -> f k (g a)) xs
"map/mapWithKey" forall f g xs. map f (mapWithKey g xs) =
  mapWithKey (\k a -> f (g k a)) xs
  #-}

traverseWithKey :: Applicative t => (Txt -> a -> t b) -> TxtTrie a -> t (TxtTrie b)
traverseWithKey f = go (dl [])
    where
        go k Empty = pure Empty
        go k (Leaf k' a) = Leaf k' <$> f ((toTxt (fromDL k)) `Txt.append` fromKey "" k') a
        go k (Branch n bs) = Branch n <$> IntMap.traverseWithKey go' bs
            where
                go' c = go (k `snoc` (chr c))
{-# INLINE traverseWithKey #-}

mapAccum :: (a -> b -> (a,c)) -> a -> TxtTrie b -> (a,TxtTrie c)
mapAccum f z t = mapAccumWithKey (\z' _ x' -> f z' x') z t

mapAccumWithKey :: (a -> Txt -> b -> (a,c)) -> a -> TxtTrie b -> (a,TxtTrie c)
mapAccumWithKey f z t = mapAccumL f z t

mapAccumL :: (a -> Txt -> b -> (a,c)) -> a -> TxtTrie b -> (a,TxtTrie c)
mapAccumL f z t = go z (dl []) t
    where
        go z k Empty = (z,Empty)
        go z k (Leaf k' b) =
            let
                (z',c) = f z (toTxt (fromDL k) `Txt.append` fromKey "" k') b
            in
                (z',Leaf k' c)
        go z k (Branch n bs) =
            let
                (z',bs') = IntMap.mapAccumWithKey go' z bs
            in
                (z',Branch n bs')
            where
                go' z' c = go z' (k `snoc` (chr c))

mapAccumRWithKey :: (a -> Txt -> b -> (a,c)) -> a -> TxtTrie b -> (a,TxtTrie c)
mapAccumRWithKey f z t = go z (dl []) t
    where
        go z k Empty = (z,Empty)
        go z k (Leaf k' b) =
            let
                (z',c) = f z (toTxt (fromDL k) `Txt.append` fromKey "" k') b
            in
                (z',Leaf k' c)
        go z k (Branch n bs) =
            let
                (z',bs') = IntMap.mapAccumRWithKey go' z bs
            in
                (z',Branch n bs')
            where
                go' z' c = go z' (k `snoc` (chr c))

{-# INLINABLE mapKeys #-}
mapKeys :: (Txt -> Txt) -> TxtTrie a -> TxtTrie a
mapKeys f = fromList . foldrWithKey (\k x xs -> (f k,x) : xs) []

{-# INLINE foldr #-}
foldr :: (a -> b -> b) -> b -> TxtTrie a -> b
foldr f z t = go t z
    where
        go Empty z' = z'
        go (Leaf _ v) z' = f v z'
        go (Branch _ bs) z' = IntMap.foldr go z' bs

{-# INLINE foldr' #-}
foldr' :: (a -> b -> b) -> b -> TxtTrie a -> b
foldr' f z t = go t z
    where
        go Empty !z' = z'
        go (Leaf _ v) z' = f v z'
        go (Branch _ bs) z' = IntMap.foldr' go z' bs

{-# INLINE foldl #-}
foldl :: (a -> b -> a) -> a -> TxtTrie b -> a
foldl f z = go z
    where
        go z' Empty = z'
        go z' (Leaf _ v) = f z' v
        go z' (Branch _ bs) = IntMap.foldl go z' bs

{-# INLINE foldl' #-}
foldl' :: (a -> b -> a) -> a -> TxtTrie b -> a
foldl' f z = go z
    where
        go !z' Empty = z'
        go z' (Leaf _ v) = f z' v
        go z' (Branch _ bs) = IntMap.foldl' go z' bs

{-# INLINE foldrWithKey #-}
foldrWithKey :: (Txt -> a -> b -> b) -> b -> TxtTrie a -> b
foldrWithKey f z = go (dl []) z
    where
        go key z Empty = z
        go key z (Leaf mk v)
            | isNone mk = f (toTxt (fromDL key)) v z
            | otherwise = f (toTxt (fromDL key) `Txt.append` fromKey "" mk) v z
        go key z (Branch _ bs) = IntMap.foldrWithKey go' z bs
            where
                go' c t z' =
                    case c of
                        (-1) -> go key z' t
                        _    -> go (key `snoc` chr c) z' t

{-# INLINE foldrWithKey' #-}
foldrWithKey' :: (Txt -> a -> b -> b) -> b -> TxtTrie a -> b
foldrWithKey' f z = go (dl []) z
    where
        go key !z Empty = z
        go key z (Leaf mk v)
            | isNone mk = f (toTxt (fromDL key)) v z
            | otherwise = f (toTxt (fromDL key) `Txt.append` fromKey "" mk) v z
        go key z (Branch _ bs) = IntMap.foldrWithKey' go' z bs
            where
                go' c t z' =
                    case c of
                        (-1) -> go key z' t
                        _    -> go (key `snoc` chr c) z' t

{-# INLINE foldlWithKey #-}
foldlWithKey :: (a -> Txt -> b -> a) -> a -> TxtTrie b -> a
foldlWithKey f z = go (dl []) z
    where
        {-# INLINE go #-}
        go key z Empty = z
        go key z (Leaf mk v)
            | isNone mk = f z (toTxt (fromDL key)) v
            | otherwise = f z (toTxt (fromDL key) `Txt.append` fromKey "" mk) v
        go key z (Branch _ bs) = IntMap.foldlWithKey go' z bs
            where
                {-# INLINE go' #-}
                go' z' c t =
                    case c of
                        (-1) -> go key z' t
                        _    -> go (key `snoc` chr c) z' t

{-# INLINE foldlWithKey' #-}
foldlWithKey' :: (a -> Txt -> b -> a) -> a -> TxtTrie b -> a
foldlWithKey' f z = go (dl []) z
    where
        {-# INLINE go #-}
        go key !z Empty = z
        go key z (Leaf mk v)
            | isNone mk = f z (toTxt (fromDL key)) v
            | otherwise = f z (toTxt (fromDL key) `Txt.append` fromKey "" mk) v
        go key z (Branch _ bs) = IntMap.foldlWithKey' go' z bs
            where
                {-# INLINE go' #-}
                go' z' c t =
                    case c of
                        (-1) -> go key z' t
                        _    -> go (key `snoc` chr c) z' t

{-# INLINE foldMapWithKey #-}
foldMapWithKey :: Monoid m => (Txt -> a -> m) -> TxtTrie a -> m
foldMapWithKey f = go (dl [])
    where
        {-# INLINE go #-}
        go key Empty = mempty
        go key (Leaf mk v)
            | isNone mk = f (toTxt (fromDL key)) v
            | otherwise = f (toTxt (fromDL key) `Txt.append` fromKey "" mk) v
        go key (Branch _ bs) = IntMap.foldMapWithKey go' bs
            where
                {-# INLINE go' #-}
                go' c t =
                    case c of
                        (-1) -> go key t
                        _    -> go (key `snoc` chr c) t

{-# INLINABLE fromList #-}
fromList :: [(Txt,a)] -> TxtTrie a
fromList xs = foldlStrict ins Empty xs
    where
        ins t (k,v) = insert k v t

{-# INLINE toList #-}
toList :: TxtTrie a -> [(Txt,a)]
toList = toAscList

toAscList :: TxtTrie a -> [(Txt,a)]
toAscList = foldrWithKey (\k v xs -> (k,v):xs) []

toDescList :: TxtTrie a -> [(Txt,a)]
toDescList = foldlWithKey (\xs k v -> (k,v):xs) []

{-# INLINE assocs #-}
assocs :: TxtTrie a -> [(Txt,a)]
assocs t = toAscList t

keys :: TxtTrie a -> [Txt]
keys = foldrWithKey (\k _ ks -> k:ks) []

elems :: TxtTrie a -> [a]
elems = foldrWithKey (\_ v vs -> v:vs) []

{-# INLINABLE recompress #-}
recompress :: TxtTrie a -> TxtTrie a
recompress = fromList . toList

{-# INLINABLE splitRoot #-}
splitRoot :: TxtTrie a -> [TxtTrie a]
splitRoot Empty = []
splitRoot (Leaf k v) = [Leaf k v]
splitRoot (Branch _ bs) = Prelude.map (\b -> Branch (internalSizes b) b) $ IntMap.splitRoot bs

{-# INLINE internalSizes #-}
internalSizes :: TxtTrieInternal a -> Int
internalSizes = IntMap.foldr' go 0
    where
        go Empty n = n
        go (Leaf _ _) n = n + 1
        go (Branch n' _) n = n + n'

foldrFB :: (Txt -> a -> b -> b) -> b -> TxtTrie a -> b
foldrFB = foldrWithKey
{-# INLINE[0] foldrFB #-}

foldlFB :: (a -> Txt -> b -> a) -> a -> TxtTrie b -> a
foldlFB = foldlWithKey
{-# INLINE[0] foldlFB #-}

{-# NOINLINE[0] elems #-}
{-# NOINLINE[0] keys #-}
{-# NOINLINE[0] toAscList #-}
{-# NOINLINE[0] toDescList #-}
{-# RULES "Trie.elems" [~1] forall t. elems t = build (\c n -> foldrFB (\_ x xs -> c x xs ) n t) #-}
{-# RULES "Trie.elemsBack" [1] foldrFB (\_ x xs -> x : xs) [] = elems #-}
{-# RULES "Trie.keys" [~1] forall t. keys t = build (\c n -> foldrFB (\k _ xs -> c k xs) n t) #-}
{-# RULES "Trie.keysBack" [1] foldrFB (\k _ xs -> k : xs) [] = keys #-}
{-# RULES "Trie.toAscList" [~1] forall t. toAscList t = build (\c n -> foldrFB (\k x xs -> c (k,x) xs) n t) #-}
{-# RULES "Trie.toAscListBack" [1] foldrFB (\k x xs -> (k,x) : xs) [] = toAscList #-}
{-# RULES "Trie.toDescList" [~1] forall t. toDescList t = build (\c n -> foldlFB (\xs k x -> c (k,x) xs) n t) #-}
{-# RULES "Trie.toDescListBack" [1] foldlFB (\xs k x -> (k,x):xs) [] = toDescList #-}
