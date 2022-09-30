{-# language RecursiveDo, NamedFieldPuns, ViewPatterns, OverloadedStrings #-}
module Data.View.Transform 
  ( transform
  , over
  , contains
  , content, contentInRange
  , tags, tagsInRange, tagsAround
  , replaceTagWith
  , split
  , drop, take
  , cut, span
  , find
  , walkr
  , foldo, foldoM
  , foldll, foldlr, foldrl, foldrr
  , foldllM, foldlrM, foldrlM, foldrrM
  ) where

import Data.Txt (Txt)
import qualified Data.Txt as Txt
import qualified Data.Set as Set
import Data.View hiding (content)

import Control.Monad.Fix
import qualified Data.List as List
import Data.Maybe
import Prelude hiding (drop,take,span)

import qualified Origami.Fold as O

-- | Generically transfrrom the nodes of a `[View]` with access to the offset
-- of the node and the length of the content within it while passing an arbitrary
-- state parameter through (from the bottom).
transform :: (Int -> Int -> a -> View -> (a,View)) -> a -> [View] -> (a,[View])
transform f a = fmap getChildren . walkr f a . flip setChildren (SimpleHTML "p")

-- | Functionally transform a `[View]` over the given range.
over :: Int -> Int -> ([View] -> [View]) -> [View] -> [View]
over start end f = go . span start end
  where 
    go (before,target,after) = before <> f target <> after

-- | Does a `[View]` contain a `View` node satisfying the predicate at the 
-- given offset?
contains :: (View -> Bool) -> [View] -> Bool
contains t = isJust . find t . flip setChildren (SimpleHTML "p")

content :: [View] -> Txt
content = mconcat . fmap go
  where
    go (Txt c) = c
    go (Children cs _) = mconcat (fmap go cs) 
    go _ = mempty

contentInRange :: Int -> Int -> View -> Txt
contentInRange start end = 
  Txt.take (end - start) . Txt.drop start . content . (:[])

-- | Extract tags for all nested views, including the root. First-billed 
-- top-down and left-to-right.
tags :: View -> [Txt]
tags = nub . collect
  where
    nub xs = List.foldr go (const []) xs mempty
      where
        go key cont hm 
          | Set.member key hm = cont hm
          | otherwise         = key : cont (Set.insert key hm)  

    collect (Children cs HTMLView  { tag }) = tag : concatMap collect cs
    collect (Children cs KHTMLView { tag }) = tag : concatMap collect cs
    collect (Children cs SVGView   { tag }) = tag : concatMap collect cs
    collect (Children cs KSVGView  { tag }) = tag : concatMap collect cs
    collect _                               = []

-- | Extract tags for all views in the text span.
-- 
-- >>> tagsInRange 0 4 (SimpleHTML "a" <||> [ SimpleHTML "b" <||> [ "ab" ], SimpleHTML "span" <||> [ "cd" ], SimpleHTML "em" <||> [ "ef" ]])
-- ["a","b","span"]
tagsInRange :: Int -> Int -> View -> [Txt]
tagsInRange start end v = 
  let (_,target,_) = cut start end v
  in tags target

tagsAround :: Int -> Int -> View -> [Txt]
tagsAround start end v 
  | start == end 
  , (before,after) <- split start v
  , right <- rightmostTags before
  , left  <- leftmostTags after
  = List.nub (right ++ left)

  | (before,target,after) <- cut start end v
  , right  <- rightmostTags before
  , center <- tags target
  , left   <- leftmostTags after
  = List.nub (right ++ center ++ left)

leftmostTags :: View -> [Txt]
leftmostTags = List.nub . go
  where
    go (Children cs HTMLView { tag }) = tag : rest cs
      where
        rest [] = []
        rest (c : _) = go c
    go _ = []

rightmostTags :: View -> [Txt]
rightmostTags = List.nub . go
  where
    go (Children cs HTMLView { tag }) = tag : rest cs
      where
        rest [] = []
        rest [c] = go c
        rest (_ : xs) = rest xs
    go _ = []

replaceTagWith :: Txt -> View -> View -> View
replaceTagWith t r = go
  where
    go (Children cs HTMLView  { tag }) | t == tag = r <||> fmap go cs
    go (Children cs KHTMLView { tag }) | t == tag = r <||> fmap go cs
    go (Children cs SVGView   { tag }) | t == tag = r <||> fmap go cs
    go (Children cs KSVGView  { tag }) | t == tag = r <||> fmap go cs
    go (Children cs v) = v <||> fmap go cs
    go v = v

split :: Int -> View -> (View,View)
split start v0 = (before,after)
  where
    before = fromMaybe Null (snd (taking start v0))
      where
        taking :: Int -> View -> (Int,Maybe View)
        taking 0 _ = (0,Nothing)

        taking n v@(Txt c@(Txt.length -> l)) 
          | n < l = (0,Just (TextView Nothing (Txt.take n c)))
          | otherwise = (n - l,Just v)
          
        taking n (Children cs v) = 
          let 
            (n',cs') = List.mapAccumL taking n cs
            r =
              case catMaybes cs' of
                [] -> Nothing
                js -> Just (v <||> js)
          in
            (n',r)
            
        taking n _ = (n,Nothing)

    after = fromMaybe Null (snd (dropping start v0))
      where
        dropping :: Int -> View -> (Int,Maybe View)
        dropping 0 v = (0,Just v)

        dropping n (Txt c@(Txt.length -> l)) 
          | n < l = (0,Just (TextView Nothing (Txt.drop n c)))
          | otherwise = (n - l,Nothing)
          
        dropping n (Children cs v) = 
          let 
            (n',cs') = List.mapAccumL dropping n cs
            r =
              case catMaybes cs' of
                [] -> Nothing
                js -> Just (v <||> js) 
          in 
            (n',r)
            
        dropping node _ = (node,Nothing)

drop :: Int -> View -> View
drop n v = after
  where
    (_before,after) = split n v

take :: Int -> View -> View
take n v = before
  where
    (before,_after) = split n v

cut :: Int -> Int -> View -> (View,View,View)
cut start end v = (before,middle,after)
  where 
    (before,rest) = split start v
    (middle,after) = split (end - start) rest

span :: Int -> Int -> [View] -> ([View],[View],[View])
span start end v = (before,middle,after)
  where
    (Children before _,Children rest _) = split start (SimpleHTML "" <||> v)
    (Children middle _,Children after _) = split (end - start) (SimpleHTML "" <||> rest)

find :: (View -> Bool) -> View -> Maybe View
find f = fst . walkr go Nothing
  where
    go _ _ b v | isNothing b && f v = (Just v,v)
    go _ _ b v = (b,v)

walkr :: (Int -> Int -> a -> View -> (a,View)) -> a -> View -> (a,View)
walkr f a0 = (\((_,_,a),v) -> (a,v)) . go (0,0,a0)
  where
    -- TODO: refactor
    -- This is why people complain about Haskellers. Sorry.
    go (o,l,a) v@(Children cs HTMLView {}) = 
      let ((o',l',a'),cs') = List.mapAccumL go (o,0,a) cs
          (a'',v') = f o l' a' (v <||> []) 
      in ((o',l + l',a''),v' <||> cs')
    
    go (o,l,a) v@(Txt t) =
      let tl = Txt.length t
          (a',v') = f o tl a v
      in ((o + tl,l + tl,a'),v')
      
    go st v = (st,v)

-- lazy monoidal fold over a View tree; use derivative folds (fold[l/r][l/r]) to avoid <loop>s.
{-# INLINE foldo #-}
foldo :: Monoid m => m -> (m -> m -> m) -> (m -> m -> m) -> (m -> m -> m -> View -> (View,m)) -> View -> (View,m)
foldo m0 c1 c2 f ta = (tb,final)
  where
    ~(tb,final) = go m0 ta
      where
        go as_st (Children cs v) = (v' <||> cs',c1 st cs_st)
          where
            ~(v',st) = f as_st cs_st final v
            ~(cs',cs_st) = O.foldo mempty c2 c1 (\_ _ _ -> go (c1 as_st st)) cs
        
        go as_st v = f as_st mempty final v

-- lazy monadic monoidal fold over a View tree; use derivative folds (fold[l/r][l/r]M) to avoid <loop>s.
{-# INLINE foldoM #-}
foldoM :: (Monoid m, MonadFix c) => m -> (m -> m -> m) -> (m -> m -> m) -> (m -> m -> m -> View -> c (View, m)) -> View -> c (View, m)
foldoM m0 c1 c2 f ta = mdo
  let go as_st (Children cs v) = mdo
        ~(v',st)     <- f as_st cs_st final v
        ~(cs',cs_st) <- O.foldoM mempty c2 c1 (\_ _ _ -> go (c1 as_st st)) cs
        return (v' <||> cs',c1 st cs_st)
      go as_st v = f as_st mempty final v
  ~(tb,final) <- go m0 ta
  return (tb,final)

-- | foldll folds from the top/start and mappends from left-to-right
{-# INLINE foldll #-}
foldll :: Monoid m => (m -> m -> m -> View -> View) -> (m -> View -> m) -> View -> (View,m)
foldll f g = foldo mempty mappend mappend (\as ds t c -> (f as ds t c,g as c))

-- | foldlr folds from the top/start and mappends from right-to-left
{-# INLINE foldlr #-}
foldlr :: Monoid m => (m -> m -> m -> View -> View) -> (m -> View -> m) -> View -> (View,m)
foldlr f g = foldo mempty mappend (flip mappend) (\as ds t c -> (f as ds t c,g as c))

-- | foldrl folds from the bottom/end and mappends from left-to-right
{-# INLINE foldrl #-}
foldrl :: Monoid m => (m -> m -> m -> View -> View) -> (m -> View -> m) -> View -> (View,m)
foldrl f g = foldo mempty (flip mappend) mappend (\as ds t c -> (f as ds t c,g ds c))

-- | foldrr folds from the bottom/end and mappends from right-to-left
{-# INLINE foldrr #-}
foldrr :: Monoid m => (m -> m -> m -> View -> View) -> (m -> View -> m) -> View -> (View,m)
foldrr f g = foldo mempty (flip mappend) (flip mappend) (\as ds t c -> (f as ds t c,g ds c))

-- | foldllM folds from the top/start and mappends from left-to-right monadically
{-# INLINE foldllM #-}
foldllM :: (Monoid m, MonadFix c) => (m -> m -> m -> View -> c View) -> (m -> View -> c m) -> View -> c (View,m)
foldllM f g = foldoM mempty mappend mappend $ \as ds t c -> mdo
  b <- f as ds t c
  m <- g as c
  return (b,m)

-- | foldlrM folds from the top/start and mappends from right-to-left monadically
{-# INLINE foldlrM #-}
foldlrM :: (Monoid m, MonadFix c) => (m -> m -> m -> View -> c View) -> (m -> View -> c m) -> View -> c (View,m)
foldlrM f g = foldoM mempty mappend (flip mappend) $ \as ds t c -> mdo
  b <- f as ds t c
  m <- g as c
  return (b,m)

-- | foldrlM folds from the bottom/end and mappends from left-to-right monadically
{-# INLINE foldrlM #-}
foldrlM :: (Monoid m, MonadFix c) => (m -> m -> m -> View -> c View) -> (m -> View -> c m) -> View -> c (View,m)
foldrlM f g = foldoM mempty (flip mappend) mappend $ \as ds t c -> mdo
  b <- f as ds t c
  m <- g ds c
  return (b,m)

-- | foldrrM folds from the bottom/end and mappends from right-to-left monadically
{-# INLINE foldrrM #-}
foldrrM :: (Monoid m, MonadFix c) => (m -> m -> m -> View -> c View) -> (m -> View -> c m) -> View -> c (View,m)
foldrrM f g = foldoM mempty (flip mappend) (flip mappend) $ \as ds t c -> mdo
  b <- f as ds t c
  m <- g ds c
  return (b,m)
