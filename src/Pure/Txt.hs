{-# LANGUAGE FlexibleContexts, FlexibleInstances, ConstraintKinds, CPP #-}
module Pure.Txt (module Pure.Txt, module Coerce, module Export

#ifdef __GHCJS__
                , JSString(..)
#endif

                ) where

import Pure.Data.Txt as Export (Txt,ToTxt(..),FromTxt(..))
import Pure.Data.Txt as Txt

import Data.Coerce as Coerce

import Data.Roles
import Data.Type.Coercion

import Pure.Data.Cond
import Pure.Data.Default

type IsTxt a = Coercible Txt a

instance {-# OVERLAPPABLE #-} IsTxt a => Default a where
  def = Pure.Txt.empty

instance {-# OVERLAPPABLE #-} IsTxt a => Cond a where
  nil = Pure.Txt.empty
  isNil = Pure.Txt.null

instance {-# OVERLAPPABLE #-} IsTxt a => FromTxt a where
  fromTxt = repack

instance {-# OVERLAPPABLE #-} IsTxt a => ToTxt a where
  toTxt = repack

{-# INLINE convert #-}
convert :: (IsTxt a, FromTxt b) => a -> b
convert a = fromTxt (repack a)

{-# INLINE repack #-}
repack :: (IsTxt a, IsTxt b) => a -> b
repack = coerce

{-# INLINE repacks #-}
repacks :: (IsTxt a, IsTxt b, Coercible a b, Representational f, Functor f) => f a -> f b
repacks = coerceWith (rep Coercion)

{-# INLINE pack #-}
pack :: IsTxt a => String -> a
pack = coerce Txt.pack

{-# INLINE unpack #-}
unpack :: IsTxt a => a -> String
unpack = coerce Txt.unpack

{-# INLINE singleton #-}
singleton :: IsTxt a => Char -> a
singleton = coerce Txt.singleton

{-# INLINE empty #-}
empty :: IsTxt a => a
empty = coerce Txt.empty

{-# INLINE cons #-}
cons :: IsTxt a => Char -> a -> a
cons = coerce Txt.cons

{-# INLINE snoc #-}
snoc :: IsTxt a => a -> Char -> a
snoc = coerce Txt.snoc

{-# INLINE append #-}
append :: IsTxt a => a -> a -> a
append = coerce Txt.append

{-# INLINE uncons #-}
uncons :: IsTxt a => a -> Maybe (Char,a)
uncons = coerce Txt.uncons

{-# INLINE head #-}
head :: IsTxt a => a -> Char
head = coerce Txt.head

{-# INLINE last #-}
last :: IsTxt a => a -> Char
last = coerce Txt.last

{-# INLINE tail #-}
tail :: IsTxt a => a -> a
tail = coerce Txt.tail

{-# INLINE init #-}
init :: IsTxt a => a -> a
init = coerce Txt.init

{-# INLINE null #-}
null :: IsTxt a => a -> Bool
null = coerce Txt.null

{-# INLINE length #-}
length :: IsTxt a => a -> Int
length = coerce Txt.length

{-# INLINE compareLength #-}
compareLength :: IsTxt a => a -> Int -> Ordering
compareLength = coerce Txt.compareLength

{-# INLINE map #-}
map :: IsTxt a => (Char -> Char) -> a -> a
map = coerce Txt.map

{-# INLINE intercalate #-}
intercalate :: IsTxt a => a -> [a] -> a
intercalate = coerce Txt.intercalate

{-# INLINE intersperse #-}
intersperse :: IsTxt a => Char -> a -> a
intersperse = coerce Txt.intersperse

{-# INLINE transpose #-}
transpose :: IsTxt a => [a] -> [a]
transpose = coerce Txt.transpose

{-# INLINE reverse #-}
reverse :: IsTxt a => a -> a
reverse = coerce Txt.reverse

{-# INLINE replace #-}
replace :: IsTxt a => a -> a -> a -> a
replace = coerce Txt.replace

{-# INLINE toCaseFold #-}
toCaseFold :: IsTxt a => a -> a
toCaseFold = coerce Txt.toCaseFold

{-# INLINE toLower #-}
toLower :: IsTxt a => a -> a
toLower = coerce Txt.toLower

{-# INLINE toUpper #-}
toUpper :: IsTxt a => a -> a
toUpper = coerce Txt.toUpper

{-# INLINE toTitle #-}
toTitle :: IsTxt a => a -> a
toTitle = coerce Txt.toTitle

{-# INLINE justifyLeft #-}
justifyLeft :: IsTxt a => Int -> Char -> a -> a
justifyLeft = coerce Txt.justifyLeft

{-# INLINE justifyRight #-}
justifyRight :: IsTxt a => Int -> Char -> a -> a
justifyRight = coerce Txt.justifyRight

{-# INLINE center #-}
center :: IsTxt a => Int -> Char -> a -> a
center = coerce Txt.center

{-# INLINE foldl #-}
foldl :: IsTxt a => (st -> Char -> st) -> st -> a -> st
foldl f st = Txt.foldl f st . coerce

{-# INLINE foldl' #-}
foldl' :: IsTxt a => (st -> Char -> st) -> st -> a -> st
foldl' f st = Txt.foldl' f st . coerce

{-# INLINE foldl1 #-}
foldl1 :: IsTxt a => (Char -> Char -> Char) -> a -> Char
foldl1 = coerce Txt.foldl1

{-# INLINE foldl1' #-}
foldl1' :: IsTxt a => (Char -> Char -> Char) -> a -> Char
foldl1' = coerce Txt.foldl1'

{-# INLINE foldr #-}
foldr :: IsTxt a => (Char -> st -> st) -> st -> a -> st
foldr f st = Txt.foldr f st . coerce

{-# INLINE foldr1 #-}
foldr1 :: IsTxt a => (Char -> Char -> Char) -> a -> Char
foldr1 = coerce Txt.foldr1

{-# INLINE concat #-}
concat :: IsTxt a => [a] -> a
concat = coerce Txt.concat

{-# INLINE concatMap #-}
concatMap :: IsTxt a => (Char -> a) -> a -> a
concatMap = coerce Txt.concatMap

{-# INLINE any #-}
any :: IsTxt a => (Char -> Bool) -> a -> Bool
any = coerce Txt.any

{-# INLINE all #-}
all :: IsTxt a => (Char -> Bool) -> a -> Bool
all = coerce Txt.all

{-# INLINE maximum #-}
maximum :: IsTxt a => a -> Char
maximum = coerce Txt.maximum

{-# INLINE minimum #-}
minimum :: IsTxt a => a -> Char
minimum = coerce Txt.minimum

{-# INLINE scanl #-}
scanl :: IsTxt a => (Char -> Char -> Char) -> Char -> a -> a
scanl = coerce Txt.scanl

{-# INLINE scanl1 #-}
scanl1 :: IsTxt a => (Char -> Char -> Char) -> a -> a
scanl1 = coerce Txt.scanl1

{-# INLINE scanr #-}
scanr :: IsTxt a => (Char -> Char -> Char) -> Char -> a -> a
scanr = coerce Txt.scanr

{-# INLINE scanr1 #-}
scanr1 :: IsTxt a => (Char -> Char -> Char) -> a -> a
scanr1 = coerce Txt.scanr1

{-# INLINE mapAccumL #-}
mapAccumL :: IsTxt a => (st -> Char -> (st,Char)) -> st -> a -> (st,a)
mapAccumL f st = coerce . Txt.mapAccumL f st . coerce

{-# INLINE mapAccumR #-}
mapAccumR :: IsTxt a => (st -> Char -> (st,Char)) -> st -> a -> (st,a)
mapAccumR f st = coerce . Txt.mapAccumR f st . coerce

{-# INLINE replicate #-}
replicate :: IsTxt a => Int -> a -> a
replicate = coerce Txt.replicate

{-# INLINE unfoldr #-}
unfoldr :: IsTxt a => (st -> Maybe (Char,st)) -> st -> a
unfoldr f st = coerce (Txt.unfoldr f st)

{-# INLINE unfoldrN #-}
unfoldrN :: IsTxt a => Int -> (st -> Maybe (Char,st)) -> st -> a
unfoldrN n f st = coerce (Txt.unfoldrN n f st)

{-# INLINE take #-}
take :: IsTxt a => Int -> a -> a
take = coerce Txt.take

{-# INLINE takeEnd #-}
takeEnd :: IsTxt a => Int -> a -> a
takeEnd = coerce Txt.takeEnd

{-# INLINE drop #-}
drop :: IsTxt a => Int -> a -> a
drop = coerce Txt.drop

{-# INLINE dropEnd #-}
dropEnd :: IsTxt a => Int -> a -> a
dropEnd = coerce Txt.dropEnd

{-# INLINE takeWhile #-}
takeWhile :: IsTxt a => (Char -> Bool) -> a -> a
takeWhile = coerce Txt.takeWhile

{-# INLINE takeWhileEnd #-}
takeWhileEnd :: IsTxt a => (Char -> Bool) -> a -> a
takeWhileEnd = coerce Txt.takeWhileEnd

{-# INLINE dropWhile #-}
dropWhile :: IsTxt a => (Char -> Bool) -> a -> a
dropWhile = coerce Txt.dropWhile

{-# INLINE dropWhileEnd #-}
dropWhileEnd :: IsTxt a => (Char -> Bool) -> a -> a
dropWhileEnd = coerce Txt.dropWhileEnd

{-# INLINE dropAround #-}
dropAround :: IsTxt a => (Char -> Bool) -> a -> a
dropAround = coerce Txt.dropAround

{-# INLINE strip #-}
strip :: IsTxt a => a -> a
strip = coerce Txt.strip

{-# INLINE stripStart #-}
stripStart :: IsTxt a => a -> a
stripStart = coerce Txt.stripStart

{-# INLINE stripEnd #-}
stripEnd :: IsTxt a => a -> a
stripEnd = coerce Txt.stripEnd

{-# INLINE splitAt #-}
splitAt :: IsTxt a => Int -> a -> (a,a)
splitAt = coerce Txt.splitAt

{-# INLINE breakOn #-}
breakOn :: IsTxt a => a -> a -> (a,a)
breakOn = coerce Txt.breakOn

{-# INLINE breakOnEnd #-}
breakOnEnd :: IsTxt a => a -> a -> (a,a)
breakOnEnd = coerce Txt.breakOnEnd

{-# INLINE break #-}
break :: IsTxt a => (Char -> Bool) -> a -> (a,a)
break = coerce Txt.break

{-# INLINE span #-}
span :: IsTxt a => (Char -> Bool) -> a -> (a,a)
span = coerce Txt.span

{-# INLINE group #-}
group :: IsTxt a => a -> [a]
group = coerce Txt.group

{-# INLINE groupBy #-}
groupBy :: IsTxt a => (Char -> Char -> Bool) -> a -> [a]
groupBy = coerce Txt.groupBy

{-# INLINE inits #-}
inits :: IsTxt a => a -> [a]
inits = coerce Txt.inits

{-# INLINE tails #-}
tails :: IsTxt a => a -> [a]
tails = coerce Txt.tails

{-# INLINE splitOn #-}
splitOn :: IsTxt a => a -> a -> [a]
splitOn = coerce Txt.splitOn

{-# INLINE split #-}
split :: IsTxt a => (Char -> Bool) -> a -> [a]
split = coerce Txt.split

{-# INLINE chunksOf #-}
chunksOf :: IsTxt a => Int -> a -> [a]
chunksOf = coerce Txt.chunksOf

{-# INLINE lines #-}
lines :: IsTxt a => a -> [a]
lines = coerce Txt.lines

{-# INLINE words #-}
words :: IsTxt a => a -> [a]
words = coerce Txt.words

{-# INLINE unlines #-}
unlines :: IsTxt a => [a] -> a
unlines = coerce Txt.unlines

{-# INLINE unwords #-}
unwords :: IsTxt a => [a] -> a
unwords = coerce Txt.unwords

{-# INLINE isPrefixOf #-}
isPrefixOf :: IsTxt a => a -> a -> Bool
isPrefixOf = coerce Txt.isPrefixOf

{-# INLINE isSuffixOf #-}
isSuffixOf :: IsTxt a => a -> a -> Bool
isSuffixOf = coerce Txt.isSuffixOf

{-# INLINE isInfixOf #-}
isInfixOf :: IsTxt a => a -> a -> Bool
isInfixOf = coerce Txt.isInfixOf

{-# INLINE stripPrefix #-}
stripPrefix :: IsTxt a => a -> a -> Maybe a
stripPrefix = coerce Txt.stripPrefix

{-# INLINE stripSuffix #-}
stripSuffix :: IsTxt a => a -> a -> Maybe a
stripSuffix = coerce Txt.stripSuffix

{-# INLINE commonPrefixes #-}
commonPrefixes :: IsTxt a => a -> a -> Maybe (a,a,a)
commonPrefixes = coerce Txt.commonPrefixes

{-# INLINE filter #-}
filter :: IsTxt a => (Char -> Bool) -> a -> a
filter = coerce Txt.filter

{-# INLINE breakOnAll #-}
breakOnAll :: IsTxt a => a -> a -> [(a,a)]
breakOnAll = coerce Txt.breakOnAll

{-# INLINE find #-}
find :: IsTxt a => (Char -> Bool) -> a -> Maybe Char
find = coerce Txt.find

{-# INLINE partition #-}
partition :: IsTxt a => (Char -> Bool) -> a -> (a,a)
partition = coerce Txt.partition

{-# INLINE index #-}
index :: IsTxt a => a -> Int -> Char
index = coerce Txt.index

{-# INLINE findIndex #-}
findIndex :: IsTxt a => (Char -> Bool) -> a -> Maybe Int
findIndex = coerce Txt.findIndex

{-# INLINE count #-}
count :: IsTxt a => a -> a -> Int
count = coerce Txt.count

{-# INLINE zip #-}
zip :: IsTxt a => a -> a -> [(Char,Char)]
zip = coerce Txt.zip

{-# INLINE zipWith #-}
zipWith :: IsTxt a => (Char -> Char -> Char) -> a -> a -> a
zipWith = coerce Txt.zipWith
