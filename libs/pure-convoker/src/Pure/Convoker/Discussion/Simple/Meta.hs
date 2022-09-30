module Pure.Convoker.Discussion.Simple.Meta
  ( Meta(..)
  , Resource(..)
  , Product(..)
  , Preview(..)
  , Amend(..)
  , Action(..)
  , Reaction(..)
  , trySetVote
  , topSorter
  , popularSorter
  , controversialSorter
  , bestSorter
  , worstSorter
  , newSorter
  , oldSorter
  , wilson
  ) where

import Control.Component hiding (pattern Meta)
import Data.Default
import Data.JSON hiding (Key)
import Data.Time
import Pure.Auth hiding (Key)
import Pure.Convoker.Comment
import Pure.Convoker.Meta
import Pure.Convoker.Mods
import Pure.Convoker.UserVotes
import Pure.Conjurer

import Data.Hashable

import Control.Monad
import Data.List as List
import Data.Maybe
import Data.Typeable
import GHC.Generics hiding (Meta)

{-
Design notes:

  What is inherited:

    data instance Context (Meta domain a)
    data instance Name (Meta domain a)

  What is implemented here:

    data instance Resource (Meta domain a)
    data instance Product (Meta domain a)
    data instance Preview (Meta domain a)
    instance Processable (Meta domain a)
    instance Producible (Meta domain a)
    instance Previewable (Meta domain a)
    instance Amendable (Meta domain a)
    data instance Action (Meta domain a)
    data instance Reaction (Meta domain a)
  
  What is overridable:
    
    instance Previewable (Meta domain a)
    instance Processable (Meta domain a)
    instance Producible  (Meta domain a)
    
-}

newtype instance Resource (Meta domain a) = RawMeta
  { votes :: Votes domain a
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

instance Default (Resource (Meta domain a)) where
  def = RawMeta (Votes [])

newtype instance Product (Meta domain a) = Meta
  { votes :: Votes domain a
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data instance Preview (Meta domain a) = NoMetaPreview
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance Processable (Meta domain a)

instance Producible (Meta domain a) where
  produce _ _ RawMeta {..} _ = pure Meta {..}

instance Previewable (Meta domain a) where
  preview _ _ _ _ = pure NoMetaPreview

instance Amendable (Meta domain a) where
  data Amend (Meta domain a)
    = SetVote (AmendVote domain a)
    deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

  amend (SetVote amnd) RawMeta {..} =
    Just RawMeta
      { votes = amendVotes amnd votes
      , ..
      }

data instance Action (Meta domain a) = NoMetaAction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)
data instance Reaction (Meta domain a) = NoMetaReaction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

trySetVote
  :: ( Typeable domain
     , Typeable a
     , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
     , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Hashable (Name a), Ord (Name a)
     )
    => Permissions (Meta domain a) -> Callbacks (Meta domain a) -> Context a -> Name a -> Username -> Key (Comment domain a) -> Int -> IO Bool
trySetVote permissions callbacks ctx nm un k v = fmap isJust do
  now <- time
  tryAmend permissions callbacks (MetaContext ctx nm) MetaName
    (SetVote (Vote un k now v))

instance
  ( Typeable domain
  , Typeable a
  , DefaultPermissions (Meta domain a), DefaultCallbacks (Meta domain a)
  , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
  , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Hashable (Name a), Ord (Name a)
  ) => DefaultCallbacks (UserVotes domain a)
  where
    callbacks Nothing = def
    callbacks (Just un) = def { onAmend = onAmend' }
      where
        onAmend' (UserVotesContext ctx nm) (UserVotesName un) res pro pre lst = \case
          Upvote comment -> void do
            now <- time
            tryAmend fullPermissions (callbacks (Just un)) (MetaContext ctx nm) MetaName
              (SetVote (Vote un comment now 1))

          Downvote comment -> void do
            now <- time
            tryAmend fullPermissions (callbacks (Just un)) (MetaContext ctx nm) MetaName
              (SetVote (Vote un comment now (-1)))

newtype SimpleSorter domain a = SimpleSorter (Double,Key (Comment domain a))
instance Eq (SimpleSorter domain a) where
  (==) (SimpleSorter ss0) (SimpleSorter ss1) = ss0 == ss1
instance Ord (SimpleSorter domain a) where
  compare (SimpleSorter (c0,k0)) (SimpleSorter (c1,k1)) =
    case compare c1 c0 of
      EQ -> compare k0 k1
      x  -> x

wilson :: Double -> Double -> Double
wilson positive total
  | total == 0 = 0
  | total < positive = 0
  | otherwise =
    let
      z = 0.96
      z2 = z ^ 2
      r = positive / total
      x = r + z2 / (2 * total)
      y = z * sqrt((r * (1 - r) + z2 / (4 * total) ) / total)
    in
      (x - y) / (1 + z2 / total)

topSorter :: CommentSorter domain a (SimpleSorter domain a)
topSorter Meta { votes = Votes vs } Comment { key } = SimpleSorter (maybe 0 (\(ups,downs,_,_) -> fromIntegral (ups - downs)) (List.lookup key vs),key)

bestSorter :: CommentSorter domain a (SimpleSorter domain a)
bestSorter Meta { votes = Votes vs } Comment { key } = SimpleSorter (maybe 0 (\(ups,downs,_,_) -> wilson (fromIntegral ups) (fromIntegral (ups + downs))) (List.lookup key vs),key)

worstSorter :: CommentSorter domain a (SimpleSorter domain a)
worstSorter Meta { votes = Votes vs } Comment { key } = SimpleSorter (maybe 0 (\(ups,downs,_,_) -> wilson (fromIntegral downs) (fromIntegral (ups + downs))) (List.lookup key vs),key)

controversialSorter :: CommentSorter domain a (SimpleSorter domain a)
controversialSorter m c@Comment { parents }
  | Parents [] <- parents
  = worstSorter m c

  | otherwise
  = bestSorter m c

popularSorter :: CommentSorter domain a (SimpleSorter domain a)
popularSorter Meta { votes = Votes vs } Comment { key } = SimpleSorter (maybe 0 (\(_,_,_,d) -> d) (List.lookup key vs),key)

newSorter :: CommentSorter domain a (SimpleSorter domain a)
newSorter Meta {} Comment { created = Created (Milliseconds ms _), key } = SimpleSorter (ms,key)

oldSorter :: CommentSorter domain a (SimpleSorter domain a)
oldSorter Meta {} Comment { created = Created (Milliseconds ms _), key } = SimpleSorter (negate ms,key)