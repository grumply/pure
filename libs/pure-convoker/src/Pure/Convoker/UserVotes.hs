module Pure.Convoker.UserVotes where

import Pure.Convoker.Comment
import Pure.Convoker.Meta

import Pure.Auth (Username(..))
import Pure.Conjurer
import Data.JSON hiding (Key)
import Control.Component

import Data.Hashable

import Control.Monad
import Data.Foldable
import Data.List as List
import Data.Maybe
import Data.Typeable
import GHC.Generics hiding (Meta)
import System.IO.Unsafe

{-
Design notes:

  What is implemented here:

    data instance Resource (UserVotes a)
    data instance Product (UserVotes a)
    data instance Preview (UserVotes a)
    data instance Context (UserVotes a)
    data instance Name (UserVotes a)
    instance Amendable (UseerVotes a)
    instance Processable (UserVotes a)
    instance Producible (UserVotes a)
    instance Previewable (UserVotes a)
    
  I don't see a reason to override any of these.

-}

data UserVotes (domain :: *) (a :: *)

data instance Resource (UserVotes domain a) = RawUserVotes
  { username  :: Username
  , upvotes   :: [Key (Comment domain a)]
  , downvotes :: [Key (Comment domain a)]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data instance Product (UserVotes domain a) = UserVotes
  { username  :: Username
  , upvotes   :: [Key (Comment domain a)]
  , downvotes :: [Key (Comment domain a)]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data instance Preview (UserVotes domain a) = NoUserVotesPreview
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

data instance Context (UserVotes domain a) = UserVotesContext (Context a) (Name a)
  deriving stock Generic
deriving instance (Eq (Context a),Eq (Name a)) => Eq (Context (UserVotes domain a))
deriving instance (Ord (Context a),Ord (Name a)) => Ord (Context (UserVotes domain a))
deriving instance (Hashable (Context a),Hashable (Name a)) => Hashable (Context (UserVotes domain a))
deriving instance (Typeable a, Pathable (Context a),Pathable (Name a)) => Pathable (Context (UserVotes domain a))
deriving instance (ToJSON (Context a),ToJSON (Name a)) => ToJSON (Context (UserVotes domain a))
deriving instance (FromJSON (Context a),FromJSON (Name a)) => FromJSON (Context (UserVotes domain a))

newtype instance Name (UserVotes domain a) = UserVotesName Username
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (Hashable,Pathable,ToJSON,FromJSON)

instance Nameable (UserVotes domain a) where
  toName RawUserVotes {..} = UserVotesName username

instance Ownable (UserVotes domain a) where
  isOwner un ctx (Just (UserVotesName un')) = pure (un == un')
  isOwner _ _ _ = pure False

instance Amendable (UserVotes domain a) where
  data Amend (UserVotes domain a)
    = Upvote   (Key (Comment domain a)) 
    | Downvote (Key (Comment domain a)) 
    deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

  amend (Upvote target) RawUserVotes {..} 
    | target `elem` downvotes =
      Just RawUserVotes
        { downvotes = List.filter (/= target) downvotes 
        , ..
        }

    | target `notElem` upvotes = 
      Just RawUserVotes
        { upvotes = target : upvotes 
        , ..
        }

  amend (Downvote target) RawUserVotes {..} 
    | target `elem` upvotes =
      Just RawUserVotes
        { upvotes = List.filter (/= target) upvotes 
        , ..
        }

    | target `notElem` downvotes =
      Just RawUserVotes
        { downvotes = target : downvotes
        , ..
        }

  amend _ _ =
    Nothing

instance Processable (UserVotes domain a)

instance Producible (UserVotes domain a) where
  produce _ _ RawUserVotes {..} _ = pure UserVotes {..}

instance Previewable (UserVotes domain a) where
  preview _ _ _ _ = pure NoUserVotesPreview

data instance Action (UserVotes domain a) = NoUserVotesAction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)
data instance Reaction (UserVotes domain a) = NoUserVotesReaction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance 
  ( Typeable domain
  , Typeable a
  , ToJSON (Context a), FromJSON (Context a), Hashable (Context a), Pathable (Context a), Ord (Context a)
  , ToJSON (Name a), FromJSON (Name a), Hashable (Name a), Pathable (Name a), Ord (Name a)
  ) => DefaultPermissions (UserVotes domain a) 
  where
    permissions Nothing = noPermissions
    permissions (Just un) = noPermissions 
      { canCreate = canCreate'
      , canRead = canRead'
      , canAmend = canAmend' 
      }
      where
        canCreate' _ (UserVotesName user) _ = pure (user == un)
        canRead' _ (UserVotesName user) = pure (user == un)
        canAmend' ctx (UserVotesName user) _ 
          | user == un = do
            tryReadResource (permissions @(UserVotes domain a) (Just un)) (callbacks @(UserVotes domain a) (Just un)) ctx (UserVotesName un) >>= \case
              Nothing -> void do
                tryCreate (fullPermissions @(UserVotes domain a)) (callbacks @(UserVotes domain a) (Just un)) ctx (RawUserVotes un [] [])
              Just _  ->
                pure ()
            pure True
          | otherwise =
            pure False

emptyUserVotes :: Username -> Product (UserVotes domain a)
emptyUserVotes un = UserVotes un [] []

tryUpvote 
  :: ( Typeable domain
     , Typeable a
     , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
     , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Hashable (Name a), Ord (Name a)
     ) => Permissions (UserVotes domain a) -> Callbacks (UserVotes domain a) -> Context a -> Name a -> Username -> Key (Comment domain a) -> IO Bool
tryUpvote permissions callbacks ctx nm un k = fmap isJust do
  tryAmend permissions callbacks (UserVotesContext ctx nm) (UserVotesName un)
    (Upvote k)

tryDownvote 
  :: ( Typeable domain
     , Typeable a
     , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
     , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Hashable (Name a), Ord (Name a)
     ) => Permissions (UserVotes domain a) -> Callbacks (UserVotes domain a) -> Context a -> Name a -> Username -> Key (Comment domain a) -> IO Bool
tryDownvote permissions callbacks ctx nm un k = fmap isJust do
  tryAmend permissions callbacks (UserVotesContext ctx nm) (UserVotesName un)
    (Downvote k)