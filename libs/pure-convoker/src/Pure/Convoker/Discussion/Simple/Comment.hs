{-# OPTIONS_GHC -Wno-missing-methods #-}
module Pure.Convoker.Discussion.Simple.Comment where

import Pure.Convoker.Admins 
import Pure.Convoker.Comment
import Pure.Convoker.Mods ( isMod )
import Pure.Convoker.Discussion.Shared.Markdown (Markdown(..),parseMarkdown)

import Pure.Auth.Data.Username (Username)
import Pure.Conjurer
import Control.Component (Component)
import Data.Default (Default(..))
import Data.JSON (ToJSON,FromJSON,traceJSON)
import Data.Time (time)
import Data.Txt (FromTxt(..))

import Data.List as List (take)
import Data.Typeable (Typeable)
import GHC.Generics (Generic)

import System.IO.Unsafe (unsafePerformIO)

{-
Design notes:

  What is inherited:

    data instance Product (Comment a)
    data instance Preview (Comment a)
    data instance Context (Comment a)
    data instance Name (Comment a)
    instance Previewable (Comment a)

  What is implemented here:

    data instance Resource (Comment a)
    instance Nameable (Comment a)
    instance Amendable (Comment a)
    data instance Action (Comment a)
    data instance Reaction (Comment a)
    instance Processable (Comment a)
    instance Producible (Comment a)
  
  What is overridable:
    
    instance Nameable (Comment a)
    instance Previewable (Comment a)
    instance Processable (Comment a)
    instance Producible (Comment a)
    
-}

-- Overridable instances. These aren't used in the default discussion views.
instance {-# OVERLAPPABLE #-} (Typeable domain, Typeable a) => Component (Preview (Comment domain a))
instance {-# OVERLAPPABLE #-} (Typeable domain, Typeable a) => Component (Product (Comment domain a))

data instance Resource (Comment domain a) = RawComment
  { author   :: Username
  , key      :: Key (Comment domain a)
  , parents  :: Parents domain a
  , created  :: Created
  , edited   :: Edited
  , deleted  :: Deleted
  , content  :: Markdown
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

-- the key, author, and creation time are replaced on the server
instance Default (Resource (Comment domain a)) where
  def = RawComment 
    { author   = fromTxt ""
    , key      = unsafePerformIO newKey 
    , parents  = Parents []
    , created  = Created (unsafePerformIO time)
    , edited   = Edited Nothing
    , deleted  = Deleted False
    , content  = Markdown ""
    }

instance Nameable (Comment domain a) where
  toName RawComment {..} = CommentName key

data instance Amend (Comment domain a) 
  = SetContent Markdown
  | Delete
  | Undelete
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance Amendable (Comment domain a) where
  amend (SetContent md) RawComment {..} | Deleted False <- deleted = 
    Just RawComment
      { content = md 
      , edited = Edited (Just (unsafePerformIO time))
      , ..
      }
      
  amend Delete RawComment {..} | Deleted False <- deleted =
    Just RawComment
      { deleted = Deleted True
      , ..
      }

  amend Undelete RawComment {..} | Deleted True <- deleted =
    Just RawComment
      { deleted = Deleted False
      , ..
      }

  amend _ _ = 
    Nothing

data instance Action (Comment domain a) = NoCommentAction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)
data instance Reaction (Comment domain a) = NoCommentReaction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance {-# OVERLAPPABLE #-} Processable (Comment domain a) where
  process RawComment {..} = do
    let Parents ps = parents
    t <- time
    k <- newKey
    pure $ Just RawComment
      { key = k 
      , created = Created t
      , parents = Parents (List.take 1 ps)
      , edited = Edited Nothing
      , ..
      }

-- This can be overridden with overlapping instances to customize processing!
instance {-# OVERLAPPABLE #-} Producible (Comment domain a) where
  produce context _ RawComment {..} _ =
    pure Comment
      { content = if deleted == Deleted True then [ "[ removed ]" ] else parseMarkdown content
      , ..
      }

instance 
  ( Typeable domain
  , Typeable a 
  , Pathable (Context a), Hashable (Context a), Ord (Context a), ToJSON (Context a), FromJSON (Context a)
  , Pathable (Name a), Hashable (Name a), Ord (Name a), ToJSON (Name a), FromJSON (Name a)
  ) => DefaultPermissions (Comment domain a) 
  where
    permissions Nothing = readPermissions
    permissions (Just un) =
      readPermissions
        { canCreate = \_ _ RawComment {..} -> pure (author == un)
        , canUpdate = canUpdate'
        , canAmend  = canAmend'
        }
      where
        canUpdate' (CommentContext ctx nm) (CommentName k) = canEditComment @domain ctx nm k un
        canAmend' (CommentContext ctx nm) (CommentName k) = \case
          SetContent _ -> canEditComment @domain ctx nm k un
          _            -> isMod @domain ctx un 

