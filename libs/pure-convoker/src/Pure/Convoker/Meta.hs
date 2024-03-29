module Pure.Convoker.Meta where

import Pure.Convoker.Comment ( Product, Comment, Name, Context )
import Pure.Convoker.Admins ( isAdmin )

import Control.Log (Logging)
import Pure.Auth.Data.Username ( Username )
import Pure.Conjurer ( Key, Product, Name, Context, Hashable, Pathable, Nameable(..), Ownable(..) )
import Data.Function (fix)
import Data.JSON ( FromJSON, ToJSON, traceJSON )
import Data.Time ( Time, pattern Milliseconds, days )

import Data.Function (on)
import qualified Data.Graph as G
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )

{-
Design notes:

  What is implemented here:

    data instance Context (Meta domain a)
    data instance Name (Meta domain a)

  What is not implemented here:

    data isntance Resource (Meta domain a)
    data instance Product (Meta domain a)
    data instance Preview (Meta domain a)
    instance Previewable (Meta domain a)
    instance Processable (Meta domain a)
    instance Producible (Meta domain a)
    instance Amendable (Meta domain a)
    data instance Action (Meta domain a)
    data instance Reaction (Meta domain a)

-}

data Meta (domain :: *) (a :: *)

data instance Context (Meta domain a) = MetaContext (Context a) (Name a)
  deriving stock Generic
deriving instance (Eq (Context a),Eq (Name a)) => Eq (Context (Meta domain a))
deriving instance (Ord (Context a),Ord (Name a)) => Ord (Context (Meta domain a))
deriving instance (Hashable (Context a),Hashable (Name a)) => Hashable (Context (Meta domain a))
deriving instance (Typeable a, Pathable (Context a),Pathable (Name a)) => Pathable (Context (Meta domain a))
deriving instance (ToJSON (Context a),ToJSON (Name a)) => ToJSON (Context (Meta domain a))
deriving instance (FromJSON (Context a),FromJSON (Name a)) => FromJSON (Context (Meta domain a))

data instance Name (Meta domain a) = MetaName
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (Hashable,Pathable,ToJSON,FromJSON)

instance Nameable (Meta domain a) where
  toName _ = MetaName

instance (Typeable domain, Logging) => Ownable (Meta domain a) where
  isOwner un _ _ = isAdmin @domain un

--------------------------------------------------------------------------------
-- Simple anonymous vote totals.

newtype Votes domain a = Votes
  { votes :: [(Key (Comment domain a),(Int,Int,Time,Double))]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data AmendVote domain a 
  = Vote Username (Key (Comment domain a)) Time Int
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

{-
amendVotes :: AmendVote domain a -> Votes domain a -> Votes domain a
amendVotes (Vote _ target t@(Milliseconds n _) vote) (Votes votes) = Votes (go votes)
  where
    go [] = 
      let
        (ups,dns) 
          | vote > 0 = (vote,0)
          | otherwise = (0,abs vote)
      in
        [(target,(ups,dns,t,fromIntegral vote))]

    go (x : rest) 
      | fst x == target = fmap go' x : rest
      | otherwise       = x : go rest

    go' (ups,dns,t0@(Milliseconds l _),decay) = 
      let
        (ups',dns')
          | vote > 0  = (ups + vote,dns)
          | otherwise = (ups,dns + abs vote)
        
        -- votes have a 1-hour half-life
        -- TODO: figure out how to customize this
        -- TODO: check that this is reasonable in the presence of negative votes (downvotes)? Should I track upvote and downvote decay separately?
        decay' = decay * 2 ** ((l - n) / 3.6e6) + fromIntegral vote

      in
        (ups',dns',t,decay')
-}

amendVotes :: Double -> AmendVote domain a -> Votes domain a -> Votes domain a
amendVotes halfLife (Vote _ target t@(Milliseconds n _) vote) (Votes votes) = Votes (go votes)
  where
    decayConstant = log 2 / halfLife

    go [] = 
      let
        (ups, dns)
          | vote > 0 = (vote, 0)
          | otherwise = (0, abs vote)
      in
        [(target, (ups, dns, t, fromIntegral vote))]

    go (x : rest) 
      | fst x == target = fmap go' x : rest
      | otherwise       = x : go rest

    go' (ups, dns, t0@(Milliseconds l _), total) = 
      let
        (ups', dns')
          | vote > 0  = (ups + vote, dns)
          | otherwise = (ups, dns + abs vote)
        
        elapsed = (n - l) / 3.6e6
        upvoteDecay = fromIntegral ups * exp (- decayConstant * elapsed)
        downvoteDecay = fromIntegral dns * exp (- decayConstant * elapsed)
        
        total' = upvoteDecay - downvoteDecay + fromIntegral vote

      in
        (ups', dns', t, total')

type CommentSorter domain a b = Ord b => Product (Meta domain a) -> Product (Comment domain a) -> b