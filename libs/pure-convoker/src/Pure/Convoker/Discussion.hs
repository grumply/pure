module Pure.Convoker.Discussion where

import Pure.Convoker.Admins as Admins
import Pure.Convoker.Comment
import Pure.Convoker.Meta
import Pure.Convoker.Mods
import Pure.Convoker.UserVotes

import Pure.Auth hiding (Key)
import Pure.Conjurer
import Control.Fold as Pure hiding (not,key,pattern Meta,meta)
import Control.Reader
import Control.State
import Data.Default
import Data.Events
import Data.HTML
import Data.JSON (ToJSON,FromJSON)
import Data.Theme
import Data.Time
import Data.Txt
import Data.View hiding (modify,get,ask,onUpdate)
import Data.View.Render
import Data.Router as R
import Effect.Async
import Effect.Websocket

import Data.Hashable

import Control.Concurrent
import Control.Monad
import Data.List as List
import Data.Maybe
import qualified Data.Graph as G
import Data.Typeable
import GHC.Generics hiding (Meta)

data Discussion (domain :: *) (a :: *)

instance {-# OVERLAPPABLE #-} (Typeable domain, Typeable a) => Theme (Discussion domain a)

data instance Resource (Discussion domain a) = RawDiscussion
  { context  :: Context a
  , name     :: Name a
  , comments :: [(Product (Comment domain a),Preview (Comment domain a))]
  } deriving stock Generic
deriving instance (ToJSON (Context a), ToJSON (Name a), ToJSON (Product (Comment domain a)),ToJSON (Preview (Comment domain a))) => ToJSON (Resource (Discussion domain a))
deriving instance (FromJSON (Context a), FromJSON (Name a), FromJSON (Product (Comment domain a)),FromJSON (Preview (Comment domain a))) => FromJSON (Resource (Discussion domain a))

data instance Product (Discussion domain a) = Discussion
  { context  :: Context a
  , name     :: Name a
  , comments :: [Product (Comment domain a)]
  } deriving stock Generic
deriving instance (ToJSON (Context a), ToJSON (Name a), ToJSON (Product (Comment domain a))) => ToJSON (Product (Discussion domain a))
deriving instance (FromJSON (Context a), FromJSON (Name a), FromJSON (Product (Comment domain a))) => FromJSON (Product (Discussion domain a))

data instance Preview (Discussion domain a) = DiscussionPreview
  { context  :: Context a
  , name     :: Name a
  , comments :: [Preview (Comment domain a)]
  } deriving stock Generic
deriving instance (ToJSON (Context a), ToJSON (Name a), ToJSON (Preview (Comment domain a))) => ToJSON (Preview (Discussion domain a))
deriving instance (FromJSON (Context a), FromJSON (Name a), FromJSON (Preview (Comment domain a))) => FromJSON (Preview (Discussion domain a))

data instance Context (Discussion domain a) = DiscussionContext (Context a) (Name a)
  deriving stock Generic
deriving instance (Eq (Context a),Eq (Name a)) => Eq (Context (Discussion domain a))
deriving instance (Ord (Context a),Ord (Name a)) => Ord (Context (Discussion domain a))
deriving instance (Hashable (Context a),Hashable (Name a)) => Hashable (Context (Discussion domain a))
deriving instance (Typeable a, Pathable (Context a),Pathable (Name a)) => Pathable (Context (Discussion domain a))
deriving instance (ToJSON (Context a),ToJSON (Name a)) => ToJSON (Context (Discussion domain a))
deriving instance (FromJSON (Context a),FromJSON (Name a)) => FromJSON (Context (Discussion domain a))

data instance Name (Discussion domain a) = DiscussionName
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (Hashable,Pathable,ToJSON,FromJSON)

instance Nameable (Discussion domain a) where
  toName _ = DiscussionName

instance Amendable (Discussion domain a) where
  data Amend (Discussion domain a)
    = SetComment (Product (Comment domain a)) (Preview (Comment domain a))
    deriving stock Generic

  amend (SetComment pro@Comment { key = target } pre) RawDiscussion {..}
    | let match (Comment { key },_) = key == target
    , List.any match comments
    = Just RawDiscussion { comments = fmap (\x -> if match x then (pro,pre) else x) comments, .. }

    | otherwise
    = Just RawDiscussion { comments = (pro,pre) : comments, .. }

deriving instance (ToJSON (Product (Comment domain a)), ToJSON (Preview (Comment domain a))) => ToJSON (Amend (Discussion domain a))
deriving instance (FromJSON (Product (Comment domain a)), FromJSON (Preview (Comment domain a))) => FromJSON (Amend (Discussion domain a))

instance Processable (Discussion domain a)

instance Producible (Discussion domain a) where
  produce _ _ RawDiscussion {..} _ =
    pure Discussion { comments = fmap fst comments, .. }

instance Previewable (Discussion domain a) where
  preview _ _ RawDiscussion {..} _ =
    pure DiscussionPreview { comments = fmap snd comments, .. }

data instance Action (Discussion domain a) = NoDiscussionAction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

data instance Reaction (Discussion domain a) = NoDiscussionReaction
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance Typeable domain => Ownable (Discussion domain a) where
  isOwner un _ _ = Admins.isAdmin @domain un

newtype Root domain a = Root (Maybe (Key (Comment domain a)))
root :: Reader (Root domain a) => Maybe (Key (Comment domain a))
root = let Root mk = ask in mk

newtype IsAdmin = IsAdmin Bool
admin :: Reader IsAdmin => Bool
admin = let IsAdmin ia = ask in ia

newtype IsMod = IsMod Bool
mod :: Reader IsMod => Bool
mod = let IsMod im = ask in im

newtype Refresher = Refresher (IO ())
refresh :: Reader Refresher => IO ()
refresh = let Refresher r = ask in r

newtype Voter domain a = Voter (Amend (UserVotes domain a) -> IO ())
vote :: Reader (Voter domain a) => Amend (UserVotes domain a) -> IO ()
vote = let Voter v = ask in v

admins :: Reader (Product (Admins domain)) => Product (Admins domain)
admins = ask

mods :: Reader (Product (Mods domain a)) => Product (Mods domain a)
mods = ask

votes :: Reader (Maybe (Product (UserVotes domain a))) => Maybe (Product (UserVotes domain a))
votes = ask

meta :: Reader (Product (Meta domain a)) => Product (Meta domain a)
meta = ask

full :: Reader (Product (Discussion domain a)) => Product (Discussion domain a)
full = ask

type DiscussionContext domain a =
  ( Websocket domain
  , Authentication domain
  , Reader (Context a)
  , Reader (Name a)
  , Reader (Root domain a)
  , Reader IsAdmin
  , Reader IsMod
  , Reader (Voter domain a)
  , Reader Refresher
  , Reader (Maybe (Product (UserVotes domain a)))
  , Reader (Product (Admins domain))
  , Reader (Product (Mods domain a))
  , Reader (Product (Meta domain a))
  , Reader (Product (Discussion domain a))
  )

type DiscussionResources domain a = Maybe (Product (Admins domain),Product (Mods domain a),Maybe (Product (UserVotes domain a)),Product (Meta domain a),Product (Discussion domain a))

-- Core discussion view generator. Given an active connection, a resource's
-- context and name, and a method of rendering a `DiscussionBuilder`, this
-- will build the `DiscussionBuilder` context and render it if the necessary
-- resources can be retrieved.
discussion
  :: forall domain a.
    ( Typeable a
    , Typeable (domain :: *)
    , ToJSON (Context a), FromJSON (Context a), Eq (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a), Eq (Name a), Ord (Name a)
    , FromJSON (Product (Meta domain a))
    , Websocket domain
    , Authentication domain
    , Reader (Context a)
    , Reader (Name a)
    , Reader (Root domain a)
    ) => (DiscussionContext domain a => View) -> View
discussion viewer =
    let
      onRefresh :: Modify Bool => IO ()
      onRefresh = modify (Prelude.not get)
    in
      state False do
        async producer do
          case await :: DiscussionResources domain a of
            Nothing -> "Problem loading discussion."
            Just (admins,mods,votes,meta,full) ->
              let
                admin =
                  guarded @domain False False do
                    let Admins as = admins
                    user @domain `elem` as

                mod =
                  guarded @domain False False do
                    let Mods ms = mods
                    user @domain `elem` ms

                upvote :: State (Maybe (Product (UserVotes domain a))) => Key (Comment domain a) -> Maybe (Product (UserVotes domain a))
                upvote k
                  | Just UserVotes {..} <- get, k `elem` downvotes
                  = Just UserVotes { downvotes = List.filter (/= k) downvotes, .. }

                  | Just UserVotes {..} <- get, k `notElem` upvotes
                  = Just UserVotes { upvotes = k : upvotes, .. }

                  | otherwise
                  = get

                downvote :: State (Maybe (Product (UserVotes domain a))) => Key (Comment domain a) -> Maybe (Product (UserVotes domain a))
                downvote k
                  | Just UserVotes {..} <- get, k `elem` upvotes
                  = Just UserVotes { upvotes = List.filter (/= k) upvotes, .. }

                  | Just UserVotes {..} <- get, k `notElem` downvotes
                  = Just UserVotes { downvotes = k : downvotes, .. }

                  | otherwise
                  = get

                onVote :: State (Maybe (Product (UserVotes domain a))) => Amend (UserVotes domain a) -> IO ()
                onVote (Upvote k) = modify (upvote k)
                onVote (Downvote k) = modify (downvote k)

              in
                reader mods do
                  reader admins do
                    reader meta do
                      reader full do
                        reader (IsAdmin admin) do
                          reader (IsMod mod) do
                            reader (Refresher onRefresh) do
                              state votes do
                                reader (Voter onVote) do
                                  viewer
  where
    producer :: Reader Bool => IO (DiscussionResources domain a)
    producer = (ask :: Bool) `seq` do

      let
        getProduct
          :: forall a.
            ( Typeable a
            , ToJSON (Context a), FromJSON (Context a), Ord (Context a)
            , ToJSON (Name a), Ord (Name a)
            , FromJSON (Product a)
            ) => Policy -> Context a -> Name a -> IO (IO (Maybe (Product a)))
        getProduct policy ctx nm = do
          mv <- newEmptyMVar
          forkIO do
            r <- req @domain policy (readingAPI @a) (readProduct @a) (ctx,nm)
            putMVar mv r
          pure (takeMVar mv)

        u = guarded @domain Nothing Nothing (Just (user @domain))

      getAdmins     <- getProduct Cached AdminsContext AdminsName
      getMods       <- getProduct Cached (ModsContext ask) ModsName
      getVotes      <- maybe (pure (pure Nothing)) (getProduct Uncached (UserVotesContext ask ask) . UserVotesName) u
      getMeta       <- getProduct Uncached (MetaContext ask ask) MetaName
      getDiscussion <- getProduct Uncached (DiscussionContext ask ask) DiscussionName

      (admins,mods,votes0,meta,full) <-
        (,,,,)
          <$> getAdmins
          <*> getMods
          <*> getVotes
          <*> getMeta
          <*> getDiscussion

      let
        -- if no votes are found, seed the UserVotes with either Nothing if the
        -- user is not logged in, or an empty userVotes if they are
        votes = maybe (maybe Nothing (Just . emptyUserVotes) u) Just votes0

      pure $ (,,,,)
        <$> admins
        <*> mods
        <*> pure votes
        <*> meta
        <*> full

newtype RenderedChildren = RenderedChildren [(Int,View)]
renderedChildren :: Reader RenderedChildren => [(Int,View)]
renderedChildren = let RenderedChildren cs = ask in cs

newtype Parent domain a = Parent (Maybe (Key (Comment domain a)))
parent :: Reader (Parent domain a) => Maybe (Key (Comment domain a))
parent = let Parent mp = ask in mp

newtype Previous domain a = Previous (Maybe (Key (Comment domain a)))
previous :: Reader (Previous domain a) => Maybe (Key (Comment domain a))
previous = let Previous mk = ask in mk

newtype Next domain a = Next (Maybe (Key (Comment domain a)))
next :: Reader (Next domain a) => Maybe (Key (Comment domain a))
next = let Next mk = ask in mk

newtype Descendants = Descendants Int
descendants :: Reader Descendants => Int
descendants = let Descendants ds = ask in ds

newtype DiscussionComment domain a = DiscussionComment (Product (Comment domain a))
comment :: Reader (DiscussionComment domain a) => Product (Comment domain a)
comment = let DiscussionComment dc = ask in dc

newtype Author = Author Username
author :: Reader Author => Username
author = let Author a = ask in a

type CommentContext domain a =
  ( DiscussionContext domain a
  , Reader RenderedChildren
  , Reader (Root domain a)
  , Reader (Parent domain a)
  , Reader (Previous domain a)
  , Reader (Next domain a)
  , Reader Descendants
  , Reader (DiscussionComment domain a)
  , Reader Author
  )

newtype CommentFormCancelAction = CommentFormCancelAction (IO ())
cancel :: Reader CommentFormCancelAction => IO ()
cancel = let CommentFormCancelAction c = ask in c

newtype NewComment domain a = NewComment (Maybe (Resource (Comment domain a)))
new :: Reader (NewComment domain a) => Maybe (Resource (Comment domain a))
new = let NewComment mr = ask in mr

type CommentFormContext domain a =
  ( DiscussionContext domain a
  , Reader CommentFormCancelAction
  , State (NewComment domain a)
  )

type DiscussionLayout (domain :: *) a b =
    ( Typeable a
    , Typeable domain
    , Theme (Discussion domain a)
    , Theme (Comment domain a)
    , Pathable (Context a), ToJSON (Context a), FromJSON (Context a)
    , Pathable (Name a), ToJSON (Name a), FromJSON (Name a)
    , ToJSON (Resource (Comment domain a)), FromJSON (Resource (Comment domain a))
    , Formable (Resource (Comment domain a))
    , Default (Resource (Comment domain a))
    , Ord b
    ) => CommentSorter domain a b
      -> (CommentFormContext domain a => View)
      -> (CommentContext domain a => View)
      -> (DiscussionContext domain a => View)

linear :: forall domain a b. DiscussionLayout domain a b
linear sorter form comment | Discussion {..} <- full =
  Div <| Themed @(Discussion domain a) |>
    (( state False do
        if get then
          reader (Previous (Nothing :: Maybe (Key (Comment domain a)))) do
            reader (Next (Nothing :: Maybe (Key (Comment domain a)))) do
              reader (Parent (Nothing :: Maybe (Key (Comment domain a)))) do
                reader (CommentFormCancelAction (modify False)) do
                  state (NewComment (Nothing :: Maybe (Resource (Comment domain a))))
                    form
        else
          Button <| OnClick (\_ -> modify True) |> [ "Add Comment" ]
      )
    : fmap run (seen $ List.sortOn (sorter ask) comments)
    )
  where
    run (previous,current,next) =
      reader (Root (Nothing :: Maybe (Key (Comment domain a)))) do
        reader (Parent (Nothing :: Maybe (Key (Comment domain a)))) do
          reader (Previous previous) do
            reader (Next next) do
              reader (RenderedChildren []) do
                reader (Descendants 0) do
                  reader (DiscussionComment current) do
                    guarded @domain Null Null do
                      reader (Author (user @domain)) comment

    seen :: [Product (Comment domain a)] -> [(Maybe (Key (Comment domain a)),Product (Comment domain a),Maybe (Key (Comment domain a)))]
    seen = start
      where
        start [] = []
        start [a] = [(Nothing,a,Nothing)]
        start (a:b@Comment { key = next }:as) = (Nothing,a,Just next) : continue (a:b:as)

        continue [a@Comment { key = prev },b] = [(Just prev,b,Nothing)]
        continue ~(a@Comment { key = prev }:b:c@Comment { key = next }:abcs) = (Just prev,b,Just next) : continue (b:c:abcs)

extendCommentCallbacks
  :: forall domain a.
    ( Typeable domain
    , Typeable a
    , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Hashable (Name a), Ord (Name a)
    ) => Permissions (Discussion domain a) -> Callbacks (Discussion domain a) -> Callbacks (Comment domain a) -> Callbacks (Comment domain a)
extendCommentCallbacks discussionPermissions discussionCallbacks cbs = cbs
  { onCreate = \(CommentContext ctx nm) cnm res pro pre lst -> void do
    onCreate cbs (CommentContext ctx nm) cnm res pro pre lst
    tryAmend @(Discussion domain a) discussionPermissions discussionCallbacks
      (DiscussionContext ctx nm) DiscussionName
        (SetComment pro pre)

  , onUpdate = \(CommentContext ctx nm) cnm res pro pre lst -> void do
    onUpdate cbs (CommentContext ctx nm) cnm res pro pre lst
    tryAmend @(Discussion domain a) discussionPermissions discussionCallbacks
      (DiscussionContext ctx nm) DiscussionName
        (SetComment pro pre)

  , onAmend = \(CommentContext ctx nm) cnm res pro pre lst amnd -> void do
    onAmend cbs (CommentContext ctx nm) cnm res pro pre lst amnd
    tryAmend @(Discussion domain a) discussionPermissions discussionCallbacks
      (DiscussionContext ctx nm) DiscussionName
        (SetComment pro pre)

  }

createDiscussion
  :: forall domain a.
    ( Typeable domain
    , Typeable a
    , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Hashable (Name a), Ord (Name a)
    , Default (Resource (Meta domain a))
    , Amendable (Meta domain a)
    , Processable (Meta domain a)
    , Previewable (Meta domain a)
    , Producible (Meta domain a)
    , FromJSON (Resource (Meta domain a)), ToJSON (Resource (Meta domain a))
    , FromJSON (Product (Meta domain a)), ToJSON (Product (Meta domain a))
    , FromJSON (Preview (Meta domain a)), ToJSON (Preview (Meta domain a))
    , FromJSON (Amend (Meta domain a)), ToJSON (Amend (Meta domain a))
    ) => Context a -> Name a -> [Username] -> IO ()
createDiscussion ctx nm mods = void do
  tryCreate @(Discussion domain a) fullPermissions def (DiscussionContext ctx nm) (RawDiscussion ctx nm [])
  tryReadResource (fullPermissions @(Mods domain a)) def (ModsContext ctx) ModsName >>= \case
    Just RawMods {} -> pure ()
    _ -> void (tryCreate @(Mods domain a) fullPermissions def (ModsContext ctx) (RawMods mods))
  tryCreate @(Meta domain a) fullPermissions def (MetaContext ctx nm) (def :: Resource (Meta domain a))

addDiscussionCreationCallbacks
  :: forall domain a.
    ( Typeable domain
    , Typeable a
    , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Hashable (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Hashable (Name a), Ord (Name a)
    , Default (Resource (Meta domain a))
    , Amendable (Meta domain a)
    , Processable (Meta domain a)
    , Previewable (Meta domain a)
    , Producible (Meta domain a)
    , FromJSON (Resource (Meta domain a)), ToJSON (Resource (Meta domain a))
    , FromJSON (Product (Meta domain a)), ToJSON (Product (Meta domain a))
    , FromJSON (Preview (Meta domain a)), ToJSON (Preview (Meta domain a))
    , FromJSON (Amend (Meta domain a)), ToJSON (Amend (Meta domain a))
    ) => [Username] -> Callbacks a -> Callbacks a
addDiscussionCreationCallbacks mods cbs = cbs { onCreate = onCreate' }
  where
    onCreate' ctx nm res pro pre lst = do
      createDiscussion @domain ctx nm mods
      onCreate cbs ctx nm res pro pre lst


