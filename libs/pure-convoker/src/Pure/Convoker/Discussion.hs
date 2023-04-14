module Pure.Convoker.Discussion where

import Pure.Convoker.Admins as Admins
import Pure.Convoker.Comment
import Pure.Convoker.Meta
import Pure.Convoker.Mods
import Pure.Convoker.UserVotes

import Pure.Auth hiding (Key)
import Pure.Conjurer
import Control.Applicative ((<|>))
import Control.State
import Data.Default
import Data.Events
import Data.Exists
import Data.HTML
import Data.JSON (ToJSON,FromJSON)
import Data.Time
import Data.Theme
import Data.View hiding (modify,get,onUpdate)
import Effect.Async
import Effect.Websocket

import Control.Concurrent
import Control.Monad (void)
import Data.List as List
import Data.Maybe (fromMaybe)
import qualified Data.Graph as G
import GHC.Generics (Generic)
import Debug.Trace

import Data.CSS ( (=:), child, has, is )
import Data.Styles (display,none)

data Discussion (domain :: *) (a :: *)

instance {-# OVERLAPPABLE #-} (Typeable domain, Typeable a) => Theme (Discussion domain a) where
  theme c =
    is c do
      has ".RawComment" do
        child (tag H2) do
          display =: none
        has (tag Label) do
          display =: none
      

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

data instance Amend (Discussion domain a)
  = SetComment (Product (Comment domain a)) (Preview (Comment domain a))
  deriving stock Generic

instance Amendable (Discussion domain a) where
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
root :: Exists (Root domain a) => Maybe (Key (Comment domain a))
root = let Root mk = it in mk

newtype IsAdmin = IsAdmin Bool
admin :: Exists IsAdmin => Bool
admin = let IsAdmin ia = it in ia

newtype IsMod = IsMod Bool
mod :: Exists IsMod => Bool
mod = let IsMod im = it in im

newtype Refresher = Refresher (IO ())
refresh :: Exists Refresher => IO ()
refresh = let Refresher r = it in r

newtype Voter domain a = Voter (Amend (UserVotes domain a) -> IO ())
vote :: Exists (Voter domain a) => Amend (UserVotes domain a) -> IO ()
vote = let Voter v = it in v

admins :: Exists (Product (Admins domain)) => Product (Admins domain)
admins = it

mods :: Exists (Product (Mods domain a)) => Product (Mods domain a)
mods = it

votes :: Exists (Product (UserVotes domain a)) => Product (UserVotes domain a)
votes = it

meta :: Exists (Product (Meta domain a)) => Product (Meta domain a)
meta = it

full :: Exists (Product (Discussion domain a)) => Product (Discussion domain a)
full = it

newtype Now = Now Time
now :: Exists Now => Time
now = let Now n = it in n

type DiscussionContext domain a =
  ( Typeable domain
  , Typeable a
  , Websocket domain
  , Authentication domain
  , Exists (Context a)
  , Exists (Name a)
  , Exists (Root domain a)
  , Exists IsAdmin
  , Exists IsMod
  , Exists (Voter domain a)
  , Exists Refresher
  , Exists Now
  , Exists (Product (Admins domain))
  , Exists (Product (Mods domain a))
  , Exists (Product (UserVotes domain a))
  , Exists (Product (Meta domain a))
  , Exists (Product (Discussion domain a))
  )

type DiscussionResources domain a = Maybe (Product (Admins domain),Product (Mods domain a),Product (UserVotes domain a),Product (Meta domain a),Product (Discussion domain a))

-- Core discussion view generator. Given an active connection, a resource's
-- context and name, and a method of rendering a `DiscussionBuilder`, this
-- will build the `DiscussionBuilder` context and render it if the necessary
-- resources can be retrieved.
{-# INLINE discussion #-}
discussion
  :: forall domain a.
    ( Typeable a
    , Typeable (domain :: *)
    , ToJSON (Context a), FromJSON (Context a), Eq (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a), Eq (Name a), Ord (Name a)
    , FromJSON (Product (Meta domain a))
    , Websocket domain
    , Authentication domain
    , Exists (Context a)
    , Exists (Name a)
    , Exists (Root domain a)
    ) => (DiscussionContext domain a => View) -> View
discussion viewer =
  stateWith (\_ -> pure) (produceDiscussionResources @domain @a it it >>= \drs -> pure (drs,def)) do
    let
      onRefresh :: IO ()
      onRefresh = produceDiscussionResources @domain @a it it >>= put

      loadUserVotes :: Authenticated domain => IO ()
      loadUserVotes = do
          muvs <- req @domain Cached (readingAPI @(UserVotes domain a)) (readProduct @(UserVotes domain a)) 
                    (UserVotesContext it it,UserVotesName (user @domain))
          let
            upd :: Product (UserVotes domain a) -> DiscussionResources domain a -> DiscussionResources domain a
            upd _ Nothing = Nothing
            upd uvs (Just (as,ms,_,m,d)) = Just (as,ms,uvs,m,d)
                        
          maybe def (modify . upd) muvs

    case it :: DiscussionResources domain a of
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

          upvote :: Key (Comment domain a) -> Product (UserVotes domain a) -> Product (UserVotes domain a)
          upvote k UserVotes {..}
            | k `elem` downvotes  = UserVotes { downvotes = List.filter (/= k) downvotes, .. }
            | k `notElem` upvotes = UserVotes { upvotes = k : upvotes, .. }
            | otherwise           = UserVotes {..}

          downvote :: Key (Comment domain a) -> Product (UserVotes domain a) -> Product (UserVotes domain a)
          downvote k UserVotes {..}
            | k `elem` upvotes      = UserVotes { upvotes = List.filter (/= k) upvotes, .. }
            | k `notElem` downvotes = UserVotes { downvotes = k : downvotes, .. }
            | otherwise             = UserVotes {..}

          onVote :: (Authentication domain, Modify (DiscussionResources domain a)) => Amend (UserVotes domain a) -> IO ()
          onVote (Upvote k) = do
            guarded @domain def def do
              req @domain Uncached (publishingAPI @(UserVotes domain a)) (amendResource @(UserVotes domain a)) 
                (UserVotesContext it it,UserVotesName (user @domain),Upvote k)
            modify @(DiscussionResources domain a) (fmap (\(admins,mods,uvs,meta,full) -> (admins,mods,upvote k uvs,meta,full)))
          onVote (Downvote k) = do
            guarded @domain def def do
              req @domain Uncached (publishingAPI @(UserVotes domain a)) (amendResource @(UserVotes domain a)) 
                (UserVotesContext it it,UserVotesName (user @domain),Downvote k)
            modify @(DiscussionResources domain a) (fmap (\(admins,mods,uvs,meta,full) -> (admins,mods,downvote k uvs,meta,full)))

        in
          with mods do
            with admins do
              with meta do
                with full do
                  with votes do
                    with (IsAdmin admin) do
                      with (IsMod mod) do
                        with (Refresher onRefresh) do
                          with (Voter onVote) do
                            stateWith (const pure) (time >>= \n -> forkIO (delay Minute >> time >>= put . Now) >>= \tid -> pure (Now n,const (killThread tid))) do
                              async (guarded @domain def def loadUserVotes) do
                                viewer

produceDiscussionResources 
  :: forall domain a. 
     ( Typeable domain, Typeable a
     , ToJSON (Context a), FromJSON (Context a), Ord (Context a)
     , ToJSON (Name a), FromJSON (Name a), Ord (Name a)
     , FromJSON (Product (Meta domain a))
     ) => Context a -> Name a -> IO (DiscussionResources domain a)
produceDiscussionResources ctx nm = do
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

  getAdmins     <- getProduct Cached AdminsContext AdminsName
  getMods       <- getProduct Cached (ModsContext ctx) ModsName
  getVotes      <- pure (pure (Just (emptyUserVotes "")))
  getMeta       <- getProduct Uncached (MetaContext ctx nm) MetaName
  getDiscussion <- getProduct Uncached (DiscussionContext ctx nm) DiscussionName

  (admins,mods,votes,meta,full) <-
    (,,,,)
      <$> getAdmins
      <*> getMods
      <*> getVotes
      <*> getMeta
      <*> getDiscussion

  pure $ (,,,,)
    <$> admins
    <*> mods
    <*> votes
    <*> meta
    <*> full

newtype RenderedChildren = RenderedChildren [(Int,View)]
renderedChildren :: Exists RenderedChildren => [(Int,View)]
renderedChildren = let RenderedChildren cs = it in cs

newtype Parent domain a = Parent (Maybe (Key (Comment domain a)))
parent :: Exists (Parent domain a) => Maybe (Key (Comment domain a))
parent = let Parent mp = it in mp

newtype Previous domain a = Previous (Maybe (Key (Comment domain a)))
previous :: Exists (Previous domain a) => Maybe (Key (Comment domain a))
previous = let Previous mk = it in mk

newtype Next domain a = Next (Maybe (Key (Comment domain a)))
next :: Exists (Next domain a) => Maybe (Key (Comment domain a))
next = let Next mk = it in mk

newtype Descendants = Descendants Int
descendants :: Exists Descendants => Int
descendants = let Descendants ds = it in ds

newtype DiscussionComment domain a = DiscussionComment (Product (Comment domain a))
comment :: Exists (DiscussionComment domain a) => Product (Comment domain a)
comment = let DiscussionComment dc = it in dc

newtype Author = Author Username
author :: Exists Author => Username
author = let Author a = it in a

type CommentContext domain a =
  ( DiscussionContext domain a
  , Exists RenderedChildren
  , Exists (Root domain a)
  , Exists (Parent domain a)
  , Exists (Previous domain a)
  , Exists (Next domain a)
  , Exists Descendants
  , Exists (DiscussionComment domain a)
  , Exists Author
  )

newtype CommentFormCancelAction = CommentFormCancelAction (IO ())
cancel :: Exists CommentFormCancelAction => IO ()
cancel = let CommentFormCancelAction c = it in c

newtype NewComment domain a = NewComment (Maybe (Resource (Comment domain a)))
new :: Exists (NewComment domain a) => Maybe (Resource (Comment domain a))
new = let NewComment mr = it in mr

type CommentFormContext domain a =
  ( DiscussionContext domain a
  , Exists (Parent domain a)
  , Exists (Previous domain a)
  , Exists (Next domain a)
  , Exists CommentFormCancelAction
  , State (NewComment domain a)
  , Authenticated domain
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
      -> (CommentFormContext domain a => IO () -> View)
      -> (CommentContext domain a => View)
      -> (DiscussionContext domain a => View)

linear :: forall domain a b. DiscussionLayout domain a b
linear sorter form comment | Discussion {..} <- full =
  Div <| Themed @(Discussion domain a) |>
    (( state False do
        if get then
          with (Previous (Nothing :: Maybe (Key (Comment domain a)))) do
            with (Next (Nothing :: Maybe (Key (Comment domain a)))) do
              with (Parent (Nothing :: Maybe (Key (Comment domain a)))) do
                with (CommentFormCancelAction (put False)) do
                  state (NewComment (Nothing :: Maybe (Resource (Comment domain a)))) do
                    guarded @domain Null (basic @domain) (form (put False))
        else
          Button <| OnClick (\_ -> put True) |> [ "Add Comment" ]
      )
    : fmap run (seen $ List.sortOn (sorter it) comments)
    )
  where
    run (previous,current,next) =
      with (Root (Nothing :: Maybe (Key (Comment domain a)))) do
        with (Parent (Nothing :: Maybe (Key (Comment domain a)))) do
          with (Previous previous) do
            with (Next next) do
              with (RenderedChildren []) do
                with (Descendants 0) do
                  with (DiscussionComment current) do
                    guarded @domain Null Null do
                      with (Author (user @domain)) comment

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




