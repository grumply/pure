{-# language DuplicateRecordFields, GADTs #-}
module Pure.Convoker.Discussion.Simple.Threaded where

import Pure.Convoker.Comment as Comment
import Pure.Convoker.Discussion.Simple.Comment as SimpleComment

import Pure.Convoker.UserVotes as UserVotes

import Pure.Convoker.Discussion.Threaded as Threaded

import Pure.Auth.Access (Authentication,Authenticated)

import qualified Pure.Convoker.UserVotes as UserVotes

import Pure.Convoker.Discussion as Discussion
import Pure.Convoker.Discussion.Shared.Ago as Export
import Pure.Convoker.Discussion.Shared.Markdown as Export
import Pure.Convoker.Discussion.Shared.Total as Export
import qualified Pure.Convoker.Discussion.Simple.Meta as Meta

import Pure hiding (Meta, jump,mod)
import Pure.Auth (Username,Authenticated,user)
import Pure.Conjurer
import Data.Animation
import Data.Default
import Data.JSON hiding (Null,Key)
import Data.Scroll
import Data.Theme
import Control.Fold hiding (pattern Meta)
import Effect.Websocket hiding (Reply)
import Data.View (View)

import Data.Hashable

import Control.Concurrent
import Data.Coerce
import qualified Data.Graph as G
import Data.List as List
import Data.Maybe
import Data.Typeable
import System.IO.Unsafe

{-
Designed to approximate the Hacker News implementation as a reasonable default.

Uses the approach in Pure.Convoker.Discussion.Threaded that sets up a basic
rendering order based on connected components using Data.Graph. The sorter
is relatively simple: sorts first by vote total and then by key (temporally 
ordered)
-}

simpleThreaded 
  :: forall domain a b. 
    ( Typeable a
    , Typeable (domain :: *)
    , Theme (Comment domain a)
    , Theme (Discussion domain a)
    , ToJSON (Resource (Comment domain a)), FromJSON (Resource (Comment domain a))
    , Formable (Resource (Comment domain a))
    , Default (Resource (Comment domain a))
    , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Eq (Context a)
    , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Eq (Name a)
    , Fieldable Markdown
    , Ord b
    , Websocket domain
    , Authentication domain
    , Reader (Context a)
    , Reader (Name a)
    , Reader (Root domain a)
    ) => CommentSorter domain a b -> View
simpleThreaded sorter = 
  threaded @domain sorter (simpleCommentForm defaultSimpleThreadedComment) defaultSimpleThreadedComment

simpleCommentForm 
  :: forall (domain :: *) (a :: *). 
    ( Typeable a
    , Typeable domain
    , ToJSON (Context a), FromJSON (Context a)
    , ToJSON (Name a), FromJSON (Name a)
    , Fieldable Markdown
    , CommentFormContext domain a 
    , Websocket domain
    , Authenticated domain
    ) => (CommentContext domain a => View) -> View
simpleCommentForm commentBuilder = 
  let 
    onPreview :: Resource (Comment domain a) -> IO View
    onPreview c = do
      r <- req @domain Uncached (publishingAPI @(Comment domain a)) (previewResource @(Comment domain a)) 
            (CommentContext ask ask
            ,c { SimpleComment.author = Pure.Auth.user
               , parents = Parents (maybeToList parent)
               }
            )
      case r of
        Nothing -> pure "Invalid comment."
        Just (_,_,_,comment,_) -> pure do
          Div <| Themed @Previewing |>
            [ commentBuilder
            {-
                root = Nothing
                children = []
                previous = Nothing
                next = Nothing
                size = 0
                admins = Admins []
                mods = Mods []
                votes = Nothing
                meta = Meta (Votes [])
                admin = False
                mod = False
                withAuthor = txt
                onVote = \_ -> pure ()
            -}
            ]
          
    restore =
      forkIO do
        void do
          -- Should be sufficient for most devices?
          -- The failure mode is simply not restoring 
          -- the scroll position, which isn't too bad.
          delay (Millisecond * 100)
          addAnimation restoreScrollPosition


    onSubmit :: Reader (DiscussionComment domain a) => Resource (Comment domain a) -> IO ()
    onSubmit c@RawComment { content, key } = do
      -- | Just _ <- Discussion.comment = do
        b <- req @domain Uncached (publishingAPI @(Comment domain a)) (amendResource @(Comment domain a)) 
            (CommentContext ask ask,CommentName key,SetContent content)
        if b then void do 
          storeScrollPosition >> restore
        else
          pure ()
{-
      | otherwise = do
        mi <- req (publishingAPI @(Comment domain a)) (createResource @(Comment domain a)) 
            (CommentContext discussionContext discussionName
            ,(c :: Resource (Comment domain a)) 
              { Comment.author = un
              , parents = Parents $ maybeToList parent 
              }
            )
        for_ mi (\_ -> storeScrollPosition >> onRefresh >> restore)
-}
  in 
    Div <| Themed @Creating |>
      [ form onSubmit onPreview (fromMaybe def ask)
      ]

newtype Total = Total Int
total :: Reader Total => Int
total = let Total t = ask in t

newtype Voted = Voted (Maybe Bool)
voted :: Reader Voted => Maybe Bool
voted = let Voted v = ask in v

newtype Editing domain a = Editing (Maybe (Resource (Comment domain a)))
editing :: Reader (Editing domain a) => Maybe (Resource (Comment domain a))
editing = let Editing e = ask in e

newtype Replying = Replying Bool
replying :: Reader Replying => Bool
replying = let Replying r = ask in r

newtype Collapsed = Collapsed Bool
collapsed :: Reader Collapsed => Bool
collapsed = let Collapsed c = ask in c

newtype Removed = Removed Bool
removed :: Reader Removed => Bool
removed = let Removed d = ask in d

type ThreadedCommentContext domain a = 
  ( State Total
  , State Voted
  , State (Editing domain a)
  , State Replying
  , State Collapsed
  , State Removed
  )

data ThreadedCommentState domain a where
  ThreadedCommentState ::
    { _total     :: Total
    , _vote      :: Voted
    , _editing   :: Editing domain a
    , _replying  :: Replying
    , _collapsed :: Collapsed
    , _removed   :: Removed
    } -> ThreadedCommentState domain a

usingThreadedCommentState 
  :: (ThreadedCommentContext domain a => View) 
  -> (State (ThreadedCommentState domain a) => View)
usingThreadedCommentState v =
  zoom _total (\t tcs -> tcs { _total = t }) do
    zoom _vote (\v tcs -> tcs { _vote = v }) do
      zoom _editing (\e tcs -> tcs { _editing = e }) do
        zoom _replying (\r tcs -> tcs { _replying = r }) do
          zoom _collapsed (\c tcs -> tcs { _collapsed = c }) do
            zoom _removed (\d tcs -> tcs { _removed = d }) do
              v

defaultSimpleThreadedComment :: forall domain a. ThreadedCommentContext domain a => View
defaultSimpleThreadedComment =
  let 
    UserVotes { upvotes, downvotes } = maybe (emptyUserVotes (maybe (fromTxt def) id Pure.Auth.user)) id Discussion.votes 
    Comment.Comment { author, deleted, created, edited, content, key } = cmt
    Threaded.Created c = created

    button name action = Button <| OnClick (\_ -> action) |> [ name ]

  in 
    Article <| Themed @Comment . Themed @(Comment domain a) . Id (toTxt key) |>
      [ Footer <| Themed @Meta.Meta |>
        [ simpleVoteButton 
        , simpleAuthorAttribution

        , let e | Threaded.Edited (Just _) <- edited = Themed @Threaded.Edited | otherwise = id
          in Section <| Themed @Created . e |>
              [ SimpleHTML "time" <| Attribute "pubdate" "" . Attribute "datetime" (toZonedDateTime c) |> 
                [ txt (ago c) ]
              ]
        , Section <| Themed @Controls |>
          [ case previous of
              Just k -> button "←" (jump (toTxt k))
              _ -> Null

          , case Discussion.root of
              Just k | Discussion.root /= parent -> button "↸" (jump (toTxt k))
              _ -> Null
              
          , case parent of
              Just k -> button "↖︎" (jump (toTxt k))
              _ -> Null

          , case Discussion.next of
              Just k -> button "→" (jump (toTxt k))
              _ -> Null
            
          , if collapsed then 
              button (fromTxt $ "[" <> toTxt (size + 1) <> " more]") (pure ()) -- (command (Uncollapse @domain @a))
            else 
              button "[-]" (pure ()) -- (command (Collapse @domain @a))

          , if author == Pure.Auth.user && isNothing editing then
              button "edit" (pure ()) -- (command (Editing @domain @a))
            else if author == Pure.Auth.user && isJust editing then
              button "cancel" (pure ()) -- (command (Editing @domain @a))
            else
              Null

          , if (admin || Discussion.mod) && Prelude.not deleted then
              button "delete" (pure ()) -- (command (Delete @domain @a))
            else if (admin || Discussion.mod) && del then
              button "undelete" (pure ()) -- (command (Undelete @domain @a))
            else
              Null

          ]

        ]

      , if collapsed then
          Null 
        else if del || removed == Removed True then -- del to avoid a reload for moderators/admins
          Section <| Themed @Markdown |> [ "[ removed ]" ]
        else 
          Section <| Themed @Markdown |> withContent content

      , if collapsed then
          Null
        else if replying then 
          Footer <| Themed @Reply |> 
            [ button "cancel" (pure ()) -- (command (Replying @domain @a)) 
            ]
        else
          Footer <| Themed @Reply |> 
            [ button "reply" (pure ()) -- (command (Replying @domain @a)) 
            ]

      , case replying of
          True | Nothing <- editing, False <- collapsed -> 
            Aside <||> -- Section? I kind of like the use of Aside here.
              [ simpleCommentForm @domain @a defaultSimpleThreadedComment
              {-
                  { parent = Just key
                  , onCancel = command (Replying @domain @a)
                  , viewer = run . SimpleComment
                  , comment = Nothing
                  , .. 
                  }
              -}
              ]
          _ -> Null

      , case editing of
          Just c | False <- replying, False <- collapsed -> 
            Aside <||> -- Section? I kind of like the use of Aside here.
              [ simpleCommentForm @domain @a defaultSimpleThreadedComment
                {-
                  { parent = Just key
                  , onCancel = command (Replying @domain @a)
                  , viewer = run . SimpleComment
                  , comment = Just c
                  , .. 
                  }
                -}
              ]
          _ -> Null

      , if Prelude.not (Prelude.null children) && Prelude.not collapsed then 
          Section <| Themed @Children |#> children 
        else 
          Null 
      ]

voting :: forall domain a. (Reader Removed, Reader Total, State Voted) => View
voting
  | removed = Null
  | otherwise = 
    Div <| Themed @UserVotes |>
      [ case Pure.Auth.user of
          Just _ -> Button <| OnClick (\_ -> downvote) |> [ if voted == Just False then "▼" else "▽" ]
          _      -> Null

      , txt (simplified total)

      , case Pure.Auth.user of
          Just _ -> Button <| OnClick (\_ -> upvote) |> [ if voted == Just True then "▲" else "△" ]
          _      -> Null

      ]
    where
      upvote = 
        case voted of
          Nothing -> modify (Voted (Just True))
          Just False -> modify (Voted Nothing)
          _ -> pure ()

      downvote =
        case voted of
          Nothing -> modify (Voted (Just False))
          Just True -> modify (Voted Nothing)
          _ -> pure ()

attribution :: Reader Author => View
attribution =
  Div <| Themed @Username |> [ withAuthor Discussion.author ]

simpleComment :: forall domain a. CommentContext domain a => View
simpleComment =
  state initialize do
    usingThreadedCommentState do
      Null
  where
    initialize :: CommentContext domain a => ThreadedCommentState domain a
    initialize 
      | UserVotes {..} <- maybe (emptyUserVotes (maybe (fromTxt def) id Pure.Auth.user)) id Discussion.votes
      , Meta.Meta { votes = Votes vs } <- Discussion.meta 
      = let 
          editing = Editing Nothing
          replying = False
          collapsed = False
          Comment.Comment { key, deleted = Deleted deleted } = Discussion.comment
          total = fromMaybe 0 (fmap (\(ups,downs,_,_) -> ups - downs) (List.lookup key vs))
          vote 
            | List.elem key upvotes   = Just True
            | List.elem key downvotes = Just False
            | otherwise             = Nothing
        in
          ThreadedCommentState {..}

data Controls
data Reply
data Children
instance {-# OVERLAPPABLE #-} (Typeable domain, Typeable a) => Theme (Comment domain a) where
  theme c =
    is c do
      margin-top =: 0.5em

      has (subtheme @Creating) do
        -- box-shadow =: Shadows.shadow Shadows.OffsetBottom 5 
        margin =: 1em
        border-radius =: 1em
        padding =: 1em

      has (subtheme @UserVotes) do
        display =: inline
        has (tag Button) do
          border =: none
          background-color =: inherit
          cursor =: pointer

          hover do
            text-decoration =: underline

      has (subtheme @Created) do
        display =: inline
        margin-left =: 0.5em
        margin-right =: 0.5em
        
        has "time" do
          display =: inline

      has (subtheme @Username) do
        display =: inline
        margin-left =: 0.5em
        margin-right =: 0.5em

      has (subtheme @Controls) do
        display =: inline
        has (tag Button) do
          border =: none
          background-color =: inherit
          cursor =: pointer

          hover do
            text-decoration =: underline

      has (subtheme @Markdown) do
        margin-left =: 1em

      has (subtheme @Reply) do
        has (tag Button) do
          border =: none
          background-color =: inherit
          cursor =: pointer

          hover do
            text-decoration =: underline

      has (subtheme @Children) do
        border-left =* [1px,solid,black]
        margin-left =: 1em
        padding-left =: 1em

      has ".RawComment" do
        child (tag H2) do
          display =: none
      
instance Theme Comment
instance Theme Meta
-- instance Theme UserVotes
-- instance Theme Edited
instance Theme Created
-- instance Theme Username
instance Theme Controls
instance Theme Reply
instance Theme Markdown
instance Theme Children

#ifdef __GHCJS__
foreign import javascript unsafe
  "window.scrollTo(0,document.getElementById($1).offsetTop)"
    jump_js :: Txt -> IO ()
jump = jump_js
#else
jump _ = pure ()
#endif
