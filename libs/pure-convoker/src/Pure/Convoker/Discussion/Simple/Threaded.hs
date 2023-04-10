{-# language DuplicateRecordFields, GADTs #-}
module Pure.Convoker.Discussion.Simple.Threaded where

import Pure.Convoker.Comment as Comment
import Pure.Convoker.Discussion.Simple.Comment as SimpleComment

import Pure.Convoker.UserVotes as UserVotes

import Pure.Auth.Access (Authentication,Authenticated,guarded)

import qualified Pure.Convoker.UserVotes as UserVotes

import Pure.Convoker.Discussion as Discussion hiding (comment)
import qualified Pure.Convoker.Discussion as Discussion (comment)
import Pure.Convoker.Discussion.Shared.Ago
import Pure.Convoker.Discussion.Shared.Markdown 
import Pure.Convoker.Discussion.Shared.Total 
import Pure.Convoker.Discussion.Simple.Meta
import Pure.Convoker.Meta (Meta,Votes(..))

import Pure hiding (Created,Delete,Meta,meta,jump,mod,user,next)
import Pure.Auth (Username,Authenticated,user,guarded)
import Pure.Conjurer hiding (form,root)
import Data.Animation
import Effect.Websocket hiding (Reply)
import Data.View (View)

import qualified Data.Graph as G
import Data.List as List
import Prelude hiding (mod)

{-
Designed to approximate the Hacker News implementation as a reasonable default.

Uses the approach in Pure.Convoker.Discussion.Threaded that sets up a basic
rendering order based on connected components using Data.Graph. The sorter
is relatively simple: sorts first by vote total and then by key (temporally 
ordered)
-}

newtype Total = Total Int
total :: Reader Total => Int
total = let Total t = ask in t

newtype Voted = Voted (Maybe Bool)
voted :: Reader Voted => Maybe Bool
voted = let Voted v = ask in v

newtype Editing = Editing Bool
editing :: Reader Editing => Bool
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

type ThreadContext domain a = 
  ( State Total
  , State Voted
  , State Editing
  , State Replying
  , State Collapsed
  , State Removed
  , CommentContext domain a
  )

data ThreadState domain a where
  ThreadState ::
    { _total     :: Total
    , _vote      :: Voted
    , _editing   :: Editing
    , _replying  :: Replying
    , _collapsed :: Collapsed
    , _removed   :: Removed
    } -> ThreadState domain a

usingThreadState 
  :: forall domain a. (Typeable domain, Typeable a, CommentContext domain a) =>
     (ThreadContext domain a => View) 
  -> (State (ThreadState domain a) => View)
usingThreadState v =
  zoom @(ThreadState domain a) _total (\t tcs -> tcs { _total = t }) do
    zoom @(ThreadState domain a) _vote (\v tcs -> tcs { _vote = v }) do
      zoom @(ThreadState domain a) _editing (\e tcs -> tcs { _editing = e }) do
        zoom @(ThreadState domain a) _replying (\r tcs -> tcs { _replying = r }) do
          zoom @(ThreadState domain a) _collapsed (\c tcs -> tcs { _collapsed = c }) do
            zoom @(ThreadState domain a) _removed (\d tcs -> tcs { _removed = d }) do
              v

thread :: forall domain a. 
          ( Ord (Context a), ToJSON (Context a), FromJSON (Context a)
          , Ord (Name a), ToJSON (Name a), FromJSON (Name a)
          , Ord (Resource (Comment domain a))
          , Ord (Amend (Comment domain a))
          , CommentContext domain a
          ) => View
thread =
  state initialize do
    usingThreadState @domain @a do
      comment @domain @a
  where
    initialize :: CommentContext domain a => ThreadState domain a
    initialize 
      | UserVotes {..} <- Discussion.votes @domain @a ? emptyUserVotes (guarded @domain "" "" (Pure.Auth.user @domain))
      , Meta { votes = Votes vs } <- meta @domain @a
      = let 
          Comment.Comment { key, deleted = Deleted removed } = Discussion.comment
          total = fmap (\(ups,downs,_,_) -> ups - downs) (vs !? key) ? 0
          vote
            | List.elem key upvotes   = Just True
            | List.elem key downvotes = Just False
            | otherwise               = Nothing
        in
          ThreadState 
            { _editing = Editing False
            , _replying = Replying False
            , _collapsed = Collapsed False
            , _removed = Removed removed
            , _total = Total total
            , _vote = Voted vote
            }

comment 
  :: forall domain a. 
    ( Ord (Context a), ToJSON (Context a), FromJSON (Context a)
    , Ord (Name a), ToJSON (Name a), FromJSON (Name a)
    , Ord (Resource (Comment domain a))
    , Ord (Amend (Comment domain a))
    , ThreadContext domain a 
    ) => View
comment =
  let 
    UserVotes { upvotes, downvotes } = Discussion.votes @domain @a ? guarded @domain (emptyUserVotes "") (emptyUserVotes "") (emptyUserVotes (user @domain))
    Comment { author, deleted = Deleted del, created, edited, content, key } = Discussion.comment @domain @a
    Created c = created

    button name action = Button <| OnClick (const action) |> [ name ]

  in 
    Article <| Themed @Comment . Themed @(Comment domain a) . Id (toTxt key) |>
      [ Footer <| Themed @Meta |>
        [ voting
        , Div <| Themed @Username |> 
          [ txt author ]

        , let e | Edited (Just _) <- edited = Themed @Edited | otherwise = id
          in Section <| Themed @Created . e |>
              [ SimpleHTML "time" <| Attribute "pubdate" "" . Attribute "datetime" (ZonedDateTime c) |> 
                [ txt (ago c) ]
              ]
        , Section <| Themed @Controls |>
          [ case previous @domain @a of
              Just k -> button "←" (jump (toTxt k))
              _ -> Null

          , case root @domain @a of
              Just k | root @domain @a /= parent -> button "↸" (jump (toTxt k))
              _ -> Null
              
          , case parent @domain @a of
              Just k -> button "↖︎" (jump (toTxt k))
              _ -> Null

          , case next @domain @a of
              Just k -> button "→" (jump (toTxt k))
              _ -> Null
            
          , if collapsed then 
              button (fromTxt $ "[" <> toTxt (size + 1) <> " more]") do
                put (Collapsed False)
            else 
              button "[-]" do
                put (Collapsed True)

          , guarded @domain Null Null do
              if author == user @domain && not editing then
                button "edit" do
                  put (Editing True)
              else if author == user @domain && editing then
                button "cancel" do
                  put (Editing False)
              else
                Null

          , if (admin || mod) && not del then
              button "delete" do
                req @domain Uncached (publishingAPI @(Comment domain a)) (amendResource @(Comment domain a)) do
                  (CommentContext it it,CommentName key,Delete)
                put (Removed True)
            else if (admin || mod) && del then
              button "undelete" do
                req @domain Uncached (publishingAPI @(Comment domain a)) (amendResource @(Comment domain a)) do
                  (CommentContext it it,CommentName key,Undelete)
                put (Removed False)
            else
              Null

          ]

        ]

      , if collapsed then
          Null 
        else if del || removed then -- del to avoid a reload for moderators/admins
          Section <| Themed @Markdown |> [ "[ removed ]" ]
        else 
          Section <| Themed @Markdown |> content -- TODO: extend with `withContent`

      , if collapsed then
          Null
        else if replying then 
          Footer <| Themed @Reply |> 
            [ button "cancel" (put (Replying False)) 
            ]
        else
          Footer <| Themed @Reply |> 
            [ button "reply" (put (Replying True)) 
            ]

      , if replying && not editing && not collapsed then
          Aside <||> -- Section? I kind of like the use of Aside here.
            [ guarded @domain Null Null (form @domain @a (comment @domain @a)) ]
        else
          Null

      , if editing && not replying && not collapsed then
          Aside <||> -- Section? I kind of like the use of Aside here.
            [ guarded @domain Null Null (form @domain @a (comment @domain @a)) ]
        else
          Null

      , if present renderedChildren && not collapsed then 
          Section <| Themed @Children |#> renderedChildren 
        else 
          Null 
      ]

voting :: forall domain a. (Reader Removed, Reader Total, State Voted) => View
voting
  | removed = Null
  | otherwise = 
    Div <| Themed @UserVotes |>
      [ Button <| OnClick (const downvote) |> [ if voted == Just False then "▼" else "▽" ]
      , txt (simplified total)
      , Button <| OnClick (const upvote) |> [ if voted == Just True then "▲" else "△" ]
      ]
    where
      upvote = 
        case voted of
          Nothing -> put (Voted (Just True))
          Just False -> put (Voted Nothing)
          _ -> pure ()

      downvote =
        case voted of
          Nothing -> put (Voted (Just False))
          Just True -> put (Voted Nothing)
          _ -> pure ()

form :: forall (domain :: *) (a :: *). 
        ( Ord (Context a), ToJSON (Context a), FromJSON (Context a)
        , Ord (Name a), ToJSON (Name a), FromJSON (Name a)
        , Ord (Resource (Comment domain a))
        , Ord (Amend (Comment domain a))
        , DiscussionContext domain a
        , Authenticated domain
        , ThreadContext domain a 
        ) => View -> View
form commentBuilder = 
  let 
    onPreview :: Resource (Comment domain a) -> IO View
    onPreview c = do
      r <- req @domain Uncached (publishingAPI @(Comment domain a)) (previewResource @(Comment domain a)) 
            (CommentContext ask ask
            ,c { SimpleComment.author = user @domain
               , parents = Parents (maybeToList parent)
               }
            )
      case r of
        Nothing -> pure "Invalid comment."
        Just (_,_,_,comment,_) -> pure do
          Div <| Themed @Previewing |>
            [ commentBuilder ]
          
    restore =
      forkIO do
        void do
          -- Should be sufficient for most devices?
          -- The failure mode is simply not restoring 
          -- the scroll position, which isn't too bad.
          delay (Millisecond * 100)
          addAnimation restoreScrollPosition


    onSubmit :: Resource (Comment domain a) -> IO ()
    onSubmit c@RawComment { content, key } = do
        b <- req @domain Uncached (publishingAPI @(Comment domain a)) (amendResource @(Comment domain a)) 
            (CommentContext ask ask,CommentName key,SetContent content)
        if b then void do 
          storeScrollPosition >> restore
        else
          pure ()

  in 
    Div <| Themed @Creating |>
      [ Null -- _ onSubmit onPreview (fromMaybe def ask)
      ]



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
instance Theme UserVotes
instance Theme Edited
instance Theme Created
-- instance Theme Username
instance Theme Controls
instance Theme Reply
instance Theme Markdown
instance Theme Children

-- This is navigational; when the user clicks on the navigational links associated with
-- a comment, we use `jump` to scroll the associated comment into view. These links include
-- `parent`, `next`, `previous`.
#ifdef __GHCJS__
foreign import javascript unsafe
  "window.scrollTo(0,document.getElementById($1).offsetTop)"
    jump_js :: Txt -> IO ()
jump = jump_js
#else
jump _ = pure ()
#endif
