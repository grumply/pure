{-# language DuplicateRecordFields, GADTs #-}
module Pure.Convoker.Discussion.Simple.Threaded where

import Pure.Convoker.Comment as Comment
import Pure.Convoker.Discussion.Simple.Comment as SimpleComment

import Pure.Convoker.UserVotes as UserVotes

import Pure.Auth.Access (Authentication,Authenticated,guarded,basic)

import qualified Pure.Convoker.UserVotes as UserVotes

import Pure.Convoker.Discussion as Discussion hiding (comment)
import qualified Pure.Convoker.Discussion as Discussion (comment)
import Pure.Convoker.Discussion.Shared.Ago
import Pure.Convoker.Discussion.Shared.Markdown
import Pure.Convoker.Discussion.Shared.Total 
import Pure.Convoker.Discussion.Simple.Meta
import Pure.Convoker.Meta (Meta,Votes(..))
import Pure.Convoker.Discussion.Threaded (threads)

import Pure hiding (Created,Delete,Meta,Product,meta,jump,mod,user,next)
import Pure.Auth (Username,Authenticated,user,guarded)
import Pure.Conjurer hiding (form,root,Previewing)
import qualified Pure.Conjurer as Conjurer
import qualified Pure.Conjurer.Formable as Formable
import Data.Animation
import Effect.Websocket hiding (Reply)
import Data.View (View)

import qualified Data.Graph as G
import Data.List as List
import Prelude hiding (mod,min)
import System.IO.Unsafe

simpleThreadedDiscussion 
  :: forall domain a. 
     ( Typeable a, Typeable domain
     , Viewable Username
     , Exists (Root domain a)
     , Websocket domain
     , Authentication domain
     , Exists (Context a), Pathable (Context a), Ord (Context a), ToJSON (Context a), FromJSON (Context a)
     , Exists (Name a), Pathable (Name a), Ord (Name a), ToJSON (Name a), FromJSON (Name a)
     , Fieldable Markdown
     ) => Maybe (Key (Comment domain a)) -> View
simpleThreadedDiscussion root =
    discussion @domain @a do
      threads @domain @a root
        bestSorter 
        (form @domain @a def) 
        (thread @domain @a False)

{-
Designed to approximate the Hacker News implementation as a reasonable default.

Uses the approach in Pure.Convoker.Discussion.Threaded that sets up a basic
rendering order based on connected components using Data.Graph. The sorter
is relatively simple: sorts first by vote total and then by key (temporally 
ordered)
-}

newtype Total = Total Int
total :: Exists Total => Int
total = let Total t = ask in t

newtype Editing = Editing Bool
editing :: Exists Editing => Bool
editing = let Editing e = ask in e

newtype Replying = Replying Bool
replying :: Exists Replying => Bool
replying = let Replying r = ask in r

newtype Collapsed = Collapsed Bool
collapsed :: Exists Collapsed => Bool
collapsed = let Collapsed c = ask in c

newtype Removed = Removed Bool
removed :: Exists Removed => Bool
removed = let Removed d = ask in d

newtype Previewing = Previewing Bool
previewing :: Exists Previewing => Bool
previewing = let Previewing p = ask in p

type ThreadContext domain a = 
  ( State Total
  , State Editing
  , State Replying
  , State Collapsed
  , State Removed
  , Exists Previewing
  , CommentContext domain a
  )

data ThreadState domain a where
  ThreadState ::
    { _total     :: Total
    , _editing   :: Editing
    , _replying  :: Replying
    , _collapsed :: Collapsed
    , _removed   :: Removed
    } -> ThreadState domain a

usingThreadState 
  :: forall domain a. (Typeable domain, Typeable a, CommentContext domain a, Exists Previewing) =>
     (ThreadContext domain a => View) 
  -> (State (ThreadState domain a) => View)
usingThreadState v =
  zoom @(ThreadState domain a) _total (\t tcs -> tcs { _total = t }) do
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
          , Formable (Resource (Comment domain a))
          , Viewable Username
          ) => Bool -> View
thread pre =
  with (Previewing pre) do
    state initialize do
      usingThreadState @domain @a do
        comment @domain @a
  where
    initialize :: CommentContext domain a => ThreadState domain a
    initialize 
      | Meta { votes = Votes vs } <- meta @domain @a
      = let 
          Comment.Comment { key, deleted = Deleted removed } = Discussion.comment
          total = fmap (\(ups,downs,_,_) -> ups - downs) (vs !? key) ? 0
        in
          ThreadState 
            { _editing = Editing False
            , _replying = Replying False
            , _collapsed = Collapsed False
            , _removed = Removed removed
            , _total = Total total
            }

comment 
  :: forall domain a. 
    ( Ord (Context a), ToJSON (Context a), FromJSON (Context a)
    , Ord (Name a), ToJSON (Name a), FromJSON (Name a)
    , Ord (Resource (Comment domain a))
    , Ord (Amend (Comment domain a))
    , ThreadContext domain a 
    , Exists Previewing
    , Formable (Resource (Comment domain a))
    , Viewable Username
    ) => View
comment =
  let 
    UserVotes { upvotes, downvotes } = Discussion.votes @domain @a 
    Comment { author, created, edited, deleted, content, key } = Discussion.comment @domain @a
    Created c = created

    button name action = Button <| OnClick (const action) |> [ name ]

  in 
    Article <| Themed @Comment . Themed @(Comment domain a) . Id (toTxt key) |>
      [ Footer <| Themed @Meta |>
        [ case deleted of
            Deleted True -> do
              let 
                ~(Edited (Just t)) = edited
                zdt = ZonedDateTime t

              Div <| Themed @Deleted . Title zdt |>
                [ SimpleHTML "time" <| Attribute "datetime" zdt |> 
                  [ "[removed ", txt (ago now t), "]" ]
                ]
            _ -> 
              Null

        , if removed then Null else
            with key (voting @domain @a)

        , if removed then Null else
            Div <| Themed @Username |> 
              [ toView author ]

        , if removed then Null else
            let zdt = ZonedDateTime c in
            Div <| Themed @Created . Title zdt|>
              [ SimpleHTML "time" <| Attribute "pubdate" "" . Attribute "datetime" zdt |> 
                [ txt (ago now c) ]
              ]
              
        , if removed then Null else
            case edited of
              Edited (Just t) -> 
                let zdt = ZonedDateTime c in
                Div <| Themed @Edited . Title zdt |>
                  [ SimpleHTML "time" <| Attribute "datetime" zdt |> 
                    [ "(edited ", txt (ago now t), ")" ]
                  ]
              _ -> 
                Null

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
              button (fromTxt $ "[" <> toTxt (descendants + 1) <> " more]") do
                put (Collapsed False)
            else 
              button "[-]" do
                put (Collapsed True)

          , guarded @domain Null Null do
              if (mod || admin || author == user @domain) && not editing && not removed && not previewing then
                button "edit" do
                  put (Editing True)
              else if author == user @domain && editing && not previewing then
                button "cancel" do
                  put (Editing False)
              else
                Null

          , if (admin || mod) && not removed && not previewing then
              button "delete" do
                req @domain Uncached (publishingAPI @(Comment domain a)) (amendResource @(Comment domain a)) do
                  (CommentContext it it,CommentName key,Delete)
                put (Removed True)
                refresh
            else if (admin || mod) && removed then
              button "undelete" do
                req @domain Uncached (publishingAPI @(Comment domain a)) (amendResource @(Comment domain a)) do
                  (CommentContext it it,CommentName key,Undelete)
                put (Removed False)
                refresh
            else
              Null

          ]

        ]

      , if collapsed then
          Null 
        else if removed then -- avoids a reload for moderators/admins
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
          Section <||>
            [ guarded @domain Null Null do
                state (NewComment @domain @a Nothing) do
                  with (CommentFormCancelAction (put (Replying False))) do
                    with (Refresher (refresh >> put (Replying False))) do
                      form @domain @a (Just key) (put (Replying False))
            ]
        else
          Null

      , if not removed && editing && not replying && not collapsed then
          Section <||>
            [ guarded @domain Null Null do
                async (req @domain Uncached (publishingAPI @(Comment domain a)) (readResource @(Comment domain a)) (CommentContext it it,CommentName key)) do
                  state (NewComment @domain @a (await :: Maybe (Resource (Comment domain a)))) do
                    with (CommentFormCancelAction (put (Editing False))) do
                      form @domain @a (Just key) (put (Editing False)) 
            ]
        else
          Null

      , if present renderedChildren && not collapsed then 
          Keyed Section <| Themed @Children |#> renderedChildren 
        else 
          Null 
      ]

voting :: forall domain a. (Authentication domain, Exists Previewing, Exists (Product (UserVotes domain a)), Exists Collapsed, Exists Removed, Exists Total, Exists (Key (Comment domain a)), Exists (Voter domain a), State Total) => View
voting =
  Div <| Themed @UserVotes |>
    [ Button <| OnClick (const downvote) . guarded @domain (Disabled "true") (Disabled "true") id . on (previewing || collapsed || removed) (Disabled "true") |> [ if v == Just False then "▼" else "▽" ]
    , Span <||> [ txt (simplified total) ]
    , Button <| OnClick (const upvote) . guarded @domain (Disabled "true") (Disabled "true") id . on (previewing || collapsed || removed) (Disabled "true") |> [ if v == Just True then "▲" else "△" ]
    ]
    where
      UserVotes {..} = Discussion.votes @domain @a 

      v | it `elem` upvotes   = Just True
        | it `elem` downvotes = Just False
        | otherwise           = Nothing

      upvote =
        case v of
          Nothing -> do
            vote @domain @a (Upvote it)
            modify \(Total t) -> Total (t + 1)
          Just False -> do
            vote @domain @a (Upvote it)
            modify \(Total t) -> Total (t + 1)
          Just True -> do
            vote @domain @a (Downvote it)
            modify \(Total t) -> Total (t - 1)
            pure ()

      downvote =
        case v of
          Nothing -> do
            vote @domain @a (Downvote it)
            modify \(Total t) -> Total (t - 1)
          Just True -> do
            vote @domain @a (Downvote it)
            modify \(Total t) -> Total (t - 1)
          Just False -> do
            vote @domain @a (Upvote it)
            modify \(Total t) -> Total (t + 1)

form :: forall (domain :: *) (a :: *). 
        ( Ord (Context a), ToJSON (Context a), FromJSON (Context a)
        , Ord (Name a), ToJSON (Name a), FromJSON (Name a)
        , CommentFormContext domain a
        , Formable (Resource (Comment domain a))
        , Viewable Username
        ) => Maybe (Key (Comment domain a)) -> IO () -> View
form parent done = 
  let 
    onPreview :: (Authenticated domain) => Resource (Comment domain a) -> IO View
    onPreview c = do
      r <- req @domain Uncached (publishingAPI @(Comment domain a)) (previewResource @(Comment domain a)) 
            ( CommentContext ask ask
            , c { SimpleComment.author = user @domain }
            )
      case r of
        Nothing -> pure "Invalid comment."
        Just (_,_,_,c,_) -> pure do
          Div <| Themed @Conjurer.Previewing |>
            [ with (DiscussionComment c) do
                with (RenderedChildren []) do
                  with (Descendants 0) do
                    with (Author (user @domain)) do
                      with (Root @domain @a Nothing) do
                        thread @domain @a True
            ]
          
    delayedJump k =
      void do
        forkIO do
          void do
            delay (Millisecond * 100)
            jump (toTxt k)

    onSubmit :: Resource (Comment domain a) -> IO ()
    onSubmit c@RawComment { content, key, .. } = do
      case it :: NewComment domain a of
        NewComment Nothing -> do
          b <- req @domain Uncached (publishingAPI @(Comment domain a)) (createResource @(Comment domain a)) 
                 ( CommentContext ask ask
                 , c { SimpleComment.author = user @domain, parents = Parents (maybeToList parent) }
                 )
          when (isJust b) do
            done
            refresh
            let CommentName k = fromJust b
            delayedJump k
        NewComment (Just c) -> do
          b <- req @domain Uncached (publishingAPI @(Comment domain a)) (amendResource @(Comment domain a)) (CommentContext ask ask,CommentName key,SetContent content)
          when b do
            done
            refresh

  in
    Div <| Themed @Creating |>
      [ Formable.form onSubmit onPreview do
          case it of
            NewComment Nothing -> def
            NewComment (Just c) -> c
      ]

data Controls
data Reply
data Children
instance {-# OVERLAPPABLE #-} (Typeable domain, Typeable a) => Theme (Comment domain a) where
  theme c = do
    is c do
      margin-top =: 0.5em

      has (subtheme @Meta) do
        opacity =: 0.6

      has (subtheme @Creating) do
        margin-left =: 1em

      has (subtheme @UserVotes) do
        display =: inline
        has (tag Span) do
          min-width =: 3ch
          display =: inline-block
          text-align =: center

        has (tag Button) do
          border =: none
          background-color =: inherit
          cursor =: pointer

          hover do
            text-decoration =: underline
            
          disabled do
            visibility =: hidden

      has (subtheme @Created) do
        display =: inline
        margin-left =: 0.5em
        margin-right =: 0.5em
        
        has "time" do
          display =: inline
 
      has (subtheme @Edited) do
        display =: inline
        margin-left =: 0.5em
        margin-right =: 0.5em
        opacity =: 0.5
        
        has "time" do
          display =: inline

      has (subtheme @Deleted) do
        display =: inline
        margin-left =: 0.5em
        margin-right =: 0.5em
        opacity =: 0.5
        
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
          text-decoration =: underline
          margin-left =: 1em

      has (subtheme @Children) do
        border-left =* [1px,solid,black]
        margin-left =: 1em
        padding-left =: 1em
   
instance Theme Comment
instance Theme Meta
instance Theme UserVotes
instance Theme Edited
instance Theme Created
instance Theme Deleted
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
