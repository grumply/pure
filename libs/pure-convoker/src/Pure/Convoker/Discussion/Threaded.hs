module Pure.Convoker.Discussion.Threaded where

import Pure.Convoker hiding (Upvote,Downvote,authenticatedEndpoints,unauthenticatedEndpoints,endpoints)
import qualified Pure.Convoker as Convoker

import Control.Reader
import Control.State
import Data.Events hiding (meta)
import Data.Foldable as Foldable
import Data.HTML
import Data.JSON hiding (Null,Key)
import Data.Theme
import Data.View hiding (modify,get)
import Data.Websocket
import Pure.Auth hiding (Key)
import Pure.Conjurer hiding (root)
import qualified Effect.Websocket as VWS

import Data.Graph as G
import Data.List as List
import Data.Maybe

#ifndef __GHCJS__
unauthenticatedEndpoints
  :: forall domain a.
    ( Typeable domain
    , Typeable a

    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , ToJSON (Context a), FromJSON (Context a)

    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    , ToJSON (Name a), FromJSON (Name a)
    
    , ToJSON (Product (Meta domain a)), FromJSON (Product (Meta domain a))
    , ToJSON (Preview (Meta domain a)), FromJSON (Preview (Meta domain a))

    ) => Websocket -> Callbacks (Discussion domain a) -> Callbacks (Meta domain a) -> Callbacks (Mods domain a) ->  IO (IO ())
unauthenticatedEndpoints = Convoker.unauthenticatedEndpoints 

authenticatedEndpoints 
  :: forall domain a. 
    ( Typeable domain
    , Typeable a

    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , ToJSON (Context a), FromJSON (Context a)

    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    , ToJSON (Name a), FromJSON (Name a)

    , Nameable    (Comment domain a)
    , Previewable (Comment domain a)
    , Processable (Comment domain a)
    , Producible  (Comment domain a)
    , Amendable   (Comment domain a)
    , Previewable (Meta domain a)
    , Processable (Meta domain a)
    , Producible  (Meta domain a)
    , Amendable   (Meta domain a)

    , ToJSON (Resource (Meta domain a)), FromJSON (Resource (Meta domain a))
    , ToJSON (Product (Meta domain a)), FromJSON (Product (Meta domain a))
    , ToJSON (Preview (Meta domain a)), FromJSON (Preview (Meta domain a))
    , ToJSON (Action (Meta domain a)), FromJSON (Action (Meta domain a))
    , ToJSON (Amend (Meta domain a)), FromJSON (Amend (Meta domain a))
    , FromJSON (Reaction (Meta domain a))

    , ToJSON (Resource (Comment domain a)), FromJSON (Resource (Comment domain a))
    , ToJSON (Action (Comment domain a)), FromJSON (Action (Comment domain a))
    , ToJSON (Reaction (Comment domain a)), FromJSON (Reaction (Comment domain a))
    , ToJSON (Reaction (Meta domain a))
    , ToJSON (Amend (Comment domain a)), FromJSON (Amend (Comment domain a))
      
    ) => Websocket 
      -> Username 
      -> Callbacks (Discussion domain a) 
      -> Callbacks (Comment domain a)
      -> Callbacks (Meta domain a)
      -> Callbacks (Mods domain a)
      -> Callbacks (UserVotes domain a)
      -> IO (IO ())
authenticatedEndpoints ws un discussionCallbacks commentCallbacks metaCallbacks modsCallbacks userVotesCallbacks = 
  Convoker.authenticatedEndpoints ws un
    (permissions (Just un))
    (permissions (Just un))
    discussionCallbacks 
    (extendCommentCallbacks fullPermissions discussionCallbacks commentCallbacks)
    metaCallbacks 
    modsCallbacks 
    userVotesCallbacks 
    (interactions (Just un))
    (interactions (Just un))
#endif

threads :: forall domain a b. DiscussionLayout domain a b
threads sorter form comment =
  Article <| Themed @(Discussion domain a) |> 
    [ state False do
        reader (Parent (Nothing :: Maybe (Key (Comment domain a)))) do
          reader (CommentFormCancelAction (put False)) do
            reader (Previous (Nothing :: Maybe (Key (Comment domain a)))) do
              reader (Next (Nothing :: Maybe (Key (Comment domain a)))) do
                state (NewComment (Nothing :: Maybe (Resource (Comment domain a)))) do
                  guarded @domain Null (basic @domain) do
                    Section <||>
                      [ Header <||> [ Button <| OnClick (\_ -> modify not) |> 
                        [ if get then "Cancel" else "Add Comment" ] ]
                      , if get then form (put False) else Null
                      ]

    , Keyed Section <||#> do
        forest Nothing Nothing do
          maybe threads isolated root
    ]
  where
    Discussion { comments = cs } = full @domain @a 

    edges = fmap (\comment@Comment { key, parents = Parents ps } -> (comment,key,ps)) cs
    
    (transposeG -> graph,nodeFromVertex,find) = graphFromEdges edges

    threads = components graph

    isolated k = dfs graph (maybe id (:) (find k) [])

    forest rt par ts = 
      let look (Node n _) = let (comment,_,_) = nodeFromVertex n in comment
          sorted = sortOn (sorter meta . look) ts 
      in 
        [ tree rt' par previous next t
        | (t,pr,nx) <- zip3 sorted (Nothing : fmap Just sorted) (tail (fmap Just sorted ++ [Nothing])) 
        , let 
            rt' = if isJust rt then rt else par
            previous = fmap (\(look -> Comment { key }) -> key) pr
            next = fmap (\(look -> Comment { key }) -> key) nx
        ]
    
    tree root par previous next node@(Node (nodeFromVertex -> (c@Comment { key , author },_,_)) sub) =
      (hash key,
        reader (Root root) do
          reader (Parent (par :: Maybe (Key (Comment domain a)))) do
            reader (Previous (previous :: Maybe (Key (Comment domain a)))) do
              reader (Next (next :: Maybe (Key (Comment domain a)))) do
                reader (DiscussionComment c) do
                  reader (RenderedChildren (forest root (Just key) sub)) do
                    reader (Descendants (Foldable.length node - 1)) do
                      reader (Author author) do
                        comment
      )
