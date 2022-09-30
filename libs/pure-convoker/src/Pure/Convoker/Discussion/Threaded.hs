module Pure.Convoker.Discussion.Threaded 
  ( module Pure.Convoker.Discussion.Threaded
  , module Export
  ) where

import Pure.Convoker as Export hiding (Upvote,Downvote,authenticatedEndpoints,unauthenticatedEndpoints,endpoints)
import qualified Pure.Convoker as Convoker
import Pure.Convoker.Discussion

import Control.Fold
import Control.Reader
import Control.State
import Data.Default
import Data.Events
import Data.Foldable as Foldable
import Data.HTML
import Data.JSON hiding (Null,Key)
import Data.Theme
import Data.View hiding (modify,get)
import Data.Websocket
import Pure.Auth hiding (Key)
import Pure.Conjurer
import qualified Effect.Websocket as VWS

import Data.Hashable

import qualified Data.Graph as G
import Data.List as List
import Data.Maybe
import Data.Typeable
import System.IO.Unsafe

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

threaded 
  :: forall domain a b. 
    ( Typeable a
    , Typeable (domain :: *)
    , Theme (Discussion domain a)
    , Theme (Comment domain a)
    , ToJSON (Resource (Comment domain a)), FromJSON (Resource (Comment domain a))
    , Formable (Resource (Comment domain a))
    , Default (Resource (Comment domain a))
    , ToJSON (Context a), FromJSON (Context a), Pathable (Context a), Eq (Context a), Ord (Context a)
    , ToJSON (Name a), FromJSON (Name a), Pathable (Name a), Eq (Name a), Ord (Name a)
    , Default (Resource (Comment domain a))
    , Ord b
    , FromJSON (Product (Meta domain a))
    , VWS.Websocket domain
    , Authentication domain
    , Reader (Context a)
    , Reader (Name a)
    , Reader (Root domain a)
    ) => (Product (Meta domain a) -> Product (Comment domain a) -> b) -> (CommentFormContext domain a => View) -> (CommentContext domain a => View) -> View
threaded sorter commentFormBuilder commentBuilder = 
  discussion @domain @a (threads @domain @a @b sorter commentFormBuilder commentBuilder)

threads :: forall domain a b. DiscussionLayout domain a b
threads sorter form comment =
  Article <| Themed @(Discussion domain a) |> 
    [ state False do
        if get then
          reader (Parent (Nothing :: Maybe (Key (Comment domain a)))) do
            reader (CommentFormCancelAction (modify False)) do
              reader (Previous (Nothing :: Maybe (Key (Comment domain a)))) do
                reader (Next (Nothing :: Maybe (Key (Comment domain a)))) do
                  state (NewComment (Nothing :: Maybe (Resource (Comment domain a)))) do
                    form 
        else
          Header <||>
            [ Button <| OnClick (\_ -> modify True) |> [ "Add Comment" ] 
            ]

    , Keyed Section <||#> do
        forest Nothing Nothing do
          maybe threads isolated Pure.Convoker.Discussion.root
    ]
  where
    Discussion { comments = cs } = full @domain @a 

    edges = fmap (\comment@Comment { key, parents = Parents ps } -> (comment,key,ps)) cs
    
    -- transpose because each vertex in our graph points to predecessors
    (G.transposeG -> graph,nodeFromVertex,find) = G.graphFromEdges edges

    threads = G.components graph

    isolated k = G.dfs graph (maybe id (:) (find k) [])

    forest rt par ts = 
      let look (G.Node n _) = let (comment,_,_) = nodeFromVertex n in comment
          sorted = List.sortOn (sorter Pure.Convoker.Discussion.meta . look) ts 
      in 
        [ tree rt par previous next t
        | (t,pr,nx) <- zip3 sorted (Nothing : fmap Just sorted) (List.tail (fmap Just sorted ++ [Nothing])) 
        , let 
            rt = if isJust rt then rt else par
            previous = fmap (\(look -> Comment { key }) -> key) pr
            next = fmap (\(look -> Comment { key }) -> key) nx
        ]
    
    tree root par previous next node@(G.Node (nodeFromVertex -> (c@Comment { key , author },_,_)) sub) =
      (hash key,
        reader (Parent (par :: Maybe (Key (Comment domain a)))) do
          reader (Previous (previous :: Maybe (Key (Comment domain a)))) do
            reader (Next (next :: Maybe (Key (Comment domain a)))) do
              reader (DiscussionComment c) do
                reader (RenderedChildren (forest root (Just key) sub)) do
                  reader (Descendants (Foldable.length node - 1)) do
                    reader (Author author) do
                      comment
      )
{-
        { children = forest root (Just key) sub
        , size = Foldable.length node - 1 -- since children are passed lazily pre-rendered
        , ..
        }
-}

    {- 
      What is the performance difference between the above and this?
      What is the difference heap-wise? My intuition says that they will be the same,
      but I'm erring on the side of caution here since this is a critical portion of the
      threaded layout. I'd like to test when I have some extensive mocking or a good 
      arbitrary instance or when I can look at some core.

      forest mparent ts = 
        let look (G.Node n _) = let (comment,_,_) = nodeFromVertex n in comment
        in [ tree t | t <- List.sortOn (sorter meta . look) ts ]
        where
          tree (G.Node (nodeFromVertex -> (Comment { key },_,_)) sub) =
            runCommentBuilder CommentBuilder 
              { children = forest (Just key) sub
              , size = Foldable.length node - 1
              , ..
              }
    -}
