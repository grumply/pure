module Pure.Convoker (module Export, module Pure.Convoker) where

import Pure.Convoker.Comment as Export
import Pure.Convoker.Discussion as Export
import Pure.Convoker.Meta as Export
import Pure.Convoker.Mods as Export
import Pure.Convoker.Admins as Export
import Pure.Convoker.UserVotes as Export

import Pure.Auth (Username)
import Pure.Conjurer
import Data.Default
import Data.JSON hiding (Null)
import Data.View
import Data.Sorcerer
import Data.Websocket

import Data.Hashable

import Data.Typeable

type Convokable domain a = 
  ( Pathable (Context a)
  , ToJSON (Context a), FromJSON (Context a)

  , Pathable (Name a)
  , ToJSON (Name a), FromJSON (Name a)

  , Amendable (Comment domain a)
  , ToJSON (Resource (Comment domain a)), FromJSON (Resource (Comment domain a))
  , ToJSON (Amend (Comment domain a)), FromJSON (Amend (Comment domain a))

  , Amendable (Meta domain a)
  , ToJSON (Resource (Meta domain a)), FromJSON (Resource (Meta domain a))
  , ToJSON (Preview (Meta domain a)), FromJSON (Preview (Meta domain a))
  , ToJSON (Product (Meta domain a)), FromJSON (Product (Meta domain a))
  , ToJSON (Amend (Meta domain a)), FromJSON (Amend (Meta domain a))
  ) 

#ifndef __GHCJS__
convoke
  :: forall domain a.
    ( Typeable domain
    , Typeable a

    , Pathable (Context a), Hashable (Context a), Ord (Context a)
    , ToJSON (Context a), FromJSON (Context a)

    , Pathable (Name a), Hashable (Name a), Ord (Name a)
    , ToJSON (Name a), FromJSON (Name a)

    , Amendable (Comment domain a)
    , ToJSON (Resource (Comment domain a)), FromJSON (Resource (Comment domain a))
    , ToJSON (Amend (Comment domain a)), FromJSON (Amend (Comment domain a))

    , Amendable (Meta domain a)
    , ToJSON (Resource (Meta domain a)), FromJSON (Resource (Meta domain a))
    , ToJSON (Preview (Meta domain a)), FromJSON (Preview (Meta domain a))
    , ToJSON (Product (Meta domain a)), FromJSON (Product (Meta domain a))
    , ToJSON (Amend (Meta domain a)), FromJSON (Amend (Meta domain a))

    ) => IO ()
convoke = do
  conjure @(Discussion domain a) 
  conjure @(Comment domain a)
  conjure @(Meta domain a)
  conjure @(Mods domain a)
  conjure @(UserVotes domain a)

-- | This should be considered an extensible API for managing an unauthenticated
-- user's discussion endpoints. You can choose to satisfy some of the 
-- constraints and partially apply this function and thereby reduce the required
-- implementation burden, e.g. as seen in Pure.Convoker.Discussion.Threaded
-- Meta has a concrete implementation that satisfies the required constraints.
--
-- Callbacks for Discussion, Meta, and Mods are required parameters since they 
-- are the extension points for the API, allowing, e.g. analytics for discussion 
-- reads, etc....
-- 
-- Returns a callback that will deactivate the endpoints.
unauthenticatedEndpoints 
  :: forall domain a. 
    ( Typeable domain
    , Typeable a

    , Pathable (Context a), Ord (Context a), Hashable (Context a)
    , ToJSON (Context a), FromJSON (Context a)

    , Pathable (Name a), Ord (Name a), Hashable (Name a)
    , ToJSON (Name a), FromJSON (Name a)

    , ToJSON (Product (Meta domain a)), FromJSON (Product (Meta domain a))
    , ToJSON (Preview (Meta domain a)), FromJSON (Preview (Meta domain a))

    ) => Websocket -> Callbacks (Discussion domain a) -> Callbacks (Meta domain a) -> Callbacks (Mods domain a) ->  IO (IO ())
unauthenticatedEndpoints socket discussionCallbacks metaCallbacks modsCallbacks = do
  discussion <- enact socket (reading @(Discussion domain a) readPermissions discussionCallbacks)
  meta       <- enact socket (reading @(Meta domain a) readPermissions metaCallbacks)
  mods       <- enact socket (reading @(Mods domain a) readPermissions modsCallbacks)
  pure do
    repeal discussion
    repeal meta
    repeal mods

-- | This should be considered an extensible API for managing an authenticated
-- user's discussion endpoints. You can choose to satisfy some of the 
-- constraints and partially apply this function and thereby reduce the required
-- implementation burden, e.g. as seen in Pure.Convoker.Discussion.Threaded 
-- where Comment and Meta have concrete implementations that satisfy all of the
-- required constraints and the only required parameters are the WebSocket, the
-- Username, and Callbacks for extensibility.
--
-- It would be easy to simplify the constraints here as `Conjurable (Meta domain a)`
-- and `Conjurable (Comment domain a)`, but that would obscure the nature of this
-- function by hiding what exactly needs to be implemented for a fully-custom 
-- discussion implemention. That is, this type signature can be read as a
-- guide for implementing your own, customized, discussion types, as they
-- are the types and instances that were omitted to maintain generality.  
--
-- Note that one place where this generality was not chased to the edge was
-- in the implementation of UserVotes. Creating a user-local sideband, much 
-- like the discussion-local sideband found in `Meta`, could be a good extension
-- point for more complex discussion dynamics, but would bring with it another 
-- extensive set of constraints. 
-- 
-- Permissions for Comment and Meta are required parameters since Comment and 
-- Meta are left open for custom implementation and it wouldn't make sense to 
-- have arbitrary defaults.
--
-- Callbacks for Discussion, Comment, Meta, Mods, and UserVotes are required 
-- parameters since they are extension points for the API, allowing, e.g.
-- user email notifications to be sent upon update or changes, or triggering 
-- of analysis on comment creation, etc....
--
-- Similarly to the case for permissioning for Comment and Meta, interactions
-- for Comment and Meta are required since it wouldn't make sense to have 
-- arbitrary defaults.
--
-- Returns a callback that will deactivate the endpoints.
authenticatedEndpoints 
  :: forall domain a. 
    ( Typeable domain
    , Typeable a

    , Pathable (Context a), Ord (Context a), Hashable (Context a)
    , ToJSON (Context a), FromJSON (Context a)

    , Pathable (Name a), Ord (Name a), Hashable (Name a)
    , ToJSON (Name a), FromJSON (Name a)

    , Nameable (Comment domain a)
    , Processable (Comment domain a)
    , Producible (Comment domain a)
    , Amendable (Comment domain a)
    , ToJSON (Resource (Comment domain a)), FromJSON (Resource (Comment domain a))
    , ToJSON (Action (Comment domain a)), FromJSON (Action (Comment domain a))
    , ToJSON (Reaction (Comment domain a)), FromJSON (Reaction (Comment domain a))
    , ToJSON (Amend (Comment domain a)), FromJSON (Amend (Comment domain a))

    , Processable (Meta domain a)
    , Producible (Meta domain a)
    , Previewable (Meta domain a)
    , Amendable (Meta domain a)
    , ToJSON (Resource (Meta domain a)), FromJSON (Resource (Meta domain a))
    , ToJSON (Product (Meta domain a)), FromJSON (Product (Meta domain a))
    , ToJSON (Preview (Meta domain a)), FromJSON (Preview (Meta domain a))
    , ToJSON (Action (Meta domain a)), FromJSON (Action (Meta domain a))
    , ToJSON (Reaction (Meta domain a)), FromJSON (Reaction (Meta domain a))
    , ToJSON (Amend (Meta domain a)), FromJSON (Amend (Meta domain a))

    , DefaultPermissions (Mods domain a)
    , DefaultPermissions (UserVotes domain a)

    , DefaultCallbacks (UserVotes domain a) 

    ) => Websocket 
      -> Username 
      -> Permissions (Comment domain a) 
      -> Permissions (Meta domain a) 
      -> Callbacks (Discussion domain a) 
      -> Callbacks (Comment domain a)
      -> Callbacks (Meta domain a)
      -> Callbacks (Mods domain a)
      -> Callbacks (UserVotes domain a)
      -> Interactions (Comment domain a) 
      -> Interactions (Meta domain a) 
      -> IO (IO ())
authenticatedEndpoints socket un commentPermissions metaPermissions discussionCallbacks commentCallbacks metaCallbacks modsCallbacks userVotesCallbacks commentInteractions metaInteractions = do 
  -- Notes:
  --   We don't cache Comment, since they're stored in Discussion.
  --   We don't cache UserVotes to reduce memory overhead and make UX more intuitive (it's confusing to not see your votes).
  --   We don't enact a publishing endpoint for discussion because
  --     discussions are manually created when their linked resource 
  --     is created.

  discussionReading   <- enact socket (reading @(Discussion domain a) readPermissions discussionCallbacks)
  commentReading      <- enact socket (reading @(Comment domain a) commentPermissions commentCallbacks)
  metaReading         <- enact socket (reading @(Meta domain a) metaPermissions metaCallbacks)
  modsReading         <- enact socket (reading @(Mods domain a) readPermissions modsCallbacks)
  userVotesReading    <- enact socket (reading @(UserVotes domain a) (permissions (Just un)) userVotesCallbacks)

  commentPublishing   <- enact socket (publishing @(Comment domain a) commentPermissions commentCallbacks commentInteractions)
  metaPublishing      <- enact socket (publishing @(Meta domain a) metaPermissions metaCallbacks metaInteractions)
  modsPublishing      <- enact socket (publishing @(Mods domain a) (permissions (Just un)) modsCallbacks (interactions (Just un)))
  userVotesPublishing <- enact socket (publishing @(UserVotes domain a) (permissions (Just un)) userVotesCallbacks (interactions (Just un)))

  pure do
    repeal discussionReading
    repeal commentReading
    repeal metaReading
    repeal modsReading
    repeal userVotesReading

    repeal commentPublishing
    repeal metaPublishing
    repeal modsPublishing
    repeal userVotesPublishing 

convokerCache 
  :: forall domain a. 
    ( Typeable domain
    , Typeable a

    , Pathable (Context a), Ord (Context a), Hashable (Context a)
    , ToJSON (Context a), FromJSON (Context a)

    , Pathable (Name a), Ord (Name a), Hashable (Name a)
    , ToJSON (Name a), FromJSON (Name a)

    , ToJSON (Product (Meta domain a)), FromJSON (Product (Meta domain a))
    , ToJSON (Preview (Meta domain a)), FromJSON (Preview (Meta domain a))
    ) => IO ()
convokerCache = do
  cache @(Discussion domain a)
  cache @(Mods domain a)
  cache @(Meta domain a)
#endif