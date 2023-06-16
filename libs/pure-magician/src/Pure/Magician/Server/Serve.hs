module Pure.Magician.Server.Serve where

import Pure.Magician.Resources
import Pure.Magician.Server.Limit (limiting,Limit,LimitMany)

import Control.Log (Logging)
import Pure.Auth (Username(..))
import Pure.Conjurer.Analytics
import Pure.Conjurer as Conjurer
import Pure.Convoker as Convoker
import Pure.Convoker.UserVotes
import Data.Default
import Data.Txt
import Data.JSON (ToJSON,FromJSON)
import Data.Websocket (Websocket, enact)
import qualified Data.Websocket as WS (remove)

import Control.Monad (void)
import Data.Kind
import Data.Typeable (Typeable)

{-

The goal here is to have a simple appoach to definining which resources are
cached, which resources have static pages generated for serving to crawlers,
and which resources have discussions associated with them. From these sets, a
server should be automatically generated that does all of the heavy lifting of 
setting up Pure.Sorcerer listeners, Pure.Conjurer caches, Pure.Conjurer static
pages, and Pure.Convoker discussion handling. 

Given these shared resources:

> -- Shared.hs
> data MyApp
>
> type instance Resources MyApp = [Resource1,Resource2,Resource3]
>
> {- resource definitions -}

A backend can look something like:

> -- Backend.hs
> main = serve @MyApp
>
> instance Serve MyApp where
>   type Caches MyApp = Resources MyApp \\ [Resource3] -- everything except Resource3
>   type Statics MyApp = Resources MyApp -- the default, omittable
>   type Discussions MyApp = [Resource1]

With this approach, given an average blog or simple website, a backend can be a
single line of code.

NOTE: If a resource is part of the Caches type family, both the resource and its
      associated discussion are cached. There is currently not way to cache a
      resource and not its discussion, or vice versa, without manually defining
      a Servable instance.

-}


--------------------------------------------------------------------------------

serveAll :: forall a. (Server a, ServeMany a (Resources a), LimitMany a (Resources a)) => Websocket -> SessionId -> Maybe Username -> IO ()
serveAll = serveMany @a @(Resources a)

class ServeMany (a :: *) (xs :: [*]) where
  serveMany :: Websocket -> SessionId -> Maybe Username -> IO ()

instance (Logging, Typeable a) => ServeMany a '[] where
  serveMany socket sid mu = void do
    defaultServe @a @(Admins a) socket sid mu 

instance (Server a, Limit x, Limit (Mods a x), Limit (Discussion a x), Limit (Meta a x), Limit (UserVotes a x), Limit (Comment a x), Servable a x (Elem x (Discussions a)) (Elem x (Analyze a)), LimitMany a xs, ServeMany a xs) => ServeMany a (x : xs) where 
  serveMany ws sid mun = serve @a @x @(Elem x (Discussions a)) @(Elem x (Analyze a)) ws sid mun >> serveMany @a @xs ws sid mun

class Servable (a :: *) (resource :: *) (discuss :: Bool) (analyze :: Bool) where
  serve :: Websocket -> SessionId -> Maybe Username -> IO ()

type family ServeConstraints a resource (discussion :: Bool) :: Constraint where
  ServeConstraints a resource True = 
    ( ServeConstraints a resource False
    , DefaultPermissions (Comment a resource), DefaultPermissions (Meta a resource)
    , DefaultCallbacks (Comment a resource), DefaultCallbacks (Meta a resource)
    , DefaultInteractions (Comment a resource), DefaultInteractions (Meta a resource)
    , DefaultCallbacks (Discussion a resource), DefaultCallbacks (Mods a resource), DefaultCallbacks (UserVotes a resource)
    , Conjurable (Meta a resource) 
    , Conjurable (Comment a resource) 
    , Default (Resource (Meta a resource))
    , Previewable (Meta a resource)
    , Producible (Meta a resource)
    , Producible (Comment a resource)
    , Processable (Meta a resource)
    , Processable (Comment a resource)
    , Amendable (Meta a resource)
    , Amendable (Comment a resource)
    , Limit (Meta a resource)
    , Limit (Discussion a resource)
    , Limit (Mods a resource)
    , Limit (Comment a resource)
    , Limit (UserVotes a resource)
    )
  ServeConstraints a resource False = 
    ( Typeable a
    , Typeable resource
    , DefaultPermissions resource
    , DefaultCallbacks resource
    , DefaultInteractions resource
    , Conjurable resource
    , Previewable resource
    , Producible resource
    , Processable resource
    , Amendable resource
    , Limit resource
    , Routable resource
    , Rootable resource
    , Logging
    )

-- Default instance for a uncached resource with discussion with analyze.
instance ( ServeConstraints a resource True ) => Servable a resource True True where
  serve = defaultServeWithDiscussionWithAnalyze @a @resource

-- Default instance for a uncached resource with discussion without analyze.
instance ( ServeConstraints a resource True ) => Servable a resource True False where
  serve = defaultServeWithDiscussion @a @resource

-- Default instance for a uncached resource without discussion with analyze.
instance ( ServeConstraints a resource False ) => Servable a resource False True where
  serve = defaultServeWithAnalyze @a @resource

-- Default instance for a uncached resource without discussion without analyze.
instance ( ServeConstraints a resource False ) => Servable a resource False False where
  serve = defaultServe @a @resource

defaultServeWithDiscussionWithAnalyze :: forall a x.  ( ServeConstraints a x True ) => Websocket -> SessionId -> Maybe Username -> IO ()
defaultServeWithDiscussionWithAnalyze ws sid = \case
  Just un -> void do
    ip <- fromWebsocket ws
    let ipt = toTxt ip
    enact ws (reading @x (limiting ipt readPermissions) (addAnalytics sid ip (callbacks (Just un))))
    enact ws (publishing @x (limiting ipt $ permissions (Just un)) (addAnalytics sid ip (addDiscussionCreationCallbacks @a [un] (callbacks (Just un)))) (interactions (Just un)))
    enact ws (analytics @x (limiting ipt $ permissions (Just un)))
    
    enact ws (reading @(Discussion a x) (limiting ipt readPermissions) (callbacks (Just un)))
    enact ws (reading @(Comment a x) (limiting ipt $ permissions (Just un)) (extendCommentCallbacks (limiting ipt fullPermissions) (callbacks (Just un)) (callbacks (Just un))))
    enact ws (reading @(Meta a x) (limiting ipt $ permissions (Just un)) (callbacks (Just un)))
    enact ws (reading @(Mods a x) (limiting ipt readPermissions) (callbacks (Just un)))
    enact ws (reading @(UserVotes a x) (limiting ipt $ permissions (Just un)) (callbacks (Just un)))

    enact ws (publishing @(Comment a x) (limiting ipt $ permissions (Just un)) (extendCommentCallbacks (limiting ipt fullPermissions) (callbacks (Just un)) (callbacks (Just un))) (interactions (Just un)))
    enact ws (publishing @(Meta a x) (limiting ipt $ permissions (Just un)) (callbacks (Just un)) (interactions (Just un)))
    enact ws (publishing @(Mods a x) (limiting ipt $ permissions (Just un)) (callbacks (Just un)) (interactions (Just un)))
    enact ws (publishing @(UserVotes a x) (limiting ipt $ permissions (Just un)) (callbacks (Just un)) (interactions (Just un)))

  _ -> void do
    ip <- fromWebsocket ws
    let ipt = toTxt ip
    enact ws (reading @x (limiting ipt readPermissions) (addAnalytics sid ip (callbacks Nothing)))
    enact ws (analytics @x (limiting ipt $ permissions Nothing))
    enact ws (reading @(Discussion a x) (limiting ipt readPermissions) (callbacks Nothing))
    enact ws (reading @(Meta a x) (limiting ipt readPermissions) (callbacks Nothing))
    enact ws (reading @(Mods a x) (limiting ipt readPermissions) (callbacks Nothing))

defaultServeWithDiscussion :: forall a x.  ( ServeConstraints a x True ) => Websocket -> SessionId -> Maybe Username -> IO ()
defaultServeWithDiscussion ws _ = \case
  Just un -> void do
    ip <- fromWebsocket ws
    let ipt = toTxt ip
    enact ws (reading @x (limiting ipt readPermissions) (callbacks (Just un)))
    enact ws (publishing @x (limiting ipt $ permissions (Just un)) (addDiscussionCreationCallbacks @a [un] (callbacks (Just un))) (interactions (Just un)))

    enact ws (reading @(Discussion a x) (limiting ipt readPermissions) (callbacks (Just un)))
    enact ws (reading @(Comment a x) (limiting ipt $ permissions (Just un)) (extendCommentCallbacks (limiting ipt fullPermissions) (callbacks (Just un)) (callbacks (Just un))))
    enact ws (reading @(Meta a x) (limiting ipt $ permissions (Just un)) (callbacks (Just un)))
    enact ws (reading @(Mods a x) (limiting ipt readPermissions) (callbacks (Just un)))
    enact ws (reading @(UserVotes a x) (limiting ipt $ permissions (Just un)) (callbacks (Just un)))

    enact ws (publishing @(Comment a x) (limiting ipt $ permissions (Just un)) (extendCommentCallbacks (limiting ipt fullPermissions) (callbacks (Just un)) (callbacks (Just un))) (interactions (Just un)))
    enact ws (publishing @(Meta a x) (limiting ipt $ permissions (Just un)) (callbacks (Just un)) (interactions (Just un)))
    enact ws (publishing @(Mods a x) (limiting ipt $ permissions (Just un)) (callbacks (Just un)) (interactions (Just un)))
    enact ws (publishing @(UserVotes a x) (limiting ipt $ permissions (Just un)) (callbacks (Just un)) (interactions (Just un)))

  _ -> void do
    ip <- fromWebsocket ws
    let ipt = toTxt ip
    enact ws (reading @x (limiting ipt readPermissions) (callbacks Nothing))
    enact ws (reading @(Discussion a x) (limiting ipt readPermissions) (callbacks Nothing))
    enact ws (reading @(Meta a x) (limiting ipt readPermissions) (callbacks Nothing))
    enact ws (reading @(Mods a x) (limiting ipt readPermissions) (callbacks Nothing))

defaultServeWithAnalyze :: forall a x.  ( ServeConstraints a x False ) => Websocket -> SessionId -> Maybe Username -> IO ()
defaultServeWithAnalyze ws sid = \case
  Just un -> void do
    ip <- fromWebsocket ws
    let ipt = toTxt ip
    enact ws (reading @x (limiting ipt readPermissions) (addAnalytics sid ip (callbacks (Just un))))
    enact ws (publishing @x (limiting ipt $ permissions (Just un)) (addAnalytics sid ip (callbacks (Just un))) (interactions (Just un)))
    enact ws (analytics @x (limiting ipt $ permissions (Just un)))

  _ -> void do
    ip <- fromWebsocket ws
    let ipt = toTxt ip
    enact ws (analytics @x (limiting ipt $ permissions Nothing))
    enact ws (reading @x (limiting ipt readPermissions) (addAnalytics sid ip (callbacks Nothing)))

defaultServe :: forall a x.  ( ServeConstraints a x False ) => Websocket -> SessionId -> Maybe Username -> IO ()
defaultServe ws _ = \case
  Just un -> void do
    ip <- fromWebsocket ws
    let ipt = toTxt ip
    enact ws (reading @x (limiting ipt readPermissions) (callbacks (Just un)))
    enact ws (publishing @x (limiting ipt $ permissions (Just un)) (callbacks (Just un)) (interactions (Just un)))

  _ -> void do
    ip <- fromWebsocket ws
    let ipt = toTxt ip
    enact ws (reading @x (limiting ipt readPermissions) (callbacks Nothing))

--------------------------------------------------------------------------------

removeAll :: forall a. (Server a, RemoveMany a (Resources a)) => Websocket -> IO ()
removeAll = removeMany @a @(Resources a)

class RemoveMany (a :: *) (xs :: [*]) where
  removeMany :: Websocket -> IO ()

instance (Removable a x (Elem x (Discussions a)) (Elem x (Analyze a)), RemoveMany a xs) => RemoveMany a (x : xs) where
  removeMany ws = remove @a @x @(Elem x (Discussions a)) @(Elem x (Analyze a)) ws >> removeMany @a @xs ws

instance Typeable a => RemoveMany a '[] where
  removeMany = defaultRemove @(Admins a)

-- This is required to be able to swap out authenticated and unauthenticated
-- enpoints when the user's token changes.
class Removable (a :: *) (resource :: *) (discussion :: Bool) (analyze :: Bool) where
  remove :: Websocket -> IO ()

instance 
  ( Typeable a
  , Typeable resource
  , ToJSON (Context resource), FromJSON (Context resource)
  ) => Removable a resource False False
  where
    remove = defaultRemove @resource 

instance 
  ( Typeable a
  , Typeable resource
  , ToJSON (Context resource), FromJSON (Context resource)
  , ToJSON (Name resource), FromJSON (Name resource)
  ) => Removable a resource True False
  where
    remove = defaultRemoveWithDiscussion @a @resource

instance 
  ( Typeable a
  , Typeable resource
  , ToJSON (Context resource), FromJSON (Context resource)
  , ToJSON (Name resource), FromJSON (Name resource)
  ) => Removable a resource False True
  where
    remove ws = do
      defaultRemove @resource ws
      defaultRemoveAnalytics @resource ws

instance 
  ( Typeable a
  , Typeable resource
  , ToJSON (Context resource), FromJSON (Context resource)
  , ToJSON (Name resource), FromJSON (Name resource)
  ) => Removable a resource True True
  where
    remove ws = do
      defaultRemoveWithDiscussion @a @resource ws
      defaultRemoveAnalytics @resource ws

defaultRemove :: forall (x :: *).  ( Typeable x, ToJSON (Context x), FromJSON (Context x) ) => Websocket -> IO ()
defaultRemove ws = do
  WS.remove ws (readingAPI @x) 
  WS.remove ws (publishingAPI @x)

defaultRemoveWithDiscussion :: forall (a :: *) (x :: *).  ( Typeable a, Typeable x, ToJSON (Context x), FromJSON (Context x), ToJSON (Name x), FromJSON (Name x) ) => Websocket -> IO ()
defaultRemoveWithDiscussion ws = do
  WS.remove ws (readingAPI @x)
  WS.remove ws (publishingAPI @x)
  WS.remove ws (readingAPI @(Discussion a x))
  WS.remove ws (readingAPI @(Comment a x))
  WS.remove ws (readingAPI @(Meta a x))
  WS.remove ws (readingAPI @(Mods a x))
  WS.remove ws (readingAPI @(UserVotes a x))
  WS.remove ws (publishingAPI @(Comment a x))
  WS.remove ws (publishingAPI @(Meta a x))
  WS.remove ws (publishingAPI @(Mods a x))
  WS.remove ws (publishingAPI @(UserVotes a x))

defaultRemoveAnalytics :: forall (x :: *). (Typeable x) => Websocket -> IO ()
defaultRemoveAnalytics ws = do
  WS.remove ws (analyticsAPI @x)