module Pure.Magician.Server.Listen where
  
import Pure.Magician.Resources
import Pure.Magician.Server.Serve

import Data.Sorcerer (sorcerer)
import Pure.Auth (authDB)
import Pure.Conjurer (Conjurable,conjure)
import Pure.Conjurer.Analytics
import Pure.Convoker (Admins,Convokable,convoke)

import Data.Typeable

listenAll :: forall a. (Server a, ListenMany a (Resources a)) => IO ()
listenAll = listenMany @a @(Resources a)

class ListenMany (a :: *) (xs :: [*]) where
  listenMany :: IO ()

instance (Listenable a x (Elem x (Discussions a)), ListenMany a xs) => ListenMany a (x : xs) where
  listenMany = listen @a @x @(Elem x (Discussions a)) >> listenMany @a @xs

instance Typeable a => ListenMany a '[] where
  listenMany = do
    authDB @a 
    conjure @(Admins a)
    sorcerer @GlobalAnalyticsMsg @'[GlobalAnalytics] 
    sorcerer @SessionMsg @'[Session]
    sorcerer @SessionsMsg @'[Sessions]

class Listenable (a :: *) (resource :: *) (discussion :: Bool) where
  listen :: IO ()

-- Default listeners for resource without discussion, not analyzed.
instance {-# OVERLAPPABLE #-} (Conjurable resource) => Listenable a resource False where
  listen = conjure @resource

-- Default listeners for resource with discussion, not analyzed.
instance {-# OVERLAPPABLE #-} (Typeable a, Conjurable resource, Convokable a resource) => Listenable a resource True where
  listen = do
    conjure @resource 
    convoke @a @resource 
