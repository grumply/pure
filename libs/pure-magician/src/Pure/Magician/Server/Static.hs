module Pure.Magician.Server.Static where

import Pure.Magician.Resources
import Pure.Magician.Server.Serve

import Pure.Conjurer
import Data.View (Viewable)
import Data.Websocket (Websocket)

import Data.Typeable (Typeable)

staticAll :: forall a. (Server a, Subset (Statics a) (Resources a) ~ True, StaticMany a (Statics a)) => IO ()
staticAll = staticMany @a @(Statics a)

class StaticMany a (xs :: [*]) where
  staticMany :: IO ()

instance (Staticable a x (Elem x (Statics a)), StaticMany a xs) => StaticMany a (x : xs) where
  staticMany = static @a @x @(Elem x (Statics a)) >> staticMany @a @xs 

instance StaticMany a '[] where
  staticMany = pure ()

class Staticable (a :: *) (resource :: *) (static :: Bool) where
  static :: IO ()

instance {-# OVERLAPPABLE #-} (Conjurable resource, Routable resource, Viewable (Product resource)) => Staticable a resource True where
  static = generateStatic @resource

instance Staticable a resource False where
  static = pure ()
