module Pure.Magician.Server.Cache where
  
import Pure.Magician.Resources
import Pure.Magician.Server.Serve

import Pure.Conjurer hiding (cache)
import qualified Pure.Conjurer as Conjurer
import Pure.Convoker as Convoker
import Data.JSON (ToJSON,FromJSON)

import Data.Typeable

cacheAll :: forall a. (Server a, Subset (Caches a) (Resources a) ~ True, CacheMany a (Caches a)) => IO ()
cacheAll = cacheMany @a @(Caches a)

class CacheMany (a :: *) (xs :: [*]) where
  cacheMany :: IO ()

instance (Cacheable a x (Elem x (Discussions a)), CacheMany a xs) => CacheMany a (x : xs) where
  cacheMany = cache @a @x @(Elem x (Discussions a)) >> cacheMany @a @xs

instance Typeable a => CacheMany a '[] where
  cacheMany = do
    Conjurer.cache @(Admins a)

class Cacheable (a :: *) (resource :: *) (discussion :: Bool) where
  cache :: IO ()

-- Default instance for a resource without discussion.
instance {-# OVERLAPPABLE #-} 
  ( Typeable resource 
  , Conjurable resource
  ) => Cacheable a resource False where
  cache = do
    Conjurer.cache @resource
  
-- Default instance for a resource with discussion.
instance {-# OVERLAPPABLE #-} 
  ( Typeable a
  , Typeable resource 
  , Conjurable resource
  , ToJSON (Product (Meta a resource)), FromJSON (Product (Meta a resource))
  , ToJSON (Preview (Meta a resource)), FromJSON (Preview (Meta a resource))
  ) => Cacheable a resource True where
  cache = do
    Conjurer.cache @resource
    Convoker.convokerCache @a @resource