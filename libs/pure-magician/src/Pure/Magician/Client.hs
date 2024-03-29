{-# language UndecidableSuperClasses #-}
module Pure.Magician.Client 
  ( client
  , magic
  , withRoute
  , unsafeWithRoute
  , getSomeRoute
  , Magic()
  , currently
  , RouteMany(..)
  , RouteMany'(..)
  , WithRoute
  , WithRoute'(..)
  , Viewables(..)
  , App
  , CURL
  ) where

import Pure.Magician.Client.Restore as Export
import Pure.Magician.Resources as Export

import qualified Pure (run)
import Control.Monad.IO.Class
import Control.Cont
import Control.Component (run,Component)
import Control.Reader (Reader,reader)
import Control.Fold (foldM)
import Control.Log
import Control.State (state')
import Data.Exists
import Data.JSON (ToJSON,FromJSON,traceJSON,logJSON)
import Data.DOM (getPathname,getSearch)
import Data.Time
import Data.Txt (Txt)
import qualified Data.Txt as Txt
import qualified Data.View (Viewable)
import Data.Router (dispatch,path,map,catchError,getRoutingState,putRoutingState,runRouting)
import qualified Data.Router as R
import Data.View
import qualified Effect.Router as Router
import qualified Effect.Websocket as WS
import Pure.Auth 
import Pure.Conjurer as C hiding (Route,Routable)
import qualified Pure.Conjurer as C
import qualified Pure.Conjurer.Analytics as C

import Control.Applicative
import Control.Monad hiding (foldM)
import Data.Bool
import Data.Kind
import Data.Maybe
import Data.Typeable
import System.IO
import System.IO.Unsafe

import Prelude hiding (map,not)

type App domain custom = (WS.Websocket domain, Authentication domain, Router.Router (Either (SomeRoute domain) custom))

type family Viewables (xs :: [*]) :: Constraint where
  Viewables '[] = ()
  Viewables (x ': xs) = (Viewable (Product x), Viewable (Preview x),Viewables xs)

{-# INLINE client #-}
client 
  :: forall domain custom. 
  ( Typeable domain, Typeable custom, RouteMany domain, Logging )
  => String 
  -> Int 
  -> (forall x. R.Routing custom x) 
  -> (App domain custom => View)
  -> View
client host port rtng v = 
  WS.websocket @domain host port False do
    Router.router (R.map Left (routeMany @domain @(Domains domain)) <|> R.map Right rtng) do
      authentication @domain do
        v

magic :: forall domain custom. (WithRoute (CURL domain) domain) => (Reader custom => App domain custom :=> View) -> (App domain custom => View)
magic v =
  case Router.current of
    Right (custom :: custom) -> fromDynamic (with custom v)
    Left sr | ~(Just p) <- withRoute @(CURL domain) @domain sr (pages @domain) -> p

currently :: forall domain f custom x. (WithRoute (Magic domain f) domain, App domain custom) => (forall a. Magic domain f a => C.Route a -> x) -> (custom -> x) -> Maybe x
currently f g =
  case Router.current :: Either (SomeRoute domain) custom of
    Left sr -> withRoute @(Magic domain f) sr f
    Right c -> Just (g c)

class (Creatable a r, Readable a r, Listable a r, Updatable a r) => CURL a r
instance (Creatable a r, Readable a r, Listable a r, Updatable a r) => CURL a r

class (Conjurable r, f r) => Magic d f r
instance (Conjurable r, f r) => Magic d f r

class (Conjurable r, Creatable a r, Readable a r, Listable a r, Updatable a r) => CRUL a r
instance (Conjurable r, Creatable a r, Readable a r, Listable a r, Updatable a r) => CRUL a r

type RouteMany a = RouteMany' a (Domains a)
type WithRoute f a = WithRoute' f a (Domains a)

class RouteMany' (a :: *) (as :: [*]) where
  routeMany :: R.Routing (SomeRoute a) x

instance (Pure.Magician.Client.Routable a x, RouteMany' a xs) => RouteMany' a (x : xs) where
  routeMany = Pure.Magician.Client.route @a @x >> routeMany @a @xs

instance RouteMany' a '[] where
  routeMany = R.continue

class Routable a resource where
  route :: R.Routing (SomeRoute a) ()

instance {-# OVERLAPPABLE #-}
  ( Typeable resource
  , Client a
  , Domains a ~ domains
  , Elem resource domains ~ True
  , C.Routable resource
  , FromJSON (Context resource), ToJSON (Context resource), Pathable (Context resource), Ord (Context resource)
  , FromJSON (Name resource), ToJSON (Name resource), Pathable (Name resource), Ord (Name resource)
  , Ownable resource
  ) => Pure.Magician.Client.Routable a resource 
    where
      route = C.routes @resource SomeRoute

instance RouteMany a => Pathable (SomeRoute a) where
  toPath (SomeRoute r) = toPath r
  fromPath = do
    st <- getRoutingState
    (r,st') <- runRouting (routeMany @a @(Domains a)) st
    case r of
      Prelude.Left (Just sr) -> do
        putRoutingState st'
        pure (Just sr)
      _ -> do
        pure Nothing

withRoute :: forall constraint a x. (WithRoute' constraint a (Domains a)) => SomeRoute a -> (forall r. constraint r => C.Route r -> x) -> Maybe x
withRoute = withSomeRoute @constraint @a @(Domains a)

-- Unsafe if you have written a custom effectful `Routable` instance for one of the `Domains` of `a`. The
-- default derived instances are safe.
unsafeWithRoute :: forall a constraint x. (RouteMany a, WithRoute constraint a) => Txt -> (forall r. constraint r => C.Route r -> x) -> Maybe x
unsafeWithRoute t f 
  | Just (_,sr :: SomeRoute a) <- unsafePerformIO (R.route (routeMany @a @(Domains a)) t)
  = withRoute @constraint @a sr f

  | otherwise
  = Nothing

getSomeRoute :: forall domain. RouteMany domain => IO (Maybe (SomeRoute domain))
getSomeRoute = do
  pn <- getPathname 
  s  <- getSearch
  fmap snd <$> R.route (routeMany @domain @(Domains domain)) (pn <> s)

-- This approach should be okay up to a point, but it does incur overhead in testing
-- the TypeRep for a match against each element of rs. Hopefully the core for this is
-- reasonable and inlines all of this into a big case statement.
class WithRoute' (constraint :: * -> Constraint) (a :: *) (rs :: [*]) where
  withSomeRoute :: forall x. SomeRoute a -> (forall r. constraint r => C.Route r -> x) -> Maybe x

instance WithRoute' constraint a '[] where
  withSomeRoute _ _ = Nothing

instance (Typeable r, WithRoute' constraint a rs, constraint r) => WithRoute' constraint a (r : rs) where
  withSomeRoute sr@(SomeRoute x) f =
    case cast x of
      Just (r :: C.Route r) -> Just (f r)
      _                     -> withSomeRoute @constraint @a @rs sr f

