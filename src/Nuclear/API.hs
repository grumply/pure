module Nuclear.API (module Export) where

import Nuclear.API.Implementation as Export
import Nuclear.API.Interface as Export
import Nuclear.API.Unification as Export
import Nuclear.API.ProxyList as Export
import Nuclear.API.TypeList as Export

-- Nuclear.API incorporates the following:
--
-- A DSL for specifying an API as a set of proxies with utilities to build endpoints
-- from the application of proxies to proxies, i.e. Proxy (k -> k') -> Proxy k -> Proxy k'
-- So you may define an api and its implementation like the following:
--
-- > -- shared code:
-- > data Get a
-- > get = Proxy :: Proxy Get
-- > int = Proxy :: Proxy Int
-- >
-- > getIntP = get <@> int
-- >
-- > instance Request (Get Int) where
-- >   type Req (Get Int) = ()
-- >   type Rsp (Get Int) = Int
-- >
-- > api = API (messages none) (requests reqs)
-- >   where
-- >     reqs = only getIntP
-- >
-- > -- server code:
-- > endpoints = Impl (accepts none) (responds reqs)
-- >   where
-- >     reqs = only respondGetInt
-- >
-- > respondGetInt = rsp False getIntP $ \() resp -> resp 42
-- >
-- > local = Unify api endpoints
-- >
-- > -- remote code:
-- > getInt = apiRequest api getIntP () $ \i -> liftIO (putStrLn $ "The answer is: " ++ show i)
--
-- For stubbing out an API, there are some list-like combinators for applying proxies to lists
-- of proxies and lists of proxies to a proxy and appending proxies and consing proxies.
--
-- precedence order from low to high ,i.e. last to first applied (all infix right):
-- <||>, |>, <++>, <|, <:>, <&>, <@>, only
-- read as:
--   <||> : top-level append :: (xs ++ ys ~ zs) => [xs :: k] -> [ys :: k] -> [zs :: k]
--   |>   : apply to list of :: (k -> k') -> [xs :: k] -> [ys :: k']
--   <++> : sub-tree append  :: (xs ++ ys ~ zs) => [xs :: k] -> [ys :: k] -> [zs :: k]
--   <|   : apply list to    :: [fs :: k -> k'] -> (x :: k) -> [ys :: k']
--   <:>  : cons             :: (x :: k) -> [xs :: k] -> (x ': xs)
--   <&>  : finally          :: (x :: k) -> (y :: k) -> '[x,y]
--   only : singleton list   :: (x :: k) -> '[x]
--
-- If you want to avoid all of this, just use (<:> and none) with Proxy (x :: *); I generally use
-- the combinators to sketch an API and then deconstruct it into the corresponding list for
-- implementation/production for clarity.
--
-- This approach allows you to use two existing non-overlapping apis/implementations and smush
-- them together to get a new API and Implementation.
--
-- I suggest this process:
--   1. stub assets
--   2. declare commands
--   3. implement Message/Request instances
--   4. build API specification
--   5. stub implementation
--   6. implement
--
-- This approach was demonstrated, approximately, above.

