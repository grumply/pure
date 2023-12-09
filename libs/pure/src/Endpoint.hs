{-# language DerivingVia, TypeApplications, ScopedTypeVariables, TypeFamilies, GADTs, MultiParamTypeClasses, AllowAmbiguousTypes, OverloadedStrings, DataKinds, UndecidableInstances #-}
module Endpoint where

import Control.Exception
import Data.Char as Char
import Data.List as List
import Data.Proxy
import Data.JSON hiding (Result)
import Data.String
import Data.Theme
import Data.Txt as Txt
import Data.Typeable
import Data.ByteString
import Data.Void

{- |

This module implements a type-oriented unification of a simplistic client and 
server scheme. 

  - `Endpoint` protects from mis-use.
  - `API` is a means of host configuration.
  - `Methods` is a class for API specification by assocating a type with endpoints.

This approach is /acceptable/, for now; since we're strongly integrated, an
implementation like this is workable for the vast majority of use-cases.
Anything more complex can drop down to the WAI interface for server handlers and 
Data.Fetch for clients.

At some point, it might be of reasonable benefit to implement a more extensive
HTTP standards-modeling framework, like that of servant. It would still be 
possible to implement this same style of API modeling, but we would also be able
to implement well-specified arbitrary HTTP APIs, which could be nice for 
specifying and interacting with 3rd-party APIs that don't have our simplified
assumptions.

For now, though, this approach is performant and well-compatible with our SPA-
style design. The benefit of this limited approach is in its simplicity - there
is little to learn, so it offers a high power-to-weight ratio in terms of 
complexity and comprehension; developers can get something running with relative
ease.

## Methods
The `Methods` class describes a set of HTTP verb endpoints:
```
class Typeable r => Methods r where
  type Create r :: * 
  type Create r = Void

  type Update r :: *
  type Update r = Void

  type Query r :: *
  type Query r = Void

  type Place r :: *
  type Place r = Void

  type Delete r :: *
  type Delete r = Void

  endpoint :: forall method x. Endpoint method x
  endpoint = defaultBase @r
```

## Unimplemented Methods
Any unimplemented methods default to `Void`. `Server` will handle this by
responding with the HTTP status '501 Not Implemented'. `Client` will handle
this by preventing requests to `Void` endpoints at compile-time.

## Default Path
An unspecified endpoint defaults to a path fragment based on the `Methods` instance
head using `defaultBase`. For example, if you declared: 
```
instance Methods (Maybe Bool)
```
Your default endpoint would be: "/maybe_bool"

## Method Shape
The `Methods` class does not constrain the associated types, but `Server` and
`Client` expect either `Void` or `IO`-returns with JSON-encodable payloads and
responses:
```
data Counter
instance Methods Counter where
  type Update Counter = Int -> IO ()
  type Query Counter = IO Int
```

## Virtualization
Sometimes you'll want a concrete API based on the resource type, and 
sometimes it will make more sense to virtualize an API. Imagine the archetypal 
library example with two endpoints: find a book, add a book.
```
newtype BookId = BookId {...}
data Book = Book {...}
-- assume derived or defined instances for To/FromJSON for BookId and Book
```
We can create a concrete API based on the `Book` idea:
```
instance Methods Book where
  type Update Library = BookId -> Book -> IO ()
  type Query Library = BookId -> IO (Maybe Book)
```
Or, it might make more sense to virtualize the api with a `Library` type:
```
data Library
instance Methods Library where
  type Update Library = BookId -> Book -> IO ()
  type Query Library = BookId -> IO (Maybe Book)
```
Your domain will often dictate what makes the most sense. If you re-use a 
primary resource across method domains, you'll likely want virtualization.

## Versioning
To version an API, a simplistic approach is often sufficient:
```
data MyAPI (version :: Nat)
instance Methods (MyAPI 1) where 
  base = "/myapi" -- no mention of version to maintain backwards-compatibility
  {...}
instance Methods (MyAPI 2) where
  base = "/myapi/v2"
  {...}
```
-}

data Method = GET | HEAD | POST | PUT | PATCH | DELETE | OPTIONS | CONNECT

data Endpoint (method :: Method) a = Endpoint !(Proxy a) Txt

type GET = Endpoint 'GET
type HEAD = Endpoint 'HEAD 
type PATCH = Endpoint 'PATCH
type POST = Endpoint 'POST 
type PUT = Endpoint 'PUT 
type DELETE = Endpoint 'DELETE 
type CONNECT = Endpoint 'CONNECT 

instance Show (Endpoint method a) where
  show (Endpoint _ t) = fromTxt t

instance IsString (Endpoint method a) where
  fromString str = Endpoint Proxy (fromString str)

instance ToTxt (Endpoint method a) where
  toTxt (Endpoint _ path) = path

instance FromTxt (Endpoint method a) where
  fromTxt = Endpoint Proxy

instance Monoid (Endpoint method a) where
  mempty = fromTxt mempty

instance Semigroup (Endpoint method a) where
  (<>) (Endpoint _ pl) (Endpoint _ pr) = fromTxt (pl <> pr)

newtype Host = Host Txt
  deriving (ToJSON,FromJSON,ToTxt,FromTxt,Show,Eq,Ord) via Txt

-- | 
newtype Agent = Agent Txt
  deriving (ToJSON,FromJSON,ToTxt,FromTxt,Show,Eq,Ord) via Txt

-- | The exception type that tells calling code that the current level of
-- authorization is insufficient to proceed. On servers, this should result in a
-- 401 Unauthorized HTTP response. On clients, this should generally be avoided
-- as the single-tenancy nature of clients should allow for unambiguous code
-- paths.
data Unauthorized = Unauthorized deriving Show
instance Exception Unauthorized

-- | Throw `Unauthorized` from anywhere. 
--
-- ## Example
-- When using `pure-auth` to construct a token-authenticated context by 
-- cerifying a request-supplied token, failure results in an `Unauthorized`
-- exception which is handled by the HTTP method dispatcher by responding with a
-- '401 Unauthorized' HTTP status.
unauthorized :: a
unauthorized = throw Unauthorized

-- | Associate a type `r` with a URL.
--
-- ## Example
-- Instances are often of the form:
-- 
-- > data MyApp
-- > instance API MyApp where 
-- >   api = "https://localhost:8081"
--
class API r where
  api :: Txt
  -- At some point, it would make sense to better integrate this, but, for now,
  -- simple is best.


class Typeable r => Methods r where
 
  type Create r :: *
  type Create r = Void

  type Update r :: *
  type Update r = Void

  type Query r :: *
  type Query r = Void

  type Replace r :: *
  type Replace r = Void

  type Delete r :: *
  type Delete r = Void

  {-# NOINLINE endpoint #-}
  endpoint :: Endpoint method x
  endpoint = defaultBase @r
    
create :: forall r. Methods r => POST (Create r)
create = endpoint @r

update :: forall r. Methods r => PATCH (Update r)
update = endpoint @r

query :: forall r. Methods r => GET (Query r)
query = endpoint @r

replace :: forall r. Methods r => PUT (Replace r)
replace = endpoint @r

delete :: forall r. Methods r => DELETE (Delete r)
delete = endpoint @r

defaultBase :: forall r method x. Typeable r => Endpoint method x
defaultBase = fromString ('/' : fmap Char.toLower rep)
  where
    rep = limit <$> go (typeRep (Proxy :: Proxy r))
      where
        limit c | isAscii c && isAlphaNum c = c | otherwise = '_'
        go tr =
          let tc = show (typeRepTyCon tr)
              trs = typeRepArgs tr
          in List.intercalate "_" (tc : fmap go trs)

