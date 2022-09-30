module Pure.Magician.Resources where

{-
Note that this module treats lists as set-like. We rely on type-level lists for
convenience, so some type families, like `Remove`, don't short-circuit like
their true set-oriented counterparts would. Reasonable uses of these type-level
functions should be unlikely to notice this.
-}

{-
The goal here is to be able to define a Resources type instance in a shared
module and use that type-level list as a basis for defining caches, static 
generation, and discussions on the backend, and use it for defining domains
on the frontend.

Shared:

> type instance Resources MyApp = [Resource1,Resource2,Resource3]

Backend:

> instance Server MyApp where
>   type Caches MyApp = Resources MyApp \\ [Resource3] -- everything except Resource3
>   type Statics MyApp = Resources MyApp -- the default, omittable
>   type Discussions MyApp = [Resource1]

Frontend:

> -- equivalent to the incoherent instance; omittable
> instance Client MyApp where
>   type Domains MyApp = Resources MyApp

-}

type family Resources (a :: *) :: [*]

class Server (a :: *) where
  type Caches a :: [*]
  type Caches a = Resources a \\ '[]

  type Statics a :: [*]
  type Statics a = Resources a \\ '[]

  type Discussions a :: [*]
  type Discussions a = '[]
  
  type Analyze a :: [*]
  type Analyze a = '[]

class Client (a :: *) where
  type Domains a :: [*]
  type Domains a = Resources a

type family And (x :: Bool) (y :: Bool) :: Bool where
  And False y = False
  And x False = False
  And x y = True

type family Not (x :: Bool) where
  Not True  = False
  Not False = True

type family Empty (xs :: [*]) :: Bool where
  Empty xs = Subset '[] xs

type family Nonempty (xs :: [*]) :: Bool where
  Nonempty xs = Not (Empty xs)

type family Elem (x :: *) (ys :: [*]) :: Bool where
  Elem x '[] = False
  Elem x (x : ys) = True
  Elem x (y : ys) = Elem x ys

type family Subset (xs :: [*]) (ys :: [*]) :: Bool where
  Subset '[] ys = True
  Subset as as = True
  Subset (a : as) ys = And (Elem a ys) (Subset as ys)

type family Add (x :: *) (xs :: [*]) :: [*] where
  Add x '[] = x : '[]
  Add x (x : ys) = x : ys
  Add x (y : ys) = y : Add x ys

type family Remove (x :: *) (xs :: [*]) :: [*] where
  Remove x '[] = '[]
  Remove x (x : ys) = Remove x ys
  Remove x (y : ys) = y : Remove x ys

type family (\\) (xs :: [*]) (ys :: [*]) :: [*] where
  xs \\ '[] = xs
  xs \\ (y : ys) = (Remove y xs) \\ ys

type family (++) (xs :: [*]) (ys :: [*]) :: [*] where
  xs ++ '[] = xs
  xs ++ (y : ys) = (Add y xs) ++ ys

class Materialize (bool :: Bool) where
  materialize :: Bool

instance Materialize True where
  materialize = True

instance Materialize False where
  materialize = False