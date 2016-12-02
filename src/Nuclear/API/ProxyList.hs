{-# language UndecidableInstances #-}
module Nuclear.API.ProxyList where

import Data.Proxy

import Nuclear.API.TypeList

-- Utilities to build lists of proxies that have shared shape.

data PList_ f (elems :: [k])
  where

    PNull
      :: PList_ f '[]

    PCons
      :: f elem
      -> PList_ f elems
      -> PList_ f (elem ': elems)

type PList = PList_ Proxy

type family (f :: k -> k') |$| (xs :: [k]) :: [k'] where
  f |$| '[] = '[]
  f |$| (x ': xs) = (f x) ': (f |$| xs)

type family (fs :: [k -> k']) |&| (x :: k) :: [k'] where
  '[] |&| x = '[]
  (f ': fs) |&| x = (f x) ':  fs |&| x

-- Apply a 
infixr 2 |>
class Ap (f :: k -> k') (ys :: [k]) where
  (|>) :: ((f |$| ys) ~ ys') => Proxy f -> PList ys -> PList ys'
instance Ap f '[] where
  (|>) _ _ = PNull
instance (Ap f ys) => Ap f (y ': ys) where
  (|>) pf (PCons ph hs') =
      PCons (pf <@> ph) (pf |> hs')

infixr 4 <|
class On (fs :: [k -> k']) (x :: k) where
  (<|) :: ((fs |&| x) ~ fs') => PList fs -> Proxy x -> PList fs'
instance On '[] x where
  (<|) _ _ = PNull
instance On xs k => On (x ': xs) k where
  (<|) (PCons pf hs) pk =
    PCons (pf <@> pk) (hs <| pk)

-- higher-kinded proxy application
infixr 6 <@>
(<@>) :: forall f a b. (b ~ f a) => Proxy (f :: k -> k') -> Proxy (a :: k) -> Proxy (b :: k')
(<@>) _ _ = Proxy :: Proxy (f a :: k')

class EmptyDefault (f :: [k] -> *) where
  none :: f '[]

instance EmptyDefault PList where
  none = PNull

only x = x <:> none

infixr 5 <:>
class Build (f :: k -> *) (g :: [k] -> *) where
  (<:>) :: forall (x :: k) (xs :: [k]). f x -> g xs -> g (x ': xs)


instance Build Proxy PList where
  (<:>) = PCons

infixr 3 <++>
class (Appended elems elems' ~ elems'')
    => Append (f :: [k] -> *) (elems :: [k]) (elems' :: [k]) (elems'' :: [k])
  where
    (<++>) :: f elems -> f elems' -> f elems''

instance (Appended '[] xs ~ xs) => Append PList '[] xs xs where
  (<++>) l (PCons x xs) = PCons x (l <++> xs)

instance ( Removed x ys ~ ys
         , Append PList xs ys zs'
         , zs ~ (x ': Appended xs ys)
         , zs ~ (z ': zs')
         )
    => Append PList (x ': xs) ys zs
  where
    (<++>) (PCons (x :: Proxy x) xs) ys = x <:> (xs <++> ys)

-- Note this is just a synonym for <++> with a lower precedence; it is expected to be used to
-- conjoin two root-level APIs after all other combinators have been applied to the two subtrees.
-- This is useful when combining two disparate APIs that are  possibly written and defined in
-- separate libraries.
infixr 1 <||>
(<||>) :: Append f xs ys zs => f xs -> f ys -> f zs
(<||>) = (<++>)

