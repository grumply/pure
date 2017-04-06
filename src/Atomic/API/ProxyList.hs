{-# language FunctionalDependencies #-}
{-# language UndecidableInstances #-}
module Atomic.API.ProxyList where

import Data.Proxy

import Ef.Type.List

-- Utilities to build lists of proxies that have shared shape.

data PList_ (f :: k -> *) (elems :: [k])
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

infixr 2 >:
class Ap (f :: k -> k') (ys :: [k]) where
  (>:) :: ((f |$| ys) ~ ys') => Proxy f -> PList ys -> PList ys'
instance Ap f '[] where
  (>:) _ _ = PNull
instance (Ap f ys) => Ap f (y ': ys) where
  (>:) pf (PCons ph hs') =
      PCons (pf <@> ph) (pf >: hs')

infixl 4 <:
class On (fs :: [k -> k']) (x :: k) where
  (<:) :: ((fs |&| x) ~ fs') => PList fs -> Proxy x -> PList fs'
instance On '[] x where
  (<:) _ _ = PNull
instance On xs k => On (x ': xs) k where
  (<:) (PCons pf hs) pk =
    PCons (pf <@> pk) (hs <: pk)

-- higher-kinded proxy application
infixl 6 <@>
(<@>) :: forall f a b. (b ~ f a) => Proxy (f :: k -> k') -> Proxy (a :: k) -> Proxy (b :: k')
(<@>) _ _ = Proxy :: Proxy (f a :: k')

class EmptyDefault (f :: [k] -> *) where
  none :: f '[]

instance EmptyDefault PList where
  none = PNull

infixr 5 <:>
class Build (f :: k -> *) (g :: [k] -> *) | g -> f where
  (<:>) :: forall (x :: k) (xs :: [k]). f x -> g xs -> g (x ': xs)

instance Build Proxy PList where
  (<:>) = PCons

infixr 3 <++>
class (Appended elems elems' ~ elems'')
    => TListAppend (f :: [k] -> *) (elems :: [k]) (elems' :: [k]) (elems'' :: [k])
  where
    (<++>) :: f elems -> f elems' -> f elems''

instance (Appended es '[] ~ es) => TListAppend f es '[] es where
  (<++>) l _ = l

instance (Appended '[] es ~ es) => TListAppend f '[] es es where
  (<++>) _ r = r

-- instance (Appended '[] xs ~ xs) => Append PList '[] xs xs where
--   (<++>) l (PCons x xs) = PCons x (l <++> xs)

instance ( Removed (y ': ys) x   ~ (y ': ys)
         , TListAppend PList xs (y ': ys) zs
         , Appended (x ': xs) (y ': ys) ~ (x ': zs)
         )
    => TListAppend PList (x ': xs) (y ': ys) (x ': zs)
  where
    (<++>) (PCons (x :: Proxy x) xs) ys = x <:> (xs <++> ys)

-- Note this is just a synonym for <++> with a lower precedence; it is expected to be used to
-- conjoin two root-level APIs after all other combinators have been applied to the two subtrees.
-- This is useful when combining two disparate APIs that are  possibly written and defined in
-- separate libraries.
infixr 1 <||>
(<||>) :: TListAppend f xs ys zs => f xs -> f ys -> f zs
(<||>) = (<++>)

