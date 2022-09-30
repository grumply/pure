{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE IncoherentInstances #-}
module Data.Websocket.API.ProxyList where

import Data.Proxy

-- Utilities to build lists of proxies that have shared shape.
-- I've generally found most of this to not be especially useful
-- and I stick with the simple <:> and <++>.

data PList_ (f :: k -> *) (elems :: [k])
  where

    PNull
      :: PList_ f '[]

    PCons
      :: f elem
      -> PList_ f elems
      -> PList_ f (elem ': elems)

type PList = PList_ Proxy

data Nat where
  Z :: Nat
  S :: Nat -> Nat

data Index (n :: Nat) where
  Index :: Index n

type family Appended (xs :: [k]) (ys :: [k]) where
  Appended '[] ys = ys
  Appended xs '[] = xs
  Appended (x ': xs) ys = x ': (Appended xs ys)

type family Offset (xs :: [k]) (x :: k) :: Nat where
  Offset (x ': xs) x = 'Z
  Offset (any ': xs) x = 'S (Offset xs x)

type family Removed xs x where
  Removed '[] x = '[]
  Removed (x ': xs) x = xs
  Removed (any ': xs) x = any ': Removed xs x

type family And (x :: Bool) (y :: Bool) :: Bool where
  And 'True 'True = 'True
  And x y = 'False

type family Or (x :: Bool) (y :: Bool) :: Bool where
  Or 'True y = 'True
  Or x 'True = 'True
  Or x y = 'False

type family (x :: k) === (y :: k) :: Bool where
  x === x = 'True
  x === y = 'False

type family xs ∪ ys where
    '[] ∪ ys = ys
    '[x] ∪ (x ': xs) = x ': xs
    '[x] ∪ (y ': xs) = y ': '[x] ∪ xs
    (x ': xs) ∪ ys = '[x] ∪ (xs ∪ ys)

type family (x :: k) ∈ (xs :: [k]) :: Bool where
    x ∈ '[] = 'False
    x ∈ (x ': xs) = 'True
    x ∈ (y ': xs) = x ∈ xs

type family (x :: k) ≠ (y :: k) :: Bool where
  x ≠ x = 'False
  x ≠ y = 'True

type family (x :: k) ∉ (ys :: [k]) :: Bool where
  x ∉ '[] = 'True
  x ∉ (x ': ys) = 'False
  x ∉ (y ': ys) = x ∉ ys

type family (xs :: [k]) ⊆ (ys :: [k]) :: Bool where
  '[] ⊆ ys = 'True
  (x ': xs) ⊆ ys = And (x ∈ ys) (xs ⊆ ys)

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
(<@>) :: forall f a b. (b ~ f a) => Proxy f -> Proxy a -> Proxy b
(<@>) _ _ = Proxy :: Proxy (f a)

class EmptyDefault (f :: [k] -> *) where
  non :: f '[]

instance EmptyDefault PList where
  non = PNull

infixr 5 <:>
class Build (f :: k -> *) (g :: [k] -> *) | g -> f where
  (<:>) :: forall (x :: k) (xs :: [k]). f x -> g xs -> g (x ': xs)

instance Build Proxy PList where
  (<:>) = PCons

infixr 3 <+++>
class (Appended elems elems' ~ elems'')
    => TListAppend (f :: [k] -> *) (elems :: [k]) (elems' :: [k]) (elems'' :: [k])
  where
    (<+++>) :: f elems -> f elems' -> f elems''

instance TListAppend f '[] '[] '[] where
  (<+++>) _ r = r

instance TListAppend f '[] es es where
  (<+++>) _ r = r

instance TListAppend f es '[] es where
  (<+++>) l _ = l

instance ( Removed (y ': ys) x   ~ (y ': ys)
         , TListAppend PList xs (y ': ys) zs
         , Appended (x ': xs) (y ': ys) ~ (x ': zs)
         )
    => TListAppend PList (x ': xs) (y ': ys) (x ': zs)
  where
    (<+++>) (PCons (x :: Proxy x) xs) ys = x <:> (xs <+++> ys)

-- Note this is just a synonym for <+++> with a lower precedence; it is expected to be used to
-- conjoin two root-level APIs after all other combinators have been applied to the two subtrees.
-- This is useful when combining two disparate APIs that are  possibly written and defined in
-- separate libraries.
infixr 1 <|||>
(<|||>) :: TListAppend f xs ys zs => f xs -> f ys -> f zs
(<|||>) = (<+++>)

