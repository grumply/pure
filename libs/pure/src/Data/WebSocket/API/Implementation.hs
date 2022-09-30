{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Websocket.API.Implementation where

import Data.Proxy
import Data.Websocket.Endpoint
import Data.Websocket.API.Interface

data Implementation (hndlr :: * -> *) (es :: [*])
  where
    ImplementationNull
      :: Implementation hndlr '[]

    ImplementationCons
      :: hndlr e
      -> Implementation hndlr es
      -> Implementation hndlr (e ': es)

instance EmptyDefault (Implementation hndlr) where
  non = ImplementationNull

instance EmptyDefault (Interface f) where
  non = InterfaceNull

instance Build (hndlr :: * -> *) (Implementation hndlr :: [*] -> *) where
  (<:>) = ImplementationCons

instance ( Removed (y ': ys) x ~ (y ': ys)
         , Appended (x ': xs) (y ': ys) ~ (x ': zs)
         , TListAppend (Implementation hndlr) xs (y ': ys) zs
         )
    => TListAppend (Implementation hndlr) (x ': xs) (y ': ys) (x ': zs)
  where
    (<+++>) (ImplementationCons x xs) ys = ImplementationCons x (xs <+++> ys)

-- instance es ~ (e ': xs)
class GetHandler' (hndlr :: * -> *) (e :: *) (es :: [*]) (n :: Nat) where
  getHandler' :: Index n -> Implementation hndlr es -> hndlr e

instance {-# OVERLAPPING #-} GetHandler' hndlr e (e ': xs) 'Z where
    getHandler' _ (ImplementationCons h _) = h

instance {-# OVERLAPPABLE #-}
         ( index ~ Offset es e
         , GetHandler' hndlr e es index
         )
    => GetHandler' hndlr e (x ': es) ('S n)
  where
    getHandler' _ (ImplementationCons _ es) =
      let index = Index :: Index index
      in getHandler' index es

class GetHandler hndlr e es where
  getHandler :: Implementation hndlr es
             -> hndlr e

instance ( index ~ Offset es e
         , GetHandler' hndlr e es index
         )
    => GetHandler hndlr e es
  where
    getHandler es =
      let index = Index :: Index index
      in getHandler' index es

class (Removed es e ~ es')
    => DeleteHandler hndlr e es es'
  where
    deleteHandler :: Proxy e
                  -> Implementation hndlr es
                  -> Implementation hndlr es'

instance {-# OVERLAPPING #-}
         (Removed (e ': es) e ~ es)
    => DeleteHandler hndlr e (e ': es) es
  where
  deleteHandler _ (ImplementationCons _ hs) = hs

instance {-# OVERLAPPABLE #-}
         ( DeleteHandler hndlr e es es''
         , Removed (x ': es) e ~ es'
         , es' ~ (x ': es'')
         ) => DeleteHandler hndlr e (x ': es) es' where
  deleteHandler p (ImplementationCons mh mhs) = ImplementationCons mh (deleteHandler p mhs)

