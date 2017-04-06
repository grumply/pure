{-# language UndecidableInstances #-}
module Atomic.Identify where

class Identify a where
  type I a :: *
  type I a = a
  identify :: (I a ~ i) => a -> i
  default identify :: (I a ~ a) => a -> a
  identify = id

instance Identify ()

instance {-# OVERLAPPABLE #-} Identify a => Identify [a] where
  type I [a] = [I a]
  identify = map identify

-- Making the first element of the tuple into the index so that
-- the default fmap works over the non-identifying element
instance {-# OVERLAPPABLE #-} Identify (a,b) where
  type I (a,b) = a
  identify = fst

instance {-# OVERLAPPABLE #-} Identify (a,b,c) where
  type I (a,b,c) = a
  identify (a,_,_) = a

instance {-# OVERLAPPABLE #-} Identify (a,b,c,d) where
  type I (a,b,c,d) = a
  identify (a,_,_,_) = a

instance {-# OVERLAPPABLE #-} Identify (a,b,c,d,e) where
  type I (a,b,c,d,e) = a
  identify (a,_,_,_,_) = a

instance {-# OVERLAPPABLE #-} Identify (a,b,c,d,e,f) where
  type I (a,b,c,d,e,f) = a
  identify (a,_,_,_,_,_) = a

instance {-# OVERLAPPABLE #-} Identify (a,b,c,d,e,f,g) where
  type I (a,b,c,d,e,f,g) = a
  identify (a,_,_,_,_,_,_) = a

instance {-# OVERLAPPABLE #-} Identify (a,b,c,d,e,f,g,h) where
  type I (a,b,c,d,e,f,g,h) = a
  identify (a,_,_,_,_,_,_,_) = a
