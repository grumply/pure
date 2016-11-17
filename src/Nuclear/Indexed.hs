module Nuclear.Indexed where

class Indexed a where
  type I a :: *
  type I a = a
  index :: (I a ~ i) => a -> i
  default index :: (I a ~ a) => a -> a
  index = id

instance Indexed ()

