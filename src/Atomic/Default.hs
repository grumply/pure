module Atomic.Default where

import Ef.Base

class Default a where
  def :: a

instance {-# OVERLAPPABLE #-} (a ~ (), Monad m) => Default (m a) where
  def = return ()

instance {-# OVERLAPS #-} (a ~ (), Monad m) => Default (x -> m a) where
  def _ = return ()

instance {-# OVERLAPS #-} (a ~ (), Monad m) => Default (x -> y -> m a) where
  def _ _ = return ()

instance (Monad c) => Default (Callback_ status result c) where
  def = Callback def def def
