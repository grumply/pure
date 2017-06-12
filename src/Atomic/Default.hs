module Atomic.Default where

import Ef.Base

class Default a where
  def :: a

instance (Monad c) => Default (Callback_ status result c) where
  def = Callback (\_ -> return ()) (\_ -> return ()) (\_ -> return ())
