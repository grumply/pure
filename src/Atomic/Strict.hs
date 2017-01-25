module Atomic.Strict where

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL

import qualified Data.Text as T
import qualified Data.Text.Lazy as TL

class Strict a b where
  strictify :: a -> b
  lazify :: b -> a

instance Strict BSL.ByteString B.ByteString where
  strictify = BSL.toStrict
  lazify = BSL.fromStrict

instance Strict TL.Text T.Text where
  strictify = TL.toStrict
  lazify = TL.fromStrict
