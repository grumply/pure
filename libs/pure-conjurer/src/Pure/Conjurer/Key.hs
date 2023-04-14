module Pure.Conjurer.Key (Key(),newKey) where

import Pure.Conjurer.Fieldable
import Pure.Conjurer.Pathable

import Data.Default
import Data.JSON hiding (Key)
import Data.Marker
import Data.Txt
import Data.View
import Control.Component as Pure 

import Data.Hashable

import System.IO.Unsafe
import GHC.Generics

newtype Key a = Key Marker
  deriving stock Generic
  deriving (ToJSON,FromJSON,ToTxt,FromTxt,Eq,Ord,Hashable) via Marker
type role Key nominal

instance Fieldable (Key a) where
  field _ _ = Data.View.Null

instance Default (Key a) where
  {-# NOINLINE def #-}
  -- This seems like a bad idea. Keys should /always/ be 
  -- created on the server and always through `newKey`.
  def = Key (unsafePerformIO markIO)

instance Pathable (Key a) where
  toPath (Key m) = toPath m
  fromPath = fmap (fmap Key) fromPath

newKey :: IO (Key b)
newKey = Key <$> markIO