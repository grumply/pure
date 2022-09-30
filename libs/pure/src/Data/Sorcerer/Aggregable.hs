{-# language PatternSynonyms, DefaultSignatures, MultiParamTypeClasses, AllowAmbiguousTypes, ScopedTypeVariables #-}
module Data.Sorcerer.Aggregable where

import Data.Hashable

import Data.Typeable
import System.FilePath

pattern Update :: a -> Maybe (Maybe a)
pattern Update a = Just (Just a)

pattern Delete :: Maybe (Maybe a)
pattern Delete = Just Nothing

pattern Ignore :: Maybe (Maybe a)
pattern Ignore = Nothing

class Aggregable ev ag where
  aggregate :: FilePath
  default aggregate :: Typeable ag => FilePath
  aggregate = show (abs (hash (typeOf (undefined :: ag)))) <.> "aggregate"

  update :: ev -> Maybe ag -> Maybe (Maybe ag)
  update _ _ = Ignore
