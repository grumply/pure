{-# language RankNTypes, BlockArguments, DefaultSignatures, TypeApplications, ScopedTypeVariables, TypeFamilies, FlexibleContexts, GADTs #-}
module Data.Sorcerer.Streamable where

import Data.Sorcerer.Aggregable
import Data.Sorcerer.JSON

import qualified Data.ByteString.Lazy.Char8 as BSLC
import qualified Data.ByteString.Lazy as BSL
import Data.Hashable
import System.Directory

import Control.Monad.IO.Class
import Data.List as List
import Data.Maybe
import Data.Typeable

class Streamable ev where
  data Stream ev

  stream :: Stream ev -> FilePath
  default stream :: (Typeable ev,Hashable (Stream ev)) => Stream ev -> FilePath
  stream sev = show (abs (hash (typeOf (undefined :: ev)))) <> "/" <> show (abs (hash sev)) <> ".stream"

  batch :: Stream ev -> Int
  batch _ = 1

{-# INLINE events #-}
events :: forall ev m. (MonadIO m, Streamable ev, FromJSON ev) => Int -> Stream ev -> m [ev]
events n s = liftIO do
  let fp = stream s
  fe <- doesFileExist fp
  if fe then do
    cnts <- BSLC.readFile fp
    pure $ fmap snd $ catMaybes $ fmap (decode_ @(Int,ev)) $ List.drop (n + 1) $ BSLC.lines cnts
  else
    pure []

data Event 
  = forall ev. (Typeable ev, ToJSON ev) => Write
    { event :: ev
    }
  | forall ag. (Typeable ag) => Read
    { callback :: Int -> Maybe ag -> IO ()
    }
  | forall ev ag. (Typeable ev, Typeable ag, ToJSON ev) => Transact
    { event   :: ev
    , inspect :: Maybe ag -> Maybe (Maybe ag) -> IO ()
    }

