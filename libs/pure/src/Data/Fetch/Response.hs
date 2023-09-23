{-# language PatternSynonyms, ViewPatterns, ScopedTypeVariables, RankNTypes, FlexibleContexts, AllowAmbiguousTypes #-}
module Data.Fetch.Response where

import Control.Exception
import Data.View
import Data.Txt
import qualified Data.JSON as JSON

pattern GET = "GET"
pattern POST = "POST"
pattern PATCH = "PATCH"
pattern PUT = "PUT"
pattern DELETE = "DELETE"

pattern Good :: Int -> Int
pattern Good x <- x@(\x -> x < 300 && x >= 200 -> True)

pattern Ok :: Int
pattern Ok = 200

pattern Bad :: Int -> Int
pattern Bad x <- x@(\x -> x < 300 && x >= 200 -> False)

pattern Client :: Int -> Int
pattern Client x <- x@(\x -> x < 500 && x >= 400 -> True)

pattern Server :: Int -> Int
pattern Server x <- x@(\x -> x < 600 && x >= 500 -> True)

pattern JSON :: (JSON.FromJSON a, JSON.ToJSON a) => a -> Txt
pattern JSON a <- (JSON.decode -> Just a) where
  JSON a = JSON.encode a

data Response = Response Int Txt
  deriving Show
instance Exception Response

response :: forall a b. JSON.FromJSON a => (Exists Response => b) -> ((Exists Response, Exists a) => b) -> (Exists Response => b)
response failure success = 
  case it of
    Response (Good _) (JSON.decode -> Just (a :: a)) -> with a success
    r -> with r failure

