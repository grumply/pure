module Main where

import Pure.Magician

import Shared

main :: IO ()
main =
  run @Blog @() "127.0.0.1" 8081 (dispatch ()) do
    request @Blog Cached (analyticsAPI @Post) (listTopForContext @Post) PostContext do
      txt (length (await :: [(Context Post, Name Post)]))
