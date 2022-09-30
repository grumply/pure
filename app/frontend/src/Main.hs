module Main where

import Pure.Magician

import Shared

main :: IO ()
main = 
  client @Blog @() "127.0.0.1" 8081 (dispatch ()) do 
    run @Blog @() do
      "Not found."