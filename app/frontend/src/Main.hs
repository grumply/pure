module Main where

import Pure.Magician

import Shared

main :: IO ()
main = 
  with pagePreview do
    with pageProduct do
      with postPreview do
        with postProduct do
          client @Blog @() "127.0.0.1" 8081 (dispatch ()) do 
            run @Blog @() do
              "Not found."
