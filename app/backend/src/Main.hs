module Main where

import Shared

import Pure.Magician
import Pure.Convoker.Discussion.Simple

main :: IO ()
main =
  with pageProduct do
    with pagePreview do
      with postProduct do
        with postPreview do
          serve @Blog defaultAuthConfig do
            sync (activate ask) do
              Null


instance Processable Post
instance Amendable Post
instance Producible Post where
  produce _ name Post {..} _ = 
    pure PostProduct
      { title    = parseMarkdown title
      , content  = parseMarkdown content
      , ..
      }
instance Previewable Post where
  preview _ _ Post { synopsis } PostProduct { title } = 
    pure PostPreview
      { synopsis = parseMarkdown synopsis
      , ..
      }

instance Processable Page
instance Amendable Page
instance Producible Page where
  produce _ _ Page {..} _ = 
    pure PageProduct
      { content = parseMarkdown content
      }
instance Previewable Page where
  preview _ _ Page { title } _ = 
    pure PagePreview
      { title = parseMarkdown title
      }
