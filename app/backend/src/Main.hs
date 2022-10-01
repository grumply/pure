module Main where

import Shared
import Markdown

import Pure.Magician hiding (render)
import Pure.Convoker.Discussion.Simple

main :: IO ()
main =
  with pageProduct do
    with pagePreview do
      with postProduct do
        with postPreview do
          serve @Blog defaultUserConfig do
            sync (activate ask) do
              Null

instance Processable Post
instance Amendable Post
instance Producible Post where
  produce _ _ Post {..} _ = 
    pure PostProduct
      { title    = render title
      , content  = render content
      }
instance Previewable Post where
  preview _ _ Post { synopsis } PostProduct { title } = 
    pure PostPreview
      { synopsis = render synopsis
      , ..
      }

instance Processable Page
instance Amendable Page
instance Producible Page where
  produce _ _ Page {..} _ = 
    pure PageProduct
      { content = render content
      }
instance Previewable Page where
  preview _ _ Page { title } _ = 
    pure PagePreview
      { title = render title
      }
