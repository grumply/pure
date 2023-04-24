module Main where

import Shared

import qualified Pure.Auth as Auth
import Pure.Auth.Data.Token
import Pure.Magician
import Pure.Convoker.Discussion.Simple
import qualified Pure.Media.Library as Media
import qualified Pure.Media.Library.API as Media
import qualified Data.Websocket as WS

main :: IO ()
main =
  with pageProduct do
    with pagePreview do
      with postProduct do
        with postPreview do
          serve @Blog authConfig do
            sync (WS.activate it) do
              Null

authConfig :: UserConfig Blog
authConfig ws sid = (defaultAuthConfig @Blog ws sid)
  { Auth.onTokenChange = \mt -> do
      -- print (isJust mt)
      WS.remove ws (Media.api @Blog)
      maybe def (\(Token (un,_)) -> void (WS.enact ws (Media.library (libraryConfig un)))) mt
      Auth.onTokenChange (defaultAuthConfig @Blog ws sid) mt
  }

libraryConfig :: Username -> Media.Config Blog
libraryConfig un = 
  Media.Config
    { root      = def
    , authorize = \un -> isAdmin @Blog un >>= \isa -> print isa >> pure isa
    , validate  = \f -> do
        now <- time
        x <- Media.media (round 1e7) un now f
        case x of
          Just _ -> print ("Just",fst f) >> pure x
          _      -> print ("Nothing",fst f) >> pure x

    }

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
