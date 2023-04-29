{-# language UndecidableInstances #-}
module Main where

import Control.Log
import Data.DOM
import Effect.Stream
import Admin
import Listing
import Pure.Magician
import Pure.Convoker.Discussion
import Pure.Convoker.Discussion.Shared.Markdown
import Pure.Convoker.Discussion.Threaded
import Pure.Convoker.Discussion.Simple
import Pure.Convoker.Discussion.Simple.Meta
import Pure.Convoker.Discussion.Simple.Comment
import Pure.Convoker.Discussion.Simple.Threaded
import Pure.Media.Library.Browser
import Pure.Media.Library as Media

import Shared

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  inject body do
    logger pPrinter do
      client @Blog @() "127.0.0.1" 8081 (dispatch ()) do
        Main <| Themed @Blog |>
          [ header
          , primary
          , sidebar
          , footer
          ]

header :: App Blog () => View
header =
  Header <||>
    [ H1 <||> [ A <| lref "/" |> [ "My Blog"] ]
    , administration
    ]

primary :: App Blog () => View
primary = Section <||> [ content ]
  where
    content =
      case current :: Either (SomeRoute Blog) () of
        Left sr
          | Just (ReadR PostContext nm) <- fromSomeRoute sr ->
            async (req @Blog Cached (readingAPI @Post) (readProduct @Post) (PostContext,nm)) do
              maybe "Post not found." (post True) await
          | Just p <-
            with pagePreview do
              with postPreview do
                with (post False) do 
                  with pageProduct do
                    withRoute @(CURL Blog) @Blog sr (pages @Blog) -> p
        _ ->
          async (Listing.recent @Blog 8 PostContext) do
            with postPreview do
              listing @Blog @Post Cached True

post :: App Blog () => Bool -> Product Post -> View
post showComments PostProduct {..} =
  Article <||>
    [ Header <||> title
    , Section <||> content
    , if showComments then
        with (Root @Blog @Post Nothing) do
          with PostContext do
            with name do
              with (txt @Username) do
                simpleThreadedDiscussion @Blog @Post Nothing
                
      else
        Null
    ]

sidebar :: App Blog () => View
sidebar = async action aside
  where
    (title,action) =
      case current :: Either (SomeRoute Blog) () of
        Left sr | Just (ReadR _ post) <- fromSomeRoute sr ->
          ("Related Posts",Listing.related @Blog 3 PostContext post)
        _ ->
          ("Recent Posts",Listing.top @Blog 5 PostContext)

    aside :: Exists [(Context Post,Name Post,Preview Post)] => View
    aside
      | [] <- await :: [(Context Post,Name Post,Preview Post)] = Null
      | otherwise =
        Aside <||>
          [ Header <||> [ H2 <||> [ title ] ]
          , with postPreview do
              listing @Blog @Post Cached True
          ]

footer :: View
footer =
  Footer <||>
    [ "Made with Haskell."
    ]

instance Theme Blog where
  theme c = do
    is (subtheme @Creating) do
      child (tag H2) do
        display =: none
      has (tag Label) do
        display =: block

data Browser = Browser
  { selection :: (Int,Int)
  , open :: Bool
  , node :: Node
  }

toggle :: Modify Browser => IO ()  
toggle = modifyIO \Browser {..} ->
  if open then 
    pure Browser 
      { open = False
      , selection = (0,0)
      , .. 
      }
  else do
    selection <- getSelection node 
    pure Browser 
      { open = True
      , .. 
      }

setBrowserNode :: Modify Browser => Node -> IO ()
setBrowserNode n = modify \Browser {..} -> Browser { node = n, .. }

insert :: Modify Browser => Txt -> IO ()
insert path = modifyIO \Browser { selection = (start,end), .. } -> do
  replaceTextInRange node start end ("[](" <> path <> ")")
  setSelection node (start + 1) (start + 1)
  pure Browser 
    { selection = (start + 1,start + 1)
    , .. 
    }

browsing :: Exists Browser => Bool
browsing = let Browser { open } = it in open

instance (Authentication Blog, Websocket Blog) => Fieldable Markdown where
  field onchange initial = 
    guarded @Blog def def do
      sync (isAdmin @Blog (user @Blog)) do
        if it then
          browser @Blog do
            state (Browser (0,0) False (coerce nullJSV)) do
              Div <||>
                [ Button <| OnClick (\_ -> toggle) . Display block |> [ if browsing then "Cancel" else "Add Media" ]
                , Div <| off browsing (Display none) |> [ input ]
                , Div <||> 
                  [ Img <| OnClick (\_ -> insert path) . Src path
                  | Media.Media {..} <- loaded @(Media.Media Blog)
                  ]
                , Textarea <| on browsing (Display none) . OnMounted (\n -> setBrowserNode n >> pure def) . OnInput (withInput (onchange . fromTxt)) . Width (80ch) . Height (40ch) |>
                  [ txt initial ]
                ]
        else 
          Textarea <| OnInput (withInput (onchange . fromTxt)) . Width (80ch) . Height (40ch) |>
            [ txt initial ]

