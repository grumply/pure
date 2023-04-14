module Main where

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

import Shared

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  inject body do
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
          async (Listing.recent @Blog 10 PostContext) do
            with postPreview do
              listing @Blog @Post Cached True

post :: App Blog () => Bool -> Product Post -> View
post showComments PostProduct {..} =
  Main <||>
    [ Header <||> title
    , Section <||> content
    , relatedPosts name
    , if showComments then
        with (Root @Blog @Post Nothing) do
          with PostContext do
            with name do
              with (txt @Username) do
                simpleThreadedDiscussion @Blog @Post Nothing
                
      else
        Null
    ]

relatedPosts :: App Blog () => Name Post -> View
relatedPosts name = async action aside
  where
    action = Listing.related @Blog 3 PostContext name

    aside :: Exists [(Context Post,Name Post,Preview Post)] => View
    aside
      | [] <- it :: [(Context Post,Name Post,Preview Post)] = Null
      | otherwise =
        Aside <||>
          [ Header <||> [ H2 <||> [ "Related Posts" ] ]
          , with postPreview (listing @Blog @Post Cached True)
          ]

sidebar :: App Blog () => View
sidebar = async action aside
  where
    (title,action) =
      case current :: Either (SomeRoute Blog) () of
        Left sr | Just (ReadR _ post) <- fromSomeRoute sr ->
          ("Related Posts",Listing.related @Blog 10 PostContext post)
        _ ->
          ("Recent Posts",Listing.top @Blog 10 PostContext)

    aside :: Exists [(Context Post,Name Post,Preview Post)] => View
    aside
      | [] <- it :: [(Context Post,Name Post,Preview Post)] = Null
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

