module Main where

import Admin
import Listing
import Page
import Pure.Magician

import Shared

main :: IO ()
main = inject body (Page.run blog)

blog :: P :=> View
blog = 
  page do
    Main <||> 
      [ header
      , primary
      , sidebar
      , footer
      ]

header :: P => View
header =
  Header <||> 
    [ H1 <||> [ "My Blog" ]
    , administration
    ]

primary :: P => View
primary = Section <||> [ content ]
  where
    content = 
      case current :: Either (SomeRoute Blog) () of
        Left sr | Just p <- withRoute @(CRUL Blog) @Blog sr (pages @Blog) -> p
        _ -> 
          async (Listing.recent @Blog 10 PostContext) do
            listing @Blog Cached True PostContext

sidebar :: P => View
sidebar = async action aside 
  where
    (title,action) = 
      case current :: Either (SomeRoute Blog) () of
        Left sr | Just (ReadR _ post) <- fromSomeRoute sr -> 
          ("Related Posts",Listing.related @Blog 10 PostContext post)
        _ -> 
          ("Recent Posts",Listing.top @Blog 10 PostContext) 

    aside :: Exists [(Name Post,Preview Post)] => View
    aside 
      | [] <- it :: [(Name Post,Preview Post)] = Null
      | otherwise =
        Aside <||>
          [ Header <||> [ H2 <||> [ title ] ]
          , listing @Blog Cached True PostContext
          ]

footer :: P => View
footer =
  Footer <||>
    [ "Made with Haskell."
    ]
