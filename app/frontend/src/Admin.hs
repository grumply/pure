module Admin where

import Pure.Auth.Access (basic)
import Pure.Magician hiding (basic)
import Shared

administrative :: Authentication Blog => View -> View
administrative v = guarded @Blog Null (basic @Blog) authenticated
  where 
    authenticated :: Authenticated Blog => View
    authenticated 
      | user @Blog == fromTxt "admin" = v
      | otherwise                     = Null

administration :: Authentication Blog => View
administration = 
  administrative do
    Nav <||>
      [ Ul <||>
        [ Li <||> [ newPost ]
        , Li <||> [ newPage ]
        ]
      ]

newPost :: View
newPost = A <| ref (CreateR PostContext) |> [ "New Post" ]

newPage :: View
newPage = A <| ref (CreateR PageContext) |> [ "New Page" ]

