module Shared where

import Pure.Magician
import Pure.Convoker.Discussion.Shared.Markdown
import Data.DOM

data Blog
type instance Resources Blog = '[Post,Page]
instance Client Blog
instance Server Blog where
  type Caches Blog = Resources Blog
  type Statics Blog = Resources Blog
  type Discussions Blog = '[Post]
  type Analyze Blog = '[Post]

data Post
data instance Resource Post = Post
  { title    :: Markdown
  , synopsis :: Markdown
  , content  :: Markdown
  } deriving stock (Generic,Eq,Ord)
    deriving anyclass (ToJSON,FromJSON,Default)

data instance Context Post = PostContext
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (ToJSON,FromJSON,Pathable,Hashable)

newtype instance Name Post = PostName (Slug Post)
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (ToJSON,FromJSON,Pathable,Hashable)

instance Rootable Post
instance Routable Post

instance Nameable Post where
  toName Post {..} = PostName (fromTxt (toTxt title))

instance Ownable Post where
  isOwner un _ _ = isAdmin @Blog un

data instance Product Post = PostProduct
  { name     :: Name Post
  , title    :: [View]
  , content  :: [View]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data instance Preview Post = PostPreview
  { title    :: [View]
  , synopsis :: [View]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data Page
data instance Resource Page = Page
  { title   :: Markdown
  , content :: Markdown
  } deriving stock (Generic,Eq,Ord)
    deriving anyclass (ToJSON,FromJSON,Default)

data instance Context Page = PageContext
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (ToJSON,FromJSON,Pathable,Hashable)

newtype instance Name Page = PageName (Slug Page)
  deriving stock (Generic,Eq,Ord)
  deriving anyclass (ToJSON,FromJSON,Pathable,Hashable)

instance Rootable Page
instance Routable Page

instance Nameable Page where
  toName Page {..} = PageName (fromTxt (toTxt title))

instance Ownable Page where
  isOwner un _ _ = isAdmin @Blog un

newtype instance Product Page = PageProduct
  { content :: [View]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

newtype instance Preview Page = PagePreview
  { title :: [View]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

instance Fieldable Markdown where
  field onchange initial = 
    Textarea <| OnMounted (\n -> focusNode n >> pure def) . OnInput (withInput (onchange . fromTxt)) . Width (80ch) . Height (40ch) |>
      [ txt initial ]

pageProduct :: Product Page -> View
pageProduct PageProduct {..} =
  Main <||> 
    [ Section <||> content
    ]

pagePreview :: Preview Page -> View
pagePreview PagePreview {..} =
  Article <||>
    [ Header <||> title 
    ]

postProduct :: Product Post -> View
postProduct PostProduct {..} =
  Main <||>
    [ Header <||> title
    , Section <||> content
    ]

postPreview :: Preview Post -> View
postPreview PostPreview {..} =
  Article <||>
    [ Header <||> title
    , Section <||> synopsis
    ]
