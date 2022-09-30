module Shared where

import Pure.Magician

data Blog
type instance Resources Blog = '[Post,Page]
instance Client Blog
instance Server Blog where
  type Caches Blog = Resources Blog
  type Statics Blog = Resources Blog
  type Discussions Blog = '[Post]
  type Analyze Blog = '[Post]

newtype Markdown = Markdown Txt
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)
  deriving (ToTxt,FromTxt,Default,Ord,Eq) via Txt

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

instance Routable Post

instance Nameable Post where
  toName Post {..} = PostName (fromTxt (toTxt title))

instance Ownable Post where
  isOwner un _ _ = isAdmin @Blog un

data instance Product Post = PostProduct
  { title    :: [View]
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
    Textarea <| OnInput (withInput (onchange . fromTxt)) |>
      [ txt initial ]

instance Pure (Product Page) where
  view PageProduct {..} = 
    Article <||> 
      [ Section <||> content
      ]

instance Pure (Preview Page) where
  view PagePreview {..} =
    Article <||>
      [ Section <||> title 
      ]

instance Pure (Product Post) where
  view PostProduct {..} =
    Article <||>
      [ Header <||> title
      , Section <||> content
      ]

instance Pure (Preview Post) where
  view PostPreview {..} =
    Article <||>
      [ Section <||> title
      ]
