module Pure.Media.Library.Data.Media (Link,File,Media(..),media) where

import Pure.Auth.Data.Username

import Data.JSON (ToJSON,FromJSON)
import Data.Marker (markIO,Marker())
import Data.Time (Time)
import Data.Txt as Txt (Txt,toTxt,null,length)
import Data.File (ByteTxt,unsafeByteTxtToTxt)

import GHC.Generics (Generic)

type Link = Txt
type File = (Txt,ByteTxt)

data Media domain = Media
  { owner   :: Username
  , created :: Time
  , path    :: Txt
  } deriving stock (Eq,Ord,Show,Generic)
    deriving anyclass (ToJSON,FromJSON)

media :: Int -> Username -> Time -> File -> IO (Maybe (Media domain))
media maxFileSizeInBytes un tm (nm,cnt)
  | let c = unsafeByteTxtToTxt cnt
  , Txt.null c || Txt.length c > maxFileSizeInBytes
  = pure Nothing

  | otherwise = do
    m <- markIO
    let p = toTxt un <> "/" <> toTxt m
    pure $ 
      Just (Media un tm p)