{-# language DeriveAnyClass, DerivingStrategies, MultiParamTypeClasses, TypeFamilies, DeriveGeneric, AllowAmbiguousTypes, TypeApplications, OverloadedStrings, ScopedTypeVariables #-}
module Pure.Media.Library.Data.Library where

import Pure.Media.Library.Data.Media (Media)
import Pure.Auth.Data.Username

import Data.Txt as Txt
import Data.JSON (ToJSON,FromJSON)

import Data.Sorcerer

import Data.Hashable (Hashable)

import Data.Char
import Data.List as List (filter,length)
import Data.Typeable
import GHC.Generics (Generic)

data Library domain = Library
  { library :: [Media domain]
  } deriving stock Generic
    deriving anyclass (ToJSON,FromJSON)

data LibraryMsg domain
  = CreateMedia (Media domain)
  | DeleteMedia (Media domain)
  deriving stock Generic
  deriving anyclass (ToJSON,FromJSON)

instance Typeable domain => Streamable (LibraryMsg domain) where
  data Stream (LibraryMsg domain) = LibraryStream Username
    deriving stock (Generic,Eq,Ord)
    deriving anyclass Hashable
    
  stream (LibraryStream lib) =
    fromTxt ("media/" <> rep @domain <> "/" <> toTxt lib <> ".stream")

instance Typeable domain => Aggregable (LibraryMsg domain) (Library domain) where
  update (CreateMedia m) Nothing =
    Update (Library [m])

  update (CreateMedia m) (Just l) | [] <- List.filter (==m) (library l) =
    Update l { library = library l ++ [m] }

  update (DeleteMedia m) (Just l) | (_:_) <- List.filter (==m) (library l) = 
    Update l { library = List.filter (/= m) (library l) }

  update _ _ =
    Ignore

  aggregate =
    "library.aggregate"

rep :: forall p. (Typeable p) => Txt
rep = Txt.map limit $ go (typeRep (Proxy :: Proxy p))
  where
    limit c | isAscii c && isAlphaNum c = c | otherwise = '_'
    go tr =
      let tc = toTxt (show (typeRepTyCon tr))
          trs = typeRepArgs tr
      in Txt.intercalate "_" (tc : fmap go trs)