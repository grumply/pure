{-# LANGUAGE DerivingVia, KindSignatures, DataKinds, RoleAnnotations, DeriveGeneric, DeriveAnyClass, FlexibleContexts, RankNTypes, AllowAmbiguousTypes, ScopedTypeVariables, ConstraintKinds #-}
module Pure.Auth.Data where

import Data.JSON hiding (Key)
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Char
import Data.Exists
import Data.Hashable
import Data.List as List
import Data.String
import Data.Time
import Data.Txt as Txt
import GHC.Generics
import GHC.TypeLits

newtype Hash (rounds :: Nat) hashOf = Hash Txt
  deriving (ToJSON,FromJSON,ToTxt,FromTxt,Show,Eq,Ord) via Txt

type role Hash nominal nominal

newtype Password = Password Txt
  deriving (ToJSON,FromJSON,ToTxt,FromTxt,Show,Eq,Ord) via Txt

newtype Username c = Username Txt
  deriving (ToJSON,ToTxt,Show,Eq,Ord,Hashable) via Txt

type role Username nominal

instance FromTxt (Username c) where
  fromTxt = Username . Txt.toLower . Txt.filter isPrint

instance FromJSON (Username c) where
  parseJSON = fmap fromTxt . parseJSON 

instance IsString (Username c) where
  fromString = fromTxt . fromString

newtype Key = Key Txt
  deriving (ToJSON,FromJSON,ToTxt,FromTxt,Show,Eq,Ord) via Txt

newtype Email = Emal Txt
  deriving (ToJSON,FromJSON,ToTxt,FromTxt,Show,Eq,Ord) via Txt

data Token (c :: *) = Token
  { owner   :: Username c
  , expires :: Time
  , claims  :: [(Txt,Txt)]
  , proof   :: Txt
  } deriving stock (Generic,Eq,Ord)
    deriving anyclass (ToJSON,FromJSON)
type role Token nominal

newtype Secret_ c = Secret BS.ByteString deriving Show
instance ToJSON (Secret_ c) where toJSON (Secret sec) = toJSON (show sec)
instance FromJSON (Secret_ c) where parseJSON v = Secret . read <$> parseJSON v
type role Secret_ nominal
type Secret c = Exists (Secret_ c)

newtype Pool_ c = Pool FilePath
type role Pool_ nominal
type Pool c = Exists (Pool_ c)

newtype Proofs_ c = Proofs (Token c)
type role Proofs_ nominal
type Proofs c = Exists (Proofs_ c)


