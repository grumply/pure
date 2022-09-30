module Pure.Conjurer.Pathable where

import Pure.Auth
import Data.Txt as Txt
import Data.Marker
import Data.Router

import Data.Typeable
import GHC.Generics as G
import GHC.TypeLits (ErrorMessage(..),TypeError(..))
import Text.Read

class Pathable a where
  toPath :: a -> Txt
  default toPath :: (Generic a, GPathable (Rep a)) => a -> Txt
  toPath = gtoPath . G.from

  fromPath :: Routing x (Maybe a)
  default fromPath :: (Generic a, GPathable (Rep a)) => Routing x (Maybe a)
  fromPath = fmap (fmap (G.to :: Rep a x -> a)) gfromPath

instance Pathable Marker where
  toPath m = "/" <> toTxt m
  fromPath = path' "/:marker" "marker"

-- Txt is not safely pathable because Pathable is used
-- to generate FilePath. 
instance 
  ( TypeError (GHC.TypeLits.Text "Txt is not safely Pathable. Use Pure.Conjurer.Slug.")
  ) => Pathable Txt 
  where
    toPath = error "unreachable"
    fromPath = error "unreachable"

-- String is not safely pathable because Pathable is used
-- to generate FilePath.
instance 
  ( TypeError (GHC.TypeLits.Text "String is not safely Pathable. Use Pure.Conjurer.Slug.")
  ) => Pathable String 
  where
    toPath = error "unreachable"
    fromPath = error "unreachable"

instance Pathable () where
  toPath _ = ""
  fromPath = pure (Just ())

instance Pathable Username where
  toPath un = "/" <> toTxt un
  fromPath = path' "/:username" "username"

instance Pathable Int where
  toPath i = "/" <> toTxt i
  fromPath = path' "/:int" ("int" >>= maybe continue pure . (readMaybe @Int))

instance Pathable Integer where
  toPath i = "/" <> Txt.take 255 (toTxt i)
  fromPath = path' "/:int" ("int" >>= maybe continue pure . (readMaybe @Integer . fromTxt . Txt.take 255))

instance Pathable a => Pathable (Maybe a) where
  toPath = maybe "" toPath
  fromPath = do
    ma <- fromPath
    case ma of
      Nothing -> pure (Just Nothing)
      Just a  -> pure (Just (Just a))

instance (Pathable a, Pathable b) => Pathable (Either a b) where
  toPath = either toPath toPath
  fromPath = do
    ma <- fromPath
    case ma of
      Just a  -> pure (Just (Left a))
      Nothing -> do
        mb <- fromPath 
        case mb of
          Just b  -> pure (Just (Right b))
          Nothing -> pure Nothing

class GPathable f where
  gtoPath :: f a -> Txt
  gfromPath :: Routing x (Maybe (f a))

instance GPathable V1 where
  gtoPath _ = ""
  gfromPath = 
    let err = "GPathable V1 => gfromPath: tried to materialize a void type."
    in pure (Just (error err))

instance GPathable U1 where
  gtoPath _ = ""
  gfromPath = pure (Just U1)

instance GPathable x => GPathable (M1 r m x) where
  gtoPath (M1 x) = gtoPath x
  gfromPath = fmap (fmap M1) gfromPath

instance Pathable x => GPathable (K1 r x) where
  gtoPath (K1 x) = toPath x
  gfromPath = fmap (fmap K1) fromPath

instance 
  ( Typeable a, Typeable b
  , GPathable a, GPathable b
  ) => GPathable ((:*:) a b) 
  where
    gtoPath (a :*: b) = gtoPath a <> gtoPath b
    gfromPath = do
      ma <- gfromPath 
      mb <- gfromPath
      pure ((:*:) <$> ma <*> mb)