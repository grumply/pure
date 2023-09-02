{-# LANGUAGE CPP, RecordWildCards, OverloadedStrings, MultiWayIf, FlexibleInstances, ViewPatterns, TemplateHaskell #-}
module Data.View.Render where

import Data.View.Build
import Control.Applicative
import Control.Concurrent
import Control.Monad
import Data.Default
import Data.DOM
import Data.Foldable
import Data.IORef
import Data.JSON
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import Data.Monoid
import Data.Proxy
import qualified Data.Set as Set
import Data.Txt (Txt,ToTxt(..),FromTxt(..))
import qualified Data.Txt as Txt
import Data.Typeable
import Data.View
import System.IO.Unsafe
import Unsafe.Coerce

#ifdef USE_TEMPLATE_HASKELL
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
#endif

newtype StaticHTML = StaticHTML { htmlText :: Txt } deriving (Eq,Ord)

instance ToTxt StaticHTML where
  toTxt (StaticHTML htmlt) = htmlt

instance FromTxt StaticHTML where
  fromTxt = StaticHTML

instance Monoid StaticHTML where
  mempty = fromTxt mempty
  mappend htmlt1 htmlt2 = fromTxt $ toTxt htmlt1 <> toTxt htmlt2

instance Semigroup StaticHTML where
  (<>) htmlt1 htmlt2 = fromTxt $ toTxt htmlt1 <> toTxt htmlt2

#ifdef USE_TEMPLATE_HASKELL
instance Lift StaticHTML where
  lift (StaticHTML (Txt.unpack -> htmls)) = [| StaticHTML (Txt.pack htmls) |]
#endif

staticHTML :: View -> StaticHTML
staticHTML = fromTxt . toTxt

shtml :: Txt -> Features -> StaticHTML -> View
shtml tag features = RawView Nothing tag features . toTxt

selfClosing tag = tag `elem` selfclosing
  where
    selfclosing =
      ["area","base","br","col","frame","command"
      ,"embed","hr","img","input","keygen","link"
      ,"meta","param","source","track","wbr"
      ]

cleanFeatures :: Features -> Features
cleanFeatures Features_ {..} = Features_
  { classes    = Set.delete "" classes
  , styles     = Map.delete "" styles
  , attributes = Map.delete "" attributes
  , properties = Map.delete "" properties
  , listeners  = []
  , lifecycles = []
  }

instance ToJSON Features where
  toJSON (cleanFeatures -> f) =
      object
        ( (if Set.null (classes f)    then [] else ["c" .= toJSON (Set.toList $ classes    f)]) <>
          (if Map.null (styles f)     then [] else ["s" .= toJSON (Map.toList $ styles     f)]) <>
          (if Map.null (attributes f) then [] else ["a" .= toJSON (Map.toList $ attributes f)]) <>
          (if Map.null (properties f) then [] else ["p" .= toJSON (Map.toList $ properties f)])
        )

instance FromJSON Features where
  parseJSON o0 = do
    flip (withObject "Features") o0 $ \o -> do
      classes    <- Set.fromList <$> ((o .: "c") <|> pure mempty)
      styles     <- Map.fromList <$> ((o .: "s") <|> pure mempty)
      attributes <- Map.fromList <$> ((o .: "a") <|> pure mempty)
      properties <- Map.fromList <$> ((o .: "p") <|> pure mempty)
      let listeners = []; lifecycles = []
      return Features_ {..}

renderer :: (View,MVar View) -> View
renderer = Component go
  where
    go self = def
      { onConstruct = do
          (v,_) <- askref self
          return v
      , onMounted = do
          v <- lookref self
          (_,mv) <- askref self
          putMVar mv v
          modifyref_ self $ \_ _ -> NullView Nothing
      , render = const id
      }

-- ToJSON for View currently doesn't handle Portals.
-- To do so, we would need the option to encode the
-- destination as a path selector....
instance ToJSON View where
  toJSON a =
      go a
    where
      go cv@ComponentView { record = r, ..} =
        case r of
          Nothing -> do
            go $ unsafePerformIO $ do
              e <- create "div"
              mv <- newEmptyMVar
              inject e (renderer (cv,mv))
              takeMVar mv

          Just ref ->
            go (unsafePerformIO (readIORef (crView ref)))

      go (TaggedView _ v) = go v

      go (TextView _ s) =
        object (if Txt.null s then [] else [ "s" .= s ])

      go (RawView _ t fs s) =
        object
          ([ "_" .= ("R" :: Txt), "t" .= t] <>
           ( if nullFeatures fs then [] else [ "f" .= toJSON fs ]) <>
           ( if Txt.null s      then [] else [ "s" .= s ])
          )

      go (KHTMLView _ t fs ks) =
        object
          ([ "_" .= ("KH" :: Txt), "t" .= t] <>
           ( if nullFeatures fs then [] else [ "f" .= toJSON fs ]) <>
           ( if Prelude.null ks then [] else [ "k" .= toJSON ks ])
          )

      go (HTMLView _ t fs cs) =
        object
          ( ["t" .= t] <>
           ( if nullFeatures fs then [] else [ "f" .= toJSON fs ]) <>
           ( if Prelude.null cs then [] else [ "c" .= toJSON cs ])
          )

      go (KSVGView _ t fs xs ks) =
        object
          ([ "_" .= ("KS" :: Txt), "t" .= t] <>
           ( if nullFeatures fs then [] else [ "f" .= toJSON fs ]) <>
           ( if Prelude.null xs then [] else [ "x" .= toJSON (Map.toList xs) ]) <>
           ( if Prelude.null ks then [] else [ "k" .= toJSON ks ])
          )

      go (SVGView _ t fs xs cs) =
        object
          ([ "_" .= ("S" :: Txt), "t" .= t] <>
           ( if nullFeatures fs then [] else [ "f" .= toJSON fs ]) <>
           ( if Prelude.null xs then [] else [ "x" .= toJSON (Map.toList xs) ]) <>
           ( if Prelude.null cs then [] else [ "c" .= toJSON cs ])
          )

      go (ReactiveView _ f) = go f
      go (WeakView _ f) = go f
      
      go (Prebuilt v) = go v

      go _ = object [ "_" .= ("N" :: Txt) ]

instance FromJSON View where
  parseJSON o0 = do
    flip (withObject "View") o0 $ \o -> do
      t <- (o .: "_") <|> pure mempty
      case t :: Txt of
        "" -> 
          (do t  <-  o .: "t"
              fs <- (o .: "f") <|> pure mempty
              cs <- (o .: "c") <|> pure mempty
              pure $ HTMLView Nothing t fs cs
          ) <|> (do s <- (o .: "s") <|> pure mempty
                    pure $ TextView Nothing s
                )
        "R" -> do
          t  <-  o .: "t"
          fs <- (o .: "f") <|> pure mempty
          s  <- (o .: "s") <|> pure mempty
          pure $ RawView Nothing t fs s
        "KH" -> do
          t  <-  o .: "t"
          fs <- (o .: "f") <|> pure mempty
          ks <- (o .: "k") <|> pure mempty
          pure $ KHTMLView Nothing t fs ks
        "KS" -> do
          t  <-  o .: "t"
          fs <- (o .: "f") <|> pure mempty
          xs <- (o .: "x") <|> pure mempty
          ks <- (o .: "k") <|> pure mempty
          pure $ KSVGView Nothing t fs (Map.fromList xs) ks
        "S" -> do
          t  <-  o .: "t"
          fs <- (o .: "f") <|> pure mempty
          xs <- (o .: "x") <|> pure mempty
          cs <- (o .: "c") <|> pure mempty
          pure $ SVGView Nothing t fs (Map.fromList xs) cs
        "N" -> pure $ NullView Nothing

instance Show Features where
  show = fromTxt . toTxt

instance ToTxt Features where
  toTxt Features_ {..} =
    let
        cs    = 
          case Set.toList $ Set.delete "" classes of
            [] -> ""
            xs -> " class='" <> (mconcat $ List.intersperse " " xs) <> "'"

        ss    = 
          case Map.toList $ Map.delete "" styles of
            [] -> ""
            xs -> " style='" <> (mconcat $ List.intersperse ";" $ fmap (\(k,v) -> k <> ":" <> v) $ xs) <> "'"

        attrs = 
          case Map.toList $ Map.delete "" attributes of
            [] -> ""
            xs -> mconcat $ " " : (List.intersperse " " $ fmap (\(k,v) -> k <> "='" <> v <> "'") xs)

        props = 
          case Map.toList $ Map.delete "" properties of
            [] -> ""
            xs -> mconcat $ " " : (List.intersperse " " $ fmap (\(k,v) -> k <> "='" <> v <> "'") xs)

    in
        cs <> ss <> attrs <> props

nullFeatures Features_ {..} =
  styles == mempty
    && classes == mempty
    && attributes == mempty
    && properties == mempty

instance Show View where
  show = go 0
    where
      go n TextView {..} = List.replicate n ' ' <> fromTxt content <> "\n"
      go n RawView {..} = 
        "<" <> fromTxt tag <> show features <> ">" <> fromTxt content <> "</" <> fromTxt tag <> ">"

      go n KHTMLView {..} =
        go n HTMLView { children = fmap snd keyedChildren, ..}

      go n KSVGView {..} =
        let fs = features { attributes = Map.union (attributes features) xlinks }
        in go n HTMLView { features = fs, children = fmap snd keyedChildren, .. }

      go n SVGView {..} =
        let fs = features { attributes = Map.union (attributes features) xlinks }
        in go n HTMLView { features = fs, .. }

      go n HTMLView {..} =
        List.replicate (2 * n) ' ' <>
        "<" <> fromTxt tag <> show features
            <> if | tag == "?xml"     -> "?>\n"
                  | tag == "!doctype" -> ">\n"
                  | selfClosing tag   -> "/>\n"
                  | otherwise         ->
                      ">\n" <> List.concatMap (go (n + 1)) children <>
                       List.replicate (2 * n) ' ' <> "</" <> fromTxt tag <> ">\n"

      go n cv@ComponentView { record = r, ..} =
        case r of
          Nothing ->
            show $ unsafePerformIO $ do
              e <- create "div"
              mv <- newEmptyMVar
              inject e (renderer (cv,mv))
              takeMVar mv

          Just ref -> show $ unsafePerformIO (readIORef (crView ref))

      go n (TaggedView _ v) = go n v

      go n (ReactiveView _ f) = go n f
      go n (WeakView _ f) = go n f
      
      go n (Prebuilt v) = go n v

      go _ _ = ""


instance ToTxt View where
  toTxt TextView {..} = content

  toTxt RawView {..} =
    "<" <> tag <> toTxt features <> ">" <> content <> "</" <> tag <> ">"

  toTxt KHTMLView {..} =
    toTxt HTMLView { children = fmap snd keyedChildren, ..}

  toTxt HTMLView {..} =
    if tag == "?xml" then
      "<?xml"  <> toTxt features <> "?>"
    else
      "<" <> tag <> toTxt features <>
        if selfClosing tag then
          "/>"
        else
          ">" <> mconcat (List.map toTxt children) <> "</" <> tag <> ">"

  toTxt SVGView {..} =
    let fs = features { attributes = Map.union (attributes features) xlinks }
    in toTxt HTMLView { features = fs, ..}

  toTxt KSVGView {..} =
    let fs = features { attributes = Map.union (attributes features) xlinks }
    in toTxt HTMLView { features = fs, children = List.map snd keyedChildren, .. }

  toTxt cv@ComponentView { record = r, ..} =
    case r of
      Nothing -> toTxt $ unsafePerformIO $ do
        e <- create "div"
        mv <- newEmptyMVar
        inject e (renderer (cv,mv))
        takeMVar mv

      Just ref -> toTxt $ unsafePerformIO (readIORef (crView ref))

  toTxt (TaggedView _ v) = toTxt v

  toTxt (ReactiveView _ f) = toTxt f
  toTxt (WeakView _ f) = toTxt f
  
  toTxt (Prebuilt v) = toTxt v

  toTxt _ = mempty

instance ToTxt [View] where
  toTxt = mconcat . fmap toTxt

