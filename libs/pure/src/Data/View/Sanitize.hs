{-# language LambdaCase, RecordWildCards, PatternSynonyms, OverloadedStrings, NamedFieldPuns #-}
module Data.View.Sanitize (sanitize,Processed(..),Options(..),allowDataAttribute,defaultOptions) where

import Data.View (View(..),Features(..),pattern Null)
import qualified Data.Map as Map (mapMaybeWithKey)
import Data.Maybe (mapMaybe)
import qualified Data.Set as Set (toList,fromList)
import Data.Txt as Txt (Txt,toLower,splitOn,isPrefixOf,fromTxt,toTxt)
import Text.HTML.SanitizeXSS (safeTagName,sanitizeAttribute,sanitaryURI)

data Processed = Allow | Disallow | Defer
processed :: a -> a -> a -> Processed -> a
processed allow disallow defer = \case
  Allow -> allow
  Disallow -> disallow
  Defer -> defer

data Options = Options
  { allowSVG :: Bool
  , allowCustomViews :: Bool
  , processTag :: Txt -> Processed
  , processAttribute :: (Txt,Txt) -> Processed
  , processProperty :: (Txt,Txt) -> Processed
  , processStyle :: (Txt,Txt) -> Processed
  , processClass :: Txt -> Processed
  }

allowDataAttribute :: (Txt,Txt) -> Processed
allowDataAttribute (k,_) | "data-" `Txt.isPrefixOf` k = Allow
allowDataAttribute _ = Defer

-- Default options defers all sanitization to xss-sanitize and
-- disallows SVG and custom views like keyed HTML, Portals, etc....
defaultOptions :: Options
defaultOptions = Options 
  { allowSVG = False
  , allowCustomViews = False
  , processTag = const Defer
  , processAttribute = const Defer
  , processProperty = const Defer
  , processStyle = const Defer
  , processClass = const Defer
  }

-- | Sanitize HTML and SVG views and discard raw views.
sanitize :: Options -> View -> View
sanitize opts@Options { allowSVG, allowCustomViews, processTag } = go
  where
    go = \case
      HTMLView {..} | t <- Txt.toLower tag -> 
        let
          safe = HTMLView 
            { features = sanitizeFeatures opts features
            , children = fmap go children
            , .. 
            } 
          unsafe = Null
        in case processTag t of
          Allow                           -> safe
          Defer | safeTagName (fromTxt t) -> safe
          _                               -> unsafe

      SVGView {..} | allowSVG, t <- Txt.toLower tag ->
        let
          safe = SVGView 
            { features = sanitizeFeatures opts features
            , children = fmap go children
            , .. 
            }
          unsafe = Null
        in case processTag t of
          Allow                           -> safe
          Defer | safeTagName (fromTxt t) -> safe
          _                               -> unsafe

      KHTMLView {..} | allowCustomViews, t <- Txt.toLower tag ->
        let
          safe = KHTMLView 
            { features = sanitizeFeatures opts features
            , keyedChildren = fmap (fmap go) keyedChildren
            , .. 
            } 
          unsafe = Null
        in case processTag t of
          Allow                           -> safe
          Defer | safeTagName (fromTxt t) -> safe
          _                               -> unsafe
        
      KSVGView {..} | allowSVG, allowCustomViews, t <- Txt.toLower tag ->
        let
          safe = KSVGView 
            { features = sanitizeFeatures opts features
            , keyedChildren = fmap (fmap go) keyedChildren
            , .. 
            }
          unsafe = Null
        in case processTag t of
          Allow                           -> safe
          Defer | safeTagName (fromTxt t) -> safe
          _                               -> unsafe

      Prebuilt v | allowCustomViews -> 
        Prebuilt (go v)

      tv@TextView{} -> tv

      -- These cases must not have come from de-serialization.
      SomeView a | allowCustomViews -> SomeView a
      LazyView f a | allowCustomViews -> LazyView f a
      TaggedView __w v | allowCustomViews -> TaggedView __w v
      PortalView pp pd pv | allowCustomViews -> PortalView pp pd pv
      ComponentView __w r c p | allowCustomViews -> ComponentView __w r c p
      
      -- RawView and any unsafe tags are discarded
      _ -> Null

-- | Sanitize features of HTML and SVG views. Note that listeners
-- are kept because they are not serialized with the base libraries
-- and, thus, must have been constructed on the client.
sanitizeFeatures :: Options -> Features -> Features
sanitizeFeatures Options {..} Features_ {..} =
  Features_
    { attributes = Map.mapMaybeWithKey (cleanAttribute processAttribute) attributes 
    , properties = Map.mapMaybeWithKey (cleanAttribute processProperty) properties
    , styles     = Map.mapMaybeWithKey (cleanStyle processStyle) styles
    , classes    = Set.fromList (mapMaybe (cleanClass processClass) (Set.toList classes))
    , ..
    } 

cleanAttribute :: ((Txt,Txt) -> Processed) -> Txt -> Txt -> Maybe Txt
cleanAttribute process k v =
  case process (k,v) of
    Allow -> Just v
    Defer | Just (k,v) <- sanitizeAttribute (Txt.fromTxt k,Txt.fromTxt v) -> Just (toTxt v)
    _ -> Nothing

cleanStyle :: ((Txt,Txt) -> Processed) -> Txt -> Txt -> Maybe Txt
cleanStyle process k v  =
  case process (k,v) of
    Allow -> Just v
    Defer | Just (_,kv) <- sanitizeAttribute ("style",Txt.fromTxt (k <> ": " <> v)) 
          , [_,v] <- Txt.splitOn ":" (Txt.toTxt kv)
          -> Just v
    _ -> Nothing

cleanClass :: (Txt -> Processed) -> Txt -> Maybe Txt
cleanClass process c =
  case process c of
    Allow -> Just c
    _     -> Nothing
