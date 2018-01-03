{-# LANGUAGE CPP #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleInstances,FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
{-# LANGUAGE MultiWayIf #-}
module Pure.Render where

-- This is very much non-robust rendering. Lots of corner cases.
-- Needs a tremendous amount of work and thought.

import Ef.Base

import Pure.App (Page(..))
import Pure.Data
import Pure.CSS
import Pure.DOM
import Pure.Types
import Pure.Attributes hiding (SVGLink)
import Pure.HTML hiding (Link,SVGLink)

import qualified Data.List as List

import qualified Pure.Data.Txt as Txt

import qualified Data.Map.Strict as M

import Data.Foldable
import Data.Typeable
import Data.Proxy

import Control.Concurrent
import Data.IORef

import System.IO.Unsafe
import Unsafe.Coerce

import qualified Data.Aeson as A

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
#ifdef USE_TEMPLATE_HASKELL
instance Lift StaticHTML where
  lift (StaticHTML htmlt) = [| StaticHTML htmlt |]
#endif

staticHTML :: View e -> StaticHTML
staticHTML = fromTxt . toTxt

shtml :: Txt -> [Feature e] -> StaticHTML -> View e
shtml _tag _attributes = raw (mkHTML _tag) _attributes . toTxt

selfClosing tag = tag `elem` selfclosing
  where
    selfclosing =
      ["area","base","br","col","frame","command"
      ,"embed","hr","img","input","keygen","link"
      ,"meta","param","source","track","wbr"
      ]

instance ToJSON (Feature ms) where
  toJSON f =
#ifdef __GHCJS__
    objectValue $
#endif
      go f
    where
      go NullFeature = object [ "type" .= ("null" :: Txt)]
      go (Property k v) = object [ "type" .= ("prop" :: Txt), "prop" .= k, "val" .= v]
      go (Attribute k v) = object [ "type" .= ("attr" :: Txt), "attr" .= k, "val" .= v]
      go (StyleMap ss) = object [ "type" .= ("style" :: Txt), "styles" .= M.toList ss ]
      go (Link e _) = object [ "type" .= ("link" :: Txt), "link" .= e]
      go (SVGLink e _) = object [ "type" .= ("svglink" :: Txt), "link" .= e ]
      go (XLink k v) = object [ "type" .= ("xlink" :: Txt), "key" .= k, "val" .= v]
      go _ = object []

instance FromJSON (Feature ms) where
  parseJSON o0 = do
#ifdef __GHCJS__
    flip (withObject "obj") o0 $ \o -> do
#else
      let (A.Object o) = o0
#endif
      t <- o .: "type"
      case t :: Txt of
        "null" ->
          pure NullFeature
        "attr" -> do
          k <- o .: "attr"
          v <- o .: "val"
          pure $ Attribute k v
        "prop" -> do
          k <- o .: "prop"
          v <- o .: "val"
          pure $ Property k v
        "style" -> do
          ss <- o .: "styles"
          pure $ StyleMap (M.fromList ss)
        "link" -> do
          l <- o .: "link"
          pure $ Link l (return ())
        "svglink" -> do
          l <- o .: "link"
          pure $ SVGLink l (return ())
        "xlink" -> do
          k <- o .: "key"
          v <- o .: "val"
          pure $ XLink k v
        _ -> Ef.Base.empty

-- toJSON for View will server-side render components, but not controllers.
instance (e <: '[]) => ToJSON (View e) where
  toJSON a =
#ifdef __GHCJS__
    objectValue $
#endif
      go a
    where
      go stv@ComponentView { componentRecord = c, ..} =
        case c of
          Nothing ->
            go $ unsafePerformIO $ do
              mtd <- newIORef (return ())
              Pure.DOM.build (\_ -> return ()) mtd Nothing stv

          Just ref ->
            go (unsafePerformIO (readIORef (crView ref)))

      go (SomeView v) = go (render v)
      go (TextView _ c) = object [ "type" .= ("text" :: Txt), "content" .= c]
      go (RawView _ t as c) = object [ "type" .= ("raw" :: Txt), "tag" .= t, "attrs" .= toJSON as, "content" .= c ]
      go (KHTMLView _ t as ks _) = object [ "type" .= ("keyed" :: Txt), "tag" .= t, "attrs" .= toJSON as, "keyed" .= toJSON (map (fmap render) ks) ]
      go (HTMLView _ t as cs) = object [ "type" .= ("html" :: Txt), "tag" .= t, "attrs" .= toJSON as, "children" .= toJSON (map render cs) ]
      go (KSVGView _ t as ks _) = object [ "type" .= ("keyedsvg" :: Txt), "tag" .= t, "attrs" .= toJSON as, "keyed" .= toJSON (map (fmap render) ks)]
      go (SVGView _ t as cs) = object [ "type" .= ("svg" :: Txt), "tag" .= t, "attrs" .= toJSON as, "children" .= toJSON (map render cs) ]

      -- Need a better approach here.
      go (ManagedView mn t as (Controller_ c)) =
        let !v = unsafePerformIO $ do
                  with c (return ())
                  Just (MVCRecord {..}) <- lookupController (key c)
                  v <- readIORef mvcrView
                  shutdown c
                  return $ mvcvCurrent v
        in go (HTMLView mn t as [unsafeCoerce v])

      go _ = object [ "type" .= ("null" :: Txt) ]

instance FromJSON (View e) where
  parseJSON o0 = do
#ifdef __GHCJS__
    flip (withObject "obj") o0 $ \o -> do
#else
      let (A.Object o) = o0
#endif
      t <- o .: "type"
      case t :: Txt of
        "text" -> do
          c <- o .: "content"
          pure $ TextView Nothing c
        "raw" -> do
          t <- o .: "tag"
          as <- o .: "attrs"
          c <- o .: "content"
          pure $ RawView Nothing t as c
        "keyed" -> do
          t <- o .: "tag"
          as <- o .: "attrs"
          ks <- o .: "keyed"
          pure $ KHTMLView Nothing t as ks mempty
        "html" -> do
          t <- o .: "tag"
          as <- o .: "attrs"
          cs <- o .: "children"
          pure $ HTMLView Nothing t as cs
        "keyedsvg" -> do
          t <- o .: "tag"
          as <- o .: "attrs"
          ks <- o .: "keyed"
          pure $ KSVGView Nothing t as ks mempty
        "svg" -> do
          t <- o .: "tag"
          as <- o .: "attrs"
          cs <- o .: "children"
          pure $ SVGView Nothing t as cs
        "null" -> pure $ NullView Nothing
        _ -> Ef.Base.empty

instance Show (Feature e) where
  show = fromTxt . toTxt

instance ToTxt (Feature e) where
  toTxt NullFeature          = mempty

  toTxt (Attribute attr val) =
    if Txt.null val then
      attr
    else
      attr <> "=\"" <> val <> "\""

  toTxt (Property prop val) =
    prop <> "=\"" <> val <> "\""

  toTxt (StyleMap pairs) =
    "style=\""
      <> Txt.intercalate
           (Txt.singleton ';')
           (fst $ renderStyles False (traverse_ (uncurry (=:)) (M.toList pairs)))
      <> "\""

  toTxt (Link href _)    = "href=\"" <> href <> "\""

  toTxt (SVGLink href _) = "xlink:href=\"" <> href <> "\""

  toTxt (XLink xl v)     = xl <> "=\"" <> v <> "\""

  toTxt _ = mempty

instance ToTxt [Feature e] where
  toTxt fs =
    Txt.intercalate
     (Txt.singleton ' ')
     (Prelude.filter (not . Txt.null) $ Prelude.map toTxt fs)

instance Show (View e) where
  show = go 0
    where
      go n NullView {} = ""
      go n TextView {..} = List.replicate n ' ' <> fromTxt content <> "\n"
      go n RawView {..} =
        "<" <> fromTxt tag
            <> (if List.null features then "" else " " <> unwords (List.map show features))
            <>
        ">" <> fromTxt content <>
        "</" <> fromTxt tag <> ">"

      go n KHTMLView {..} =
        go n HTMLView { children = List.map snd keyedChildren, ..}

      go n KSVGView {..} =
        go n HTMLView { children = List.map snd keyedChildren, .. }

      go n SVGView {..} = go n HTMLView { .. }

      go n HTMLView {..} =
        List.replicate (2 * n) ' ' <>
        "<" <> fromTxt tag <> (if null features then "" else " " <> unwords (List.map show features))
            <> if | tag == "?xml"     -> "?>\n"
                  | tag == "!doctype" -> ">\n"
                  | selfClosing tag   -> "/>\n"
                  | otherwise         ->
                      ">\n" <> List.concatMap (go (n + 1)) children <>
                       List.replicate (2 * n) ' ' <> "</" <> fromTxt tag <> ">\n"
      go n ManagedView {..} =
        case controller of
          Controller_ Controller {..} ->
            List.replicate (2 * n) ' ' <> "<" <> fromTxt tag <> (if List.null features then "" else " " <> unwords (List.map show features)) <>
              ">"  <> show (render $ view model) <> "</" <> fromTxt tag <> ">"

      go n stv@ComponentView { componentRecord = c, ..} =
        case c of
          Nothing -> show $ unsafePerformIO $ do
                      mtd <- newIORef (return ())
                      Pure.DOM.build (\_ -> return ()) mtd Nothing stv
          Just ref -> show $ unsafePerformIO (readIORef (crView ref))

      go n (SomeView c) = show (render c)


instance ToTxt (View e) where
  toTxt NullView {} = mempty

  toTxt TextView {..} = content

  toTxt RawView {..} =
    "<" <> tag <> (if null features then "" else " " <> Txt.intercalate " " (map toTxt features)) <>
      ">" <> content <> "</" <> tag <> ">"

  toTxt KHTMLView {..} =
    if tag == "?xml" then
      "<?xml"  <> (if null features then "" else " " <> Txt.intercalate " " (map toTxt features)) <> "?>"
    else
      "<" <> tag <> (if null features then "" else " " <> Txt.intercalate " " (map toTxt features)) <>
        if selfClosing tag then
          "/>"
        else
          ">" <> Txt.concat (map (toTxt . render . snd) keyedChildren) <> "</" <> tag <> ">"

  toTxt HTMLView {..} =
    if tag == "?xml" then
      "<?xml"  <> (if null features then "" else " " <> Txt.intercalate " " (map toTxt features)) <> "?>"
    else
      "<" <> tag <> (if null features then "" else " " <> Txt.intercalate " " (map toTxt features)) <>
        if selfClosing tag then
          "/>"
        else
          ">" <> Txt.concat (map (toTxt . render) children) <> "</" <> tag <> ">"

  toTxt SVGView {..} =
    "<" <> tag <> (if null features then "" else " " <> Txt.intercalate " " (map toTxt features)) <>
      if selfClosing tag then
        "/>"
      else
        ">" <> Txt.concat (map (toTxt . render) children) <> "</" <> tag <> ">"

  toTxt KSVGView {..} =
    "<" <> tag <> (if null features then "" else " " <> Txt.intercalate " " (map toTxt features)) <>
      if selfClosing tag then
        "/>"
      else
        ">" <> Txt.concat (map (toTxt . render . snd) keyedChildren) <> "</" <> tag <> ">"

  toTxt ManagedView {..} =
    case controller of
      Controller_ Controller {..} ->
        "<" <> tag <> (if null features then "" else " " <> Txt.intercalate " " (map toTxt features)) <>
          ">"  <> toTxt (render $ view model) <> "</" <> tag <> ">"

  toTxt stv@ComponentView { componentRecord = c, ..} =
    case c of
      Nothing -> toTxt $ unsafePerformIO $ do
                   mtd <- newIORef (return ())
                   Pure.DOM.build (\_ -> return ()) mtd Nothing stv
      Just ref -> toTxt $ unsafePerformIO (readIORef (crView ref))

  toTxt (SomeView c) = toTxt (render c)

instance ToTxt [View e] where
  toTxt = mconcat . map toTxt

renderPage :: Page -> Txt
renderPage (Page h c) =
  "<!DOCTYPE html>" <>
    case h of
      Controller_ Controller {..} ->
        let htm :: View '[]
            htm =
              Html []
                [ unsafeCoerce $ render $ view model
                , Body []
                    [ case c of
                        Controller_ a@Controller {} -> mvc Div [ Id "pure" ] a
                    ]
                ]
        in
          toTxt (render htm)
renderPage (Partial c) =
  ("<!DOCTYPE html>" <>) $
    let htm :: View '[]
        htm =
          Html []
            [ Head [] []
            , Body []
                [ case c of
                    Controller_ a@Controller {} -> mvc Div [ Id "pure" ] a
                ]
            ]
    in
      toTxt (render htm)

renderPageBootstrap :: Page -> Txt -> Txt
renderPageBootstrap (Page h c) mainScript =
  "<!DOCTYPE html>" <>
    case h of
      Controller_ Controller {..} ->
        let htm :: View '[]
            htm =
              Html []
                [ Head [] []
                , Body []
                    [ case c of
                        Controller_ a@Controller {} -> mvc Div [ Id "pure" ] a
                    , Script [ Src mainScript, Defer True ] []
                    ]
                ]
        in
          toTxt (render htm)
renderPageBootstrap (Partial c) mainScript =
  "<!DOCTYPE html>" <>
    case c of
      Controller_ a@Controller {} ->
        let htm :: View '[]
            htm =
              Html []
                [ Head [] []
                , Body []
                    [ mvc Div [ Id "pure" ] a
                    , Script [ Src mainScript, Defer True ] []
                    ]
                ]
        in
          toTxt (render htm)

renderDynamicPage :: Page -> IO Txt
renderDynamicPage (Page (Controller_ h) (Controller_ c)) = do
  let dt = "<!DOCTYPE html><html>"
  Just h_ <- demandMaybe =<< currentHTML h
  head_html <- renderDynamicHTML h_
  let bdy :: View '[]
      bdy = Body [] [ mvc Div [ Id "pure" ] c]
  body_html <- renderDynamicHTML (render bdy)
  return $ dt <> head_html <> body_html <> "</html>"
renderDynamicPage (Partial (Controller_ c)) = do
  let dt = "<!DOCTYPE html><head></head>"
  Just c_ <- demandMaybe =<< currentHTML c
  let bdy :: View '[]
      bdy = Body [] [ mvc Div [ Id "pure" ] c ]
  body_html <- renderDynamicHTML (render bdy)
  return $ dt <> body_html <> "</html>"

renderDynamicPageBootstrap :: Page -> Txt -> IO Txt
renderDynamicPageBootstrap (Page (Controller_ h) (Controller_ c)) mainScript = do
  let dt = "<!DOCTYPE html><html>"
  Just h_ <- demandMaybe =<< currentHTML h
  head_html <- renderDynamicHTML h_
  let bdy :: View '[]
      bdy = Body [] [ mvc Div [ Id "pure" ] c, Script [ Src mainScript, Defer True ] [] ]
  body_html <- renderDynamicHTML (render bdy)
  return $ dt <> head_html <> body_html <> "</html>"
renderDynamicPageBootstrap (Partial (Controller_ c)) mainScript = do
  let dt = "<!DOCTYPE html><html><head></head>"
  let bdy :: View '[]
      bdy = Body [] [ mvc Div [ Id "pure" ] c, Script [ Src mainScript, Defer True ] [] ]
  body_html <- renderDynamicHTML (render bdy)
  return $ dt <> body_html <> "</html>"

renderDynamicHTML :: forall e. View e -> IO Txt
renderDynamicHTML h =
  case h of
    NullView {} -> return mempty

    TextView {..} -> return content

    RawView {..} ->
      return $
        "<" <> tag <> (if null features then "" else " " <> Txt.intercalate " " (map toTxt features))
          <> ">"<> content <> "</" <> tag <> ">"

    KHTMLView {..} -> do
      cs <- traverse (\(_,c) -> renderDynamicHTML c) keyedChildren
      return $
        "<" <> tag <> (if null features then "" else " " <> Txt.intercalate " " (map toTxt features))
          <> if selfClosing tag then
               "/>"
             else
               ">" <> Txt.concat cs <> "</" <> tag <> ">"

    HTMLView {..} -> do
      cs <- traverse renderDynamicHTML children
      return $
        "<" <> tag <> (if null features then "" else " " <> Txt.intercalate " " (map toTxt features))
          <> if selfClosing tag then
              "/>"
            else
              ">" <> Txt.concat cs <> "</" <> tag <> ">"

    stv@ComponentView { componentRecord = c, ..} ->
      case c of
        Nothing -> do
          mtd <- newIORef (return ())
          renderDynamicHTML =<< Pure.DOM.build (\_ -> return ()) mtd Nothing stv
        Just ref -> do
          v <- readIORef (crView ref)
          renderDynamicHTML v

    SVGView {..} -> do
      cs <- traverse renderDynamicHTML children
      return $
        "<" <> tag <> (if null features then "" else " " <> Txt.intercalate " " (map toTxt features))
          <> if selfClosing tag then
               "/>"
             else
               ">" <> Txt.concat cs <> "</" <> tag <> ">"

    KSVGView {..} -> do
      cs <- traverse (\(_,c) -> renderDynamicHTML c) keyedChildren
      return $
        "<" <> tag <> (if null features then "" else " " <> Txt.intercalate " " (map toTxt features))
          <> if selfClosing tag then
               "/>"
             else
               ">" <> Txt.concat cs <> "</" <> tag <> ">"

    ManagedView {..} ->
      case controller of
        Controller_ a@Controller {..} -> do
          Just v <- demandMaybe =<< currentHTML a
          inner <- renderDynamicHTML v
          return $
            "<" <> tag <> (if null features then "" else " " <> Txt.intercalate " " (map toTxt features))
              <> ">"  <> inner <> "</" <> tag <> ">"

    SomeView c ->
      renderDynamicHTML (render c)
