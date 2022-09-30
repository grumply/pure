{-# language CPP, ViewPatterns, ScopedTypeVariables, OverloadedStrings #-}
module Data.View.Reflect 
  ( parseDocumentFragment
  , parse
  , unsafeParseDocumentFragment
  , unsafeParse
  ) where

import Data.Coerce
import Data.DOM
import Data.Txt as Txt
import Data.View hiding (attributes,styles)

import Data.Traversable
import qualified Data.List as List

#ifdef __GHCJS__
import GHCJS.Marshal (FromJSVal(..))
#endif

parseDocumentFragment :: Node -> IO [View]
parseDocumentFragment (coerce -> n)
  | Just (11 :: Int) <- n .# "nodeType" = do
    mcs :: Maybe [JSV] <- n ..# "childNodes"
    case mcs of
      Just cs -> traverse (parse . coerce) cs
      _       -> pure []

  | otherwise = 
    pure []

unsafeParseDocumentFragment :: Node -> IO [View]
unsafeParseDocumentFragment (coerce -> n)
  | Just (11 :: Int) <- n .# "nodeType" = do
    mcs :: Maybe [JSV] <- n ..# "childNodes"
    case mcs of
      Just cs -> traverse (unsafeParse . coerce) cs
      _       -> pure []

  | otherwise = 
    pure []

-- | Parse a live DOM node into a View. Parses classes, styles, data attributes,
-- tags and text content only. Anything other than element and text nodes are 
-- parsed to Null views.
parse :: Node -> IO View
parse node@(coerce -> n) = 
  case n .# "nodeType" of
    Just (3 :: Int) | Just cnt <- n .# "textContent" -> 
      pure (fromTxt cnt)
    
    Just 1 -> do
      mcs :: Maybe [JSV] <- n ..# "childNodes"
      case mcs of
        Just cs 
          | Just (t :: Txt)     <- n .# "tagName"
          , mcs :: Maybe [Txt]  <- fmap Txt.words (n .# "class")
          -> do
            ss <- styles node
            as <- entries node
            let 
              styles
                | [] <- ss = id
                | otherwise = 
                  let style (k,v) = Style k v
                  in flip (List.foldr style) ss 

              classes 
                | Just cls <- mcs = flip (List.foldr Class) cls
                | otherwise       = id

              attrs = flip (List.foldr (uncurry Attribute)) as
              
            cs' <- traverse (parse . coerce) cs

            let 
              wrap 
                | n .# "namespaceURI" == Just ("http://www.w3.org/2000/svg" :: Txt) = SimpleSVG
                | otherwise = SimpleHTML

            pure $ 
              wrap (Txt.toLower t) <| styles . classes . attrs |> 
                cs'

        _ -> pure Null

    _ -> pure Null

#ifdef __GHCJS__
foreign import javascript unsafe
  "var d = $1.dataset; var o = {}; o['keys'] = Object.keys(d); o['values'] = Object.values(d); $r = o;"
    data_entries_js :: Node -> IO JSV
#endif

entries :: Node -> IO [(Txt,Txt)]
entries node = do
#ifdef __GHCJS__
  o <- data_entries_js node
  case (,) <$> o .# "keys" <*> o .# "values" of
    Just (ks,vs) -> do
      Just keys   <- fromJSValListOf ks
      Just values <- fromJSValListOf vs
      pure (List.zip keys values)
    _ -> 
      pure []
#else
  pure []
#endif

-- | Parse a live DOM node into a View. Parses all attributes and deconstructs 
-- classes and styles. Anything other than element and text nodes are parsed to 
-- Null views.
unsafeParse :: Node -> IO View
unsafeParse node@(coerce -> n) = 
  case n .# "nodeType" of
    Just (3 :: Int) | Just cnt <- n .# "textContent" -> 
      pure (fromTxt cnt)
    
    Just 1 -> do
      mcs :: Maybe [JSV] <- n ..# "childNodes"
      case mcs of
        Just cs 
          | Just (t :: Txt)     <- n .# "tagName"
          , mcs :: Maybe [Txt]  <- fmap Txt.words (n .# "class")
          -> do
            ss <- styles node
            as <- attributes node
            let 
              styles
                | [] <- ss = id
                | otherwise = 
                  let style (k,v) = Style k v
                  in flip (List.foldr style) ss 

              classes 
                | Just cls <- mcs = flip (List.foldr Class) cls
                | otherwise       = id

              attrs = flip (List.foldr (uncurry Attribute)) as
              
            cs' <- traverse (unsafeParse . coerce) cs

            let 
              wrap 
                | n .# "namespaceURI" == Just ("http://www.w3.org/2000/svg" :: Txt) = SimpleSVG
                | otherwise = SimpleHTML

            pure $
              wrap (Txt.toLower t) <| styles . classes . attrs |> 
                cs'

        _ -> pure Null

    _ -> pure Null

#ifdef __GHCJS__
foreign import javascript unsafe
  "var d = $1.attributes; var o = {}; for (var i = d.length - 1; i >= 0; i--) { o[d[i].name] = d[i].value }; var r = {}; r['keys'] = Object.keys(o); r['values'] = Object.values(o); $r = r;"
    attributes_js :: Node -> IO JSV
#endif

attributes :: Node -> IO [(Txt,Txt)]
attributes node = do
#ifdef __GHCJS__
  o <- attributes_js node
  case (,) <$> o .# "keys" <*> o .# "values" of
    Just (ks,vs) -> do
      Just keys   <- fromJSValListOf ks
      Just values <- fromJSValListOf vs
      pure (List.filter (\(k,_) -> k /= "style" && k /= "class") (List.zip keys values))
    _ -> 
      pure []
#else
  pure []
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "var s = $1.style; var o = {}; for (var i = s.length - 1; i >= 0; i--) { o[s[i]] = s[i] }; var r = {}; r['keys'] = Object.keys(o); r['values'] = Object.values(o); $r = r;"
    styles_js :: Node -> IO JSV
#endif

styles :: Node -> IO [(Txt,Txt)]
styles node = do
#ifdef __GHCJS__
  s <- styles_js node
  case (,) <$> s .# "keys" <*> s .# "values" of
    Just (ks,vs) -> do
      Just keys   <- fromJSValListOf ks
      Just values <- fromJSValListOf vs
      pure (List.zip keys values)
    _ ->
      pure []
#else
  pure []
#endif  
