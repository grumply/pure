{-# language CPP, OverloadedStrings, FlexibleInstances #-}
module Data.View.Parse where

import Data.Maybe
import Data.List as List
import Data.View
import Data.Txt as Txt
import Text.HTML.TagSoup.Tree as TS
import Text.HTML.TagSoup as TS
import Text.StringLike

#ifdef __GHCJS__
instance StringLike Txt where
  empty = Txt.empty
  cons = Txt.cons
  uncons t =
    -- careful here because of a ghcjs bug
    if Txt.null t then
      Nothing
    else
      let ~(Just ~(c,rest)) = Txt.uncons t
      in (Just (c,rest))
  toString = Txt.unpack
  fromChar = Txt.singleton
  strConcat = Txt.concat
  strNull = Txt.null
  append = Txt.append
  strMap = Txt.map
#endif

parseView :: Txt -> [View]
parseView = fmap convertTree . parseTree
  where
    convertTree :: TagTree Txt -> View
    convertTree tree =
      case tree of
        TagBranch t as cs ->
          if t == "svg" then
            XLinks (List.foldr xlinks [] as) $ SVGView Nothing t (List.foldr addFeature mempty as) mempty (fmap convertSVGTree cs)
          else
            HTMLView Nothing t (List.foldr addFeature mempty as) (fmap convertTree cs)
        TagLeaf t ->
          case t of
            TagText tt   -> TextView Nothing tt
            TagOpen t as -> HTMLView Nothing t (List.foldr addFeature mempty as) []
            _            -> NullView Nothing

    convertSVGTree :: TagTree Txt -> View
    convertSVGTree tree =
      case tree of
        TagBranch t as cs ->
          if t == "foreignObject" then
            XLinks (List.foldr xlinks [] as) $ SVGView Nothing t (List.foldr addFeature mempty as) mempty (fmap convertTree cs)
          else
            XLinks (List.foldr xlinks [] as) $ SVGView Nothing t (List.foldr addFeature mempty as) mempty (fmap convertSVGTree cs)
        TagLeaf t ->
          case t of
            TagText tt   -> TextView Nothing tt
            TagOpen t as -> XLinks (List.foldr xlinks [] as) $
              SVGView Nothing t (List.foldr addFeature mempty as) mempty []
            _            -> NullView Nothing

    addFeature :: TS.Attribute Txt -> Features -> Features
    addFeature (k,v) = let k' = Txt.toLower k in
      if k' == "style" then
        let ss = Txt.splitOn ";" v
            brk t =
              let (pre,suf) = Txt.break (== ':') t
                 -- ghcjs bug requires this
              in if Txt.null suf then
                   Nothing
                 else
                   Just (pre,Txt.tail suf)
            kvs = mapMaybe brk ss
        in
          Styles kvs
      else if k' == "classname" || k' == "class" then
          Classes (List.filter (Prelude.not . Txt.null) (Txt.splitOn " " v))
      else if Txt.isPrefixOf "xlink:" k' then
          id
      else
          Attribute k' v

    xlinks (k,v) = let k' = Txt.toLower k in
      if Txt.isPrefixOf "xlink:" k' then
        ((k',v):)
      else
        id

-- dumbRelativeCheck :: Txt -> Bool
-- dumbRelativeCheck t = T.isPrefixOf "/" t && not (T.isPrefixOf "//" t)
