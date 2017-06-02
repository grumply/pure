{-# language OverloadedStrings #-}
{-# language CPP #-}
module Atomic.Services.Cookie where

import Ef.Base

import Data.Txt

import Atomic.Component (Doc,getDocument)
import Atomic.Service

import Data.Maybe
import Data.Monoid
import Data.List

#ifdef __GHCJS__
import qualified GHCJS.DOM.Document as D
import qualified Data.JSString as Txt
#else
import qualified Data.Text as Txt
#endif

import qualified Data.HashMap.Strict as Map

getCookie_internal :: MonadIO c => Doc -> c (Maybe Txt)
getCookie_internal d =
#ifdef __GHCJS__
  D.getCookie d
#else
  return Nothing
#endif

setCookie_internal :: MonadIO c => Doc -> Maybe Txt -> c ()
setCookie_internal d mc =
#ifdef __GHCJS__
  D.setCookie d mc
#else
  return ()
#endif

cookieS :: Service '[State () (Map.HashMap Txt [Txt])]
cookieS = Service {..}
  where

    key = "atomic.cookie"

    build base = do
      cs <- parseCookies
      return (state cs *:* base)

    prime = return ()

getCookies :: ('[State () (Map.HashMap Txt [Txt])] <: ms) => Ef ms IO (Map.HashMap Txt [Txt])
getCookies = get

setCookies :: ('[State () (Map.HashMap Txt [Txt])] <: ms) => Map.HashMap Txt [Txt] -> Ef ms IO ()
setCookies = put

addCookie :: (Functor (Messages ms), MonadIO c)
          => Txt -> Txt -> Ef ms c (Promise ())
addCookie nm val =
  with cookieS $ do
    cs <- getCookies
    let newCs = Map.insertWith (++) nm [val] cs
    setCookies newCs
    doc <- getDocument
    mcs <- getCookie_internal doc
    let dcs = fromMaybe "" mcs
        newdcs =
          case dcs of
            "" ->              nm <> "=" <> val
            _ -> dcs <> ";" <> nm <> "=" <> val
    setCookie_internal doc $ Just newdcs

deleteCookie :: (Functor (Messages ms), MonadIO c)
             => Txt -> Txt -> Ef ms c (Promise ())
deleteCookie nm val =
  with cookieS $ do
    cs <- getCookies
    let mvs = Map.lookup nm cs
    let newcs = maybe cs (\vs -> Map.insert nm (Prelude.filter (/= val) vs) cs) mvs
    setCookies newcs
    doc <- getDocument
    setCookie_internal doc $ Just $ renderCookies newcs

clearCookie :: (Functor (Messages ms), MonadIO c)
            => Txt -> Ef ms c (Promise ())
clearCookie nm =
  with cookieS $ do
    cs <- getCookies
    let newcs = Map.delete nm cs
    setCookies newcs
    doc <- getDocument
    setCookie_internal doc $ Just $ renderCookies newcs

getCookie :: (Functor (Messages ms), MonadIO c)
          => Txt -> Ef ms c (Promise [Txt])
getCookie nm =
  with cookieS $ do
    cs <- getCookies
    return $ fromMaybe [] $ Map.lookup nm cs

parseCookies :: IO (Map.HashMap Txt [Txt])
parseCookies = do
  doc <- getDocument
  mcs <- getCookie_internal doc
  let rawcs = fromMaybe "" mcs
      cs = parse rawcs
      rawcs' = renderCookies cs
  setCookie_internal doc $ Just rawcs'
  return $ parse rawcs
  where
    parse = go Map.empty
      where
        go acc xs =
          case Txt.uncons xs of
            Nothing -> acc
            Just _ ->
              let (nv,crest) = Txt.span (/= ';') xs
                  acc' = go' acc nv
              in case Txt.uncons crest of
                  Just (';',rest) ->
                    case Txt.uncons rest of
                      Nothing -> acc
                      Just (_,_) -> go acc' (Txt.tail rest)
                  _ -> acc
              where
                go' acc nv =
                  let (n,ev) = Txt.span (/= '=') nv
                  in case Txt.uncons ev of
                      Just ('=',v) -> Map.insertWith (++) n [v] acc
                      _ -> acc

renderCookies :: Map.HashMap Txt [Txt] -> Txt
renderCookies cs =
      let cs' = Map.toList cs
          raw = Txt.intercalate ";"
              $ Prelude.map
                  (\(nm,val) ->
                    Txt.intercalate ";" (Prelude.map ((nm <> "=") <>) val)
                  )
                  cs'
      in raw
