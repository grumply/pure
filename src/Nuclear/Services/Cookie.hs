{-# language OverloadedStrings #-}
{-# language CPP #-}
module Nuclear.Services.Cookie where

import Ef.Base

import Data.JSText

import Nuclear.Atom (Doc,getDocument)
import Nuclear.Service
import Nuclear.With

import Data.Maybe
import Data.Monoid
import Data.List

#ifdef __GHCJS__
import qualified GHCJS.DOM.Document as D
import qualified Data.JSString as JSText
#else
import qualified Data.Text as JSText
#endif

import qualified Data.HashMap.Strict as Map

getCookie_internal :: MonadIO c => Doc -> c (Maybe JSText)
getCookie_internal d =
#ifdef __GHCJS__
  D.getCookie d
#else
  return Nothing
#endif

setCookie_internal :: MonadIO c => Doc -> Maybe JSText -> c ()
setCookie_internal d mc =
#ifdef __GHCJS__
  D.setCookie d mc
#else
  return ()
#endif

type CookieS = (State () (Map.HashMap JSText [JSText])) ': Service_

cookieS :: S '[State () (Map.HashMap JSText [JSText])]
cookieS = Service {..}
  where

    key = "Fusion.cookieS"

    build base = do
      cs <- parseCookies
      return (state cs *:* base)

    prime = return ()

getCookies :: Code CookieS IO (Map.HashMap JSText [JSText])
getCookies = get

setCookies :: Map.HashMap JSText [JSText] -> Code CookieS IO ()
setCookies = put

addCookie :: (Functor (Messages ms), MonadIO c)
          => JSText -> JSText -> Code ms c (Promise ())
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
             => JSText -> JSText -> Code ms c (Promise ())
deleteCookie nm val =
  with cookieS $ do
    cs <- getCookies
    let mvs = Map.lookup nm cs
    let newcs = maybe cs (\vs -> Map.insert nm (Prelude.filter (/= val) vs) cs) mvs
    setCookies newcs
    doc <- getDocument
    setCookie_internal doc $ Just $ renderCookies newcs

clearCookie :: (Functor (Messages ms), MonadIO c)
            => JSText -> Code ms c (Promise ())
clearCookie nm =
  with cookieS $ do
    cs <- getCookies
    let newcs = Map.delete nm cs
    setCookies newcs
    doc <- getDocument
    setCookie_internal doc $ Just $ renderCookies newcs

getCookie :: (Functor (Messages ms), MonadIO c)
          => JSText -> Code ms c (Promise [JSText])
getCookie nm =
  with cookieS $ do
    cs <- getCookies
    return $ fromMaybe [] $ Map.lookup nm cs

parseCookies :: IO (Map.HashMap JSText [JSText])
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
          case JSText.uncons xs of
            Nothing -> acc
            Just _ ->
              let (nv,crest) = JSText.span (/= ';') xs
                  acc' = go' acc nv
              in case JSText.uncons crest of
                  Just (';',rest) ->
                    case JSText.uncons rest of
                      Nothing -> acc
                      Just (_,_) -> go acc' (JSText.tail rest)
                  _ -> acc
              where
                go' acc nv =
                  let (n,ev) = JSText.span (/= '=') nv
                  in case JSText.uncons ev of
                      Just ('=',v) -> Map.insertWith (++) n [v] acc
                      _ -> acc

renderCookies :: Map.HashMap JSText [JSText] -> JSText
renderCookies cs =
      let cs' = Map.toList cs
          raw = JSText.intercalate ";"
              $ Prelude.map
                  (\(nm,val) ->
                    JSText.intercalate ";" (Prelude.map ((nm <> "=") <>) val)
                  )
                  cs'
      in raw
