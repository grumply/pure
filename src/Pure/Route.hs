{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE CPP #-}
module Pure.Route where

import Ef.Base
import Pure.Data
import Pure.Data.Txt (Txt,ToTxt(..),FromTxt(..))

import Data.Bifunctor
import Data.Function
import Data.List as List
import Data.Maybe
import Data.Proxy
import Data.String

import Unsafe.Coerce

#ifndef __GHCJS__
-- for Network.URI's url decoding
import Data.Char (ord, chr, isHexDigit, isAsciiUpper, isAsciiLower, isDigit, digitToInt)
import Data.Bits ((.|.),(.&.),shiftL,shiftR)
import Numeric
#endif

import qualified Pure.Data.Txt as Txt

data Route k
    = GetPath (Txt -> k)
    | SetPath Txt k
    | GetRawUrl (Txt -> k)

    | GetParams ([(Txt,Txt)] -> k)
    | GetParam Txt (Maybe Txt -> k)
    | SetParam Txt Txt k

    | forall c a. Reroute (Narrative Route c a)

    | forall c a. Subpath Txt (Narrative Route c a) k

    | forall c a. Path Txt (Narrative Route c a) k

    | forall a. Route a
    | Keep

instance Functor Route where
  fmap f (GetPath jk) = GetPath (fmap f jk)
  fmap f (SetPath j k) = SetPath j (f k)
  fmap f (GetRawUrl jk) = GetRawUrl (fmap f jk)
  fmap f (GetParams jjsk) = GetParams (fmap f jjsk)
  fmap f (GetParam j mjk) = GetParam j (fmap f mjk)
  fmap f (SetParam j j' k) = SetParam j j' (f k)
  fmap f (Reroute c) = Reroute c
  fmap f (Subpath j c k) = Subpath j c (f k)
  fmap f (Path j c k) = Path j c (f k)
  fmap f (Route a) = Route a
  fmap f Keep = Keep

instance (Monad c,FromTxt a) => IsString (Narrative Route c a) where
  fromString = getParamOrKeep . fromString

getRawUrl :: (Monad c) => Narrative Route c Txt
getRawUrl = send (GetRawUrl id)

setPath :: (Monad c) => Txt -> Narrative Route c ()
setPath url = send (SetPath url ())

getPath :: (Monad c) => Narrative Route c Txt
getPath = send (GetPath id)

getParams :: (Monad c) => Narrative Route c [(Txt,Txt)]
getParams = send (GetParams id)

setParam :: (Monad c) => Txt -> Txt -> Narrative Route c ()
setParam p v = send (SetParam p v ())

getParam :: (Monad c) => Txt -> Narrative Route c (Maybe Txt)
getParam p = send (GetParam p id)

getParamOrKeep :: (FromTxt a, Monad c) => Txt -> Narrative Route c a
getParamOrKeep p = do
  mp <- getParam p
  case mp of
    Nothing -> keep
    Just p -> return (fromTxt p)

subpath :: (Monad c) => Txt -> Narrative Route c a -> Narrative Route c ()
subpath match handler = send (Subpath match handler ())

path :: (Monad c) => Txt -> Narrative Route c a -> Narrative Route c ()
path stencil handler = send (Path stencil handler ())

dispatch :: (Monad c) => a -> Narrative Route c a
dispatch a = send (Route a)

keep :: (Monad c) => Narrative Route c a
keep = send Keep

reroute :: (Monad c) => Narrative Route c a -> Narrative Route c b
reroute rtr = send (Reroute rtr)

stripTrailingSlashes = Txt.dropWhileEnd (== '/')

breakRoute url =
  let (path,params0) = Txt.span (/= '?') (stripTrailingSlashes url)
      params =
        case Txt.uncons params0 of
          Just ('?',qps) ->
            List.map (second safeTail) $
            List.map (Txt.breakOn "=")
                     (Txt.splitOn "&" qps)
          _ -> []
      safeTail x =
        case Txt.uncons x of
          Just (_,rest) -> rest
          _ -> ""
  in (stripTrailingSlashes $ Txt.takeWhile (/= '#') path,params)

-- this is a very rough implementation in need of much love
route :: forall ms c a. (MonadIO c) => Narrative Route c a -> Txt -> c (Maybe a)
route rtr url0@(breakRoute -> (path,params)) =
  withUrl path params rtr
  where

      withUrl :: forall b.
                 Txt
              -> [(Txt,Txt)]
              -> Narrative Route c b
              -> c (Maybe b)
      withUrl url params = go
          where

              go :: forall x.
                    Narrative Route c x
                 -> c (Maybe x)
              go (Return _) = return Nothing
              go (Lift sup) = sup >>= go
              go (Do msg) =
                case msg of
                  GetRawUrl sk -> go (sk url0)
                  GetPath sk -> go (sk url)
                  SetPath nr k -> withUrl nr params k
                  GetParams psk -> go $ psk params
                  GetParam p mvk -> go $ mvk (List.lookup p params)
                  SetParam p v k -> withUrl url (nubBy ((==) `on` fst) ((p,v):params)) k
                  Subpath section more k -> do
                    espv <- liftIO $ match section url
                    case espv of
                      Just (Left subpath) -> do
                        res <- withUrl subpath params $ unsafeCoerce more
                        case res of
                          Nothing -> go k
                          Just n -> return (Just n)
                      Just (Right ((p,v),subpath)) -> do
                        res <- withUrl subpath (nubBy ((==) `on` fst) ((p,v):params)) (unsafeCoerce more)
                        case res of
                          Nothing -> go k
                          Just n -> return (Just n)
                      Nothing ->
                        go k
                  Path pttrn more k -> do
                    mps <- liftIO $ stencil pttrn url
                    case mps of
                      Just ps -> do
                        res <- withUrl Txt.empty (nubBy ((==) `on` fst) (ps ++ params)) (unsafeCoerce more)
                        case res of
                          Nothing -> go k
                          Just n -> return (Just n)
                      Nothing -> go k
                  Reroute rtr' ->
                    route (unsafeCoerce rtr') url0
                  Route a ->
                    return (Just $ unsafeCoerce a)
                  Keep ->
                    return Nothing


      match x y  =
        case (Txt.uncons x,Txt.uncons y) of
          (Just (':',param),Just ('/',path)) -> do
            let (valueEnc,path') = Txt.break (== '/') path
                value = decodeURI valueEnc
            return $ Just $ Right ((param,value),path')

          (Just matchPath,Just ('/',path)) -> do
            let (subpathEnc,rest) = Txt.splitAt (Txt.length x) path
                subpath = decodeURI subpathEnc
            return $
              if subpath == x then
                if Txt.null rest then
                  Just $ Left rest
                else
                  case Txt.uncons rest of
                    Just ('/',_) -> Just $ Left rest
                    _            -> Nothing
              else
                Nothing

          _ -> return Nothing


      stencil = withAcc []
        where

          withAcc acc = go
            where

              go x y =
                if Txt.null x && Txt.null y then
                  return $ Just acc
                else
                  case (Txt.uncons x,Txt.uncons y) of
                    (Just ('/',ps),Just ('/',cs)) -> do
                      let
                        (p, ps') = Txt.break (== '/') ps
                        (c_,cs') = Txt.break (== '/') cs
                        c  = decodeURI c_
                      case Txt.uncons p of
                        Just (':',pat) -> withAcc ((pat,c):acc) ps' cs'

                        _ -> if p == c
                             then go ps' cs'
                             else return Nothing

                    (Nothing,Nothing) -> return $ Just acc

                    _ -> return Nothing

decodeURI :: Txt -> Txt
decodeURI encoded =
#ifdef __GHCJS__
  decodeURIComponent_js encoded
#else
  unEscapeString encoded
#endif

encodeURI :: Txt -> Txt
encodeURI uri =
#ifdef __GHCJS__
  encodeURIComponent_js uri
#else
  escapeString uri
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "$r = decodeURIComponent($1);"
  decodeURIComponent_js :: Txt -> Txt

foreign import javascript unsafe
  "$r = encodeURIComponent($1);"
  encodeURIComponent_js :: Txt -> Txt
#else
--------------------------------------------------------------------------------
-- percent-decoding from Network.URI; copied to avoid dependency on Parsec
-- and modified to support Txts
-- From http://hackage.haskell.org/package/network-uri
-- by Ezra Cooper, BSD3; minorly modified to appease the hlint god.

escapeString :: Txt -> Txt
escapeString = escapeURIString isUnescapedInURIComponent

isAlphaChar :: Char -> Bool
isAlphaChar c = isAsciiUpper c || isAsciiLower c

isDigitChar :: Char -> Bool
isDigitChar = isDigit

isAlphaNumChar :: Char -> Bool
isAlphaNumChar c = isAlphaChar c || isDigitChar c

isReserved :: Char -> Bool
isReserved c = isGenDelims c || isSubDelims c

isGenDelims :: Char -> Bool
isGenDelims c = c `elem` (":/?#[]@" :: String)

isSubDelims :: Char -> Bool
isSubDelims c = c `elem` ("!$&'()*+,;=" :: String)

isUnreserved :: Char -> Bool
isUnreserved c = isAlphaNumChar c || (c `elem` ("-_.~" :: String))

-- | Returns 'True' if the character is allowed unescaped in a URI.
--
-- >>> escapeURIString isUnescapedInURI "http://haskell.org:80?some_param=true&other_param=їґ"
-- "http://haskell.org:80?some_param=true&other_param=%D1%97%D2%91"
isUnescapedInURI :: Char -> Bool
isUnescapedInURI c = isReserved c || isUnreserved c

-- | Returns 'True' if the character is allowed unescaped in a URI component.
--
-- >>> escapeURIString isUnescapedInURIComponent "http://haskell.org:80?some_param=true&other_param=їґ"
-- "http%3A%2F%2Fhaskell.org%3A80%3Fsome_param%3Dtrue%26other_param%3D%D1%97%D2%91"
isUnescapedInURIComponent :: Char -> Bool
isUnescapedInURIComponent c = not (isReserved c || not (isUnescapedInURI c))

-- |Escape character if supplied predicate is not satisfied,
--  otherwise return character as singleton string.
--
escapeChar :: (Char->Bool) -> Char -> Txt
escapeChar p c
    | p c       = Txt.singleton c
    | otherwise = Txt.pack $ List.concatMap (\i -> '%' : myShowHex i "") (utf8EncodeChar c)
    where
        myShowHex :: Int -> ShowS
        myShowHex n r =  case showIntAtBase 16 toChrHex n r of
            []  -> "00"
            [x] -> ['0',x]
            cs  -> cs
        toChrHex d
            | d < 10    = chr (ord '0' + fromIntegral d)
            | otherwise = chr (ord 'A' + fromIntegral (d - 10))

-- From http://hackage.haskell.org/package/utf8-string
-- by Eric Mertens, BSD3
-- Returns [Int] for use with showIntAtBase
utf8EncodeChar :: Char -> [Int]
utf8EncodeChar = List.map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `shiftR` 6)
                        , 0x80 + oc .&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `shiftR` 12)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `shiftR` 18)
                        , 0x80 + ((oc `shiftR` 12) .&. 0x3f)
                        , 0x80 + ((oc `shiftR` 6) .&. 0x3f)
                        , 0x80 + oc .&. 0x3f
                        ]

-- |Can be used to make a string valid for use in a URI.
--
escapeURIString
    :: (Char->Bool)     -- ^ a predicate which returns 'False'
                        --   if the character should be escaped
    -> Txt           -- ^ the string to process
    -> Txt           -- ^ the resulting URI string
escapeURIString p = Txt.concatMap (escapeChar p)

-- |Turns all instances of escaped characters in the string back
--  into literal characters.
--
unEscapeString :: Txt -> Txt
unEscapeString s =
  case Txt.uncons s of
    Nothing     -> Txt.empty
    Just (c,cs) ->
      case unEscapeByte s of
        Just (byte, rest) -> unEscapeUtf8 byte rest
        Nothing -> Txt.cons c (unEscapeString cs)

unEscapeByte :: Txt -> Maybe (Int, Txt)
unEscapeByte xs =
  case Txt.uncons xs of
    Just ('%',xs') ->
      case Txt.uncons xs' of
        Just (x1,xs'') ->
          case Txt.uncons xs'' of
            Just (x2,s) ->
              if isHexDigit x1 && isHexDigit x2 then
                Just (digitToInt x1 * 16 + digitToInt x2,s)
              else
                Nothing
            _ -> Nothing
        _ -> Nothing
    _ -> Nothing

-- Adapted from http://hackage.haskell.org/package/utf8-string
-- by Eric Mertens, BSD3
unEscapeUtf8 :: Int -> Txt -> Txt
unEscapeUtf8 c rest
    | c < 0x80 = Txt.cons (chr c) (unEscapeString rest)
    | c < 0xc0 = Txt.cons replacement_character (unEscapeString rest)
    | c < 0xe0 = multi1
    | c < 0xf0 = multi_byte 2 0xf 0x800
    | c < 0xf8 = multi_byte 3 0x7 0x10000
    | c < 0xfc = multi_byte 4 0x3 0x200000
    | c < 0xfe = multi_byte 5 0x1 0x4000000
    | otherwise = Txt.cons replacement_character (unEscapeString rest)
    where
    replacement_character = '\xfffd'
    multi1 =
      case unEscapeByte rest of
        Just (c1, ds) | c1 .&. 0xc0 == 0x80 ->
          let d = ((fromEnum c .&. 0x1f) `shiftL` 6) .|.  fromEnum (c1 .&. 0x3f)
          in
            if d >= 0x000080 then
              Txt.cons (toEnum d) (unEscapeString ds)
            else
              Txt.cons replacement_character (unEscapeString ds)
        _ -> Txt.cons replacement_character (unEscapeString rest)

    multi_byte :: Int -> Int -> Int -> Txt
    multi_byte i mask overlong =
      aux i rest (unEscapeByte rest) (c .&. mask)
      where
        aux 0 rs _ acc
          | overlong <= acc && acc <= 0x10ffff &&
            (acc < 0xd800 || 0xdfff < acc)     &&
            (acc < 0xfffe || 0xffff < acc)      = Txt.cons (chr acc) (unEscapeString rs)
          | otherwise = Txt.cons replacement_character (unEscapeString rs)

        aux n _ (Just (r, rs)) acc
          | r .&. 0xc0 == 0x80 = aux (n-1) rs (unEscapeByte rs)
                               $! shiftL acc 6 .|. (r .&. 0x3f)

        aux _ rs _ _ = Txt.cons replacement_character (unEscapeString rs)
#endif
