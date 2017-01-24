{-# language OverloadedStrings #-}
{-# language ViewPatterns #-}
{-# language CPP #-}
module Nuclear.Route where

import Data.JSText

import Ef.Base

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

#ifdef __GHCJS__
import qualified Data.JSString as JSS
#else
import qualified Data.Text as JSS
#endif

data Route k
    = GetPath (JSText -> k)
    | SetPath JSText k
    | GetRawUrl (JSText -> k)

    | GetParams ([(JSText,JSText)] -> k)
    | GetParam JSText (Maybe JSText -> k)
    | SetParam JSText JSText k

    | forall ms c a. Reroute (Code '[Route] c a)

    | forall ms c a. Subpath JSText (Code '[Route] c a) k

    | forall ms c a. Path JSText (Code '[Route] c a) k

    | forall a. Dispatch a
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
  fmap f (Dispatch a) = Dispatch a
  fmap f Keep = Keep

instance Delta Route Route

instance Monad c => IsString (Code '[Route] c JSText) where
  fromString = getParamOrKeep . fromString

getRawUrl :: (Monad c) => Code '[Route] c JSText
getRawUrl = Send (GetRawUrl Return)

setPath :: (Monad c) => JSText -> Code '[Route] c ()
setPath url = Send (SetPath url (Return ()))

getPath :: (Monad c) => Code '[Route] c JSText
getPath = Send (GetPath Return)

getParams :: (Monad c) => Code '[Route] c [(JSText,JSText)]
getParams = Send (GetParams Return)

setParam :: (Monad c) => JSText -> JSText -> Code '[Route] c ()
setParam p v = Send (SetParam p v (Return ()))

getParam :: (Monad c) => JSText -> Code '[Route] c (Maybe JSText)
getParam p = Send (GetParam p Return)

getParamOrKeep :: (Monad c) => JSText -> Code '[Route] c JSText
getParamOrKeep p = do
  mp <- getParam p
  case mp of
    Nothing -> keep
    Just p -> return p

subpath :: (Monad c)
        => JSText
        -> Code '[Route] c a
        -> Code '[Route] c ()
subpath match handler = Send (Subpath match handler (Return ()))

path :: (Monad c)
     => JSText
     -> Code '[Route] c a
     -> Code '[Route] c ()
path stencil handler = Send (Path stencil handler (Return ()))

dispatch :: (Monad c)
         => a
         -> Code '[Route] c a
dispatch a = Send (Dispatch a)

keep :: (Monad c) => Code '[Route] c a
keep = Send Keep

reroute :: (Monad c)
        => Code '[Route] c a
        -> Code '[Route] c b
reroute rtr = Send (Reroute rtr)

stripTrailingSlashes = JSS.dropWhileEnd (== '/')

breakRoute url =
  let (path,params0) = JSS.span (/= '?') (stripTrailingSlashes url)
      params =
        case JSS.uncons params0 of
          Just ('?',qps) ->
            List.map (second safeTail) $
            List.map (JSS.breakOn "=")
                     (JSS.splitOn "&" qps)
          _ -> []
  in (stripTrailingSlashes $ JSS.takeWhile (/= '#') path,params)

safeTail x =
  case JSS.uncons x of
    Just (_,rest) -> rest
    _ -> ""

-- this is a very rough implementation in need of much love
route :: forall ms c a.
         (MonadIO c)
      => Code '[Route] c a
      -> JSText
      -> c (Maybe a)
route rtr url0@(breakRoute -> (path,params)) =
  withUrl path params rtr
  where

      withUrl :: forall b.
                 JSText
              -> [(JSText,JSText)]
              -> Code '[Route] c b
              -> c (Maybe b)
      withUrl url params = go
          where

              go :: forall x.
                    Code '[Route] c x
                 -> c (Maybe x)
              go (Return _) = return Nothing
              go (Lift sup) = sup >>= go
              go (Do msg) =
                case prj msg of
                  ~(Just x) ->
                    case x of
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
                            res <- withUrl JSS.empty (nubBy ((==) `on` fst) (ps ++ params)) (unsafeCoerce more)
                            case res of
                              Nothing -> go k
                              Just n -> return (Just n)
                          Nothing -> go k
                      Reroute rtr' ->
                        route (unsafeCoerce rtr') url0
                      Dispatch a ->
                        return (Just $ unsafeCoerce a)
                      Keep ->
                        return Nothing


      match x y  =
        case (JSS.uncons x,JSS.uncons y) of
          (Just (':',param),Just ('/',path)) -> do
            let (valueEnc,path') = JSS.break (== '/') path
                value = decodeURI valueEnc
            return $ Just $ Right ((param,value),path')

          (Just matchPath,Just ('/',path)) -> do
            let (subpathEnc,rest) = JSS.splitAt (JSS.length x) path
                subpath = decodeURI subpathEnc
            return $
              if subpath == x then
                if JSS.null rest then
                  Just $ Left rest
                else
                  case JSS.uncons rest of
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
                if JSS.null x && JSS.null y then
                  return $ Just acc
                else
                  case (JSS.uncons x,JSS.uncons y) of
                    (Just ('/',ps),Just ('/',cs)) -> do
                      let
                        (p, ps') = JSS.break (== '/') ps
                        (c_,cs') = JSS.break (== '/') cs
                        c  = decodeURI c_
                      case JSS.uncons p of
                        Just (':',pat) -> withAcc ((pat,c):acc) ps' cs'

                        _ -> if p == c
                             then go ps' cs'
                             else return Nothing

                    (Nothing,Nothing) -> return $ Just acc

                    _ -> return Nothing

decodeURI :: JSText -> JSText
decodeURI encoded =
#ifdef __GHCJS__
  decodeURIComponent_js encoded
#else
  unEscapeString encoded
#endif

encodeURI :: JSText -> JSText
encodeURI uri =
#ifdef __GHCJS__
  encodeURIComponent_js uri
#else
  escapeString uri
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "$r = decodeURIComponent($1);"
  decodeURIComponent_js :: JSText -> JSText

foreign import javascript unsafe
  "$r = encodeURIComponent($1);"
  encodeURIComponent_js :: JSText -> JSText
#else
--------------------------------------------------------------------------------
-- percent-decoding from Network.URI; copied to avoid dependency on Parsec
-- and modified to support JSTexts
-- From http://hackage.haskell.org/package/network-uri
-- by Ezra Cooper, BSD3; minorly modified to appease the hlint god.

escapeString :: JSText -> JSText
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
escapeChar :: (Char->Bool) -> Char -> JSText
escapeChar p c
    | p c       = JSS.singleton c
    | otherwise = JSS.pack $ List.concatMap (\i -> '%' : myShowHex i "") (utf8EncodeChar c)
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
    -> JSText           -- ^ the string to process
    -> JSText           -- ^ the resulting URI string
escapeURIString p = JSS.concatMap (escapeChar p)

-- |Turns all instances of escaped characters in the string back
--  into literal characters.
--
unEscapeString :: JSText -> JSText
unEscapeString s =
  case JSS.uncons s of
    Nothing     -> JSS.empty
    Just (c,cs) ->
      case unEscapeByte s of
        Just (byte, rest) -> unEscapeUtf8 byte rest
        Nothing -> JSS.cons c (unEscapeString cs)

unEscapeByte :: JSText -> Maybe (Int, JSText)
unEscapeByte xs =
  case JSS.uncons xs of
    Just ('%',xs') ->
      case JSS.uncons xs' of
        Just (x1,xs'') ->
          case JSS.uncons xs'' of
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
unEscapeUtf8 :: Int -> JSText -> JSText
unEscapeUtf8 c rest
    | c < 0x80 = JSS.cons (chr c) (unEscapeString rest)
    | c < 0xc0 = JSS.cons replacement_character (unEscapeString rest)
    | c < 0xe0 = multi1
    | c < 0xf0 = multi_byte 2 0xf 0x800
    | c < 0xf8 = multi_byte 3 0x7 0x10000
    | c < 0xfc = multi_byte 4 0x3 0x200000
    | c < 0xfe = multi_byte 5 0x1 0x4000000
    | otherwise = JSS.cons replacement_character (unEscapeString rest)
    where
    replacement_character = '\xfffd'
    multi1 =
      case unEscapeByte rest of
        Just (c1, ds) | c1 .&. 0xc0 == 0x80 ->
          let d = ((fromEnum c .&. 0x1f) `shiftL` 6) .|.  fromEnum (c1 .&. 0x3f)
          in
            if d >= 0x000080 then
              JSS.cons (toEnum d) (unEscapeString ds)
            else
              JSS.cons replacement_character (unEscapeString ds)
        _ -> JSS.cons replacement_character (unEscapeString rest)

    multi_byte :: Int -> Int -> Int -> JSText
    multi_byte i mask overlong =
      aux i rest (unEscapeByte rest) (c .&. mask)
      where
        aux 0 rs _ acc
          | overlong <= acc && acc <= 0x10ffff &&
            (acc < 0xd800 || 0xdfff < acc)     &&
            (acc < 0xfffe || 0xffff < acc)      = JSS.cons (chr acc) (unEscapeString rs)
          | otherwise = JSS.cons replacement_character (unEscapeString rs)

        aux n _ (Just (r, rs)) acc
          | r .&. 0xc0 == 0x80 = aux (n-1) rs (unEscapeByte rs)
                               $! shiftL acc 6 .|. (r .&. 0x3f)

        aux _ rs _ _ = JSS.cons replacement_character (unEscapeString rs)
#endif
