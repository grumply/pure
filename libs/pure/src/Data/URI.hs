{-# LANGUAGE CPP #-}
module Data.URI (decodeURI,decodeURIComponent,encodeURI,encodeURIComponent) where

import Data.Txt (Txt,ToTxt(..),FromTxt(..))
import qualified Data.Txt as Txt

import Data.Bits ((.|.),(.&.),shiftL,shiftR)
import Data.Char (ord, chr, isHexDigit, isAsciiUpper, isAsciiLower, isDigit, digitToInt)
import Data.List as List
import Numeric

-- Decodes percent encodings without discretion.
decodeURIComponent :: Txt -> Txt
decodeURIComponent c =
#ifdef __GHCJS__
  decodeURIComponent_js c
#else
  unescapeString c
#endif

-- Decodes percent encodings in uri components. This is like
-- calling decodeURIComponent over each component of a URI but
-- not, for instance, the uri scheme. 
-- Note: on GHC, this just calls `decodeURIComponent`.
decodeURI :: Txt -> Txt
decodeURI uri =
#ifdef __GHCJS__
  decodeURI_js uri
#else
  decodeURIComponent uri
#endif

-- Percent-encodes without discretion.
encodeURIComponent :: Txt -> Txt
encodeURIComponent c =
#ifdef __GHCJS__
  encodeURIComponent_js c
#else
  escapeString c
#endif

-- Percent-encodes in uri components. This is like calling 
-- encodeURIComponent over each component of a URI but not, 
-- for instance, the uri scheme.
encodeURI :: Txt -> Txt
encodeURI uri =
#ifdef __GHCJS__
  encodeURI_js uri
#else
  escapeURI uri
#endif

#ifdef __GHCJS__
foreign import javascript unsafe
  "$r = decodeURIComponent($1);" decodeURIComponent_js :: Txt -> Txt

foreign import javascript unsafe
  "$r = encodeURIComponent($1);" encodeURIComponent_js :: Txt -> Txt

foreign import javascript unsafe
  "$r = decodeURI($1)" decodeURI_js :: Txt -> Txt

foreign import javascript unsafe
  "$r = encodeURI($1)" encodeURI_js :: Txt -> Txt
#else

-- Convert a URI into it's percent-encoded equivalent. This does not
-- percent-encode non-component parts, like `://`.
escapeURI :: Txt -> Txt
escapeURI = escapeURIString isUnescapedInURI

-- Convert a string into it's percent-encoded equivalent. This does
-- percent-encode non-component parts, like `://`.
escapeString :: Txt -> Txt
escapeString = escapeURIString isUnescapedInURIComponent

-- This is a dummy implementation as we do not yet have an implementation
-- of unescapeURI. In principle, we would want an equivalent to javascripts
-- `decodeURI()`.
unescapeURI :: Txt -> Txt
unescapeURI = id

-- Convert a string into it's non-percent-encoded equivlant. This does
-- percent-decode non-component parts, like `http%3A%2F%2F`.
unescapeString :: Txt -> Txt
unescapeString = unEscapeString

--------------------------------------------------------------------------------
-- percent-decoding from Network.URI; copied to avoid dependency on Parsec
-- and modified to support Txt
-- From http://hackage.haskell.org/package/network-uri
-- by Ezra Cooper, BSD3; minorly modified to appease the hlint god.

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

-- Returns 'True' if the character is allowed unescaped in a URI.
--
-- >>> escapeURIString isUnescapedInURI "http://haskell.org:80?some_param=true&other_param=їґ"
-- "http://haskell.org:80?some_param=true&other_param=%D1%97%D2%91"
isUnescapedInURI :: Char -> Bool
isUnescapedInURI c = isReserved c || isUnreserved c

-- Returns 'True' if the character is allowed unescaped in a URI component.
--
-- >>> escapeURIString isUnescapedInURIComponent "http://haskell.org:80?some_param=true&other_param=їґ"
-- "http%3A%2F%2Fhaskell.org%3A80%3Fsome_param%3Dtrue%26other_param%3D%D1%97%D2%91"
isUnescapedInURIComponent :: Char -> Bool
isUnescapedInURIComponent c = not (isReserved c || not (isUnescapedInURI c))

-- Escape character if supplied predicate is not satisfied,
--  otherwise return character as singleton string.
--
escapeChar :: (Char->Bool) -> Char -> Txt
escapeChar p c
    | p c       = Txt.singleton c
    | otherwise = toTxt $ List.concatMap (\i -> '%' : myShowHex i "") (utf8EncodeChar c)
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

-- Can be used to make a string valid for use in a URI.
--
escapeURIString
    :: (Char->Bool)     -- ^ a predicate which returns 'False'
                        --   if the character should be escaped
    -> Txt           -- ^ the string to process
    -> Txt           -- ^ the resulting URI string
escapeURIString p = Txt.concatMap (escapeChar p)

-- Turns all instances of escaped characters in the string back
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
