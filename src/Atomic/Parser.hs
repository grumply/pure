{-# language FunctionalDependencies #-}
{-# language OverloadedStrings #-}
module Atomic.Parser where

import Ef.Base hiding (sequence)

import Atomic.FromTxt
import Atomic.ToTxt

import qualified Data.Txt as T

import Data.Char
import Data.Maybe
import Data.Monoid
import Data.Typeable

import Unsafe.Coerce

import Prelude hiding (take,takeWhile,sequence)

newtype FailureHandler = FailureHandler Int deriving Eq
newtype PartialHandler = PartialHandler Int deriving Eq

data Parse ts e k where
  GetTokens :: (ts -> k) -> Parse ts e k
  SetTokens :: ts -> k -> Parse ts e k
  Satisfy   :: (t -> Bool) -> (t -> k) -> Parse ts e k
  Plus      :: k -> k -> (a -> k) -> Parse ts e k
  Try       :: k -> (a -> k) -> Parse ts e k
  Ann       :: k -> (a -> k) -> [e] -> Parse ts e k
  Failed    :: [e] -> Parse ts e k

instance Functor (Parse ts e) where
  fmap f (GetTokens tsk) = GetTokens (fmap f tsk)
  fmap f (SetTokens ts k) = SetTokens ts (f k)
  fmap f (Satisfy ib ik) = Satisfy ib (fmap f ik)
  fmap f (Plus l r ak) = Plus (f l) (f r) (fmap f ak)
  fmap f (Try k ak) = Try (f k) (fmap f ak)
  fmap f (Failed msg) = (Failed msg)

getTokens :: forall ts e c. (Monad c) => Parser ts e c ts
getTokens = Parser (Send (GetTokens Return :: Parse ts e (Code '[Parse ts e] c ts)))

setTokens :: forall ts e c. (Monad c) => ts -> Parser ts e c ()
setTokens ts = Parser (Send (SetTokens ts (Return ()) :: Parse ts e (Code '[Parse ts e] c ())))

satisfy :: forall ts e c t. (Monad c, TokenStream ts t) => (t -> Bool) -> Parser ts e c t
satisfy f = Parser (Send (Satisfy f Return :: Parse ts e (Code '[Parse ts e] c t)))

plus :: forall ts e c a. (Monad c) => Parser ts e c a -> Parser ts e c a -> Parser ts e c a
plus (Parser l) (Parser r) = Parser (Send (Plus l r Return :: Parse ts e (Code '[Parse ts e] c a)))

try :: forall ts e c a. (Monad c) => Parser ts e c a -> Parser ts e c a
try (Parser p) = Parser (Send (Try p Return :: Parse ts e (Code '[Parse ts e] c a)))

infix 0 <?>
(<?>) :: forall ts e c a. (Monad c) => Parser ts e c a -> [e] -> Parser ts e c a
(<?>) (Parser p) ann = Parser (Send (Ann p Return ann :: Parse ts e (Code '[Parse ts e] c a)))

failed :: forall ts e c r. (Monad c) => [e] -> Parser ts e c r
failed msg = Parser (Send (Failed msg :: Parse ts e (Code '[Parse ts e] c r)))

satisfyWith :: (Monad c, FromTxt e, TokenStream ts t) => (t -> a) -> (a -> Bool) -> Parser ts e c a
satisfyWith f p = do
  t <- f <$> anyToken
  if p t then
    return t
  else do
    failed [fromTxt "satisfyWith"]

skip :: (Monad c, FromTxt e, TokenStream ts t) => (t -> Bool) -> Parser ts e c ()
skip f = do
  t <- anyToken
  if f t then
    return ()
  else
    failed [fromTxt "skip"]

anyToken :: (Monad c, TokenStream ts t) => Parser ts e c t
anyToken = satisfy (const True)

pushback :: (Monad c, TokenStream ts t) => t -> Parser ts e c ()
pushback t = getTokens >>= setTokens . cons t

take :: (Monad c, Monoid ts, TokenStream ts t) => Int -> Parser ts e c ts
take n
  | n < 0 = error "getItemsN: requested item count less than 0"
  | n == 0 = return mempty
  | otherwise = go n
  where
    go n = do
      t <- anyToken
      if n == 1 then do
        return (cons t mempty)
      else do
        rest <- go (n - 1)
        return (cons t rest)

takeWith :: (Monad c, Monoid ts, FromTxt e, TokenStream ts t) => Int -> (ts -> Bool) -> Parser ts e c ts
takeWith n f = do
  ts <- take n
  if f ts then
    return ts
  else
    failed [fromTxt "takeWith"]

lookahead :: (Monad c, TokenStream ts t) => Parser ts e c (Maybe t)
lookahead = lookaheadN 1

lookaheadN :: (Monad c, TokenStream ts t) => Int -> Parser ts e c (Maybe t)
lookaheadN n
  | n < 0 = return Nothing
  | otherwise = go [] n
  where
    go acc n = do
      t <- anyToken
      if n == 0 then do
        forM_ acc pushback
        return (Just t)
      else
        go (t:acc) (n - 1)

takeWhile :: (Monad c, Monoid ts, TokenStream ts t) => (t -> Bool) -> Parser ts e c ts
takeWhile f = go
  where
    go = do
      t <- anyToken
      if f t then do
        rest <- go
        return (cons t rest)
      else do
        pushback t
        return mempty

takeUntil :: (Monad c, TokenStream ts t) => (t -> Bool) -> Parser ts e c ts
takeUntil f = takeWhile (not . f)

skipWhile :: (Monad c, TokenStream ts t) => (t -> Bool) -> Parser ts e c ()
skipWhile f = go
  where
    go = do
      t <- anyToken
      if f t then
        go
      else
        pushback t

skipUntil :: (Monad c, TokenStream ts t) => (t -> Bool) -> Parser ts e c ()
skipUntil f = skipWhile (not . f)

sequence :: (Monad c, Eq t, TokenStream ts t, TokenStream ts' t) => ts' -> Parser ts e c ts'
sequence ts0 = go ts0
  where
    go ts =
      case uncons ts of
        Nothing -> return ts0
        Just (t,rest) -> do
          satisfy (== t)
          go rest

string :: (Monad c, FromTxt e, TokenStream ts Char) => T.Txt -> Parser ts e c T.Txt
string s = sequence s <?> [fromTxt $ "string: " <> toTxt s]

asciiCI :: (Monad c, FromTxt e, TokenStream ts Char) => T.Txt -> Parser ts e c T.Txt
asciiCI ts0 = go ts0
  where
    go ts =
      case uncons ts of
        Nothing -> return ts
        Just (t,rest) -> do
          satisfy (\x -> toLower x == toLower t) <?> [fromTxt $ "asciiCI: " <> ts0 <> ", " <> rest]
          go rest

char :: (Monad c, FromTxt e, TokenStream ts Char) => Char -> Parser ts e c Char
char c = satisfy (== c) <?> [ fromTxt $ T.singleton c ]

notChar :: (Monad c, FromTxt e, TokenStream ts Char) => Char -> Parser ts e c Char
notChar c = satisfy (/= c) <?> [fromTxt $ "not " <> T.singleton c]

endOfLine :: (Monad c, FromTxt e, TokenStream ts Char) => Parser ts e c ()
endOfLine = (char '\n' >> return ()) <|> (string "\r\n" >> return ())

endOfInput :: (Monad c, ToTxt ts, FromTxt e, TokenStream ts t) => Parser ts e c ()
endOfInput = do
  ts <- getTokens
  case uncons ts of
    Nothing -> return ()
    Just _ -> failed [fromTxt $ "endOfInput: " <> toTxt ts]

newtype Parser ts e c r = Parser { parser :: Code '[Parse ts e] c r }
instance (Monad c) => Functor (Parser ts e c) where
  fmap f (Parser p) = Parser (fmap f p)
instance (Monad c) => Applicative (Parser ts e c) where
  pure = Parser . pure
  (Parser fab) <*> (Parser fa) = Parser (fab <*> fa)
instance (Monad c) => Monad (Parser ts e c) where
  return = Parser . return
  ma >>= amb = Parser $ parser ma >>= parser . amb
instance MonadTrans (Parser ts e) where
  lift = Parser . lift
instance (MonadIO c) => MonadIO (Parser ts e c) where
  liftIO = Parser . liftIO
instance (Monad c) => Alternative (Parser ts e c) where
  empty = failed []
  (<|>) = plus
instance (Monad c) => MonadPlus (Parser ts e c) where
  mzero = failed []
  mplus = (<|>)
instance (Monad c) => Monoid (Parser ts e c a) where
  mempty = failed []
  mappend = (<|>)

-- instance (Monad c,a ~ Txt) => IsString (Parser Txt c a) where
--   fromString = 

-- instance (Monad c,a ~ ()) => IsList (Parser c a) where
--   type Item (Parser ms c a) = Txt
--   fromList = try . Prelude.foldr1 (<|>) . Prelude.map string
--   toList _ = []

class Monoid s => TokenStream s i | s -> i where
  uncons :: s -> Maybe (i,s)
  cons :: i -> s -> s

instance TokenStream T.Txt Char where
  uncons t =
    if T.null t then
      Nothing
    else
      let ~(Just ~(c,rest)) = T.uncons t
      in (Just (c,rest))
  cons = T.cons

instance TokenStream [tok] tok where
  uncons [] = Nothing
  uncons (x:xs) = Just (x,xs)
  cons = (:)

-- interesting; not useful, but interesting
-- instance (Monad c,TokenStream ts c a) => TokenStream (Parser ts c a) (Parser ts c) a where
--   uncons :: Parser ts c a -> Parser ts c (Maybe (a,Parser ts c a))
--   uncons p = do
--     ts <- getTokens
--     res <- lift $ runParser p ts
--     case res of
--       Done ts' r -> do
--         setTokens ts'
--         return (Just (r,p))
--       _ ->
--         return Nothing

data ParseResult ts e c r
  = Done ts r
  | Failure [e] ts

runParser :: forall t ts e c a. (Monad c, TokenStream ts t) => Parser ts e c a -> ts -> c (ParseResult ts e c a)
runParser (Parser p) = flip go p
  where
    go ts (Return a) = return (Done ts a)
    go ts (Lift m) = m >>= go ts
    go ts (Do m) =
      case prj m of
        ~(Just (c :: Parse ts e (Code '[Parse ts e] c a))) ->
          case c of
            GetTokens f -> go ts (f ts)
            SetTokens ts' k -> go ts' k
            Ann k ak msg -> do
              res <- go ts k
              case res of
                Done ts' a -> go ts' (unsafeCoerce ak a)
                Failure ms _ -> return (Failure ms ts)
            Satisfy ib ik ->
              case uncons ts of
                Nothing -> return (Failure [] ts)
                Just (i,ts') ->
                  if unsafeCoerce ib i then
                    go ts' (unsafeCoerce ik i)
                  else
                    return (Failure [] ts)
            Plus l r ak -> do
              res <- go ts l
              case res of
                Done ts' a -> go ts' (unsafeCoerce ak a)
                Failure ms _ -> do
                  res' <- go ts r
                  case res' of
                    Done ts' a -> go ts' (unsafeCoerce ak a)
                    Failure ms' _ -> return (Failure (ms ++ ms') ts)
            Try f ak -> do
              res <- go ts f
              case res of
                Done ts' a -> go ts' (unsafeCoerce ak a)
                Failure m _ -> return (Failure m ts)
            Failed msg -> return (Failure msg ts)
