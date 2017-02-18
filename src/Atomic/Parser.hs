{-# language FunctionalDependencies #-}
{-# language OverloadedStrings #-}
-- lightweight parser
module Atomic.Parser where

import Ef.Base hiding (sequence)

import Atomic.FromTxt
import Atomic.ToTxt

import qualified Data.Txt as T

import Data.Char
import Data.Maybe
import Data.Monoid
import Data.Typeable
import Data.String

import GHC.Exts

import Debug.Trace

import qualified Prelude
import Prelude hiding (take,takeWhile,sequence)


class Monoid s => TokenStream s i | s -> i where
  uncons :: s -> Maybe (i,s)
  cons :: i -> s -> s

instance TokenStream T.Txt Char where
  {-# INLINE uncons #-}
  uncons t =
    if T.null t then
      Nothing
    else
      let ~(Just ~(c,rest)) = T.uncons t
      in (Just (c,rest))
  {-# INLINE cons #-}
  cons = T.cons

instance TokenStream [tok] tok where
  {-# INLINE uncons #-}
  uncons [] = Nothing
  uncons (x:xs) = Just (x,xs)
  {-# INLINE cons #-}
  cons = (:)

data Parse ts e k where
  GetTokens :: !(ts -> k) -> Parse ts e k
  SetTokens :: !ts -> !k -> Parse ts e k
  Satisfy   :: TokenStream ts t => !(t -> Bool) -> !(t -> k) -> Parse ts e k
  Ann       :: !k -> [e] -> Parse ts e k
  Failed    :: [e] -> Parse ts e k

instance Functor (Parse ts e) where
  fmap f (GetTokens tsk) = GetTokens (fmap f tsk)
  fmap f (SetTokens ts k) = SetTokens ts (f k)
  fmap f (Satisfy p tk) = Satisfy p (fmap f tk)
  fmap f (Ann k es) = Ann (f k) es
  fmap f (Failed msg) = (Failed msg)

{-# INLINE getTokens #-}
getTokens :: forall ts e c. (Monad c) => Parser ts e c ts
getTokens = Parser (Send (GetTokens Return :: Parse ts e (Code '[Parse ts e] c ts)))

{-# INLINE setTokens #-}
setTokens :: forall ts e c. (Monad c) => ts -> Parser ts e c ()
setTokens ts = Parser (Send (SetTokens ts (Return ()) :: Parse ts e (Code '[Parse ts e] c ())))

{-# INLINE (<?>) #-}
infix 0 <?>
(<?>) :: forall ts e c a. (Monad c) => Parser ts e c a -> [e] -> Parser ts e c a
(<?>) (Parser p) ann = Parser (Send (Ann p ann :: Parse ts e (Code '[Parse ts e] c a)))

{-# INLINE failed #-}
failed :: forall ts e c r. (Monad c) => [e] -> Parser ts e c r
failed msg = Parser (Send (Failed msg :: Parse ts e (Code '[Parse ts e] c r)))

{-# INLINE satisfy #-}
satisfy :: forall ts e c t. (Monad c, TokenStream ts t) => (t -> Bool) -> Parser ts e c t
satisfy p = Parser (Send (Satisfy p Return :: Parse ts e (Code '[Parse ts e] c t)))

{-# INLINE satisfyWith #-}
satisfyWith :: (Monad c, TokenStream ts t) => (t -> a) -> (a -> Bool) -> Parser ts e c a
satisfyWith f p = do
  t <- f <$> anyToken
  if p t then
    return t
  else do
    failed []

{-# INLINE skip #-}
skip :: (Monad c, TokenStream ts t) => (t -> Bool) -> Parser ts e c ()
skip f = do
  t <- anyToken
  if f t then
    return ()
  else
    failed []

{-# INLINE anyToken #-}
anyToken :: (Monad c, TokenStream ts t) => Parser ts e c t
anyToken = satisfy (const True)

{-# INLINE pushback #-}
pushback :: (Monad c, TokenStream ts t) => t -> Parser ts e c ()
pushback t = getTokens >>= setTokens . cons t

{-# INLINE take #-}
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

{-# INLINE takeWith #-}
takeWith :: (Monad c, Monoid ts, TokenStream ts t) => Int -> (ts -> Bool) -> Parser ts e c ts
takeWith n f = do
  ts <- take n
  if f ts then
    return ts
  else
    failed []

{-# INLINE peek #-}
peek :: (Monad c, TokenStream ts t) => Parser ts e c (Maybe t)
peek = peekN 1

{-# INLINE peekN #-}
peekN :: (Monad c, TokenStream ts t) => Int -> Parser ts e c (Maybe t)
peekN n
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

{-# INLINE takeWhile #-}
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

{-# INLINE takeWhile1 #-}
takeWhile1 :: (Monad c, Monoid ts, TokenStream ts t) => (t -> Bool) -> Parser ts e c ts
takeWhile1 f = do
  c <- satisfy f
  rest <- takeWhile f
  return (cons c rest)

{-# INLINE takeUntil #-}
takeUntil :: (Monad c, TokenStream ts t) => (t -> Bool) -> Parser ts e c ts
takeUntil f = takeWhile (not . f)

{-# INLINE skipWhile #-}
skipWhile :: (Monad c, TokenStream ts t) => (t -> Bool) -> Parser ts e c ()
skipWhile f = go
  where
    go = do
      t <- anyToken
      if f t then
        go
      else
        pushback t

{-# INLINE skipWhile1 #-}
skipWhile1 :: (Monad c, TokenStream ts t) => (t -> Bool) -> Parser ts e c ()
skipWhile1 f = do
  skip f
  skipWhile f

{-# INLINE skipUntil #-}
skipUntil :: (Monad c, TokenStream ts t) => (t -> Bool) -> Parser ts e c ()
skipUntil f = skipWhile (not . f)

{-# INLINE sequence #-}
sequence :: (Monad c, Eq t, TokenStream ts t, TokenStream ts' t) => ts' -> Parser ts e c ts'
sequence ts0 = go ts0
  where
    go ts =
      case uncons ts of
        Nothing -> return ts0
        Just (t,rest) -> do
          satisfy (== t)
          go rest

{-# INLINE string #-}
string :: (Monad c) => T.Txt -> Parser T.Txt e c T.Txt
string s = sequence s

{-# INLINE asciiCI #-}
asciiCI :: (Monad c) => T.Txt -> Parser T.Txt e c T.Txt
asciiCI ts0 = go ts0
  where
    go ts =
      case uncons ts of
        Nothing -> return ts
        Just (t,rest) -> do
          satisfy (\x -> toLower x == toLower t)
          go rest

{-# INLINE char #-}
char :: (Monad c, TokenStream ts Char) => Char -> Parser ts e c Char
char c = satisfy (== c)

{-# INLINE notChar #-}
notChar :: (Monad c, TokenStream ts Char) => Char -> Parser ts e c Char
notChar c = satisfy (/= c)

{-# INLINE endOfLine #-}
endOfLine :: (Monad c) => Parser T.Txt e c ()
endOfLine = (char '\n' >> return ()) <|> (string "\r\n" >> return ())

{-# INLINE endOfInput #-}
endOfInput :: (Monad c, TokenStream ts t) => Parser ts e c ()
endOfInput = do
  ts <- getTokens
  case uncons ts of
    Nothing -> return ()
    Just _ -> failed []

{-# INLINE lookahead #-}
lookahead :: (Monad c) => Parser ts e c a -> Parser ts e c a
lookahead p = do
  ts <- getTokens
  res <- (Just <$> p) <|> pure Nothing
  setTokens ts
  case res of
    Nothing -> failed []
    Just r -> return r

newtype Parser ts e c r = Parser { parser :: Code '[Parse ts e] c r }
instance (Monad c) => Functor (Parser ts e c) where
  {-# INLINE fmap #-}
  fmap f (Parser p) = Parser (fmap f p)
instance (Monad c) => Applicative (Parser ts e c) where
  {-# INLINE pure #-}
  pure = Parser . pure
  {-# INLINE (<*>) #-}
  fab <*> fa = do
    ab <- fab
    a <- fa
    return (ab a)
  {-# INLINE (*>) #-}
  l *> r = l >>= \_ -> r
  {-# INLINE (<*) #-}
  l <* r = l >>= \a -> r >> return a
instance (Monad c) => Monad (Parser ts e c) where
  {-# INLINE return #-}
  return = Parser . return
  {-# INLINE (>>=) #-}
  ma >>= amb = Parser $ parser ma >>= parser . amb
instance MonadTrans (Parser ts e) where
  {-# INLINE lift #-}
  lift = Parser . lift
instance (MonadIO c) => MonadIO (Parser ts e c) where
  {-# INLINE liftIO #-}
  liftIO = Parser . liftIO
instance (Monad c) => Alternative (Parser ts e c) where
  {-# INLINE empty #-}
  empty = failed []
  {-# INLINE (<|>) #-}
  (<|>) l r = do
    ts <- getTokens
    res <- lift $ runParser l ts
    case res of
      Failure ms _ -> do
        res' <- lift $ runParser r ts
        case res' of
          Failure ms' _ -> failed (ms ++ ms')
          Done ts r -> do
            setTokens ts
            return r
      Done ts r -> do
        setTokens ts
        return r
instance (Monad c) => MonadPlus (Parser ts e c) where
  {-# INLINE mzero #-}
  mzero = failed []
  {-# INLINE mplus #-}
  mplus = (<|>)
instance (Monad c) => Monoid (Parser ts e c a) where
  {-# INLINE mempty #-}
  mempty = failed []
  {-# INLINE mappend #-}
  mappend = (<|>)

instance (Monad c,FromTxt e,ts ~ T.Txt,a ~ T.Txt) => IsString (Parser ts e c a) where
  {-# INLINE fromString #-}
  fromString = asciiCI . toTxt

instance (Monad c,FromTxt e,ts ~ T.Txt, a ~ T.Txt) => IsList (Parser ts e c a) where
  type Item (Parser ts e c a) = T.Txt
  {-# INLINE fromList #-}
  fromList = Prelude.foldr1 (<|>) . Prelude.map asciiCI
  toList _ = []

{-# INLINE choice #-}
choice :: Alternative f => [f a] -> f a
choice = foldr (<|>) empty

{-# INLINE option #-}
option :: Alternative f => a -> f a -> f a
option x p = p <|> pure x

{-# INLINE many1 #-}
many1 :: Alternative f => f a -> f [a]
many1 p = liftA2 (:) p (many p)

{-# INLINE sepBy #-}
sepBy :: Alternative f => f a -> f s -> f [a]
sepBy p s = liftA2 (:) p (( s *> sepBy1 p s) <|> pure []) <|> pure []

{-# INLINE sepBy1 #-}
sepBy1 :: Alternative f => f a -> f s -> f [a]
sepBy1 p s = scan
  where
    scan = liftA2 (:) p ((s *> scan) <|> pure [])

{-# INLINE manyTill #-}
manyTill :: Alternative f => f a -> f b -> f [a]
manyTill p end = scan
  where
    scan = (end *> pure []) <|> liftA2 (:) p scan

{-# INLINE someTill #-}
someTill :: Alternative f => f a -> f b -> f [a]
someTill p end = liftA2 (:) p (manyTill p end)

{-# INLINE skipMany #-}
skipMany :: Alternative f => f a -> f ()
skipMany p = scan
  where
    scan = (p *> scan) <|> pure ()

{-# INLINE skipSome #-}
skipSome :: Alternative f => f a -> f ()
skipSome p = p *> skipMany p

{-# INLINE count #-}
count :: Monad m => Int -> m a -> m [a]
count n p = Prelude.sequence (replicate n p)

{-# INLINE eitherP #-}
eitherP :: Alternative f => f a -> f b -> f (Either a b)
eitherP l r = (Left <$> l) <|> (Right <$> r)

data ParseResult ts e r
  = Done ts r
  | Failure [e] ts
  deriving (Show,Functor)


{-# INLINE runParser #-}
runParser :: forall ts e c a. (Monad c) => Parser ts e c a -> ts -> c (ParseResult ts e a)
runParser (Parser p) = flip go p
  where
    {-# INLINE go #-}
    go :: ts -> Code '[Parse ts e] c a -> c (ParseResult ts e a)
    go ts (Do m) =
      case prj m of
        ~(Just (c :: Parse ts e (Code '[Parse ts e] c a))) ->
          case c of
            GetTokens f -> go ts (f ts)
            SetTokens ts' k -> go ts' k
            Satisfy tb tk ->
              case uncons ts of
                Nothing -> return (Failure [] ts)
                Just (t,rest) ->
                  if tb t then
                    go rest (tk t)
                  else
                    return (Failure [] ts)
            Ann k ms -> do
              res <- go ts k
              case res of
                Failure ms' _ -> return (Failure (ms' ++ ms) ts)
                r -> return r
            Failed ms ->
              return (Failure ms ts)
    go ts (Return a) = return (Done ts a)
    go ts (Lift m) = m >>= go ts
