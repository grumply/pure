{-# language TemplateHaskell #-}
{-# language UndecidableInstances #-}
module Atomic.Grammar (module Atomic.Grammar) where

import Ef.Base
import Atomic.API
import Atomic.Revent
import Atomic.Endpoint
import Atomic.TypeRep
import Atomic.ToTxt
import Data.Txt hiding (empty)
import qualified Data.Txt as T

import qualified Data.HashMap.Strict as Map

import Data.Char
import Data.IORef
import Data.Proxy
import Data.String
import Data.Typeable

import GHC.Exts
import GHC.Generics

import Language.Haskell.TH.Syntax hiding (lift)
import Language.Haskell.TH

import Unsafe.Coerce

pos :: [TyVarBndr] -> String -> Q [Dec]
pos vs nm = do
  let name = mkName nm
      conName = capitalize name
      proxyName = uncapitalize name
  p <- [t|Proxy|]
  return
    [ DataD [] conName vs [] []
    , SigD proxyName (AppT p (ConT conName))
    , ValD (VarP proxyName) (NormalB (ConE 'Proxy)) []
    ]
  where
    uncapitalize = mkName . mapHead Data.Char.toLower . nameBase
    capitalize = mkName . mapHead Data.Char.toUpper . nameBase
    mapHead :: (a -> a) -> [a] -> [a]
    mapHead f [] = []
    mapHead f (x:xs) = f x : xs

-- usage: noun "item" / noun "Item"
-- produces:
--   > data Item
--   > item :: Proxy Item
--   > item = Proxy
noun :: String -> Q [Dec]
noun = pos []

-- usage: verb "create" / verb "Create"
-- produces:
--   > data Create (a :: *)
--   > create :: Proxy Create
--   > create = Proxy
verb :: String -> Q [Dec]
verb = pos [KindedTV (mkName "a") StarT]

-- usage: conjunction "And" / conjunction "and"
-- produces:
--   > data And (a :: k -> *) (b :: k -> *) (c :: k)
--   > and :: Proxy And
--   > and = Proxy
conjunction :: String -> Q [Dec]
conjunction =
  let
    k = VarT (mkName "k")
    kToStar = AppT (AppT ArrowT k) StarT
    t x = AppT x kToStar
    t' = t (AppT ArrowT kToStar)
  in
    pos [KindedTV (mkName "a") kToStar, KindedTV (mkName "b") kToStar, KindedTV (mkName "c") k]

-- usage: preposition "with" / preposition "With"
-- produces:
--   > data With (a :: *) (b :: *)
--   > with :: Proxy With
--   > with = Proxy
preposition :: String -> Q [Dec]
preposition = pos [KindedTV (mkName "a") StarT, KindedTV (mkName "b") StarT]

-- usage: adjective "with" / adjective "With"
-- produces:
--   > data With (a :: *) (b :: *)
--   > with :: Proxy With
--   > with = Proxy
adjective :: String -> Q [Dec]
adjective = pos [KindedTV (mkName "a") StarT]

-- usage: determiner "some" / determiner "Some"
-- produces:
--   > data Some (a :: *)
--   > some :: Proxy Some
--   > some = Proxy
determiner :: String -> Q [Dec]
determiner = pos [KindedTV (mkName "a") StarT]

-- usage: adjective "quickly" / adjective "Quickly"
-- produces:
--   > data Quickly (a :: k -> *) (b :: k)
--   > quickly :: Proxy Quickly
--   > quickly = Proxy
adverb :: String -> Q [Dec]
adverb =
  let
    kTy = VarT (mkName "k")
    verbType = AppT (AppT ArrowT kTy) StarT
  in
    pos [KindedTV (mkName "a") verbType, KindedTV (mkName "b") kTy]

-- usage: pronoun "it" / pronoun "It"
-- produces:
--   > data It
--   > it :: Proxy It
--   > it = Proxy
pronoun :: String -> Q [Dec]
pronoun = pos []

-- usage: interjection "wow" / interjection "Wow"
-- produces:
--   > data Wow
--   > wow :: Proxy Wow
--   > wow = Proxy
interjection :: String -> Q [Dec]
interjection = pos []

-- type Id a = a

data Parse k where
  Next :: (Maybe Char -> k) -> Parse k
  GetInput :: (Txt -> k) -> Parse k
  SetInput :: Txt -> k -> Parse k
  Fail :: Parse k
  deriving Functor

getNext :: forall a ms c. ('[] <: ms, Monad c) => Parser ms c (Maybe Char)
getNext = Parser (Send (Next Return :: Parse (Code '[Parse] (Code ms c) (Maybe Char))))

getInput :: forall a ms c. ('[] <: ms, Monad c) => Parser ms c Txt
getInput = Parser $ Send (GetInput Return :: Parse (Code '[Parse] (Code ms c) Txt))

setInput :: forall a ms c. ('[] <: ms, Monad c) => Txt -> Parser ms c ()
setInput t = Parser $ Send (SetInput t (Return ()) :: Parse (Code '[Parse] (Code ms c) ()))

failed :: forall a ms c r. ('[] <: ms, Monad c) => Parser ms c r
failed = Parser $ Send (Fail :: Parse (Code '[Parse] (Code ms c) r))

try :: (Monad c, '[] <: ms) => Parser ms c () -> Parser ms c ()
try p = do
  inp <- getInput
  r <- Parser $ lift (runParser p inp)
  case r of
    Nothing -> failed
    Just (lo,_) -> setInput lo

-- use: exhaust (spaces <|> punctuation)
exhaust :: (Monad c, '[] <: ms) => Parser ms c () -> Parser ms c ()
exhaust l = do
  inp <- getInput
  r <- Parser $ lift (runParser l inp)
  forM_ r (\(lo,_) -> setInput lo >> exhaust l)

spaces :: (Monad c, '[] <: ms) => Parser ms c ()
spaces = do
  inp <- getInput
  setInput $ T.filter (not . isSpace) inp

punctuation :: (Monad c, '[] <: ms) => Parser ms c ()
punctuation = do
  inp <- getInput
  setInput $ T.filter (not . isPunctuation) inp

string :: (Monad c, '[] <: ms) => Txt -> Parser ms c ()
string t = do
  let l = T.length t
  inp <- getInput
  case T.compareLength inp l of
    LT -> failed
    _ -> do
      let (pre,suf) = T.splitAt l inp
      if pre == t then do
        setInput suf
      else
        failed

takeWithoutPunctuation :: Int -> Txt -> Maybe (Txt,Txt)
takeWithoutPunctuation = go mempty
  where
    go res 0 cnt = Just (T.reverse res,cnt)
    go res n cnt =
      case T.uncons cnt of
        Nothing -> Nothing
        Just (h,r) ->
          if isPunctuation h then
            go res n r
          else
            go (T.cons h res) (n - 1) r

word :: (Monad c, '[] <: ms) => Txt -> Parser ms c ()
word t = do
  let unpunctuated = T.filter isPunctuation t
  inp <- getInput
  case takeWithoutPunctuation (T.length unpunctuated) inp of
    Nothing -> failed
    Just (t',r) ->
      if unpunctuated == t' then
        setInput r
      else
        failed

instance (Monad c,'[] <: ms,a ~ ()) => IsString (Parser ms c a) where
  fromString = try . Prelude.foldr1 (<|>) . Prelude.map word . T.words . toTxt

instance (Monad c,'[] <: ms,a ~ ()) => IsList (Parser ms c a) where
  type Item (Parser ms c a) = String
  fromList = mapM_ (\s -> exhaust (spaces <|> punctuation) >> fromString s)
  toList _ = []

runParser :: forall ms c a. ('[] <: ms, Monad c) => Parser ms c a -> Txt -> Code ms c (Maybe (Txt,a))
runParser (Parser p) = flip go p
  where
    go :: Txt -> Code '[Parse] (Code ms c) a -> Code ms c (Maybe (Txt,a))
    go t (Return a) = return (Just (t,a))
    go t (Lift m) = m >>= go t
    go t (Do m) =
      case prj m of
        Just (c :: Parse (Code '[Parse] (Code ms c) a)) ->
          case c of
            Next f ->
              case T.uncons t of
                Nothing -> go t (f Nothing)
                Just (c,t') -> go t' (f (Just c))
            GetInput f ->
              go t (f t)
            SetInput t' k ->
              go t' k
            Fail ->
              return Nothing
        Nothing ->
          return Nothing

data Parsers
  = Parsers
    { pHandlers :: !(forall a. IORef (Map.HashMap Txt (Network a)))
    }

parsers :: (MonadIO c',Monad c,'[State () Parsers] <. ts) => c' (State () Parsers (Action ts c))
parsers = do
  ps <- liftIO (newIORef Map.empty)
  return (state (Parsers $ unsafeCoerce ps))

newtype ProductionRep = ProductionRep Txt
proxyResult :: (Production ms c a, '[] <: ms, Monad c)
            => Proxy (a :: *)
            -> Produced a
            -> Code '[Parse] (Code ms c) (ProductionRep,Produced a)
proxyResult proxy pr = return (ProductionRep (qualRep proxy),pr)

class Typeable a => Production ms c a where -- munging jargon here, but I like this name
  type Produced a :: *
  parse :: Proxy a -> ProductionRep -> Parser ms c (ProductionRep,Produced a)

newtype Parser ms c r = Parser (Code '[Parse] (Code ms c) r)
instance (Monad c, Functor (Messages ms)) => Functor (Parser ms c) where
  fmap f (Parser p) = Parser (fmap f p)
instance (Monad c, Functor (Messages ms)) => Applicative (Parser ms c) where
  pure = Parser . pure
  (Parser fab) <*> (Parser fa) = Parser (fab <*> fa)
instance (Monad c,Functor (Messages ms)) => Monad (Parser ms c) where
  return = Parser . return
  (Parser ma) >>= amb = Parser $ do
    a <- ma
    let Parser mb = amb a
    mb
  fail _ = failed
instance (Functor (Messages ms)) => MonadTrans (Parser ms) where
  lift = Parser . lift . lift
instance (MonadIO c, Functor (Messages ms)) => MonadIO (Parser ms c) where
  liftIO = Parser . liftIO
instance (Monad c, Functor (Messages ms)) => Alternative (Parser ms c) where
  empty = failed
  l <|> r = do
    inp <- getInput
    res <- Parser $ lift (runParser l inp)
    case res of
      Nothing -> do
        res' <- Parser $ lift (runParser r inp)
        case res' of
          Nothing    -> failed
          Just (_,a) -> return a
      Just (_,a) ->
        return a
instance (Monad c, Functor (Messages ms)) => MonadPlus (Parser ms c) where
  mzero = empty
  mplus = (<|>)
instance (Monad c, Functor (Messages ms)) => Monoid (Parser ms c a) where
  mempty = empty
  mappend = (<|>)

data ProductionHandler ms c a
  where
    ProductionHandler
      :: ( Monad c
         , '[Revent,State () Parsers] <: ms
         , Production ms c a
         , Produced a ~ pr
         )
      => Proxy a
      -> (pr -> Code '[Event pr] (Code ms c) ())
      -> ProductionHandler ms c a

handle :: (Monad c, '[Revent,State () Parsers] <: ms, Production ms c a, Produced a ~ pr)
       => Proxy a -> (pr -> Code '[Event pr] (Code ms c) ()) -> ProductionHandler ms c a
handle = ProductionHandler

consume :: ( MonadIO c
           , '[Revent,State () Parsers] <: ms
           , Production ms c a
           , Produced a ~ pr
           , Typeable a
           )
        => Proxy (a :: *)
        -> (pr -> Code '[Event pr] (Code ms c) ())
        -> Code ms c (Endpoint ms c pr)
consume a_proxy f = do
  buf <- getReventBuffer
  p <- periodical
  newn <- network
  let uniqueRep = qualRep a_proxy
  Just sb <- subscribe p f
  Parsers {..} <- get
  n <- liftIO $ atomicModifyIORef' (unsafeCoerce pHandlers) $ \ihs ->
    case Map.lookup uniqueRep ihs of
      Nothing -> (Map.insert uniqueRep newn ihs,newn)
      Just n  -> (ihs,n)
  joinNetwork n p buf
  return $ Endpoint uniqueRep sb p

type Productions ms c = API (Production ms c)

type ProductionHandlers ms c as = Endpoints ProductionHandler ms c as

instance ( GetHandler ProductionHandler a as'
         , Removed as' a ~ as''
         , DeleteHandler ProductionHandler a as' as''
         , EnactEndpoints (Productions ms c) ProductionHandler ms c as as''
         , Production ms c a
         , Produced a ~ pr
         , Typeable a
         , MonadIO c
         )
  => EnactEndpoints (Productions ms c) ProductionHandler ms c (a ': as) as' where
  enactEndpoints (APICons pa ps) phs =
    case getHandler phs :: ProductionHandler ms c a of
      ProductionHandler _ f -> do
        let p = Proxy :: Proxy a
            phs' = deleteHandler p phs :: ProductionHandlers ms c as''
        aph <- consume p ((unsafeCoerce f) :: pr -> Code '[Event pr] (Code ms c) ())
        aps <- enactEndpoints ps phs'
        return $ ActiveEndpointsCons pa aph aps

data Language ms c as as'
  where
    Language
      :: ( EnactEndpoints (Productions ms c) ProductionHandler ms c as as'
         , MonadIO c
         )
      => Productions ms c as
      -> Endpoints ProductionHandler ms c as'
      -> Language ms c as as'

(<|+|>) :: ( TListAppend (Endpoints ProductionHandler ms c) asl' asr' as'
           , TListAppend (API (Production ms c)) asl asr as
           , EnactEndpoints (Productions ms c) ProductionHandler ms c as as'
           )
        => Language ms c asl asl'
        -> Language ms c asr asr'
        -> Language ms c as as'
(Language ps es) <|+|> (Language ps' es') = Language (ps <++> ps') (es <++> es')

class Evaluate ms c as where
  evaluate :: Productions ms c as -> Txt -> Code ms c (Maybe ())

instance (Monad c, Functor (Messages ms)) => Evaluate ms c '[] where
  evaluate _ _ = return Nothing

instance (MonadIO c, '[State () Parsers] <: ms, Typeable (p :: *), Evaluate ms c ps) => Evaluate ms c (p ': ps) where
  evaluate (APICons _ ps) t = do
    let r = qualRep (Proxy :: Proxy p)
    res <- runParser (parse (Proxy :: Proxy p) (ProductionRep r)) t
    case res of
      Nothing ->
        evaluate ps t
      Just (_,(ProductionRep r,res)) -> do
        Parsers {..} <- get
        hs <- liftIO (readIORef pHandlers)
        case Map.lookup r hs of
          Nothing -> return (Just ()) -- or Nothing?
          Just nw -> do
            liftIO $ syndicate (unsafeCoerce nw) res
            return (Just ())

sentences :: ToAPI (Production ms c) as => PList as -> API (Production ms c) as
sentences = toAPI

buildLanguage :: Language ms c as as' -> Code ms c (ActiveEndpoints ms c as)
buildLanguage (Language ps eps) = enactEndpoints ps eps

runLanguage :: Evaluate ms c as => Language ms c as as' -> Txt -> Code ms c (Maybe ())
runLanguage (Language ps _) t = evaluate ps t

instance ToAPI (Production ms c) '[] where
  toAPI _ = APINull

instance (Production ms c x, ToAPI (Production ms c) xs) => ToAPI (Production ms c) (x ': xs) where
  toAPI (PCons p ps) = APICons p (toAPI ps)

instance FromAPI (Production ms c) '[] where
  fromAPI _ = PNull

instance (Production ms c x, FromAPI (Production ms c) xs) => FromAPI (Production ms c) (x ': xs) where
  fromAPI (APICons p ps) = PCons p (fromAPI ps)
