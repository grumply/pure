{-# language TemplateHaskell #-}
module Atomic.Grammar (module Atomic.Grammar, mkName) where

import Ef.Base
import Atomic.API
import Atomic.Revent
import Atomic.Endpoint
import Atomic.TypeRep

import qualified Data.HashMap.Strict as Map
import Data.IORef
import Data.Txt hiding (empty)
import qualified Data.Txt as T
import Data.Typeable

import Data.Char

import GHC.Generics

import Data.Proxy

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
  Fail :: [Txt] -> Parse k
  deriving Functor

getNext :: forall a ms c. ('[] <: ms, Monad c) => Parser ms c (Maybe Char)
getNext = Parser (Send (Next Return :: Parse (Code '[Parse] (Code ms c) (Maybe Char))))

getInput :: forall a ms c. ('[] <: ms, Monad c) => Parser ms c Txt
getInput = Parser $ Send (GetInput Return :: Parse (Code '[Parse] (Code ms c) Txt))

setInput :: forall a ms c. ('[] <: ms, Monad c) => Txt -> Parser ms c ()
setInput t = Parser $ Send (SetInput t (Return ()) :: Parse (Code '[Parse] (Code ms c) ()))

failed :: forall a ms c r. ('[] <: ms, Monad c) => [Txt] -> Parser ms c r
failed sugs = Parser $ Send (Fail sugs :: Parse (Code '[Parse] (Code ms c) r))

runParser :: forall ms c a. ('[] <: ms, Monad c) => Parser ms c a -> Txt -> Code ms c (Either [Txt] a)
runParser (Parser p) = flip go p
  where
    go :: Txt -> Code '[Parse] (Code ms c) a -> Code ms c (Either [Txt] a)
    go t (Return a) = return (Right a)
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
            Fail ts ->
              return (Left ts)
        Nothing ->
          return (Left [])

data Parsers
  = Parsers
    { pHandlers :: !(forall a. IORef (Map.HashMap Txt (Network a)))
    }

class Typeable a => Production ms c a where -- munging jargon here, but I like this name
  type Produced a :: *
  parse :: Proxy a -> Txt -> Parser ms c (Txt,Produced a)

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
instance (Functor (Messages ms)) => MonadTrans (Parser ms) where
  lift = Parser . lift . lift
instance (MonadIO c, Functor (Messages ms)) => MonadIO (Parser ms c) where
  liftIO = Parser . liftIO
instance (Monad c, Functor (Messages ms)) => Alternative (Parser ms c) where
  empty = failed []
  l <|> r = do
    inp <- getInput
    res <- Parser $ lift (runParser l inp)
    case res of
      Left xs -> do
        res' <- Parser $ lift (runParser r inp)
        case res' of
          Left xs' -> failed (xs ++ xs')
          Right a -> return a
      Right a ->
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
      -> (pr -> Code ms c ())
      -> ProductionHandler ms c a

handle :: (Monad c, '[Revent,State () Parsers] <: ms, Production ms c a, Produced a ~ pr)
       => Proxy a -> (pr -> Code ms c ()) -> ProductionHandler ms c a
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
  evaluate :: Productions ms c as -> [Txt] -> Txt -> Code ms c (Either [Txt] ())

instance (Monad c, Functor (Messages ms)) => Evaluate ms c '[] where
  evaluate _ acc _ = return (Left acc)

instance (MonadIO c, '[State () Parsers] <: ms, Typeable (p :: *), Evaluate ms c ps) => Evaluate ms c (p ': ps) where
  evaluate (APICons _ ps) acc t = do
    let r = qualRep (Proxy :: Proxy p)
    res <- runParser (parse (Proxy :: Proxy p) r) t
    case res of
      Left xs ->
        evaluate ps (acc ++ xs) t
      Right res -> do
        Parsers {..} <- get
        hs <- liftIO (readIORef pHandlers)
        case Map.lookup r hs of
          Nothing -> return (Right ()) -- or (Left acc)?
          Just nw -> do
            liftIO $ syndicate (unsafeCoerce nw) res
            return (Right ())

runLanguage (Language ps _) t = evaluate ps [] t
