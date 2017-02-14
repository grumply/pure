{-# language TemplateHaskell #-}
{-# language UndecidableInstances #-}
{-# language FunctionalDependencies #-}
module Atomic.Grammar (module Atomic.Grammar) where

import Ef.Base hiding (Token)
import Atomic.API
import Atomic.Revent
import Atomic.Endpoint
import Atomic.Parser
import Atomic.TypeRep
import Atomic.ToTxt
import Data.JSON hiding (Parser,Result,parse)
import Data.Txt hiding (empty,uncons)
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

-- usage: preposition "best" / preposition "Best"
-- produces:
--   > data Best (a :: *) (b :: *)
--   > best :: Proxy Best
--   > best = Proxy
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

-- usage: adverb "quickly" / adverb "Quickly"
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

data Token
  = Word Txt
  | Punctuation Txt
  | Number Txt
  | End
  | NullToken -- ?
  deriving (Eq,Show,Ord,Generic,ToJSON,FromJSON)

data Parsers
  = Parsers
    { pHandlers :: !(forall a. IORef (Map.HashMap Txt (Network a)))
    }

parsers :: (MonadIO c',Monad c,'[State () Parsers] <. ts) => c' (State () Parsers (Action ts c))
parsers = do
  ps <- liftIO (newIORef Map.empty)
  return (state (Parsers $ unsafeCoerce ps))

newtype ProductionRep = ProductionRep Txt
proxyResult :: (Production ms c e a, '[] <: ms, Monad c)
            => Proxy (a :: *)
            -> Produced a
            -> Code '[Parse [Token] e] (Code ms c) (ProductionRep,Produced a)
proxyResult proxy pr = return (ProductionRep (qualRep proxy),pr)

class Typeable a => Production ms c e a | a -> e where -- munging jargon here, but I like this name
  type Produced a :: *
  parse :: Proxy a -> ProductionRep -> Parser [Token] e (Code ms c) (ProductionRep,Produced a)

data ProductionHandler e ms c a
  where
    ProductionHandler
      :: ( Monad c
         , '[Revent,State () Parsers] <: ms
         , Production ms c e a
         , Produced a ~ pr
         )
      => Proxy a
      -> (pr -> Code '[Event pr] (Code ms c) ())
      -> ProductionHandler e ms c a

handle :: (Monad c, '[Revent,State () Parsers] <: ms, Production ms c e a, Produced a ~ pr)
       => Proxy a -> (pr -> Code '[Event pr] (Code ms c) ()) -> ProductionHandler e ms c a
handle = ProductionHandler

consume :: ( MonadIO c
           , '[Revent,State () Parsers] <: ms
           , Production ms c e a
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

type Productions ms c e = API (Production ms c e)

type ProductionHandlers e ms c as = Endpoints (ProductionHandler e) ms c as

instance ( GetHandler (ProductionHandler e) a as'
         , Removed as' a ~ as''
         , DeleteHandler (ProductionHandler e) a as' as''
         , EnactEndpoints (Productions ms c e) (ProductionHandler e) ms c as as''
         , Production ms c e a
         , Produced a ~ pr
         , Typeable a
         , MonadIO c
         )
  => EnactEndpoints (Productions ms c e) (ProductionHandler e) ms c (a ': as) as' where
  enactEndpoints (APICons pa ps) phs =
    case getHandler phs :: ProductionHandler e ms c a of
      ProductionHandler _ f -> do
        let p = Proxy :: Proxy a
            phs' = deleteHandler p phs :: ProductionHandlers e ms c as''
        aph <- consume p ((unsafeCoerce f) :: pr -> Code '[Event pr] (Code ms c) ())
        aps <- enactEndpoints ps phs'
        return $ ActiveEndpointsCons pa aph aps

data Language ms c e as as'
  where
    Language
      :: ( EnactEndpoints (Productions ms c e) (ProductionHandler e) ms c as as'
         , MonadIO c
         )
      => Productions ms c e as
      -> Endpoints (ProductionHandler e) ms c as'
      -> Language ms c e as as'

(<|+|>) :: ( TListAppend (Endpoints (ProductionHandler e) ms c) asl' asr' as'
           , TListAppend (API (Production ms c e)) asl asr as
           , EnactEndpoints (Productions ms c e) (ProductionHandler e) ms c as as'
           )
        => Language ms c e asl asl'
        -> Language ms c e asr asr'
        -> Language ms c e as as'
(Language ps es) <|+|> (Language ps' es') = Language (ps <++> ps') (es <++> es')

class Evaluate ms c as where
  evaluate :: Productions ms c e as -> [e] -> [Token] -> Code ms c [e]

instance (Monad c, Functor (Messages ms)) => Evaluate ms c '[] where
  evaluate _ es _ = return es

instance (MonadIO c, '[State () Parsers] <: ms, Typeable (p :: *), Evaluate ms c ps) => Evaluate ms c (p ': ps) where
  evaluate (APICons _ ps) es t = do
    let r = qualRep (Proxy :: Proxy p)
    res <- runParser (parse (Proxy :: Proxy p) (ProductionRep r)) t
    case res of
      Done _ (ProductionRep rep,r) -> do
        Parsers {..} <- get
        hs <- liftIO (readIORef pHandlers)
        case Map.lookup rep hs of
          Nothing -> return [] -- hmm?
          Just nw -> do
            liftIO $ syndicate (unsafeCoerce nw) r
            return []
      Failure es' _ -> evaluate ps (es ++ es') t

sentences :: ToAPI (Production ms c e) as => PList as -> API (Production ms c e) as
sentences = toAPI

buildLanguage :: Language ms c e as as' -> Code ms c (ActiveEndpoints ms c as)
buildLanguage (Language ps eps) = enactEndpoints ps eps

runLanguage :: Evaluate ms c as => Language ms c e as as' -> [Token] -> Code ms c [e]
runLanguage (Language ps _) t = evaluate ps [] t

instance ToAPI (Production ms c e) '[] where
  toAPI _ = APINull

instance (Production ms c e x, ToAPI (Production ms c e) xs) => ToAPI (Production ms c e) (x ': xs) where
  toAPI (PCons p ps) = APICons p (toAPI ps)

instance FromAPI (Production ms c e) '[] where
  fromAPI _ = PNull

instance (Production ms c e x, FromAPI (Production ms c e) xs) => FromAPI (Production ms c e) (x ': xs) where
  fromAPI (APICons p ps) = PCons p (fromAPI ps)

class Functor c => TokenStream s c i | s -> i where
  uncons :: s -> c (Maybe (i,s))

tokenize :: Txt -> [Token]
tokenize = undefined

