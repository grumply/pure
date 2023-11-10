{-# LANGUAGE ViewPatterns, GeneralizedNewtypeDeriving, OverloadedStrings, MultiParamTypeClasses #-}
module Data.Router
  ( Routing(..), RoutingState(..), MonadError(..)
  , getOriginalUrl, getOriginalPath, getOriginalParams
  , getRoutingState, putRoutingState
  , getPath, getParams
  , tryParam, param, path, path', continue, dispatch, match
  , route, route'
  , map
  , runRouting, evalRouting
  , goto
  , stencil
  ) where

import Data.DOM
import Data.Txt (Txt,ToTxt(..),FromTxt(..))
import qualified Data.Txt as Txt
import Data.URI

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.Fail
import Data.String

import Control.Monad.State  as St
import Control.Monad.Except as E
import Control.Monad.IO.Class

import qualified Data.Map as Map

import Prelude hiding (map)

goto :: Txt -> IO ()
goto rt = do
  pushState rt
  popState

--------------------------------------------------------------------------------
-- Routing DSL Type
-- 
-- The continuation monad allows short-circuiting with a route result.
-- The state monad allows delimiting route blocks.
-- The reader monad allows global access to the root route and params.
--
-- Paths are not guaranteed to be valid URI path segments because percent 
-- decoding is applied.

data RoutingState = RoutingState
  { _url      :: Txt
  , _path     :: Txt
  , _params   :: Map.Map Txt Txt
  }

newtype Routing rt a = MkRouting 
  { unRouting :: ExceptT (Maybe rt) (StateT RoutingState IO) a 
  } deriving (Functor,Applicative)

evalRouting :: MonadIO m => Routing rt a -> RoutingState -> m (Either (Maybe rt) a)
evalRouting rtng st = liftIO (evalStateT (runExceptT (unRouting rtng)) st)

runRouting :: MonadIO m => Routing rt a -> RoutingState -> m (Either (Maybe rt) a,RoutingState)
runRouting rtng st = liftIO (runStateT (runExceptT (unRouting rtng)) st)

getRoutingState :: Routing rt RoutingState 
getRoutingState = St.get

putRoutingState :: RoutingState -> Routing rt ()
putRoutingState = St.put

instance MonadState RoutingState (Routing rt) where
  get = MkRouting St.get
  put = MkRouting . St.put

instance MonadError (Maybe rt) (Routing rt) where
  throwError mrt = MkRouting (throwError mrt)
  catchError rtng f = do
    st  <- St.get
    (ema,st') <- runRouting rtng st
    case ema of
      Left Nothing -> f Nothing
      Left (Just rt) -> do
        St.put st'
        f (Just rt)
      Right a -> do
        St.put st'
        pure a

instance FromTxt a => IsString (Routing rt a) where
  fromString = param . toTxt

instance MonadIO (Routing rt) where
  liftIO = MkRouting . liftIO

instance MonadFail (Routing rt) where
  fail _ = MkRouting (throwError Nothing)

instance Monad (Routing rt) where
  return = MkRouting . return
  (>>=) ma amb = MkRouting $ unRouting ma >>= unRouting . amb

instance Alternative (Routing rt) where
  empty = throwError Nothing
  (<|>) rl rr = do
    st@(RoutingState url path params) <- St.get 
    lr <- evalRouting rl st
    case lr of
      Left (Just rt) -> throwError (Just rt)
      Left Nothing   -> rr
      Right a        -> return a

instance MonadPlus (Routing rt) where
  mzero = empty
  -- dubious
  mplus rl rr = do
    st@(RoutingState url path params) <- St.get 
    lr <- evalRouting rl st
    case lr of
      Left (Just rt) -> throwError (Just rt)
      _ -> rr

--------------------------------------------------------------------------------
-- API

getOriginalUrl :: Routing rt Txt
getOriginalUrl = do
  RoutingState url _ _<- St.get
  pure url

getOriginalPath :: Routing rt Txt
getOriginalPath = do
  RoutingState url _ _ <- St.get
  let (p,_) = breakRoute url
  pure p

getOriginalParams :: Routing rt (Map.Map Txt Txt)
getOriginalParams = do
  RoutingState url _ _ <- St.get
  let (_,ps) = breakRoute url
  pure (Map.fromList ps)

getPath :: Routing rt Txt
getPath = do
  RoutingState _ path _ <- St.get
  pure path

getParams :: Routing rt (Map.Map Txt Txt)
getParams = do
  RoutingState _ _ params <- St.get
  pure params

tryParam :: FromTxt a => Txt -> Routing rt (Maybe a)
tryParam p = do
  ps <- getParams
  pure ( fmap fromTxt $ Map.lookup p ps )

param :: FromTxt a => Txt -> Routing rt a
param p = do
  ps <- getParams
  case Map.lookup p ps of
    Nothing -> continue
    Just a  -> pure ( fromTxt a )

path :: Txt -> Routing rt a -> Routing rt (Maybe a)
path stncl rt = do
  st@(RoutingState url path params) <- St.get 
  case stencil stncl path of
    Nothing -> return Nothing
    Just (sub,ps) -> do
      let newRS = RoutingState url sub (Map.union (Map.fromList ps) params)
      lr <- evalRouting rt newRS
      case lr of
        Left (Just rt) -> dispatch rt
        Left Nothing   -> return Nothing
        Right a        -> return (Just a)

path' :: Txt -> Routing rt a -> Routing rt (Maybe a)
path' stncl rt = do
  st@(RoutingState url path params) <- St.get 
  case stencil stncl path of
    Nothing -> return Nothing
    Just (sub,ps) -> do
      let newRS = RoutingState url sub (Map.union (Map.fromList ps) params)
      (lr,st') <- runRouting rt newRS
      case lr of
        Left (Just rt) -> throwError (Just rt)
        Left Nothing   -> return Nothing
        Right a        -> do
          St.put st'
          return (Just a)

map :: (rt -> rt') -> Routing rt a -> Routing rt' a
map f rt = do
  st <- St.get
  (lr,st') <- runRouting rt st
  St.put st'
  case lr of
    Left (Just rt) -> throwError (Just (f rt))
    Left Nothing   -> throwError Nothing
    Right a        -> pure a

continue :: Routing rt a
continue = throwError Nothing

dispatch :: rt -> Routing rt a
dispatch = throwError . Just

match :: Txt -> Routing rt a -> Routing rt (Maybe a)
match pth rt =
  path pth $ do
    p <- getPath
    if Txt.null p then 
      rt 
    else 
      continue

--------------------------------------------------------------------------------
-- DSL executor

route' :: rt -> Routing rt a -> Txt -> IO rt
route' def rt url = do
  let (path,params) = breakRoute url
  fmap (either (maybe def id) id) $ (`evalStateT` (RoutingState url path (Map.fromList params))) $ runExceptT $ 
    unRouting (rt >> return def)

route :: Routing rt a -> Txt -> IO (Maybe (Txt,rt))
route rt url = do
  let (path,params) = breakRoute url
  r <- (`runStateT` (RoutingState url path (Map.fromList params))) $ runExceptT $ 
    unRouting (rt >> return Nothing)
  case r of
    (Left (Just rt),RoutingState _ path _) -> pure (Just (path,rt))
    _ -> pure Nothing

--------------------------------------------------------------------------------
-- Utils

-- | Normalize path, decode as uri, and extract query parameters.
--
-- prop> breakRoute "/has%20space"
-- ("/has space",[])
--
-- prop> breakRoute "/a?p=b"
-- ("/a",[("p","b")])
--
-- prop> breakRoute "/a?p1=b&p2=c"
-- ("/a",[("p1","b"),("p2","c")])
--
breakRoute :: Txt -> (Txt,[(Txt,Txt)])
breakRoute (decodeURI . Txt.takeWhile (/= '#') -> uri) =
  let (path,params0) = Txt.span (/= '?') uri
      params =
        case Txt.uncons params0 of
          Just ('?',qps) -> 
            fmap (second safeTail) .
              fmap (Txt.breakOn "=") .
                Txt.splitOn "&" $
                  qps
          _ -> []
      safeTail x =
        case Txt.uncons x of
          Just (_,rest) -> rest
          _ -> ""
  in (path,params)

-- | Our core matcher pairs the given stencil against a given path segment.
--
-- prop> stencil "/:param" "/test"
-- Just ("",[("param","test")])
--
-- prop> stencil "/:param/a" "/test/a"
-- Just ("",[("param","test")])
--
-- prop> stencil "/a/:param/b" "/a/test/b"
-- Just ("",[("param","test")])
--
-- prop> stencil "/a" "/a/b"
-- Just ("/b",[])
--
-- prop> stencil "/a/:param" "/a/b/c"
-- Just ("/c",[("param","b")])
--
-- prop> stencil "/a" "/b"
-- Nothing
--
-- prop> stencil "/a" "/a/"
-- Just ("",[])
--
stencil :: Txt -> Txt -> Maybe (Txt,[(Txt,Txt)])
stencil = withAcc []
  where
    withAcc acc = go
      where
        go x y =
          -- the DSL doesn't have a method of handling trailing slashes, so they are ignored
          if Txt.null x && (Txt.null y || Txt.null (Txt.dropWhileEnd (== '/') y)) then
            Just (x,acc)
          else
            case (Txt.uncons x,Txt.uncons y) of
              (Just ('/',ps),Just ('/',cs)) -> do
                let
                  (p, ps') = Txt.break (== '/') ps
                  (c_,cs') = Txt.break (== '/') cs
                  c  = decodeURIComponent c_
                case Txt.uncons p of
                  Just (':',pat) -> withAcc ((pat,c):acc) ps' cs'
                  _ | p == c     -> go ps' cs' 
                    | otherwise  -> Nothing

              (Nothing,_) -> Just (y,acc)

              _ -> Nothing


