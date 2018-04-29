{-# LANGUAGE CPP #-}
#ifdef USE_TEMPLATE_HASKELL
{-# LANGUAGE TemplateHaskell #-}
#endif
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeOperators #-}
module Pure.Data
  ( module Pure.Data
  , module Export
  ) where

import Pure.Data.CB as Export hiding (Callback)
import Pure.Data.Cond as Export
import Pure.Data.Default as Export
import Pure.Data.Ease as Export
import Pure.Data.Identify as Export
import Pure.Data.JSON as Export
import Pure.Data.JSV -- as Export
import Pure.Data.Key as Export
import Pure.Data.Micros as Export
import Pure.Data.Millis as Export
import Pure.Data.Try as Export
import Pure.Data.UnsafeEq as Export
import Pure.Data.Vault as Export
import Pure.Data.ServerId as Export
import Data.Hashable as Export

import Pure.Data.Txt as Export (Txt,ToTxt(..),FromTxt(..),pattern Translated)
import qualified Pure.Data.Txt as Txt

import Data.List as List

import Ef.Base as Export hiding (child,As,Index,transform,observe,uncons,distribute,embed,render,Nat(..),End,initialize,construct,maps,send,run,(!))

import Pure.Types

import Data.Typeable as Export
import Data.Unique

import Debug.Trace

import System.IO.Unsafe
import Unsafe.Coerce

#ifdef USE_TEMPLATE_HASKELL
import Language.Haskell.TH.Syntax
#endif

import GHC.Generics as Export (Generic(..),to,from)

import Data.Foldable as Export hiding (all,and,any,or)
import Data.Traversable as Export

#ifdef __GHCJS__
foreign import javascript unsafe
  "console.time($1)" timeRenderStart_js :: Txt -> IO ()

foreign import javascript unsafe
  "console.timeEnd($1)" timeRenderEnd_js :: Txt -> IO ()

foreign import javascript unsafe
  "console.timeStamp($1)" timestamp_js :: Txt -> IO ()
#endif

time :: Txt -> IO a -> IO a
time t ioa = do
#ifdef __GHCJS__
  timeRenderStart_js t
  a <- ioa
  timeRenderEnd_js t
#else
  a <- ioa
#endif
  return a

timestamp :: Txt -> IO ()
timestamp t = do
#ifdef __GHCJS__
  timestamp_js t
#else
  return ()
#endif

data Renderable (a :: *) (ms :: [* -> *]) = Render a

instance (Monad c) => Default (Callback_ status result c) where
  def = Callback (\_ -> return ()) (\_ -> return ()) (\_ -> return ())

fresh :: MonadIO c => c Int
fresh = hashUnique <$> liftIO newUnique

parseIp :: String -> String
parseIp str =
  let (ip,_) = List.span (/= ':') str
  in (if List.null ip then "127.0.0.1" else ip)

parsePort :: String -> Int
parsePort str =
  let (_,':':port) = List.span (/= ':') str
  in read port

#ifdef __GHCJS__
foreign import javascript unsafe
  "console.log($1);console.log($2);"
  printAny_js :: Txt -> JSV -> IO ()
#endif

printAny :: (MonadIO c) => Txt -> a -> c ()
printAny label a =
#ifdef __GHCJS__
  liftIO $ printAny_js label (unsafeCoerce a)
#else
  liftIO $ print label 
#endif

traceAny :: Txt -> a -> b -> b
traceAny label a b =
#ifdef __GHCJS__
  let prnt = unsafePerformIO (printAny_js label (unsafeCoerce a))
  in prnt `seq` b
#else
  traceShow label b
#endif

instance FromMillis Micros where
 --  fromMillis :: Millis -> Micros
  fromMillis jt = Micros $ (getMillis jt) * 1000

instance FromMicros Millis where
  -- fromMicrotime :: Micros -> Millis
  -- truncate rather than round
  fromMicros mt = Millis $ (getMicros mt) `Prelude.div` 1000

ghc :: Monad m => m () -> m ()
ghc =
#ifndef __GHCJS__
  id
#else
  const (return ())
#endif

ghcjs :: Monad m => m () -> m ()
ghcjs =
#ifdef __GHCJS__
  id
#else
  const (return ())
#endif

type TxtLike as = Constrain '[FromTxt,ToTxt] as
type JSON as = Constrain '[ToJSON,FromJSON,Typeable] as
type (==>) as c = Constrain '[c] as

-- -- Build a Getter through an intermediate type from an Iso.
-- -- This is equivalent to writing the often impossible combinator
-- --
-- -- > 'l . from l'
-- --
-- -- which, in general, produces a function equivalent to 'id'.
-- --
-- -- This is especially useful when l is overloaded via a typeclass
-- -- as is the case in 'txt'. Thus, a common use is:
-- --
-- -- > via txt
-- --
-- -- to go through a textual intermediary to produce a result, like when
-- -- turning some identifier (w) witnessing ToTxt into a Key for a controller:
-- --
-- -- > key = w ^. via txt
-- via :: (Functor f, Profunctor p, Contravariant f) => Iso s a i i -> Optic' p f s a
-- via f = to (withIso f $ \f t -> t . f)

-- -- translate is equivalent to 'via txt'
-- translated :: (Functor f, Profunctor p, FromTxt a, ToTxt s, Contravariant f)
--            => Optic' p f s a
-- translated = via txt

-- identified :: (Functor f, Identify a, Profunctor p, Contravariant f) => Optic' p f a (I a)
-- identified = to identify

-- named :: (Functor f, Identify a, Profunctor p, Contravariant f) => Optic' p f a (I a)
-- named = to identify

-- renamed :: (Functor f, Identify a, FromTxt x, Profunctor p, ToTxt (I a), Contravariant f)
--         => p x (f x) -> p a (f a)
-- renamed = named . translated

-- txt :: (FromTxt t, ToTxt a) => Iso a t Txt Txt
-- txt = iso toTxt fromTxt

-- pattern Txt a <- (Control.Lens.view txt -> a) where
--   Txt a = review txt a

scoped :: (FromTxt x) => (?scope :: Txt) => Txt -> x
scoped t = fromTxt (?scope <> t)

#ifdef USE_TEMPLATE_HASKELL
this :: Q Exp
this = do
  md <- fmap loc_module qLocation
  let t = dash $ Txt.pack md
  [| fromTxt t |]
#endif

dash :: Txt -> Txt
dash = Txt.map (\x -> if x == '.' then '-' else x)
