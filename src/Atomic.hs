{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE PolyKinds #-}
module Atomic
  ( module Atomic
  , module Export
  ) where

-- This module is a bit of a sin bin

import Ef.Base as Export hiding (child,As,Index,transform,observe,uncons,distribute,embed,render,Nat(..),End,initialize,construct,maps)

import Atomic.Base          as Export hiding (String,Text,Null,(!),hashed)

import qualified Data.Txt as Txt

import Data.IORef
import Data.Typeable

import Debug.Trace

import System.IO.Unsafe
import Unsafe.Coerce

import Language.Haskell.TH.Syntax
import Language.Haskell.TH

-- grid :: StaticCSS
-- grid = $( let g = staticCSS flexboxGrid in [| g |] )

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

this :: Q Exp
this = do
  md <- fmap loc_module qLocation
  let t = dash $ Txt.pack md
  [| fromTxt t |]

dash :: Txt -> Txt
dash = Txt.map (\x -> if x == '.' then '-' else x)
