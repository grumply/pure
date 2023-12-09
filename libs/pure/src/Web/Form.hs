{-# language AllowAmbiguousTypes, DefaultSignatures, BlockArguments, TypeApplications, RankNTypes, ScopedTypeVariables, TypeOperators, NamedFieldPuns, RecordWildCards, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, FlexibleContexts, PolyKinds, OverloadedStrings, InstanceSigs, ViewPatterns, PatternSynonyms #-}
{-# OPTIONS_GHC -O0 #-}
module Web.Form 
  ( Form(..)
  , fractional
  , integral
  , char
  , text
  , string
  , defaulting
  , Submit(..)
  , onSubmit
  ) where

import Control.Monad
import Control.Parallel
import Data.Complex
import Data.Default
import Data.Foldable
import Data.Functor.Identity
import Data.HTML
import Data.IORef
import Data.Key
import Data.List as List
import Data.List.NonEmpty (NonEmpty)
import qualified Data.Map as M
import Data.Marker
import Data.Maybe
import qualified Data.Sequence as Seq
import qualified Data.Set as S
import Data.Tree
import Data.Txt as Txt
import qualified Data.Vector as V
import Data.View hiding (Form,form)
import Data.Void
import GHC.Generics as G
import System.IO.Unsafe
import Text.Read
import Unsafe.Coerce
import Web
import Web.Events
import Data.Events (pattern OnSubmitWith,intercept)

data Submit = Submit
{-# NOINLINE onSubmit #-}
onSubmit :: View -> (Producer Submit => View)
onSubmit = OnSubmitWith intercept (\_ -> yield Submit)

defaulting :: forall a. Default a => View -> (Producer a => View)
defaulting = OnMounted (\_ -> yield (def :: a) >> def)

-- Strange instance. Markers are supposed to be generated server-side, so
-- we use a default 0-marker in cases where the data structure requires one.
-- If one does not exist (creation form), we yield it once. If one does exist,
-- we do nothing, since we could never generate a more valid marker than the
-- existing one.
--
-- All of this is purely for convenience. This can help to avoid the need
-- to either make an alternate structure just for a form or to make the marker
-- optional.
instance (Typeable k, Typeable (a :: k)) => Form (Marker a) where
  form f Nothing = [ consume f (produce False (fromTxt "" :: Marker a)) ]
  form _ _       = []
  
-- Strange instance. Keys are supposed to be generated server-side, so
-- we use a default 0-key in cases where the data structure requires one.
-- If one does not exist (creation form), we yield it once. If one does exist,
-- we do nothing, since we could never generate a more valid marker than the
-- existing one.
--
-- All of this is purely for convenience. This can help to avoid the need
-- to either make an alternate structure just for a form or make the key
-- optional.
instance Form Key where
  form f Nothing = [ consume f (produce False (fromTxt "" :: Key)) ]
  form _ _       = []

fractional :: forall a. (Real a, Fractional a) => a -> (Producer a => View)
fractional a = (realToFrac :: Double -> a) # (fracInternal (realToFrac a))
  where
    fracInternal :: Double -> (Producer Double => View)
    fracInternal n = do
      let
        input :: Exists Input => IO ()
        input | In InputEvent { value } <- it =
          case readMaybe (fromTxt value) of
            Just i -> yield (i :: Double)
            _      -> pure ()

      Input <| Attribute "value" (toTxt (Prelude.show n)) . inputs input

integral :: forall a. Integral a => a -> (Producer a => View)
integral a = (fromIntegral :: Int -> a) # (numInternal (fromIntegral a))
  where
    numInternal :: Int -> (Producer Int => View)
    numInternal n = do
      let
        input :: Exists Input => IO ()
        input | In InputEvent { value } <- it =
          case readMaybe (fromTxt value) of
            Just i -> yield (i :: Int)
            _      -> pure ()

      Input <| Attribute "value" (toTxt (Prelude.show n)) . inputs input

char :: Char -> (Producer Char => View)
char c = 
  let
    input :: Exists Input => IO ()
    input 
      | In InputEvent { value } <- it 
      , Just (c,_) <- Txt.uncons value
      = yield c
      
      | otherwise 
      = pure ()
  in
    Input <| Attribute "value" (Txt.singleton c) . Type "text" . MaxLength 1 . inputs input

text :: forall t. (FromTxt t, ToTxt t) => t -> (Producer t => View)
text t =
  let
    input :: Exists Input => IO ()
    input 
      | In InputEvent { value } <- it 
      = yield (fromTxt value :: t)
      
      | otherwise 
      = pure ()
  in
    Input <| Attribute "value" (toTxt t) . Type "text" . inputs input

string :: String -> (Producer String => View)
string = text

class Typeable a => Form a where
  form :: (a -> IO ()) -> Maybe a -> [View]
  default form :: (Generic a, GForm (Rep a)) => (a -> IO ()) -> Maybe a -> [View]
  form f = gForm (f . (G.to :: Rep a x -> a)) . fmap G.from

class GForm f where
  gForm :: (f p -> IO ()) -> Maybe (f p) -> [View]

instance GForm V1 where
  gForm f _ = [ ]

instance GForm U1 where
  gForm f _ = [ once (f U1) ]

instance Form c => GForm (K1 i c) where
  gForm h = form (h . K1) . fmap unK1

instance (GForm x, Selector s) => GForm (S1 s x) where
  gForm h (fmap unM1 -> mx)
    | selectorName == "" = 
      gForm (h . M1) mx

    | otherwise =
      [ Label <||>
        ( txt selectorName
        : gForm (h . M1) mx
        )
      ]
    where
      selectorName :: String
      selectorName = selName proxy
        where
          proxy :: S1 s x z
          proxy = undefined

instance (GForm f, GForm g) => GForm (f :*: g) where
  gForm :: forall p. ((f :*: g) p -> IO ()) -> Maybe ((f :*: g) p) -> [View]
  gForm h mx = l ++ r
    where
      l = flip gForm (fmap (\(f :*: _) -> f) mx) \c -> join do
            atomicModifyIORef' ref \(_,mc') ->
              ( (Just c,mc')
              , for_ mc' (\c' -> h (c :*: c'))
              ) 
          
      r = flip gForm (fmap (\(_ :*: g) -> g) mx) \c' -> join do
            atomicModifyIORef' ref \(mc,_) ->
              ( (mc,Just c')
              , for_ mc (\c -> h (c :*: c'))
              )

      {-# NOINLINE ref #-}
      ref :: IORef (Maybe (f p),Maybe (g p))
      ref = unsafePerformIO (newIORef (Nothing,Nothing))

instance (GForm x, Constructor c) => GForm (C1 c x) where
  gForm h (fmap unM1 -> mx) =
    [ Fieldset <||>
      ( Legend <||> [ txt constructorName ]
      : gForm (h . M1) mx
      )
    ]
    where
      constructorName :: String
      constructorName = conName proxy
        where
          proxy :: C1 c x p
          proxy = undefined

instance (GForm x, Constructor c) => GForm (D1 d (C1 c x)) where
  gForm h (fmap (unM1 . unM1) -> mx) =
    [ Fieldset <||>
      ( Legend <||> [ txt constructorName ]
      : gForm (h . M1 . M1) mx
      )
    ]
    where
      constructorName :: String
      constructorName = conName proxy
        where
          proxy :: C1 c x p
          proxy = undefined

instance (GForm f, GForm g, GChoices f, GChoices g, Datatype d) => GForm (D1 d (f :+: g)) where
  gForm :: forall p. (D1 d (f :+: g) p -> IO ()) -> Maybe (D1 d (f :+: g) p) -> [View]
  gForm h (fmap unM1 -> mfg) = [ state (List.head cs) multi ]
    where
      all_cs = gChoices @(f :+: g) (h . M1) Nothing
      cs = gChoices @(f :+: g) (h . M1) mfg

      multi :: State (Txt,[View]) => View
      multi = let (l,v) = it in
        Fieldset <||>
          ( Legend <||> [ txt (datatypeName (undefined :: D1 d (f :+: g) p)) ]
          : Select <| changes (let Change ChangeEvent { value } = it in put (value,fromJust (List.lookup value all_cs))) |>
            [ Option <| Value c . (if l == c then Attribute "selected" "" else id) |> [ fromTxt c ]
            | (c,_) <- all_cs
            ]
          : v
          ) 

class GChoices f where
  gChoices :: (f p -> IO ()) -> Maybe (f p) -> [(Txt,[View])]

instance GChoices (S1 s x) where
  gChoices _ _ = []

instance GChoices (f :*: g) where
  gChoices _ _ = []

instance GChoices x => GChoices (D1 d x) where
  gChoices f = gChoices (f . M1) . fmap unM1

instance (Constructor c, GForm x) => GChoices (C1 c x) where
  gChoices f (fmap unM1 -> mx) = 
    [ ( toTxt (conName (undefined :: C1 c x p))
      , gForm (f . M1) mx
      )
    ]

instance (GChoices f, GChoices g) => GChoices (f :+: g) where
  gChoices f mfg = 
    case mfg of
      Just (L1 l) -> gChoices (f . L1) (Just l)
      Just (R1 r) -> gChoices (f . R1) (Just r)
      Nothing     -> gChoices (f . L1) Nothing ++ gChoices (f . R1) Nothing

instance Form a => Form (V.Vector a) where
  form f = form (f . V.fromList) . fmap V.toList
instance (Ord a, Form a) => Form (S.Set a) where
  form f = form (f . S.fromList) . fmap S.toList
instance (Ord a, Form a, Form b) => Form (M.Map a b) where
  form f = form (f . M.fromList) . fmap M.assocs
instance Form a => Form (Seq.Seq a) where
  form f = form (f . Seq.fromList) . fmap toList
instance Form a => Form (Tree a)
instance Form a => Form [a]
instance Form a => Form (Maybe a)
instance (Form a, Form b) => Form (Either a b)
instance Form a => Form (Complex a)
instance Form a => Form (Identity a)
instance Form a => Form (NonEmpty a)
instance (Form a, Form b) => Form (a,b)
instance (Form a, Form b, Form c) => Form (a,b,c)
instance (Form a, Form b, Form c, Form d) => Form (a,b,c,d)
instance (Form a, Form b, Form c, Form d, Form e) => Form (a,b,c,d,e)
instance (Form a, Form b, Form c, Form d, Form e, Form f) => Form (a,b,c,d,e,f)
instance (Form a, Form b, Form c, Form d, Form e, Form f, Form g) => Form (a,b,c,d,e,f,g)
