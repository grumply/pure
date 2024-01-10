{-# language AllowAmbiguousTypes, DefaultSignatures, BlockArguments, TypeApplications, RankNTypes, ScopedTypeVariables, TypeOperators, NamedFieldPuns, RecordWildCards, TypeSynonymInstances, FlexibleInstances, ExistentialQuantification, FlexibleContexts, PolyKinds, OverloadedStrings, InstanceSigs, ViewPatterns, PatternSynonyms #-}
{-# OPTIONS_GHC -O0 #-}
module Web.Form 
  ( Form(..)
  , slug
  , slugWith
  , fractional
  , fractionalWith
  , integral
  , integralWith
  , char
  , charWith
  , text
  , textWith
  , string
  , stringWith
  , textarea
  , textareaWith
  , defaulting
  , onSubmit
  , GForm(..)
  , Options(..)
  , bare
  , label
  , labelWith
  , fieldset
  , fieldsetWith
  , header
  , headerWith
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
import Data.Styles (pattern Display,block)
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
import Data.Slug

import Data.Unique

{-# NOINLINE onSubmit #-}
onSubmit :: a -> View -> (Producer a => View)
onSubmit a = OnSubmitWith intercept (\_ -> yield a)

defaulting :: forall a. Default a => View -> (Producer a => View)
defaulting = lifecycle def { onStart = yield (def :: a) }

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
  form opts f mk = [ once (f (fromMaybe (decodeBase16 "") mk)) ]
  
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
  form opts f mk = [ once (f (fromMaybe (toKey (decodeBase16 "" :: Marker ())) mk)) ]

slugWith :: forall a. (View -> View) -> Maybe (Slug a) -> (Producer (Slug a) => View)
slugWith f ms =
  let
    input :: Exists Input => IO ()
    input 
      | In InputEvent { value } <- it 
      = yield (fromTxt value :: Slug a)
      
      | otherwise 
      = pure ()
  in
    lifecycle def { onStart = yield (fromMaybe (fromTxt "") ms) } do
      f (Input <| Value (maybe "" toTxt ms) . Type "text" . inputs input)

slug :: forall a. Maybe (Slug a) -> (Producer (Slug a) => View)
slug = slugWith id

fractionalWith :: forall a. (Real a, Fractional a) => (View -> View) -> Maybe a -> (Producer a => View)
fractionalWith f ma = (realToFrac :: Double -> a) # (fracInternal (maybe 0 realToFrac ma))
  where
    fracInternal :: Double -> (Producer Double => View)
    fracInternal n = do
      let
        input :: Exists Input => IO ()
        input | In InputEvent { value } <- it =
          case readMaybe (fromTxt value) of
            Just i -> yield (i :: Double)
            _      -> pure ()

      lifecycle def { onStart = maybe def yield ma } do
        f (Input <| Attribute "value" (toTxt (Prelude.show n)) . inputs input)

fractional :: forall a. (Real a, Fractional a) => Maybe a -> (Producer a => View)
fractional = fractionalWith id

integralWith :: forall a. Integral a => (View -> View) -> Maybe a -> (Producer a => View)
integralWith f ma = (fromIntegral :: Int -> a) # (numInternal (maybe 0 fromIntegral ma))
  where
    numInternal :: Int -> (Producer Int => View)
    numInternal n = do
      let
        input :: Exists Input => IO ()
        input | In InputEvent { value } <- it =
          case readMaybe (fromTxt value) of
            Just i -> yield (i :: Int)
            _      -> pure ()

      lifecycle def { onStart = maybe def yield ma } do
        f (Input <| Attribute "value" (toTxt (Prelude.show n)) . inputs input)

integral :: forall a. Integral a => Maybe a -> (Producer a => View)
integral = integralWith id

charWith :: (View -> View) -> Maybe Char -> (Producer Char => View)
charWith f mc = go (fromMaybe 'a' mc)
  where
    go c =
      let
        input :: Exists Input => IO ()
        input 
          | In InputEvent { value } <- it 
          , Just (c,_) <- Txt.uncons value
          = yield c
          
          | otherwise 
          = pure ()
      in
        lifecycle def { onStart = maybe def yield mc } do
          f (Input <| Attribute "value" (Txt.singleton c) . Type "text" . MaxLength 1 . inputs input)

char :: Maybe Char -> (Producer Char => View)
char = charWith id

textWith :: forall t. (FromTxt t, ToTxt t) => (View -> View) -> Maybe t -> (Producer t => View)
textWith f mt = go (maybe "" toTxt mt)
  where
    go t = 
      let
        input :: Exists Input => IO ()
        input 
          | In InputEvent { value } <- it 
          = yield (fromTxt value :: t)
          
          | otherwise 
          = pure ()
      in
        lifecycle def { onStart = maybe def yield mt } do
          f (Input <| Attribute "value" t . Type "text" . inputs input)

text :: (ToTxt t, FromTxt t) => Maybe t -> (Producer t => View)
text = textWith id

stringWith :: (View -> View) -> Maybe String -> (Producer String => View)
stringWith = textWith

string :: Maybe String -> (Producer String => View)
string = stringWith id

textareaWith :: forall t. (FromTxt t, ToTxt t) => (View -> View) -> Maybe t -> (Producer t => View)
textareaWith f mt = go (maybe "" toTxt mt)
  where
    go t =
      let
        input :: Exists Input => IO ()
        input 
          | In InputEvent { value } <- it 
          = yield (fromTxt value :: t)
          
          | otherwise 
          = pure ()
      in
        lifecycle def { onStart = maybe def yield mt } do
          f (Textarea <| Type "text" . inputs input |> [ txt t ])

textarea :: forall t. (FromTxt t, ToTxt t) => Maybe t -> (Producer t => View)
textarea = textareaWith id

data Options = Options
  { selector :: Txt -> [View] -> [View]
  , constructor :: Txt -> [View] -> [View]
  , datatype :: Txt -> [View] -> [View]
  }

instance Default Options where
  def = Options label fieldset (header 2)

bare :: Options
bare = Options (const id) (const id) (const id)

fieldsetWith :: ToTxt a => (View -> View) -> (View -> View) -> a -> [View] -> [View]
fieldsetWith f g nm vs = 
  [ Fieldset <| f |> 
    ( Legend <| g |> [ txt nm ]
    : vs
    )
  ]

fieldset :: ToTxt a => a -> [View] -> [View]
fieldset = fieldsetWith id id 

-- Note that, to minimize necessary markup, the <label> wraps the associated
-- `[View]`. This way, a `for` attribute on the label and an `id` on the 
-- inputs - wherever they may exist in the `[View]` - is unnecessary.
labelWith :: ToTxt a => (View -> View) -> a -> [View] -> [View]
labelWith f nm vs =
  [ Label <| f |>
    ( txt nm
    : vs
    )
  ]

label :: ToTxt a => a -> [View] -> [View]
label = labelWith id 

headerWith :: ToTxt a => Int -> (View -> View) -> a -> [View] -> [View]
headerWith level0 f nm vs =
  SimpleHTML ("H" <> toTxt level) <| f |> [ txt nm ] : vs
  where
    level = clamp 1 7 level0

    clamp :: Ord n => n -> n -> n -> n
    clamp lo hi = min hi . max lo

header :: ToTxt a => Int -> a -> [View] -> [View]
header n = headerWith n id

class Typeable a => Form a where
  form :: Options -> (a -> IO ()) -> Maybe a -> [View]
  default form :: (Generic a, GForm (Rep a)) => Options -> (a -> IO ()) -> Maybe a -> [View]
  form opts f = gForm opts (f . (G.to :: Rep a x -> a)) . fmap G.from

class GForm f where
  gForm :: Options -> (f p -> IO ()) -> Maybe (f p) -> [View]

instance GForm V1 where
  gForm _ f _ = [ ]

instance GForm U1 where
  gForm _ f mup = [ weak mup (once (f U1)) ]

instance Form c => GForm (K1 i c) where
  gForm opts h = form opts (h . K1) . fmap unK1

instance (GForm x, Selector s) => GForm (S1 s x) where
  gForm opts h (fmap unM1 -> mx) = 
    selector opts (toTxt selectorName) (gForm opts (h . M1) mx)
    where
      selectorName :: String
      selectorName = selName proxy
        where
          proxy :: S1 s x p
          proxy = undefined

instance (GForm f, GForm g) => GForm (f :*: g) where
  gForm :: forall p. Options -> ((f :*: g) p -> IO ()) -> Maybe ((f :*: g) p) -> [View]
  gForm opts h mx = l ++ r
    where
      l = flip (gForm opts) (fmap (\(f :*: _) -> f) mx) \c -> join do
            atomicModifyIORef' ref \(_,mc') ->
              ( (Just c,mc')
              , for_ mc' (\c' -> h (c :*: c'))
              ) 
          
      r = flip (gForm opts) (fmap (\(_ :*: g) -> g) mx) \c' -> join do
            atomicModifyIORef' ref \(mc,_) ->
              ( (mc,Just c')
              , for_ mc (\c -> h (c :*: c'))
              )

      {-# NOINLINE ref #-}
      ref :: IORef (Maybe (f p),Maybe (g p))
      ref = unsafePerformIO (newIORef (Nothing,Nothing))

instance (GForm x, Constructor c) => GForm (C1 c x) where
  gForm opts h (fmap unM1 -> mx) =
    constructor opts (toTxt constructorName) (gForm opts (h . M1) mx)
    where
      constructorName :: String
      constructorName = conName proxy
        where
          proxy :: C1 c x p
          proxy = undefined

instance (GForm x, Datatype d) => GForm (D1 d x) where
  gForm opts h (fmap unM1 -> mx) = 
    datatype opts (toTxt dataName) (gForm opts (h . M1) mx)
    where
      dataName :: String
      dataName = datatypeName proxy
        where
          proxy :: D1 d x p
          proxy = undefined

instance (GForm f, GForm g, GChoices f, GChoices g) => GForm (f :+: g) where
  gForm :: forall p. Options -> ((f :+: g) p -> IO ()) -> Maybe ((f :+: g) p) -> [View]
  gForm opts h mfg = [ state (List.head cs) multi ]
    where
      all_cs = gChoices @(f :+: g) opts h Nothing
      cs = gChoices @(f :+: g) opts h mfg

      multi :: State (Txt,[View]) => View
      multi = let (l,v) = it in
        Div <||>
          ( Select <| changes (let Change ChangeEvent { value } = it in put (value,fromJust (List.lookup value all_cs))) |>
            [ Option <| Value c . (if l == c then Attribute "selected" "" else id) |> [ fromTxt c ]
            | (c,_) <- all_cs
            ]
          : v
          ) 

class GChoices f where
  gChoices :: Options -> (f p -> IO ()) -> Maybe (f p) -> [(Txt,[View])]

instance GChoices (S1 s x) where
  gChoices _ _ _ = []

instance GChoices (f :*: g) where
  gChoices _ _ _ = []

instance GChoices x => GChoices (D1 d x) where
  gChoices opts f = gChoices opts (f . M1) . fmap unM1

instance (Constructor c, GForm x) => GChoices (C1 c x) where
  gChoices opts f (fmap unM1 -> mx) = 
    [ ( toTxt (conName (undefined :: C1 c x p))
      , gForm opts (f . M1) mx
      )
    ]

instance (GChoices f, GChoices g) => GChoices (f :+: g) where
  gChoices opts f mfg = 
    case mfg of
      Just (L1 l) -> gChoices opts (f . L1) (Just l)
      Just (R1 r) -> gChoices opts (f . R1) (Just r)
      Nothing     -> gChoices opts (f . L1) Nothing ++ gChoices opts (f . R1) Nothing

instance Form a => Form (V.Vector a) where
  form opts f = form opts (f . V.fromList) . fmap V.toList
instance (Ord a, Form a) => Form (S.Set a) where
  form opts f = form opts (f . S.fromList) . fmap S.toList
instance (Ord a, Form a, Form b) => Form (M.Map a b) where
  form opts f = form opts (f . M.fromList) . fmap M.assocs
instance Form a => Form (Seq.Seq a) where
  form opts f = form opts (f . Seq.fromList) . fmap toList
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
