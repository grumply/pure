module Pure.Conjurer.Formable (Formable(..),PreviewingForm(..),overwriteTitle) where

import Control.Component as Component
import Pure.Conjurer.Fieldable
import Pure hiding (left,right,current)

import Data.Typeable
import GHC.Generics as G
import GHC.TypeLits
import Unsafe.Coerce

overwriteTitle :: Txt -> CSS ()
overwriteTitle new =
  void do
    has (tag H2) do
      visibility =: hidden
      position =: relative
      after do
        let left = "left" 
        visibility =: visible
        position =: absolute
        left =: 0
        content =: ("'" <> new <> "'")

data PreviewingForm x = PreviewingForm 
  { onSubmit  :: x -> IO ()
  , onPreview :: x -> IO View
  , runForm :: (x -> IO ()) -> x -> View
  , initial :: x
  }

instance Typeable x => Component (PreviewingForm x) where
  data Model (PreviewingForm x) = Model
    { current :: x
    , preview :: Maybe View
    }
    
  initialize PreviewingForm {..} = pure Model 
    { current = initial
    , preview = Nothing
    }

  data Msg (PreviewingForm x)
    = Edit | Preview | Submit | Update x
 
  upon msg PreviewingForm {..} mdl@Model {..} =
    case msg of
      Edit ->
        pure mdl { preview = Nothing }
        
      Preview -> do
        v <- onPreview current
        pure mdl { preview = Just v }

      Submit -> do
        onSubmit current
        pure mdl

      Update x -> do
        pure mdl { current = x }
        
  view PreviewingForm {..} Model {..} 
    | Just p <- preview =
      Div <||>
        [ p
        , Div <||>
          [ Button <| OnClick (\_ -> command (Edit @x))   |> [ "Edit"   ]
          , Button <| OnClick (\_ -> command (Submit @x)) |> [ "Submit" ]
          ]
        ]
    | otherwise =
      Div <||>
        [ runForm (command . Update @x) current
        , Div <||>
          [ Button <| OnClick (\_ -> command (Preview @x)) |> [ "Preview" ]
          , Button <| OnClick (\_ -> command (Submit @x))  |> [ "Submit"  ]
          ]
        ]

class Formable rec where
  form :: (rec -> IO ()) -> (rec -> IO View) -> rec -> View
  default form :: (Typeable rec, Generic rec, GFormable (Rep rec)) => (rec -> IO ()) -> (rec -> IO View) -> rec -> View
  form onSubmit onPreview initial = 
    Component.run PreviewingForm 
      { runForm = \f x -> gform (f . G.to) (G.from x)
      , ..
      }

instance {-# OVERLAPPABLE #-} (Typeable x, Generic x, GFormable (Rep x)) => Formable x 

class GFormable f where
  gform :: (forall x. f x -> IO ()) -> f x -> View

instance (Typeable x, GFormable x) => GFormable (M1 D (MetaData name _m _p _nt) x) where
  gform f (M1 x) = gform (f . M1) x

instance (KnownSymbol name, Typeable x, GFormable x) => GFormable (M1 C (MetaCons name _fix True) x) where
  gform f (M1 x) = 
    Div <| Class (toTxt (symbolVal @name Proxy)) |>
      [ H2 <||> [ txt (symbolVal @name Proxy) ]
      , gform (f . M1) x
      ]

instance 
  ( Typeable x, GFormable x
  ) => GFormable (M1 C (MetaCons name _fix False) x) 
  where
    gform f (M1 x) = gform (f . M1) x

-- Without the strictness annotations, we end up unsafely observing 
-- thunked laziness as a result of the coercions!
data FormableProductState a b = FPS 
  { _left :: !(forall x. a x) 
  , _right :: !(forall x. b x) 
  }

left :: forall a b. Exists (FormableProductState a b) => (forall x. a x)
left = let FPS l _ = get @(FormableProductState a b) in l

right :: forall a b. Exists (FormableProductState a b) => (forall x. b x)
right = let FPS _ r = get @(FormableProductState a b) in r

setLeft :: forall a b x. Modify (FormableProductState a b) => a x -> IO ()
setLeft l = modify @(FormableProductState a b) (\fps -> fps { _left = unsafeCoerce l })

setRight :: forall a b x. Modify (FormableProductState a b) => b x -> IO ()
setRight r = modify @(FormableProductState a b) (\fps -> fps { _right = unsafeCoerce r })

-- Sadly, this instance induces nesting of sequential record fields.
instance 
  ( Typeable a, Typeable b
  , GFormable a, GFormable b
  ) => GFormable ((:*:) a b) 
  where
    gform f (a :*: b) =
      state (FPS (unsafeCoerce a :: a x) (unsafeCoerce b :: b x)) $ 
        Div <||>
          [ gform (\l -> f (unsafeCoerce l :*: unsafeCoerce (right @a @b)) >> setLeft @a @b l) (left @a @b)
          , gform (\r -> f (unsafeCoerce (left @a @b) :*: unsafeCoerce r) >> setRight @a @b r) (right @a @b)
          ]

instance 
  ( GFieldable (M1 S (MetaSel (Just name) _u _s _s') x)
  , KnownSymbol name
  ) => GFormable (M1 S (MetaSel (Just name) _u _s _s') x) 
  where
    gform f x = 
      Div <| Class (toTxt (symbolVal @name Proxy)) |>
        [ Label <||> [ txt (symbolVal @name Proxy) ]
        , gfield f x
        ]

instance GFormable x => GFormable (M1 S (MetaSel Nothing _u _s _s') x) where
  gform f (M1 x) = gform (f . M1) x

