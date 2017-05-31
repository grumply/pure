{-# language DeriveDataTypeable #-}
{-# language OverloadedStrings #-}
{-# language TemplateHaskell #-}
{-# language StandaloneDeriving #-}
{-# language PatternSynonyms #-}
{-# language ViewPatterns #-}
{-# language CPP #-}
module Atomic.Attribute where

import Ef.Base hiding (Object,object)

import Data.Txt as T hiding (readIntMaybe)
import qualified Data.Txt as T
import Data.JSON hiding (Options)

import Atomic.Default
import Atomic.FromTxt
import Atomic.ToTxt
import Atomic.Cond
import Atomic.UnsafeEq

import Data.Data
import Data.Maybe
import Data.String
import Data.Typeable
import Data.Void

import GHC.Exts

import qualified Data.Function as F
import Data.List (sortBy)

import Control.Lens (makePrisms,makeLenses)

import Prelude

#ifdef __GHCJS__
import qualified Data.JSString as JSS
import qualified GHCJS.DOM.Types as T
import qualified GHCJS.DOM.Window as W
import qualified GHCJS.DOM.Document as D
import qualified GHCJS.DOM.Location as L
import qualified Data.JSString.Read as T
#else
import qualified Data.Text as JSS
import Data.Aeson (Value(..))
import Data.Text.Read as T
#endif

type Win =
#ifdef __GHCJS__
  W.Window
#else
  ()
#endif

type Doc =
#ifdef __GHCJS__
  D.Document
#else
  ()
#endif

type ENode =
#ifdef __GHCJS__
  T.Element
#else
  ()
#endif

type TNode =
#ifdef __GHCJS__
  T.Text
#else
  ()
#endif

type NNode =
#ifdef __GHCJS__
  T.Node
#else
  ()
#endif

type Loc =
#ifdef __GHCJS__
  L.Location
#else
  ()
#endif

data Options = Options
  { _preventDef :: Bool
  , _stopProp   :: Bool
  } deriving (Eq)

instance Default Options where
  def = Options False False

data Feature (ms :: [* -> *])
  = NullFeature
  | Attribute
    { _attr :: Txt
    , _value :: Txt
    }
  | DelayedAttribute
    { _attr :: Txt
    , _value :: Txt
    }
  | Property
    { _prop :: Txt
    , _value :: Txt
    }
  | DelayedProperty
    { _prop :: Txt
    , _value :: Txt
    }
  | StyleF
    { _stylePairs :: [(Txt,Txt)] }
  | OnE
    { _eventName :: Txt
    , _eventOptions :: Options
    , _eventCreate :: IO () -> ENode -> Obj -> IO (Maybe (Code ms IO ()))
    , _eventListener :: Maybe (IO ())
    }
  | OnWin
    { _eventName :: Txt
    , _eventOptions :: Options
    , _eventWinCreate :: !(IO () -> ENode -> Win -> Obj -> IO (Maybe (Code ms IO ())))
    , _eventListener :: Maybe (IO ())
    }
  | OnDoc
    { _eventName :: Txt
    , _eventOptions :: Options
    , _eventDocCreate :: IO () -> ENode -> Doc -> Obj -> IO (Maybe (Code ms IO ()))
    , _eventListener :: Maybe (IO ())
    }
  | OnBuild
    { _buildEvent :: Code ms IO ()
    }
  | OnDestroy
    { _destroyEvent :: Code ms IO ()
    }
  | OnWillMount
    { _willMountEvent :: ENode -> IO ()
    }
  | OnDidMount
    { _didMountEvent :: ENode -> IO ()
    }
  | forall model. Typeable model => OnUpdate
    { _updateModel :: model
    , _updateEvent :: model -> model -> ENode -> IO ()
    }
  | forall model. Typeable model => OnModel
    { _watchModel :: model
    , _modelEvent :: model -> model -> ENode -> Code ms IO ()
    }
  | OnWillUnmount
    { _willUnmountEvent :: ENode -> IO ()
    }
  | OnDidUnmount
    { _didUnmountEvent :: ENode -> IO ()
    }
  | LinkTo
    { _link :: Txt
    , _eventListener :: Maybe (IO ())
    }
  | SVGLinkTo
    { _link :: Txt
    , _eventListener :: Maybe (IO ())
    }
  | XLink
    { _attr :: Txt
    , _value :: Txt
    }

instance ToJSON (Feature ms) where
  toJSON f =
#ifdef __GHCJS__
    objectValue $
#endif
      go f
    where
      go NullFeature = object [ "type" .= ("null" :: Txt)]
      go (Property k v) = object [ "type" .= ("prop" :: Txt), "prop" .= k, "val" .= v]
      go (DelayedProperty k v) = object [ "type" .= ("dprop" :: Txt), "prop" .= k, "val" .= v]
      go (Attribute k v) = object [ "type" .= ("attr" :: Txt), "attr" .= k, "val" .= v]
      go (DelayedAttribute k v) = object ["type" .= ("dattr" :: Txt), "attr" .= k, "val" .= v]
      go (StyleF ss) = object [ "type" .= ("style" :: Txt), "styles" .= ss ]
      go (LinkTo e _) = object [ "type" .= ("link" :: Txt), "link" .= e]
      go (SVGLinkTo e _) = object [ "type" .= ("svglink" :: Txt), "link" .= e ]
      go (XLink k v) = object [ "type" .= ("xlink" :: Txt), "key" .= k, "val" .= v]
      go _ = object []

instance FromJSON (Feature ms) where
  parseJSON o0 = do
#ifdef __GHCJS__
    flip (withObject "obj") o0 $ \o -> do
#else
      let (Object o) = o0
#endif
      t <- o .: "type"
      case t :: Txt of
        "null" ->
          pure NullFeature
        "attr" -> do
          k <- o .: "attr"
          v <- o .: "val"
          pure $ Attribute k v
        "dattr" -> do
          k <- o .: "attr"
          v <- o .: "val"
          pure $ DelayedAttribute k v
        "prop" -> do
          k <- o .: "prop"
          v <- o .: "val"
          pure $ Property k v
        "dprop" -> do
          k <- o .: "prop"
          v <- o .: "val"
          pure $ DelayedProperty k v
        "style" -> do
          ss <- o .: "styles"
          pure $ StyleF ss
        "link" -> do
          l <- o .: "link"
          pure $ LinkTo l Nothing
        "svglink" -> do
          l <- o .: "link"
          pure $ SVGLinkTo l Nothing
        "xlink" -> do
          k <- o .: "key"
          v <- o .: "val"
          pure $ XLink k v
        _ -> Ef.Base.empty

instance Eq (Feature ms) where
  (==) NullFeature NullFeature = True
  (==) (Property p v) (Property p' v') =
    prettyUnsafeEq p p' && prettyUnsafeEq v v'
  (==) (DelayedProperty p v) (DelayedProperty p' v') =
    prettyUnsafeEq p p' && prettyUnsafeEq v v'
  (==) (Attribute a v) (Attribute a' v') =
    prettyUnsafeEq a a' && prettyUnsafeEq v v'
  (==) (DelayedAttribute a v) (DelayedAttribute a' v') =
    prettyUnsafeEq a a' && prettyUnsafeEq v v'
  (==) (StyleF ss) (StyleF ss') =
    reallyUnsafeEq ss ss' || (==) (sortBy (compare `F.on` fst) ss) (sortBy (compare `F.on` fst) ss')
  (==) (OnE e os ev _) (OnE e' os' ev' _) =
    prettyUnsafeEq e e' && prettyUnsafeEq os os'
  (==) (OnWin e os ev _) (OnWin e' os' ev' _) =
    prettyUnsafeEq e e' && prettyUnsafeEq os os'
  (==) (OnDoc e os ev _) (OnDoc e' os' ev' _) =
    prettyUnsafeEq e e' && prettyUnsafeEq os os'
  (==) (OnBuild e) (OnBuild e') =
    reallyUnsafeEq e e'
  (==) (OnDestroy e) (OnDestroy e') =
    reallyUnsafeEq e e'
  (==) (OnWillMount e) (OnWillMount e') =
    reallyUnsafeEq e e'
  (==) (OnDidMount e) (OnDidMount e') =
    reallyUnsafeEq e e'
  (==) (OnUpdate m f) (OnUpdate m' f') =
    typeOf m == typeOf m' && reallyVeryUnsafeEq m m' && reallyVeryUnsafeEq f f'
  (==) (OnModel m f) (OnModel m' f') =
    typeOf m == typeOf m' && reallyVeryUnsafeEq m m' && reallyVeryUnsafeEq f f'
  (==) (OnWillUnmount e) (OnWillUnmount e') =
    reallyUnsafeEq e e'
  (==) (OnDidUnmount e) (OnDidUnmount e') =
    reallyUnsafeEq e e'
  (==) (LinkTo t _) (LinkTo t' _) =
    prettyUnsafeEq t t'
  (==) (SVGLinkTo t _) (SVGLinkTo t' _) =
    prettyUnsafeEq t t'
  (==) (XLink t _) (XLink t' _) =
    prettyUnsafeEq t t'
  (==) _ _ = False

instance Cond (Feature ms) where
  nil = NullFeature

-- instance IsString (Feature ms) where
--   fromString = Attribute "class" . fromString

-- instance GHC.Exts.IsList (Feature ms) where
--   type Item (Feature ms) = Txt
--   fromList = fromTxt . T.intercalate " "
--   toList (Attribute "class" cs) = T.words cs
--   toList _ = []

-- -- is this a terrible idea?
-- instance GHC.Exts.IsList ([a] -> [a]) where
--   type Item ([a] -> [a]) = a
--   fromList = go
--     where
--       go [] xs' = xs'
--       go (x:xs) xs' = x:go xs xs'
--   toList f = f []

-- instance {-# OVERLAPS #-} IsString [Feature ms] where
--   fromString s = [fromString s]

-- instance FromTxt (Feature ms) where
--   fromTxt = Attribute "class" . fromTxt

-- instance FromTxt [Feature ms] where
--   fromTxt t = [fromTxt t]

pattern Attr k v <- (Attribute k v) where
  Attr k v = Attribute k v

pattern DelayedAttr k v <- (DelayedAttribute k v) where
  DelayedAttr k v = DelayedAttribute k v

pattern Prop k v <- (Property k v) where
  Prop k v = Property k v

pattern DelayedProp k v <- (DelayedProperty k v) where
  DelayedProp k v = DelayedProperty k v

delayedFeature (DelayedAttribute k v) = Attribute k v
delayedFeature (DelayedProperty k v) = Property k v
delayedFeature x = x

delayFeature (Attribute k v) = DelayedAttribute k v
delayFeature (Property k v) = DelayedProperty k v
delayFeature x = x

pattern Delay x <- (delayedFeature -> x) where
  Delay x = delayFeature x

checkNotNull p = if T.null p then (False,p) else (True,p)

pattern BoolProp k b <- (Property k (checkNotNull -> (b,_))) where
  BoolProp k b = Property k (if b then "true" else "")

on :: Txt -> Code ms IO () -> Feature ms
on ev e = OnE ev def (\_ _ _ -> return (Just e)) Nothing

pattern On ev opts f <- (OnE ev opts f _) where
  On ev opts f = OnE ev opts f Nothing

pattern OnDocument ev opts f <- (OnDoc ev opts f _) where
  OnDocument ev opts f = OnDoc ev opts f Nothing

pattern OnWindow ev opts f <- (OnWin ev opts f _) where
  OnWindow ev opts f = OnWin ev opts f Nothing

-- runs when the feature is created; top-down
-- onBuild is guaranteed to run before onDestroy, but ordering w.r.t. mount/update/unmount is undetermined
pattern OnAdd f <- (OnBuild f) where
  OnAdd f = OnBuild f

-- runs when the feature is destroyed; top-down
-- onDestroy is guaranteed to run after onBuild, but ordering w.r.t. mount/update/unmount is undetermined
pattern OnRemove f <- (OnDestroy f) where
  OnRemove f = OnDestroy f

pattern OnMounting f <- (OnWillMount f) where
  OnMounting f = OnWillMount f

pattern OnMounted f <- (OnDidMount f) where
  OnMounted f = OnDidMount f

-- watches a model, as supplied, and calls the callback during feature diffing; top-down
-- make sure the body of the function will not change; must be totally static modulo the model/enode!
-- That is, the only way to witness a value before and after a change is to track it via the model!
-- If it is untracked, the function will only ever see the value after the change because of the
-- way diffing is performed on Features.
pattern Watch mdl f <- (OnModel mdl f) where
  Watch mdl f = OnModel mdl f

-- watches a model, as supplied, and calls the callback during feature diffing; top-down
-- make sure the body of the function will not change; must be totally static modulo the model/enode!
-- That is, the only way to witness a value before and after a change is to track it via the model!
-- If it is untracked, the function will only ever see the value after the change because of the
-- way diffing is performed on Features.
pattern WatchIO mdl f <- (OnUpdate mdl f) where
  WatchIO mdl f = OnUpdate mdl f

-- runs when the attribute is being cleaned up; top-down
pattern OnUnmounting f <- (OnWillUnmount f) where
  OnUnmounting f = OnWillUnmount f

-- runs when the element is remove()'d, not when it is removed from the DOM; bottom-up
pattern OnUnmounted f <- (OnDidUnmount f) where
  OnUnmounted f = OnDidUnmount f

preventDefault :: Feature ms -> Feature ms
preventDefault (OnE ev os f m) = OnE ev (os { _preventDef = True }) f m
preventDefault (OnDoc ev os f m) = OnDoc ev (os { _preventDef = True }) f m
preventDefault (OnWin ev os f m) = OnWin ev (os { _preventDef = True }) f m
preventDefault f = f

stopPropagation :: Feature ms -> Feature ms
stopPropagation (OnE ev os f m) = OnE ev (os { _stopProp = True }) f m
stopPropagation (OnDoc ev os f m) = OnDoc ev (os { _stopProp = True }) f m
stopPropagation (OnWin ev os f m) = OnWin ev (os { _stopProp = True }) f m
stopPropagation f = f

intercept :: Feature ms -> Feature ms
intercept (OnE ev os f m) = OnE ev (os { _preventDef = True, _stopProp = True }) f m
intercept (OnDoc ev os f m) = OnDoc ev (os { _preventDef = True, _stopProp = True }) f m
intercept (OnWin ev os f m) = OnWin ev (os { _preventDef = True, _stopProp = True }) f m
intercept f = f

pattern Styles ss <- (StyleF ss) where
  Styles ss = StyleF ss

-- l for local
pattern Lref l <- (LinkTo l _) where
  Lref l = LinkTo l Nothing

pattern Href v <- (Property "href" v) where
  Href v = Property "href" v

pattern Value v <- (Property "value" v) where
  Value v = Property "value" v

xlink :: Txt -> Txt -> Feature ms
xlink xl v = XLink xl v

pattern SVGLink l <- (SVGLinkTo l _) where
  SVGLink l = SVGLinkTo l Nothing

makePrisms ''Feature
makeLenses ''Feature
makePrisms ''Options
makeLenses ''Options

pattern Classes cs <- (Attribute "class" (T.splitOn " " -> cs)) where
  Classes cs = Attribute "class" $ T.intercalate " " cs

toBool :: Txt -> Bool
toBool t = if t == "" then False else True

fromBool :: Bool -> Txt
fromBool b = if b then "true" else ""

readIntMaybe :: Txt -> Maybe Int
readIntMaybe t =
#ifdef __GHCJS__
  T.readIntMaybe t
#else
  either (\_ -> Nothing) (Just . fst) (T.signed T.decimal t)
#endif

-- not a fan of the inefficiency
-- addClass :: Txt -> [Feature ms] -> [Feature ms]
-- addClass c = go False
--   where
--     go False [] = [Attribute "class" c]
--     go True [] = []
--     go _ ((Attribute "class" cs):fs) = (Attribute "class" (c <> " " <> cs)) : go True fs
--     go b (f:fs) = f:go b fs

pattern Id p <- (Property "id" p) where
  Id p = Property "id" p

pattern TitleP p <- (Property "title" p) where
  TitleP p = Property "title" p

pattern Hidden b <- (Property "hidden" (toBool -> b)) where
  Hidden b = Property "hidden" (fromBool b)

pattern Type p <- (Property "type" p) where
  Type p = Property "type" p

pattern Role v <- (Attribute "role" v) where
  Role v = Attribute "role" v

pattern DefaultValue v <- (Attribute "default-value" v) where
  DefaultValue v = Attribute "default-value" v

pattern Checked b <- (Property "checked" (toBool -> b)) where
  Checked b = Property "checked" (fromBool b)

pattern DefaultChecked <- (Attribute "checked" "checked") where
  DefaultChecked = Attribute "checked" "checked"

pattern Placeholder p <- (Property "placeholder" p) where
  Placeholder p = Property "placeholder" p

pattern Selected b <- (Property "selected" (toBool -> b)) where
  Selected b = Property "selected" (fromBool b)

pattern Accept p <- (Property "accept" p) where
  Accept p = Property "accept" p

pattern AcceptCharset p <- (Property "accept-charset" p) where
  AcceptCharset p = Property "accept-charset" p

pattern Autocomplete b <- (Property "autocomplete" (toBool -> b)) where
  Autocomplete b = Property "autocomplete" (fromBool b)

pattern Autofocus b <- (Property "autofocus" (toBool -> b)) where
  Autofocus b = Property "autofocus" (fromBool b)

pattern Disabled b <- (Property "disabled" (toBool -> b)) where
  Disabled b = Property "disabled" (fromBool b)

pattern Enctyp p <- (Property "enctyp" p) where
  Enctyp p = Property "enctyp" p

pattern Formaction v <- (Attribute "formaction" v) where
  Formaction v = Attribute "formaction" v

pattern ListA v <- (Attribute "list" v) where
  ListA v = Attribute "list" v

pattern Maxlength i <- (Attribute "maxlength" (readIntMaybe -> Just i)) where
  Maxlength i = Attribute "maxlength" (toTxt i)

pattern Minlength i <- (Attribute "minlength" (readIntMaybe -> Just i)) where
  Minlength i = Attribute "minlength" (toTxt i)

pattern Method p <- (Property "method" p) where
  Method p = Property "method" p

pattern Multiple b <- (Property "multiple" (checkNotNull -> (b,_))) where
  Multiple b = Property "multiple" (if b then "multiple" else "")

pattern Muted b <- (Property "muted" (checkNotNull -> (b,_))) where
  Muted b = Property "muted" (if b then "muted" else "")

pattern Name p <- (Property "name" p) where
  Name p = Property "name" p

pattern Novalidate b <- (Property "novalidate" (toBool -> b)) where
  Novalidate b = Property "novalidate" (fromBool b)

pattern Pattern p <- (Property "pattern" p) where
  Pattern p = Property "pattern" p

pattern Readonly b <- (Property "readonly" (toBool -> b)) where
  Readonly b = Property "readonly" (fromBool b)

pattern Required b <- (Property "required" (toBool -> b)) where
  Required b = Property "required" (fromBool b)

pattern Size i <- (Attribute "size" (readIntMaybe -> Just i)) where
  Size i = Attribute "size" (toTxt i)

pattern HtmlFor p <- (Property "htmlFor" p) where
  HtmlFor p = Property "htmlFor" p

pattern FormA v <- (Attribute "form" v) where
  FormA v = Attribute "form" v

pattern Max p <- (Property "max" p) where
  Max p = Property "max" p

pattern Min p <- (Property "min" p) where
  Min p = Property "min" p

pattern Step p <- (Property "step" p) where
  Step p = Property "step" p

pattern Cols i <- (Attribute "cols" (readIntMaybe -> Just i)) where
  Cols i = Attribute "cols" (toTxt i)

pattern Rows i <- (Attribute "rows" (readIntMaybe -> Just i)) where
  Rows i = Attribute "rows" (toTxt i)

pattern Wrap p <- (Property "wrap" p) where
  Wrap p = Property "wrap" p

pattern Target p <- (Property "target" p) where
  Target p = Property "target" p

pattern Download b <- (Property "download" (toBool -> b)) where
  Download b = Property "download" (fromBool b)

pattern DownloadAs p <- (Property "download" (checkNotNull -> (True,p))) where
  DownloadAs p = Property "download" p

pattern Hreflang p <- (Property "hreflang" p) where
  Hreflang p = Property "hreflang" p

pattern Media v <- (Attribute "media" v) where
  Media v = Attribute "media" v

pattern Rel v <- (Attribute "rel" v) where
  Rel v = Attribute "rel" v

pattern Ismap b <- (Property "ismap" (toBool -> b)) where
  Ismap b = Property "ismap" (fromBool b)

pattern Usemap p <- (Property "usemap" p) where
  Usemap p = Property "usemap" p

pattern Shape p <- (Property "shape" p) where
  Shape p = Property "shape" p

pattern Coords p <- (Property "coords" p) where
  Coords p = Property "coords" p

pattern Keytype p <- (Property "keytype" p) where
  Keytype p = Property "keytype" p

pattern Src p <- (Property "src" p) where
  Src p = Property "src" p

pattern Height i <- (Attribute "height" (readIntMaybe -> Just i)) where
  Height i = Attribute "height" (toTxt i)

pattern Width i <- (Attribute "width" (readIntMaybe -> Just i)) where
  Width i = Attribute "width" (toTxt i)

pattern Alt p <- (Property "alt" p) where
  Alt p = Property "alt" p

pattern Autoplay b <- (Property "autoplay" (toBool -> b)) where
  Autoplay b = Property "autoplay" (fromBool b)

pattern Controls b <- (Property "controls" (toBool -> b)) where
  Controls b = Property "controls" (fromBool b)

pattern Loop b <- (Property "loop" (toBool -> b)) where
  Loop b = Property "loop" (fromBool b)

pattern Preload p <- (Property "preload" p) where
  Preload p = Property "preload" p

pattern Poster p <- (Property "poster" p) where
  Poster p = Property "poster" p

pattern Default b <- (Property "default" (toBool -> b)) where
  Default b = Property "default" (fromBool b)

pattern Kind p <- (Property "kind" p) where
  Kind p = Property "kind" p

pattern Srclang p <- (Property "srclang" p) where
  Srclang p = Property "srclang" p

pattern Sandbox p <- (Property "sandbox" p) where
  Sandbox p = Property "sandbox" p

pattern Seamless b <- (Property "seamless" (toBool -> b)) where
  Seamless b = Property "seamless" (fromBool b)

pattern Srcdoc p <- (Property "srcdoc" p) where
  Srcdoc p = Property "srcdoc" p

pattern Reversed b <- (Property "reversed" (toBool -> b)) where
  Reversed b = Property "reversed" (fromBool b)

pattern Start p <- (Property "start" p) where
  Start p = Property "start" p

pattern Align p <- (Property "align" p) where
  Align p = Property "align" p

pattern Colspan i <- (Attribute "colspan" (readIntMaybe -> Just i)) where
  Colspan i = Attribute "colspan" (toTxt i)

pattern Rowspan i <- (Attribute "rowspan" (readIntMaybe -> Just i)) where
  Rowspan i = Attribute "rowspan" (toTxt i)

pattern Headers p <- (Property "headers" p) where
  Headers p = Property "headers" p

pattern Scope p <- (Property "scope" p) where
  Scope p = Property "scope" p

pattern Async b <- (Property "async" (toBool -> b)) where
  Async b = Property "async" (fromBool b)

pattern Charset v <- (Attribute "charset" v) where
  Charset v = Attribute "charset" v

pattern Content p <- (Property "content" p) where
  Content p = Property "content" p

pattern Defer b <- (Property "defer" (toBool -> b)) where
  Defer b = Property "defer" (fromBool b)

pattern HttpEquiv p <- (Property "http-equiv" p) where
  HttpEquiv p = Property "http-equiv" p

pattern Language p <- (Property "language" p) where
  Language p = Property "language" p

pattern Scoped b <- (Property "scoped" (toBool -> b)) where
  Scoped b = Property "scoped" (fromBool b)

pattern Accesskey p <- (Property "accesskey" p) where
  Accesskey p = Property "accesskey" p

pattern Contenteditable b <- (Property "contenteditable" (toBool -> b)) where
  Contenteditable b = Property "contenteditable" (fromBool b)

pattern Contextmenu v <- (Attribute "contextmenu" v) where
  Contextmenu v = Attribute "contextmenu" v

pattern Dir p <- (Property "dir" p) where
  Dir p = Property "dir" p

pattern Draggable b <- (Attribute "draggable" (checkNotNull -> (b,_))) where
  Draggable b = Attribute "draggable" (if b then "true" else "false")

pattern Dropzone p <- (Property "dropzone" p) where
  Dropzone p = Property "dropzone" p

pattern Itemprop v <- (Attribute "itemprop" v) where
  Itemprop v = Attribute "itemprop" v

pattern Lang p <- (Property "lang" p) where
  Lang p = Property "lang" p

pattern Spellcheck b <- (Property "spellcheck" (toBool -> b)) where
  Spellcheck b = Property "spellcheck" (fromBool b)

pattern Tabindex i <- (Attribute "tabindex" (readIntMaybe -> Just i)) where
  Tabindex i = Attribute "tabindex" (toTxt i)

pattern CiteA p <- (Property "cite" p) where
  CiteA p = Property "cite" p

pattern Datetime v <- (Attribute "datetime" v) where
  Datetime v = Attribute "datetime" v

pattern Manifest v <- (Attribute "manifest" v) where
  Manifest v = Attribute "manifest" v

--------------------------------------------------------------------------------
-- SVG Attributes

pattern AccentHeight v <- (Attribute "accent-height" v) where
  AccentHeight v = Attribute "accent-height" v

pattern Accumulate v <- (Attribute "accumulate" v) where
  Accumulate v = Attribute "accumulate" v

pattern Additive v <- (Attribute "additive" v) where
  Additive v = Attribute "additive" v

pattern AlignmentBaseline v <- (Attribute "alignment-baseline" v) where
  AlignmentBaseline v = Attribute "alignment-baseline" v

pattern AllowReorder v <- (Attribute "allowReorder" v) where
  AllowReorder v = Attribute "allowReorder" v

pattern Alphabetic v <- (Attribute "alphabetic" v) where
  Alphabetic v = Attribute "alphabetic" v

pattern ArabicForm v <- (Attribute "arabic-form" v) where
  ArabicForm v = Attribute "arabic-form" v

pattern Ascent v <- (Attribute "ascent" v) where
  Ascent v = Attribute "ascent" v

pattern AttributeName v <- (Attribute "attributeName" v) where
  AttributeName v = Attribute "attributeName" v

pattern AttributeType v <- (Attribute "attributeType" v) where
  AttributeType v = Attribute "attributeType" v

pattern AutoReverse v <- (Attribute "autoReverse" v) where
  AutoReverse v = Attribute "autoReverse" v

pattern Azimuth v <- (Attribute "azimuth" v) where
  Azimuth v = Attribute "azimuth" v

pattern BaseFrequency v <- (Attribute "baseFrequency" v) where
  BaseFrequency v = Attribute "baseFrequency" v

pattern BaslineShift v <- (Attribute "basline-shift" v) where
  BaslineShift v = Attribute "basline-shift" v

pattern BaseProfile v <- (Attribute "baseProfile" v) where
  BaseProfile v = Attribute "baseProfile" v

pattern Bbox v <- (Attribute "bbox" v) where
  Bbox v = Attribute "bbox" v

pattern Begin v <- (Attribute "begin" v) where
  Begin v = Attribute "begin" v

pattern Bias v <- (Attribute "bias" v) where
  Bias v = Attribute "bias" v

pattern By v <- (Attribute "by" v) where
  By v = Attribute "by" v

pattern CalcMode v <- (Attribute "calcMode" v) where
  CalcMode v = Attribute "calcMode" v

pattern CapHeight v <- (Attribute "cap-height" v) where
  CapHeight v = Attribute "cap-height" v

pattern Class v <- (Attribute "class" v) where
  Class v = Attribute "class" v

pattern Clip v <- (Attribute "clip" v) where
  Clip v = Attribute "clip" v

pattern ClipPathUnits v <- (Attribute "clipPathUnits" v) where
  ClipPathUnits v = Attribute "clipPathUnits" v

pattern ClipPath v <- (Attribute "clip-path" v) where
  ClipPath v = Attribute "clip-path" v

pattern ClipRule v <- (Attribute "clip-rule" v) where
  ClipRule v = Attribute "clip-rule" v

pattern Color v <- (Attribute "color" v) where
  Color v = Attribute "color" v

pattern ColorInterpolation v <- (Attribute "color-interpolation" v) where
  ColorInterpolation v = Attribute "color-interpolation" v

pattern ColorInterpolationFilters v <- (Attribute "color-interpolation-filters" v) where
  ColorInterpolationFilters v = Attribute "color-interpolation-filters" v

pattern ColorProfile v <- (Attribute "color-profile" v) where
  ColorProfile v = Attribute "color-profile" v

pattern ColorRendering v <- (Attribute "color-rendering" v) where
  ColorRendering v = Attribute "color-rendering" v

pattern ContentScriptType v <- (Attribute "contentScriptType" v) where
  ContentScriptType v = Attribute "contentScriptType" v

pattern ContentStyleType v <- (Attribute "contentStyleType" v) where
  ContentStyleType v = Attribute "contentStyleType" v

pattern Cursor v <- (Attribute "cursor" v) where
  Cursor v = Attribute "cursor" v

pattern Cx v <- (Attribute "cx" v) where
  Cx v = Attribute "cx" v

pattern Cy v <- (Attribute "cy" v) where
  Cy v = Attribute "cy" v

pattern D v <- (Attribute "d" v) where
  D v = Attribute "d" v

pattern Decelerate v <- (Attribute "decelerate" v) where
  Decelerate v = Attribute "decelerate" v

pattern Descent v <- (Attribute "descent" v) where
  Descent v = Attribute "descent" v

pattern DiffuseConstant v <- (Attribute "diffuseConstant" v) where
  DiffuseConstant v = Attribute "diffuseConstant" v

pattern Direction v <- (Attribute "direction" v) where
  Direction v = Attribute "direction" v

pattern Display v <- (Attribute "display" v) where
  Display v = Attribute "display" v

pattern Divisor v <- (Attribute "divisor" v) where
  Divisor v = Attribute "divisor" v

pattern DominantBaseline v <- (Attribute "dominant-baseline" v) where
  DominantBaseline v = Attribute "dominant-baseline" v

pattern Dur v <- (Attribute "dur" v) where
  Dur v = Attribute "dur" v

pattern Dx v <- (Attribute "dx" v) where
  Dx v = Attribute "dx" v

pattern Dy v <- (Attribute "dy" v) where
  Dy v = Attribute "dy" v

pattern EdgeMode v <- (Attribute "edgeMode" v) where
  EdgeMode v = Attribute "edgeMode" v

pattern Elevation v <- (Attribute "elevation" v) where
  Elevation v = Attribute "elevation" v

pattern EnableBackground v <- (Attribute "enable-background" v) where
  EnableBackground v = Attribute "enable-background" v

pattern End v <- (Attribute "end" v) where
  End v = Attribute "end" v

pattern Exponent v <- (Attribute "exponent" v) where
  Exponent v = Attribute "exponent" v

pattern ExternalResourcesRequired v <- (Attribute "externalResourcesRequired" v) where
  ExternalResourcesRequired v = Attribute "externalResourcesRequired" v

pattern Fill v <- (Attribute "fill" v) where
  Fill v = Attribute "fill" v

pattern FillOpacity v <- (Attribute "fill-opacity" v) where
  FillOpacity v = Attribute "fill-opacity" v

pattern FillRule v <- (Attribute "fill-rule" v) where
  FillRule v = Attribute "fill-rule" v

pattern Filter v <- (Attribute "filter" v) where
  Filter v = Attribute "filter" v

pattern FilterRes v <- (Attribute "filterRes" v) where
  FilterRes v = Attribute "filterRes" v

pattern FilterUnits v <- (Attribute "filterUnits" v) where
  FilterUnits v = Attribute "filterUnits" v

pattern FloodColor v <- (Attribute "flood-color" v) where
  FloodColor v = Attribute "flood-color" v

pattern FontFamily v <- (Attribute "font-family" v) where
  FontFamily v = Attribute "font-family" v

pattern FontSize v <- (Attribute "font-size" v) where
  FontSize v = Attribute "font-size" v

pattern FontSizeAdjust v <- (Attribute "font-size-adjust" v) where
  FontSizeAdjust v = Attribute "font-size-adjust" v

pattern FontStretch v <- (Attribute "font-stretch" v) where
  FontStretch v = Attribute "font-stretch" v

pattern FontStyle v <- (Attribute "font-style" v) where
  FontStyle v = Attribute "font-style" v

pattern FontVariant v <- (Attribute "font-variant" v) where
  FontVariant v = Attribute "font-variant" v

pattern FontWeight v <- (Attribute "font-weight" v) where
  FontWeight v = Attribute "font-weight" v

pattern Format v <- (Attribute "format" v) where
  Format v = Attribute "format" v

pattern From v <- (Attribute "from" v) where
  From v = Attribute "from" v

pattern Fx v <- (Attribute "fx" v) where
  Fx v = Attribute "fx" v

pattern Fy v <- (Attribute "fy" v) where
  Fy v = Attribute "fy" v

pattern G1 v <- (Attribute "g1" v) where
  G1 v = Attribute "g1" v

pattern G2 v <- (Attribute "g2" v) where
  G2 v = Attribute "g2" v

pattern GlyphName v <- (Attribute "glyph-name" v) where
  GlyphName v = Attribute "glyph-name" v

pattern GlyphOrientationHorizontal v <- (Attribute "glyph-orientation-horizontal" v) where
  GlyphOrientationHorizontal v = Attribute "glyph-orientation-horizontal" v

pattern GlyphOrientationVertical v <- (Attribute "glyph-orientation-vertial" v) where
  GlyphOrientationVertical v = Attribute "glyph-orientation-vertical" v

pattern GlyphRef v <- (Attribute "glyphRef" v) where
  GlyphRef v = Attribute "glyphRef" v

pattern GradientTransform v <- (Attribute "gradientTransform" v) where
  GradientTransform v = Attribute "gradientTransform" v

pattern GradientUnits v <- (Attribute "gradientUnits" v) where
  GradientUnits v = Attribute "gradientUnits" v

pattern Hanging v <- (Attribute "hanging" v) where
  Hanging v = Attribute "hanging" v

pattern HorizAdvX v <- (Attribute "horiz-adv-x" v) where
  HorizAdvX v = Attribute "horiz-adv-x" v

pattern HorizOriginX v <- (Attribute "horiz-origin-x" v) where
  HorizOriginX v = Attribute "horiz-origin-x" v

pattern Ideographic v <- (Attribute "ideographic" v) where
  Ideographic v = Attribute "ideographic" v

pattern ImageRendering v <- (Attribute "image-rendering" v) where
  ImageRendering v = Attribute "image-rendering" v

pattern In v <- (Attribute "in" v) where
  In v = Attribute "in" v

pattern In2 v <- (Attribute "in2" v) where
  In2 v = Attribute "in2" v

pattern Intercept v <- (Attribute "intercept" v) where
  Intercept v = Attribute "intercept" v

pattern K v <- (Attribute "k" v) where
  K v = Attribute "k" v

pattern K1 v <- (Attribute "k1" v) where
  K1 v = Attribute "k1" v

pattern K2 v <- (Attribute "k2" v) where
  K2 v = Attribute "k2" v

pattern K3 v <- (Attribute "k3" v) where
  K3 v = Attribute "k3" v

pattern K4 v <- (Attribute "k4" v) where
  K4 v = Attribute "k4" v

pattern KernelMatrix v <- (Attribute "kernelMatrix" v) where
  KernelMatrix v = Attribute "kernelMatrix" v

pattern KernelUnitLength v <- (Attribute "kernelUnitLength" v) where
  KernelUnitLength v = Attribute "kernelUnitLength" v

pattern Kerning v <- (Attribute "kerning" v) where
  Kerning v = Attribute "kerning" v

pattern KeyPoints v <- (Attribute "keyPoints" v) where
  KeyPoints v = Attribute "keyPoints" v

pattern KeySplines v <- (Attribute "keySplines" v) where
  KeySplines v = Attribute "keySplines" v

pattern KeyTimes v <- (Attribute "keyTimes" v) where
  KeyTimes v = Attribute "keyTimes" v

pattern LengthAdjust v <- (Attribute "lengthAdjust" v) where
  LengthAdjust v = Attribute "lengthAdjust" v

pattern LetterSpacing v <- (Attribute "letter-spacing" v) where
  LetterSpacing v = Attribute "letter-spacing" v

pattern LightingColor v <- (Attribute "lighting-color" v) where
  LightingColor v = Attribute "lighting-color" v

pattern LimitingConeAngle v <- (Attribute "limitingConeAngle" v) where
  LimitingConeAngle v = Attribute "limitingConeAngle" v

pattern Local v <- (Attribute "local" v) where
  Local v = Attribute "local" v

pattern MarkerEnd v <- (Attribute "marker-end" v) where
  MarkerEnd v = Attribute "marker-end" v

pattern MarkerMid v <- (Attribute "marker-mid" v) where
  MarkerMid v = Attribute "marker-mid" v

pattern MarkerStart v <- (Attribute "marker-start" v) where
  MarkerStart v = Attribute "marker-start" v

pattern MarkerHeight v <- (Attribute "markerHeight" v) where
  MarkerHeight v = Attribute "markerHeight" v

pattern MarkerUnits v <- (Attribute "markerUnits" v) where
  MarkerUnits v = Attribute "markerUnits" v

pattern MarkerWidth v <- (Attribute "markerWidth" v) where
  MarkerWidth v = Attribute "markerWidth" v

pattern MaskA v <- (Attribute "maskA" v) where
  MaskA v = Attribute "maskA" v

pattern MaskContentUnits v <- (Attribute "maskContentUnits" v) where
  MaskContentUnits v = Attribute "maskContentUnits" v

pattern MaskUnits v <- (Attribute "maskUnits" v) where
  MaskUnits v = Attribute "maskUnits" v

pattern Mathematical v <- (Attribute "mathematical" v) where
  Mathematical v = Attribute "mathematical" v

pattern Mode v <- (Attribute "mode" v) where
  Mode v = Attribute "mode" v

pattern NumOctaves v <- (Attribute "numOctaves" v) where
  NumOctaves v = Attribute "numOctaves" v

pattern Offset v <- (Attribute "offset" v) where
  Offset v = Attribute "offset" v

pattern Onabort v <- (Attribute "onabort" v) where
  Onabort v = Attribute "onabort" v

pattern Onactivate v <- (Attribute "onactivate" v) where
  Onactivate v = Attribute "onactivate" v

pattern Onbegin v <- (Attribute "onbegin" v) where
  Onbegin v = Attribute "onbegin" v

pattern Onclick v <- (Attribute "onclick" v) where
  Onclick v = Attribute "onclick" v

pattern Onend v <- (Attribute "onend" v) where
  Onend v = Attribute "onend" v

pattern Onerror v <- (Attribute "onerror" v) where
  Onerror v = Attribute "onerror" v

pattern Onfocusin v <- (Attribute "onfocusin" v) where
  Onfocusin v = Attribute "onfocusin" v

pattern Onfocusout v <- (Attribute "onfocusout" v) where
  Onfocusout v = Attribute "onfocusout" v

pattern Onload v <- (Attribute "onload" v) where
  Onload v = Attribute "onload" v

pattern Onmousedown v <- (Attribute "onmousedown" v) where
  Onmousedown v = Attribute "onmousedown" v

pattern Onmousemove v <- (Attribute "onmousemove" v) where
  Onmousemove v = Attribute "onmousemove" v

pattern Onmouseout v <- (Attribute "onmouseout" v) where
  Onmouseout v = Attribute "onmouseout" v

pattern Onmouseover v <- (Attribute "onmouseover" v) where
  Onmouseover v = Attribute "onmouseover" v

pattern Onmouseup v <- (Attribute "onmouseup" v) where
  Onmouseup v = Attribute "onmouseup" v

pattern Onrepeat v <- (Attribute "onrepeat" v) where
  Onrepeat v = Attribute "onrepeat" v

pattern Onresize v <- (Attribute "onresize" v) where
  Onresize v = Attribute "onresize" v

pattern Onscroll v <- (Attribute "onscroll" v) where
  Onscroll v = Attribute "onscroll" v

pattern Onunload v <- (Attribute "onunload" v) where
  Onunload v = Attribute "onunload" v

pattern Onzoom v <- (Attribute "onzoom" v) where
  Onzoom v = Attribute "onzoom" v

pattern Opacity v <- (Attribute "opacity" v) where
  Opacity v = Attribute "opacity" v

pattern Operator v <- (Attribute "operator" v) where
  Operator v = Attribute "operator" v

pattern Order v <- (Attribute "order" v) where
  Order v = Attribute "order" v

pattern Orient v <- (Attribute "orient" v) where
  Orient v = Attribute "orient" v

pattern Orientation v <- (Attribute "orientation" v) where
  Orientation v = Attribute "orientation" v

pattern Origin v <- (Attribute "origin" v) where
  Origin v = Attribute "origin" v

pattern Overflow v <- (Attribute "overflow" v) where
  Overflow v = Attribute "overflow" v

pattern OverlinePosition v <- (Attribute "overline-position" v) where
  OverlinePosition v = Attribute "overline-position" v

pattern OverlineThickness v <- (Attribute "overline-thickness" v) where
  OverlineThickness v = Attribute "overline-thickness" v

pattern Panose1 v <- (Attribute "panose-1" v) where
  Panose1 v = Attribute "panose-1" v

pattern PaintOrder v <- (Attribute "paint-order" v) where
  PaintOrder v = Attribute "paint-order" v

pattern PathLength v <- (Attribute "pathLength" v) where
  PathLength v = Attribute "pathLength" v

pattern PatternContentUnits v <- (Attribute "patternContentUnits" v) where
  PatternContentUnits v = Attribute "patternContentUnits" v

pattern PatternTransform v <- (Attribute "patternTransform" v) where
  PatternTransform v = Attribute "patternTransform" v

pattern PatternUnits v <- (Attribute "patternUnits" v) where
  PatternUnits v = Attribute "patternUnits" v

pattern PointerEvents v <- (Attribute "pointer-events" v) where
  PointerEvents v = Attribute "pointer-events" v

pattern Points v <- (Attribute "points" v) where
  Points v = Attribute "points" v

pattern PointsAtX v <- (Attribute "pointsAtX" v) where
  PointsAtX v = Attribute "pointsAtX" v

pattern PointsAtY v <- (Attribute "pointsAtY" v) where
  PointsAtY v = Attribute "pointsAtY" v

pattern PointsAtZ v <- (Attribute "pointsAtZ" v) where
  PointsAtZ v = Attribute "pointsAtZ" v

pattern PreserveAlpha v <- (Attribute "preserveAlpha" v) where
  PreserveAlpha v = Attribute "preserveAlpha" v

pattern PreserveAspectRatio v <- (Attribute "preserveAspectRatio" v) where
  PreserveAspectRatio v = Attribute "preserveAspectRatio" v

pattern PrimitiveUnits v <- (Attribute "primitiveUnits" v) where
  PrimitiveUnits v = Attribute "primitiveUnits" v

pattern R v <- (Attribute "r" v) where
  R v = Attribute "r" v

pattern Radius v <- (Attribute "radius" v) where
  Radius v = Attribute "radius" v

pattern RefX v <- (Attribute "refX" v) where
  RefX v = Attribute "refX" v

pattern RefY v <- (Attribute "refY" v) where
  RefY v = Attribute "refY" v

pattern RenderingIntent v <- (Attribute "rendering-intent" v) where
  RenderingIntent v = Attribute "rendering-intent" v

pattern RepeatCount v <- (Attribute "repeatCount" v) where
  RepeatCount v = Attribute "repeatCount" v

pattern RepeatDur v <- (Attribute "repeatDur" v) where
  RepeatDur v = Attribute "repeatDur" v

pattern RequiredExtensions v <- (Attribute "requiredExtensions" v) where
  RequiredExtensions v = Attribute "requiredExtensions" v

pattern RequiredFeatures v <- (Attribute "requiredFeatures" v) where
  RequiredFeatures v = Attribute "requiredFeatures" v

pattern Restart v <- (Attribute "restart" v) where
  Restart v = Attribute "restart" v

pattern Result v <- (Attribute "result" v) where
  Result v = Attribute "result" v

pattern Rotate v <- (Attribute "rotate" v) where
  Rotate v = Attribute "rotate" v

pattern Rx v <- (Attribute "rx" v) where
  Rx v = Attribute "rx" v

pattern Ry v <- (Attribute "ry" v) where
  Ry v = Attribute "ry" v

pattern Scale v <- (Attribute "scale" v) where
  Scale v = Attribute "scale" v

pattern Seed v <- (Attribute "seed" v) where
  Seed v = Attribute "seed" v

pattern ShapeRendering v <- (Attribute "shape-rendering" v) where
  ShapeRendering v = Attribute "shape-rendering" v

pattern Slope v <- (Attribute "slope" v) where
  Slope v = Attribute "slope" v

pattern Spacing v <- (Attribute "spacing" v) where
  Spacing v = Attribute "spacing" v

pattern SpecularConstant v <- (Attribute "specularConstant" v) where
  SpecularConstant v = Attribute "specularConstant" v

pattern SpecularExponent v <- (Attribute "specularExponent" v) where
  SpecularExponent v = Attribute "specularExponent" v

pattern Speed v <- (Attribute "speed" v) where
  Speed v = Attribute "speed" v

pattern SpreadMethod v <- (Attribute "spreadMethod" v) where
  SpreadMethod v = Attribute "spreadMethod" v

pattern StartOffset v <- (Attribute "startOffset" v) where
  StartOffset v = Attribute "startOffset" v

pattern StdDeviationA v <- (Attribute "stdDeviationA" v) where
  StdDeviationA v = Attribute "stdDeviationA" v

pattern Stemh v <- (Attribute "stemh" v) where
  Stemh v = Attribute "stemh" v

pattern Stemv v <- (Attribute "stemv" v) where
  Stemv v = Attribute "stemv" v

pattern StitchTiles v <- (Attribute "stitchTiles" v) where
  StitchTiles v = Attribute "stitchTiles" v

pattern StopColor v <- (Attribute "stop-color" v) where
  StopColor v = Attribute "stop-color" v

pattern StopOpacity v <- (Attribute "stop-opacity" v) where
  StopOpacity v = Attribute "stop-opacity" v

pattern StrikethroughPosition v <- (Attribute "strikethrough-position" v) where
  StrikethroughPosition v = Attribute "strikethrough-position" v

pattern StrikethroughThickness v <- (Attribute "strikethrough-thickness" v) where
  StrikethroughThickness v = Attribute "strikethrough-thickness" v

pattern StringA v <- (Attribute "string" v) where
  StringA v = Attribute "string" v

pattern Stroke v <- (Attribute "stroke" v) where
  Stroke v = Attribute "stroke" v

pattern StrokeDasharray v <- (Attribute "stroke-dasharray" v) where
  StrokeDasharray v = Attribute "stroke-dasharray" v

pattern StrokeDashoffset v <- (Attribute "stroke-dashoffset" v) where
  StrokeDashoffset v = Attribute "stroke-dashoffset" v

pattern StrokeLinecap v <- (Attribute "stroke-linecap" v) where
  StrokeLinecap v = Attribute "stroke-linecap" v

pattern StrokeLinejoin v <- (Attribute "stroke-linejoin" v) where
  StrokeLinejoin v = Attribute "stroke-linejoin" v

pattern StrokeMiterlimit v <- (Attribute "stroke-miterlimit" v) where
  StrokeMiterlimit v = Attribute "stroke-miterlimit" v

pattern StrokeOpacity v <- (Attribute "stroke-opacity" v) where
  StrokeOpacity v = Attribute "stroke-opacity" v

pattern StrokeWidth v <- (Attribute "stroke-width" v) where
  StrokeWidth v = Attribute "stroke-width" v

pattern SurfaceScale v <- (Attribute "surfaceScale" v) where
  SurfaceScale v = Attribute "surfaceScale" v

pattern SystemLanguage v <- (Attribute "systemLanguage" v) where
  SystemLanguage v = Attribute "systemLanguage" v

pattern TableValues v <- (Attribute "tableValues" v) where
  TableValues v = Attribute "tableValues" v

pattern TargetX v <- (Attribute "targetX" v) where
  TargetX v = Attribute "targetX" v

pattern TargetY v <- (Attribute "targetY" v) where
  TargetY v = Attribute "targetY" v

pattern TextAnchor v <- (Attribute "text-anchor" v) where
  TextAnchor v = Attribute "text-anchor" v

pattern TextDecoration v <- (Attribute "text-decoration" v) where
  TextDecoration v = Attribute "text-decoration" v

pattern TextRendering v <- (Attribute "text-rendering" v) where
  TextRendering v = Attribute "text-rendering" v

pattern TextLength v <- (Attribute "textLength" v) where
  TextLength v = Attribute "textLength" v

pattern To v <- (Attribute "to" v) where
  To v = Attribute "to" v

pattern Transform v <- (Attribute "transform" v) where
  Transform v = Attribute "transform" v

pattern U1 v <- (Attribute "u1" v) where
  U1 v = Attribute "u1" v

pattern U2 v <- (Attribute "u2" v) where
  U2 v = Attribute "u2" v

pattern UnerlinePosition v <- (Attribute "unerline-position" v) where
  UnerlinePosition v = Attribute "unerline-position" v

pattern UnderlineThickness v <- (Attribute "underline-thickness" v) where
  UnderlineThickness v = Attribute "underline-thickness" v

pattern Unicode v <- (Attribute "unicode" v) where
  Unicode v = Attribute "unicode" v

pattern UnicodeBidi v <- (Attribute "unicode-bidi" v) where
  UnicodeBidi v = Attribute "unicode-bidi" v

pattern UnicodeRange v <- (Attribute "unicode-range" v) where
  UnicodeRange v = Attribute "unicode-range" v

pattern UnitsPerEm v <- (Attribute "units-per-em" v) where
  UnitsPerEm v = Attribute "units-per-em" v

pattern VAlphabetic v <- (Attribute "v-alphabetic" v) where
  VAlphabetic v = Attribute "v-alphabetic" v

pattern VHanging v <- (Attribute "v-hanging" v) where
  VHanging v = Attribute "v-hanging" v

pattern VIdeographic v <- (Attribute "v-ideographic" v) where
  VIdeographic v = Attribute "v-ideographic" v

pattern VMathematical v <- (Attribute "v-mathematical" v) where
  VMathematical v = Attribute "v-mathematical" v

pattern Values v <- (Attribute "values" v) where
  Values v = Attribute "values" v

pattern Version v <- (Attribute "version" v) where
  Version v = Attribute "version" v

pattern VertAdvY v <- (Attribute "vert-adv-y" v) where
  VertAdvY v = Attribute "vert-adv-y" v

pattern VertOriginX v <- (Attribute "vert-origin-x" v) where
  VertOriginX v = Attribute "vert-origin-x" v

pattern VerOriginY v <- (Attribute "ver-origin-y" v) where
  VerOriginY v = Attribute "ver-origin-y" v

pattern ViewBox v <- (Attribute "viewBox" v) where
  ViewBox v = Attribute "viewBox" v

pattern ViewTarget v <- (Attribute "viewTarget" v) where
  ViewTarget v = Attribute "viewTarget" v

pattern Visibility v <- (Attribute "visibility" v) where
  Visibility v = Attribute "visibility" v

pattern Widths v <- (Attribute "widths" v) where
  Widths v = Attribute "widths" v

pattern WordSpacing v <- (Attribute "word-spacing" v) where
  WordSpacing v = Attribute "word-spacing" v

pattern WritingMode v <- (Attribute "writing-mode" v) where
  WritingMode v = Attribute "writing-mode" v

pattern X v <- (Attribute "x" v) where
  X v = Attribute "X" v

pattern XHeight v <- (Attribute "xHeight" v) where
  XHeight v = Attribute "xHeight" v

pattern X1 v <- (Attribute "x1" v) where
  X1 v = Attribute "x1" v

pattern X2 v <- (Attribute "x2" v) where
  X2 v = Attribute "x2" v

pattern XChannelSelector v <- (Attribute "xChannelSelector" v) where
  XChannelSelector v = Attribute "xChannelSelector" v

pattern XLinkActuate v <- (XLink "xlink:actuate" v) where
  XLinkActuate v = XLink "xlink:actuate" v

pattern XLinkArcrole v <- (XLink "xlink:arcrole" v) where
  XLinkArcrole v = XLink "xlink:arcrole" v

pattern XLinkHref v <- (XLink "xlink:href" v) where
  XLinkHref v = XLink "xlink:href" v

pattern XLinkRole v <- (XLink "xlink:role" v) where
  XLinkRole v = XLink "xlink:role" v

pattern XLinkShow v <- (XLink "xlink:show" v) where
  XLinkShow v = XLink "xlink:show" v

pattern XLinkTitle v <- (XLink "xlink:title" v) where
  XLinkTitle v = XLink "xlink:title" v

pattern XLinkType v <- (XLink "xlink:type" v) where
  XLinkType v = XLink "xlink:type" v

pattern XMLBase v <- (Attribute "xml:base" v) where
  XMLBase v = Attribute "xml:base" v

pattern XMLLang v <- (Attribute "xml:lang" v) where
  XMLLang v = Attribute "xml:lang" v

pattern XMLSpace v <- (Attribute "xml:space" v) where
  XMLSpace v = Attribute "xml:space" v

pattern Y v <- (Attribute "y" v) where
  Y v = Attribute "y" v

pattern Y1 v <- (Attribute "y1" v) where
  Y1 v = Attribute "y1" v

pattern Y2 v <- (Attribute "y2" v) where
  Y2 v = Attribute "y2" v

pattern YChannelSelector v <- (Attribute "yChannelSelector" v) where
  YChannelSelector v = Attribute "yChannelSelector" v

pattern Z v <- (Attribute "z" v) where
  Z v = Attribute "z" v

pattern ZoomAndPan v <- (Attribute "zoomAndPan" v) where
  ZoomAndPan v = Attribute "zoomAndPan" v

--------------------------------------------------------------------------------
-- Event listener 'Attribute's

pattern OnClick opts f <- (OnE "click" opts f _) where
  OnClick opts f = OnE "click" opts f Nothing

pattern OnDoubleClick opts f <- (OnE "dblclick" opts f _) where
  OnDoubleClick opts f = OnE "dblclick" opts f Nothing

pattern OnMouseDown opts f <- (OnE "mousedown" opts f _) where
  OnMouseDown opts f = OnE "mousedown" opts f Nothing

pattern OnMouseUp opts f <- (OnE "mouseup" opts f _) where
  OnMouseUp opts f = OnE "mouseup" opts f Nothing

pattern OnTouchStart opts f <- (OnE "touchstart" opts f _) where
  OnTouchStart opts f = OnE "touchstart" opts f Nothing

pattern OnTouchEnd opts f <- (OnE "touchend" opts f _) where
  OnTouchEnd opts f = OnE "touchend" opts f Nothing

pattern OnMouseEnter opts f <- (OnE "mouseenter" opts f _) where
  OnMouseEnter opts f = OnE "mouseenter" opts f Nothing

pattern OnMouseLeave opts f <- (OnE "mouseleave" opts f _) where
  OnMouseLeave opts f = OnE "mouseleave" opts f Nothing

pattern OnMouseOver opts f <- (OnE "mouseover" opts f _) where
  OnMouseOver opts f = OnE "mouseover" opts f Nothing

pattern OnMouseOut opts f <- (OnE "mouseout" opts f _) where
  OnMouseOut opts f = OnE "mouseout" opts f Nothing

pattern OnMouseMove opts f <- (OnE "mousemove" opts f _) where
  OnMouseMove opts f = OnE "mousemove" opts f Nothing

pattern OnTouchMove opts f <- (OnE "touchmove" opts f _)  where
  OnTouchMove opts f = OnE "touchmove" opts f Nothing

pattern OnTouchCancel opts f <- (OnE "touchcancel" opts f _) where
  OnTouchCancel opts f = OnE "touchcancel" opts f Nothing

pattern OnInput opts f <- (OnE "input" opts f _) where
  OnInput opts f = OnE "input" opts f Nothing

pattern OnChange opts f <- (OnE "change" opts f _) where
  OnChange opts f = OnE "change" opts f Nothing

pattern OnSubmit opts f <- (OnE "submit" opts f _) where
  OnSubmit opts f = OnE "submit" opts f Nothing

pattern OnBlur opts f <- (OnE "blur" opts f _) where
  OnBlur opts f = OnE "blur" opts f Nothing

pattern OnFocus opts f <- (OnE "focus" opts f _) where
  OnFocus opts f = OnE "focus" opts f Nothing

pattern OnKeyUp opts f <- (OnE "keyup" opts f _) where
  OnKeyUp opts f = OnE "keyup" opts f Nothing

pattern OnKeyDown opts f <- (OnE "keydown" opts f _) where
  OnKeyDown opts f = OnE "keydown" opts f Nothing

pattern OnKeyPress opts f <- (OnE "keypress" opts f _) where
  OnKeyPress opts f = OnE "keypress" opts f Nothing

-- onInput :: (Txt -> Code ms IO ()) -> Feature ms
-- onInput f = on' "input" $ \_ _ -> fmap return $ flip parse $ \o -> do
--   target <- o .: "target"
--   value <- target .: "value"
--   pure $ f value

-- onInputChange :: (Txt -> Code ms IO ()) -> Feature ms
-- onInputChange f = on' "change" $ \_ _ -> fmap return $ flip parse $ \o -> do
--   target <- o .: "target"
--   value <- target .: "value"
--   pure $ f value

-- onCheck :: (Bool -> Code ms IO ()) -> Feature ms
-- onCheck f = on' "change" $ \_ _ -> fmap return $ flip parse $ \o -> do
--   target <- o .: "target"
--   checked <- target .: "checked"
--   pure $ f checked

-- onSubmit :: Code ms IO () -> Feature ms
-- onSubmit e = intercept $ on' "submit" $ \_ _ _ -> return $ Just e

-- onBlur :: Code ms IO () -> Feature ms
-- onBlur = on "blur"

-- onFocus :: Code ms IO () -> Feature ms
-- onFocus = on "focus"

-- onKeyUp :: (Int -> Code ms IO ()) -> Feature ms
-- onKeyUp f = on' "keyup" $ \_ _ -> fmap return $ flip parse $ \o -> do
--   key <- o .: "keyCode"
--   pure $ f key


-- onKeyDown :: (Int -> Code ms IO ()) -> Feature ms
-- onKeyDown f = on' "keydown" $ \_ _ -> fmap return $ flip parse $ \o -> do
--   key <- o .: "keyCode"
--   pure $ f key

-- onKeyPress :: (Int -> Code ms IO ()) -> Feature ms
-- onKeyPress f = on' "keypress" $ \_ _ -> fmap return $ flip parse $ \o -> do
--   key <- o .: "keyCode"
--   pure $ f key

-- ignoreClick :: Feature ms
-- ignoreClick = intercept $ on' "click" $ \_ _ _ -> return Nothing
