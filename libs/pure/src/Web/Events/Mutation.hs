{-# LANGUAGE CPP, MultiParamTypeClasses, TypeFamilies, DeriveGeneric, DeriveAnyClass, OverloadedStrings, FlexibleContexts, DerivingVia, RankNTypes, MagicHash, TypeApplications, RecordWildCards, ViewPatterns #-}
module Web.Events.Mutation (Mutation(..),Options(..),mutation,mutations) where

import Control.Producer
import Control.Reader
import Control.State
import Data.Default
import Data.DOM hiding (Options)
import Data.Exists
import Data.HTML
import Data.JSON
import Data.Txt
import Data.View

import Control.Arrow ((&&&))
import Control.Monad
import Data.Coerce
import Data.Foldable (for_,traverse_)
import Data.Function ((&))
import Data.IORef
import Data.Maybe
import GHC.Exts
import GHC.Generics as G
import System.IO.Unsafe

#ifdef __GHCJS__
import GHCJS.Marshal.Internal
import JavaScript.Object.Internal as JS (Object(..),create,setProp)
#endif

mutations :: Options -> (Exists Mutation => IO ()) -> View -> View
mutations options f = events @Mutation f (mutation options)

data Mutation 
  = AttributeMutation
    { target :: Node
    , attributeName :: Txt
    , attributeNamespace :: Txt
    , oldAttribute :: Maybe Txt
    , newAttribute :: Maybe JSV
    }
  | ContentMutation
    { target :: Node
    , oldContent :: Maybe Txt
    , newContent :: Maybe Txt
    }
  | ChildrenMutation
    { target :: Node
    , addedNodes :: [Node]
    , removedNodes :: [Node]
    , previousSibling :: Maybe Node
    , nextSibling :: Maybe Node
    }

data Options = Options
  { subtree :: Bool
  , childList :: Bool
  , attributes :: Bool
  , attributeFilter :: [Txt]
  , attributeOldValue :: Bool
  , characterData :: Bool
  , characterDataOldValue :: Bool
  }

instance Default Options where
  def = Options True False False [] False False False

mutation :: Options -> View -> (Producer Mutation => View)
mutation options v = 
  unsafePerformIO (do
    writeIORef ref yield
    join $ atomicModifyIORef' opts $ \os -> 
      (options,unless (isTrue# (reallyUnsafePtrEquality# options os)) $ do
        n <- readIORef node
        if isNull n then pure () else do 
          writeIORef opts options
          join (readIORef shutdown)
          sd <- observeWith n options (\ms -> readIORef ref >>= \f -> traverse_ f ms)
          writeIORef shutdown sd
      )
    ) `seq` OnMounted go v
  where
    {-# NOINLINE opts #-}
    opts :: IORef Options
    opts = unsafePerformIO (newIORef undefined)

    {-# NOINLINE ref #-}
    ref :: IORef (Mutation -> IO ())
    ref = unsafePerformIO (newIORef undefined)

    {-# NOINLINE node #-}
    node :: IORef Node
    node = unsafePerformIO (newIORef (coerce nullJSV :: Node))

    {-# NOINLINE shutdown #-}
    shutdown :: IORef (IO ())
    shutdown = unsafePerformIO (newIORef def)

    {-# NOINLINE go #-}
    go n = do
      writeIORef node n
      writeIORef opts options
      join (readIORef shutdown)
      sd <- observeWith n options (\ms -> readIORef ref >>= \f -> traverse_ f ms)
      writeIORef shutdown sd
      pure (join (readIORef shutdown))

observeWith :: Node -> Options -> ([Mutation] -> IO ()) -> IO (IO ())
observeWith n o f = do
#ifdef __GHCJS__
  obj <- JS.create
  when (subtree o) (JS.setProp "subtree" (pToJSVal True) obj)
  when (childList o) (JS.setProp "childList" (pToJSVal True) obj)
  when (Web.Events.Mutation.attributes o) (JS.setProp "attributes" (pToJSVal True) obj)
  mas <- if Prelude.null (attributeFilter o) then pure Nothing else Just <$> toJSValListOf (attributeFilter o)
  for_ mas $ \as -> JS.setProp "attributeFilter" as obj 
  when (attributeOldValue o) (JS.setProp "attributeOldValue" (pToJSVal True) obj)
  when (characterData o) (JS.setProp "characterData" (pToJSVal True) obj)
  when (characterDataOldValue o) (JS.setProp "characterDataOldValue" (pToJSVal True) obj)
  cb <- syncCallback1 ContinueAsync $ \arr -> do
    Just mutations <- fromJSValListOf arr 
    ms <- traverse mkMutation mutations
    f ms
  obs <- observer_js cb
  observe_js obs n (coerce obj)
  pure (disconnect_js obs >> releaseCallback cb)
#else
  pure (pure ())
#endif

mkMutation :: JSV -> IO Mutation
mkMutation jsv =
#ifdef __GHCJS__
    let
      Just s = jsv .# "type"
      Just t = jsv .# "target"
    in case s of
        "attributes"    -> mkAttributeMutation t jsv
        "characterData" -> mkContentMutation t jsv
        "childList"     -> mkChildrenMutation t jsv
        _               -> error ("Web.Events..Mutation.mkMutation: unknown mutation event type " ++ fromTxt s)
  where
    mkAttributeMutation :: JSV -> JSV -> IO Mutation
    mkAttributeMutation t jsv = do
      let 
        target = coerce t
        attributeName = fromMaybe "" (jsv .# "attributeName")
        attributeNamespace = fromMaybe "" (jsv .# "attributeNamespace")
        oldAttribute = jsv .# "oldValue"
        newAttribute = t .# attributeName
      pure AttributeMutation {..} 

    mkContentMutation :: JSV -> JSV -> IO Mutation
    mkContentMutation t jsv = do
      let 
        target = coerce t
        oldContent = jsv .# "oldValue"
        newContent = t .# "textContent"
      pure ContentMutation {..}

    mkChildrenMutation :: JSV -> JSV -> IO Mutation
    mkChildrenMutation t jsv = do
      let 
        target = coerce t
        previousSibling = (coerce :: Maybe JSV -> Maybe Node) (jsv .# "previousSibling")
        nextSibling = (coerce :: Maybe JSV -> Maybe Node) (jsv .# "nextSibling")
        Just added = jsv .# "addedNodes"
        Just removed = jsv .# "removedNodes"
      (maybe [] (coerce :: [JSV] -> [Node]) -> addedNodes) <- fromJSValListOf added
      (maybe [] (coerce :: [JSV] -> [Node]) -> removedNodes) <- fromJSValListOf removed
      pure ChildrenMutation {..}
#else
  pure (AttributeMutation (coerce ()) def def def def)
#endif

#ifdef __GHCJS__
foreign import javascript safe
  "$r = new MutationObserver($1)" observer_js :: Callback (JSV -> IO ()) -> IO JSV

foreign import javascript safe
  "$1.observe($2,$3)" observe_js :: JSV -> Node -> JSV -> IO ()

foreign import javascript safe
  "$1.disconnect()" disconnect_js :: JSV -> IO ()
#endif
