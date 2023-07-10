{-# language PatternSynonyms, BlockArguments, LambdaCase, ScopedTypeVariables, FlexibleContexts, TypeApplications, DataKinds, BangPatterns #-}
module Effect.Suspense where

import Control.Fold
import Control.State
import Data.Effect
import Data.Exists
import Data.Time
import Data.View
import Browser

import Control.Concurrent (forkIO,killThread,newEmptyMVar,tryPutMVar,takeMVar,putMVar,readMVar)
import Control.Exception (SomeException,evaluate,handle,throw,onException,mask)
import Control.Monad (void,unless)
import Data.Function (fix)
import Data.Typeable (Typeable)
import System.IO.Unsafe (unsafePerformIO)

{-# INLINE suspense #-}
-- | Deep suspense. Crosses component boundaries via use of `prebuild`.
-- Works across, for example, nested `request`s.
--
-- Suppose you have a primary request whose response dictates further nested
-- requests. With `suspense`, it is possible to apply suspense to the full 
-- view, including the secondary requests, as if it were a unified request.
--
-- In the following example, a suspense view will render after 100ms unless
-- all requests have resolved:
--
-- > suspense (Milliseconds 100 0) "Loading" do
-- >   request @SomeDomain someCachingPolicy someAPI someReq someReqPayload
-- >     Div <||>
-- >       [ request @SomeDomain someCachingPolicy' someAPI' someReq' x do
-- >           { ... await :: someRsp' ... }
-- >       | x <- await :: [someReqPayload']
-- >       ]
--
-- To bypass deep suspense, see `Effect.Fork.fork`, which initially renders
-- a `Null` and then subsequently replaces it with the desired view.
--
-- Only the actions of `build` are subject to suspense. The component threads
-- generated in build are forked and, therefore, not subject to suspense.
-- Therefore, suspense includes `onConstruct`, `onMount`, and the initial 
-- `render`/nested `build`.
--
-- Note that `suspense` is not reactive. Once it is rendered, only changes
-- to the supplied target view will be seen, without added suspense.
--
suspense :: Time -> View -> View -> View
suspense t sus = suspense' True [(t,sus)]

{-# INLINE anticipation #-}
-- | Shallow suspense. Does not cross component boundaries. 
-- Useful for fine-grained suspense of `await` from Effect.Async.
-- 
-- Importantly, the following will not work and requires `suspense`:
--
-- > anticipation 0 "Loading" do
-- >   request @SomeDomain someCachingPolicy someAPI someReq somePL do
-- >     { ... await ... }
--
-- But, this could work (assuming no nested component boundaries in the body):
--
-- > request @SomeDomain someCachingPolicy someAPI someReq somePL do
-- >   anticipation 0 "Loading" do
-- >     { ... await ... }
--
-- Note that `anticipation` is not reactive. Once it is rendered, only changes
-- to the supplied target view will be seen, without added suspense.
--
anticipation :: Time -> View -> View -> View
anticipation t sus = suspense' False [(t,sus)]

{-# INLINE suspense' #-}
-- | suspense' allows for a sequence of replacing transclusions at timed
-- intervals while a target `View` is being built. This approach is likely to
-- cause bugs, especially in the presence of rich/dynamic views. If `deep`, the
-- view is built with `prebuild`, if not `deep`, the view is simply forced with
-- `evaluate`.
--
-- Note that `supsense'` is not reactive. Once it is rendered, only changes to
-- the supplied target view will be seen, without added suspense. It isn't
-- immediately obvious how to make `suspense'` deeply reactive with the
-- `prebuild` strategy without incurring a severe performance cost.
--
suspense' :: Bool -> [(Time,View)] -> View -> View
suspense' deep tvs v = stateWith' (\_ -> pure) initialize it
  where
    initialize :: Modify View => IO (View,View -> IO ())
    initialize = do
      mv <- newEmptyMVar

      t1 <- 
        forkIO do
          flip fix tvs $ \k -> \case
            (t,v):tvs ->
              timeout t (readMVar mv) >>= \case
                Just x -> put x
                _      -> put v >> k tvs
            _ -> readMVar mv >>= put

      t2 <- 
        forkIO do
          void do
            -- Mask exceptions while building so we can correctly
            -- cleanup the prebuilt view in the case that suspense
            -- is unmounted and a killThread is sent here.
            mask $ \restore -> do
              -- `prebuild` is how deep suspense is achieved. If we instead
              -- just used `evaluate`, the initialization methods of 
              -- components would not be run and suspense would only apply 
              -- to any `unsafePerformIO` or similar in the view preimage.
              -- That would still be useful inside an `async` when `await`
              -- is called, but it is much less interesting than the deep 
              -- suspense achieved with prebuild.
              x <- if deep then prebuild v else evaluate v
              restore (putMVar mv x) `onException` (if deep then cleanup x else pure ())

      pure (Null,const (killThread t1 >> killThread t2))

-- | Delay the materialization of a value. This can be useful for creating 
-- suspense where it might not always exist. Some UIs need to be slowed 
-- down to improve the user experience or to introduce consistency across
-- devices, and careful use of `delayed` is a simple primitive that can
-- serve that purpose. Note that `delayed` is synchronous! The delays 
-- introduced from multiple `delayed` calls within a view will be
-- additive as they do not overlap. 
-- 
-- The following is guaranteed to show `spinner` for at least 500ms:
--
-- > post slug =
-- >   suspense 500 spinner do
-- >     delayed 1000 do
-- >       let
-- >         Post { author } = get slug
-- >         Author { realName, bio } = get author
-- >       {...}
--
{-# INLINE delayed #-}
delayed :: Time -> a -> a
delayed t a = unsafePerformIO do
  mv <- newEmptyMVar
  forkIO (evaluate a >>= putMVar mv)
  delay t
  takeMVar mv
