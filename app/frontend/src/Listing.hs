{-# language AllowAmbiguousTypes, NoMonomorphismRestriction #-}
{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
module Listing where

import Control.Log (Logging)
import Pure.Magician
import Pure.Magician.Client
import Control.Concurrent.Async (forConcurrently)

recent :: forall domain resource. _ => Logging => Int -> Context resource -> IO [(Context resource,Name resource,Preview resource)]
recent n ctx = fmap catMaybes do
  cns <- req @domain Cached (analyticsAPI @resource) (listRecentForContext @resource) ctx
  forConcurrently (take n cns) \(c,n) -> do
    mp <- req @domain Cached (readingAPI @resource) readPreview (c,n) 
    for mp \p -> pure (c,n,p)

top :: forall domain resource. _ => Logging => Int -> Context resource -> IO [(Context resource,Name resource,Preview resource)] 
top n ctx = fmap catMaybes do
  cns <- req @domain Cached (analyticsAPI @resource) (listTopForContext @resource) ctx
  forConcurrently (take n cns) \(c,n) -> do
    mp <- req @domain Cached (readingAPI @resource) readPreview (c,n) 
    for mp \p -> pure (c,n,p)

related :: forall domain resource. _ => Logging => Int -> Context resource -> Name resource -> IO [(Context resource,Name resource,Preview resource)] 
related n ctx nm = fmap catMaybes do
  ps <- req @domain Cached (analyticsAPI @resource) (listRelatedPopularForResource @resource) (ctx,nm)
  forConcurrently (take n ps) \p -> do
    mr <- route (fromPath @(SomeRoute domain)) p 
    case mr of
      Just (_,sr) | Just (ReadR c n) <- fromSomeRoute @domain sr -> do
        mp <- req @domain Cached (readingAPI @resource) readPreview (c,n) 
        for mp \p -> pure (c,n,p)
      _ -> 
        pure Nothing
 